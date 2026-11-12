# PROC SQL execution: queries (FROM / joins / WHERE / GROUP BY / HAVING /
# SELECT / DISTINCT / ORDER BY / set operations) and statements (SELECT,
# CREATE TABLE/VIEW, INSERT, UPDATE, DELETE, DROP, ALTER).
#
# Scalar expressions are evaluated in R with the DATA step runtime
# (sql_expr.R); DuckDB computes join candidate pairs and grouped aggregates
# (sql_runtime.R). Result rows follow SAS where SAS defines the order (ORDER
# BY, DISTINCT, GROUP BY, set operators without ALL, single-table scans);
# results of joins without ORDER BY are flagged as unordered.

# Relations -----------------------------------------------------------------------

sql_blank <- function(c, n) if (c$type == "char") rep(strrep(" ", c$len), n) else rep(NA_real_, n)

sql_rel_rows <- function(rel, idx) {
  d <- rel$data[idx, , drop = FALSE]
  if (anyNA(idx)) {
    miss <- is.na(idx)
    for (c in rel$cols) if (c$type == "char") d[[c$id]][miss] <- strrep(" ", c$len)
  }
  rownames(d) <- NULL
  list(data = d, cols = rel$cols, ordered = rel$ordered, src = rel$src)
}

sql_empty_rel <- function(n = 1L) list(data = tibble::tibble(.rows = n), cols = list(), ordered = TRUE)

# A WORK data set (or view) as a relation, with data set options applied.
sql_table_rel <- function(src, eng, outer) {
  name <- src$name
  quals <- src$alias %||% name
  if (!is.null(eng$views[[name]])) {
    res <- sql_query(eng$views[[name]], eng)
    res$cols <- lapply(res$cols, function(c) {
      c$quals <- quals
      c
    })
    return(res)
  }
  d <- get0(name, envir = eng$wenv, inherits = FALSE)
  if (is.null(d)) abort_runtime(paste0("File WORK.", toupper(name), ".DATA does not exist."), src$line)
  m <- eng$meta[[name]] %||% meta_from_df(d)
  o <- src$options
  if (!is.null(o$in_var)) abort_unsupported("IN= in PROC SQL", src$line)
  vars <- names(d)
  if (!is.null(o$keep)) vars <- intersect(vars, resolve_ds_items(o$keep, vars, src$line))
  if (!is.null(o$drop)) vars <- setdiff(vars, resolve_ds_items(o$drop, vars, src$line))
  out_names <- vars
  if (!is.null(o$rename)) out_names[match(names(o$rename), vars)] <- unname(o$rename)
  inobs <- eng$sql_opts$inobs
  cols <- list()
  data <- list()
  for (j in seq_along(vars)) {
    v <- vars[j]
    id <- sql_new_id(eng)
    char <- is.character(d[[v]])
    cols[[j]] <- list(id = id, name = out_names[j], disp = if (out_names[j] == v) (m$disp[[v]] %||% v) else out_names[j],
                      quals = unique(c(quals, name)), type = if (char) "char" else "num",
                      len = if (char) (m$len[[v]] %||% max(1L, nchar(d[[v]]))) else (m$len[[v]] %||% 8L),
                      format = m$formats[[v]], informat = m$informats[[v]], label = m$labels[[v]], src_table = name)
    data[[id]] <- d[[v]]
  }
  # `src`: the single data set read in its stored order, with its sort flag
  # (PROC SQL keeps that order within GROUP BY groups only when the data set
  # is flagged as sorted by the group columns)
  rel <- list(data = tibble::as_tibble(data, .rows = nrow(d)), cols = cols, ordered = TRUE,
              src = list(name = name, sortedby = m$sortedby,
                         var_of = stats::setNames(vars, vapply(cols, `[[`, "", "id"))))
  if (!is.null(o$where)) {
    ctx <- sql_ctx(eng, rel)
    keep <- sas_true(sql_eval(sql_from_ds_expr(o$where, rel), ctx))
    rel <- sql_rel_rows(rel, which(keep))
  }
  if (!is.null(o$firstobs) || !is.null(o$obs)) {
    n <- nrow(rel$data)
    from <- o$firstobs %||% 1
    to <- min(o$obs %||% n, n)
    rel <- sql_rel_rows(rel, seq_len(max(0, to - from + 1)) + from - 1)
  }
  if (!is.null(inobs) && nrow(rel$data) > inobs) rel <- sql_rel_rows(rel, seq_len(inobs))
  rel
}

# A WHERE= data set option is a DATA step expression: turn its variables into
# column references.
sql_from_ds_expr <- function(e, rel) {
  if (!is.list(e)) return(e)
  if (identical(e$node, "var")) return(list(node = "col", table = NULL, name = e$name, text = e$text, line = e$line))
  if (identical(e$node, "in")) {
    e$node <- "in_list"
  }
  for (k in names(e)) if (is.list(e[[k]])) e[[k]] <- sql_from_ds_expr(e[[k]], rel)
  e
}

sql_source_rel <- function(src, eng, outer) {
  switch(src$kind,
    table = sql_table_rel(src, eng, outer),
    subquery = {
      res <- sql_query(src$query, eng)
      quals <- src$alias %||% character()
      res$cols <- lapply(res$cols, function(c) {
        c$quals <- quals
        c$id <- c$id
        c
      })
      res
    },
    join = sql_join(src, eng, outer)
  )
}

sql_conjuncts <- function(e) {
  if (is.null(e)) return(list())
  if (e$node == "binary" && e$op == "and" && !isTRUE(e$paren)) return(c(sql_conjuncts(e$lhs), sql_conjuncts(e$rhs)))
  if (e$node == "binary" && e$op == "and") return(c(sql_conjuncts(e$lhs), sql_conjuncts(e$rhs)))
  list(e)
}

sql_and_all <- function(es) {
  if (length(es) == 0) return(NULL)
  Reduce(function(a, b) list(node = "binary", op = "and", lhs = a, rhs = b, line = a$line), es)
}

# Which side (l / r / both / none) the columns of an expression come from.
sql_expr_side <- function(e, lcols, rcols) {
  sides <- character()
  ok <- TRUE
  sql_walk(e, function(x) {
    if (x$node %in% c("subq", "exists", "in_subq", "quant", "calc")) ok <<- FALSE
    if (x$node == "col") {
      hl <- length(sql_col_match(lcols, x$name, x$table)) > 0
      hr <- length(sql_col_match(rcols, x$name, x$table)) > 0
      if (hl && hr) ok <<- FALSE
      if (hl) sides <<- c(sides, "l")
      if (hr) sides <<- c(sides, "r")
      if (!hl && !hr) ok <<- FALSE
    }
    TRUE
  })
  if (!ok) return("other")
  sides <- unique(sides)
  if (length(sides) == 0) "none" else if (length(sides) == 2) "both" else sides
}

# Join two relations. `conds` are the ON conjuncts (or pushed-down WHERE
# conjuncts for inner/cross joins).
sql_join_rels <- function(l, r, type, conds, eng, outer) {
  keys_l <- list()
  keys_r <- list()
  residual <- list()
  lctx <- sql_ctx(eng, l, outer)
  rctx <- sql_ctx(eng, r, outer)
  for (cnd in conds) {
    if (cnd$node == "compare" && cnd$op == "=" && !isTRUE(cnd$colon)) {
      sl <- sql_expr_side(cnd$lhs, l$cols, r$cols)
      sr <- sql_expr_side(cnd$rhs, l$cols, r$cols)
      pair <- NULL
      if (sl == "l" && sr == "r") pair <- list(cnd$lhs, cnd$rhs)
      if (sl == "r" && sr == "l") pair <- list(cnd$rhs, cnd$lhs)
      if (!is.null(pair)) {
        kl <- sql_eval(pair[[1]], lctx)
        kr <- sql_eval(pair[[2]], rctx)
        if (is.character(kl) != is.character(kr)) {
          abort_syntax("Expression using equals (=) has components that are of different data types.", cnd$line)
        }
        keys_l <- c(keys_l, list(kl))
        keys_r <- c(keys_r, list(kr))
        next
      }
    }
    residual <- c(residual, list(cnd))
  }
  nl <- nrow(l$data)
  nr <- nrow(r$data)
  if (length(keys_l) > 0) {
    pr <- sql_duck_pairs(eng, keys_l, keys_r)
  } else {
    pr <- list(l = rep(seq_len(nl), each = nr), r = rep(seq_len(nr), times = nl))
  }
  joined <- function(li, ri) {
    a <- sql_rel_rows(l, li)
    b <- sql_rel_rows(r, ri)
    list(data = dplyr::bind_cols(a$data, b$data), cols = c(l$cols, r$cols), ordered = FALSE)
  }
  if (length(residual) > 0 && length(pr$l) > 0) {
    cand <- joined(pr$l, pr$r)
    keep <- sas_true(sql_eval(sql_and_all(residual), sql_ctx(eng, cand, outer)))
    pr <- list(l = pr$l[keep], r = pr$r[keep])
  }
  li <- pr$l
  ri <- pr$r
  if (type %in% c("left", "full")) {
    miss <- setdiff(seq_len(nl), li)
    li <- c(li, miss)
    ri <- c(ri, rep(NA_integer_, length(miss)))
    o <- order(li, method = "radix")
    li <- li[o]
    ri <- ri[o]
  }
  if (type %in% c("right", "full")) {
    miss <- setdiff(seq_len(nr), ri)
    li <- c(li, rep(NA_integer_, length(miss)))
    ri <- c(ri, miss)
    if (type == "right") {
      o <- order(ri, method = "radix")
      li <- li[o]
      ri <- ri[o]
    }
  }
  joined(li, ri)
}

sql_join <- function(src, eng, outer) {
  l <- sql_source_rel(src$lhs, eng, outer)
  r <- sql_source_rel(src$rhs, eng, outer)
  sql_join_rels(l, r, src$type, sql_conjuncts(src$on), eng, outer)
}

# FROM: the joined relation and the WHERE conjuncts not used as join keys.
sql_from <- function(from, where, eng, outer) {
  if (length(from) == 0) return(list(rel = sql_empty_rel(1L), where = where))
  rels <- lapply(from, sql_source_rel, eng = eng, outer = outer)
  rel <- rels[[1]]
  if (length(from) > 1 || from[[1]]$kind == "join") {
    rel$ordered <- FALSE
    rel$src <- NULL
  }
  rest <- sql_conjuncts(where)
  for (k in seq_along(rels)[-1]) {
    r <- rels[[k]]
    use <- vapply(rest, function(cnd) {
      cnd$node == "compare" && cnd$op == "=" && !isTRUE(cnd$colon) &&
        setequal(c(sql_expr_side(cnd$lhs, rel$cols, r$cols), sql_expr_side(cnd$rhs, rel$cols, r$cols)), c("l", "r"))
    }, logical(1))
    rel <- sql_join_rels(rel, r, "inner", rest[use], eng, outer)
    rest <- rest[!use]
  }
  list(rel = rel, where = sql_and_all(rest))
}

# Queries ---------------------------------------------------------------------------

#' @noRd
sql_query <- function(q, eng, outer = NULL) {
  res <- if (q$node == "setop") sql_setop(q, eng, outer) else sql_select(q, eng, outer)
  if (q$node == "setop" && length(q$order) > 0) res <- sql_order_result(res, q$order, q$line)
  res
}

sql_item_name <- function(it) {
  if (!is.null(it$alias)) return(list(name = it$alias, disp = it$alias_text %||% it$alias))
  if (it$expr$node == "col") return(NULL)
  list(name = "", disp = "")
}

sql_expand_items <- function(items, rel) {
  out <- list()
  for (it in items) {
    if (!isTRUE(it$star)) {
      out <- c(out, list(it))
      next
    }
    cols <- if (is.null(it$table)) rel$cols else Filter(function(c) it$table %in% c$quals, rel$cols)
    if (!is.null(it$table) && length(cols) == 0) abort_syntax(paste0("Unresolved reference to table/correlation name ", it$table, "."), it$line)
    for (c in cols) {
      out <- c(out, list(list(star = FALSE, expr = list(node = "col", table = NULL, name = c$name, text = c$disp, line = it$line, id = c$id),
                              alias = NULL, line = it$line, from_star = TRUE)))
    }
  }
  out
}

# Columns referenced outside summary functions that are not GROUP BY items
# (they make the query remerge).
sql_col_id <- function(x, ctx) {
  if (!is.null(x$id)) return(x$id)
  m <- sql_col_match(ctx$rel$cols, x$name, x$table)
  if (length(m) == 1) ctx$rel$cols[[m]]$id else NA_character_
}

# SAS decides by column: an item is a group key only when it is a GROUP BY
# column, or the GROUP BY names it (alias, CALCULATED or position); a GROUP BY
# expression does not make an identical SELECT expression a group key.
sql_needs_remerge <- function(e, group, ctx, item_k = NULL) {
  if (is.null(e)) return(FALSE)
  gitems <- unlist(lapply(group, attr, "item"))
  if (!is.null(item_k) && item_k %in% gitems) return(FALSE)
  galias <- unlist(lapply(ctx$items[gitems], `[[`, "alias"))
  gids <- unlist(lapply(group, function(g) if (g$node == "col") sql_col_id(g, ctx)))
  hit <- FALSE
  sql_walk(e, function(x) {
    if (hit) return(FALSE)
    if (x$node == "call" && is_sql_agg(x)) return(FALSE)
    if (x$node == "col") {
      id <- sql_col_id(x, ctx)
      if (!is.na(id) && !id %in% gids) hit <<- TRUE
    }
    if (x$node == "calc") {
      if (x$name %in% galias) return(FALSE)
      it <- sql_find_item(ctx, x$name)
      if (!is.null(it) && sql_needs_remerge(it$expr, group, ctx)) hit <<- TRUE
      return(FALSE)
    }
    TRUE
  })
  hit
}

# Resolve a GROUP BY / ORDER BY item that is a position or a SELECT alias.
sql_resolve_item_ref <- function(e, items, rel) {
  item <- function(k) structure(items[[k]]$expr, item = k)
  if (e$node == "num") {
    k <- as.integer(e$value)
    if (k < 1 || k > length(items)) abort_syntax(paste0("Invalid column position ", k, "."), e$line)
    return(item(k))
  }
  alias <- NULL
  if (e$node == "col" && is.null(e$table) && length(sql_col_match(rel$cols, e$name)) == 0) alias <- e$name
  if (e$node == "calc") alias <- e$name
  if (!is.null(alias)) {
    for (k in seq_along(items)) if (identical(items[[k]]$alias, alias)) return(item(k))
  }
  e
}

sql_select <- function(q, eng, outer) {
  fr <- sql_from(q$from, q$where, eng, outer)
  rel <- fr$rel
  ctx <- sql_ctx(eng, rel, outer, items = q$items)
  qt <- ctx$qt
  if (!is.null(fr$where)) {
    keep <- sas_true(sql_eval(fr$where, ctx))
    rel <- sql_rel_rows(rel, which(keep))
    ctx <- sql_new_rows(ctx, rel)
  }
  items <- sql_expand_items(q$items, rel)
  ctx$items <- items
  group <- lapply(q$group, sql_resolve_item_ref, items = items, rel = rel)
  order <- q$order
  agg_exprs <- c(lapply(items, `[[`, "expr"), list(q$having))
  aggs <- list()
  for (e in agg_exprs) if (!is.null(e)) aggs <- c(aggs, sql_find_aggs(e))
  for (o in order) {
    if (sql_has_agg(o$expr)) abort_syntax("Summary functions are restricted to the SELECT and HAVING clauses only.", o$expr$line)
  }
  aggs <- aggs[!duplicated(names(aggs))]
  if (length(group) > 0 && length(aggs) == 0) {
    # GROUP BY without a summary function acts as ORDER BY
    order <- c(lapply(group, function(g) list(expr = g, desc = FALSE)), order)
    group <- list()
  }
  grouped <- length(aggs) > 0
  remerge <- FALSE
  if (grouped) {
    n <- nrow(rel$data)
    if (length(group) > 0) {
      raw <- lapply(group, function(g) sql_eval(g, ctx))
      kn <- paste0("k", seq_along(raw))
      kdf <- as.data.frame(stats::setNames(lapply(raw, sql_norm_key), kn), stringsAsFactors = FALSE)
      gid <- vctrs::vec_group_id(kdf)
      G <- attr(gid, "n")
      first <- match(seq_len(G), gid)
      rdf <- tibble::as_tibble(stats::setNames(lapply(raw, function(x) x[first]), kn))
      gorder <- sas_order(rdf, kn)
      rank <- integer(G)
      rank[gorder] <- seq_len(G)
      gid <- rank[gid]
      first <- first[gorder]
    } else {
      gid <- rep(1L, n)
      G <- 1L
      first <- if (n > 0) 1L else NA_integer_
    }
    specs <- lapply(aggs, function(a) {
      star <- identical(a$args[[1]]$node, "star")
      if (star && !a$name %in% c("count", "freq", "n")) abort_syntax(paste0("'*' is not valid in ", toupper(a$name), "()"), a$line)
      arg <- if (star) NULL else sql_eval(a$args[[1]], ctx)
      # MIN/MAX keep the type of their argument; the other statistics are numeric
      char <- !is.null(arg) && is.character(arg) && a$name %in% c("min", "max")
      list(fn = if (a$name == "mean") "avg" else a$name, arg = arg, distinct = isTRUE(a$distinct),
           type = if (char) "char" else "num",
           len = if (char) sql_expr_type(a$args[[1]], ctx)$len else 8L)
    })
    if (n == 0 && length(group) == 0) {
      vals <- lapply(specs, function(s) if (s$fn %in% c("count", "freq", "n", "nmiss")) 0 else if (s$type == "char") NA_character_ else NA_real_)
    } else {
      vals <- sql_duck_aggregate(eng, gid, G, specs, order_known = sql_group_order_known(rel, group, ctx), line = q$line)
    }
    for (k in seq_along(items)) if (sql_needs_remerge(items[[k]]$expr, group, ctx, item_k = k)) remerge <- TRUE
    if (sql_needs_remerge(q$having, group, ctx)) remerge <- TRUE
    if (remerge) {
      ord <- if (length(group) > 0) order(gid, seq_len(n), method = "radix") else seq_len(n)
      rel <- sql_rel_rows(rel, ord)
      gid <- gid[ord]
      rows_gid <- gid
      sql_note(eng, "NOTE: The query requires remerging summary statistics back with the original data.")
      # SAS does not keep the input order within a group when it remerges
      if (length(group) > 0) rel$ordered <- FALSE
    } else {
      rel <- sql_rel_rows(rel, first)
      rows_gid <- seq_len(G)
    }
    agg_map <- list()
    for (k in seq_along(aggs)) {
      id <- sql_new_id(eng, ".a")
      v <- vals[[k]][rows_gid]
      s <- specs[[k]]
      if (s$type == "char") v <- sas_char(ifelse(is.na(v), "", v), s$len)
      rel$data[[id]] <- v
      rel$cols <- c(rel$cols, list(list(id = id, name = id, disp = id, quals = character(), type = s$type,
                                        len = s$len, hidden = TRUE)))
      agg_map[[names(aggs)[k]]] <- list(id = id)
    }
    ctx <- sql_ctx(eng, rel, outer, items = items, allow_alias = TRUE)
    ctx$qt <- qt
    ctx$aggs <- agg_map
    if (!is.null(q$having)) {
      keep <- sas_true(sql_eval(q$having, ctx))
      rel <- sql_rel_rows(rel, which(keep))
      ctx <- sql_new_rows(ctx, rel)
    }
  } else if (!is.null(q$having)) {
    ctx$allow_alias <- TRUE
    keep <- sas_true(sql_eval(q$having, ctx))
    rel <- sql_rel_rows(rel, which(keep))
    ctx <- sql_new_rows(ctx, rel)
  }
  # SELECT list
  out_cols <- list()
  out_data <- list()
  for (it in items) {
    e <- it$expr
    v <- sql_eval(e, ctx)
    ty <- sql_expr_type(e, ctx)
    id <- sql_new_id(eng)
    col <- list(id = id, name = "", disp = "", quals = character(), type = ty$type, len = ty$len)
    if (e$node == "col") {
      sid <- sql_col_id(e, ctx)
      src <- if (is.na(sid)) NULL else Find(function(c) identical(c$id, sid), ctx$rel$cols)
      if (!is.null(src)) {
        col$name <- src$name
        col$disp <- src$disp
        col$format <- src$format
        col$informat <- src$informat
        col$label <- src$label
        col$len <- src$len
      }
    }
    nm <- sql_item_name(it)
    if (!is.null(nm)) {
      col$name <- nm$name
      col$disp <- nm$disp
    }
    for (a in c("format", "informat", "label")) if (!is.null(it[[a]])) col[[a]] <- it[[a]]
    if (!is.null(it$length)) col$len <- it$length
    # a computed character column whose DATA step length is the 200 default
    # takes the width of its longest value
    const_args <- e$node == "call" && length(e$args) > 0 &&
      all(vapply(e$args, function(a) a$node %in% c("num", "str", "missing"), logical(1)))
    if (ty$type == "char" && is.null(it$length) && const_args && e$name %in% sql_fold_fns) {
      # constant arguments: SAS evaluates the call once; the column has the length of the value (SAS 9.4)
      col$len <- max(1L, nchar(v[1] %||% ""))
    } else if (ty$type == "char" && is.null(it$length) && const_args && identical(fn_table[[e$name]]$len, 200L) &&
               e$name %in% sql_200_fns) {
      col$len <- 200L
    } else if (ty$type == "char" && is.null(it$length) && e$node == "call" && identical(fn_table[[e$name]]$len, 200L)) {
      col$len <- max(1L, nchar(sas_rtrim(v)))
    }
    if (ty$type == "char") v <- sas_char(v, col$len)
    out_cols <- c(out_cols, list(col))
    out_data[[id]] <- v
  }
  out <- list(data = tibble::as_tibble(out_data, .rows = nrow(rel$data)), cols = out_cols,
              ordered = isTRUE(rel$ordered) || (grouped && !remerge))
  # ORDER BY keys are computed on the pre-projection rows
  okeys <- NULL
  if (length(order) > 0) {
    ctx$allow_alias <- TRUE
    okeys <- lapply(order, function(o) {
      e <- o$expr
      if (e$node == "num") {
        k <- as.integer(e$value)
        if (k < 1 || k > length(out_cols)) abort_syntax(paste0("ORDER BY item ", k, " is out of range."), e$line)
        return(out$data[[out_cols[[k]]$id]])
      }
      if (e$node == "col" && is.null(e$table)) {
        hit <- which(vapply(out_cols, function(c) identical(c$name, e$name), logical(1)))
        if (length(hit) > 0) return(out$data[[out_cols[[hit[1]]]$id]])
      }
      sql_eval(e, ctx)
    })
  }
  if (isTRUE(q$distinct)) {
    keyd <- as.data.frame(lapply(out$data, sql_norm_key), stringsAsFactors = FALSE)
    if (is.null(okeys)) {
      o <- if (ncol(out$data) > 0) sas_order(out$data, names(out$data)) else seq_len(nrow(out$data))
      out <- sql_rel_rows(out, o)
      keyd <- keyd[o, , drop = FALSE]
    }
    dup <- duplicated(keyd)
    out <- sql_rel_rows(out, which(!dup))
    if (!is.null(okeys)) okeys <- lapply(okeys, function(k) k[!dup])
    out$ordered <- TRUE
  }
  if (!is.null(okeys)) {
    kd <- as.data.frame(stats::setNames(okeys, paste0("k", seq_along(okeys))), stringsAsFactors = FALSE)
    o <- sas_order(kd, names(kd), vapply(order, function(x) isTRUE(x$desc), logical(1)))
    out <- sql_rel_rows(out, o)
    out$ordered <- TRUE
  }
  out$used_outer <- isTRUE(qt$used_outer)
  out
}

# Does PROC SQL read the rows of each group in the order of `rel`?  Without
# GROUP BY it reads a single table in its stored order; with GROUP BY it sorts
# the rows (not stably) unless the data set is flagged as sorted by the group
# columns (SAS gold tests/conformance/proc_stats/sql_order_probe).
sql_group_order_known <- function(rel, group, ctx) {
  if (!isTRUE(rel$ordered)) return(FALSE)
  if (length(group) == 0) return(TRUE)
  sb <- rel$src$sortedby
  if (is.null(sb) || length(group) > length(sb$vars) || any(sb$descending[seq_along(group)])) return(FALSE)
  ids <- vapply(group, function(g) if (identical(g$node, "col")) sql_col_id(g, ctx) else NA_character_, "")
  if (anyNA(ids)) return(FALSE)
  identical(tolower(unname(rel$src$var_of[ids])), tolower(sb$vars[seq_along(group)]))
}

sql_new_rows <- function(ctx, rel) {
  c2 <- sql_ctx(ctx$eng, rel, ctx$outer, items = ctx$items, allow_alias = ctx$allow_alias)
  c2$aggs <- ctx$aggs
  c2$qt <- ctx$qt
  c2
}

sql_order_result <- function(res, order, line) {
  keys <- lapply(order, function(o) {
    e <- o$expr
    k <- NA_integer_
    if (e$node == "num") k <- as.integer(e$value)
    if (e$node == "col") k <- which(vapply(res$cols, function(c) identical(c$name, e$name), logical(1)))[1]
    if (is.na(k) || k < 1 || k > length(res$cols)) abort_syntax("ORDER BY items of a set operation must be result columns or positions.", line)
    res$data[[res$cols[[k]]$id]]
  })
  kd <- as.data.frame(stats::setNames(keys, paste0("k", seq_along(keys))), stringsAsFactors = FALSE)
  o <- sas_order(kd, names(kd), vapply(order, function(x) isTRUE(x$desc), logical(1)))
  res <- sql_rel_rows(res, o)
  res$ordered <- TRUE
  res
}

# Set operations ----------------------------------------------------------------------

sql_setop <- function(q, eng, outer) {
  a <- sql_query(q$lhs, eng, outer)
  b <- sql_query(q$rhs, eng, outer)
  used <- isTRUE(a$used_outer) || isTRUE(b$used_outer)
  if (q$op == "outer union") {
    res <- sql_outer_union(a, b, q, eng)
    res$used_outer <- used
    return(res)
  }
  if (q$corr) {
    common <- intersect(vapply(a$cols, `[[`, "", "name"), vapply(b$cols, `[[`, "", "name"))
    pick <- function(r) {
      k <- match(common, vapply(r$cols, `[[`, "", "name"))
      list(data = r$data[vapply(r$cols[k], `[[`, "", "id")], cols = r$cols[k])
    }
    a <- c(pick(a), ordered = TRUE)
    b <- c(pick(b), ordered = TRUE)
  }
  if (length(a$cols) != length(b$cols)) {
    # the narrower contributor is extended with missing columns
    widen <- function(r, other) {
      for (k in seq(length(r$cols) + 1L, length(other$cols))) {
        c <- other$cols[[k]]
        c$id <- sql_new_id(eng)
        r$data[[c$id]] <- sql_blank(c, nrow(r$data))
        r$cols <- c(r$cols, list(c))
      }
      r
    }
    if (length(a$cols) < length(b$cols)) a <- widen(a, b) else b <- widen(b, a)
    sql_note(eng, sprintf("WARNING: A table has been extended with null columns to perform the %s set operation.", toupper(q$op)))
  }
  cols <- a$cols
  for (k in seq_along(cols)) {
    if (a$cols[[k]]$type != b$cols[[k]]$type) {
      abort_syntax(paste0("Column ", k, " from the first contributor of ", toupper(q$op), " is not the same type as its counterpart from the second."), q$line)
    }
    if (cols[[k]]$type == "char") cols[[k]]$len <- max(a$cols[[k]]$len, b$cols[[k]]$len)
    if (!nzchar(cols[[k]]$name)) {
      cols[[k]]$name <- b$cols[[k]]$name
      cols[[k]]$disp <- b$cols[[k]]$disp
    }
  }
  ids <- vapply(cols, `[[`, "", "id")
  norm <- function(r) {
    d <- r$data
    names(d) <- ids
    for (k in seq_along(cols)) if (cols[[k]]$type == "char") d[[ids[k]]] <- sas_char(sas_rtrim(d[[ids[k]]]), cols[[k]]$len)
    d
  }
  da <- norm(a)
  db <- norm(b)
  keyof <- function(d) do.call(paste, c(lapply(d, function(x) {
    x <- sql_norm_key(x)
    ifelse(is.na(x), "\001NA", as.character(x))
  }), sep = "\002"))
  ka <- if (ncol(da) > 0) keyof(da) else rep("", nrow(da))
  kb <- if (ncol(db) > 0) keyof(db) else rep("", nrow(db))
  d <- switch(q$op,
    union = dplyr::bind_rows(da, db),
    except = if (q$all) {
      cnt <- table(kb)
      occ <- stats::ave(seq_along(ka), ka, FUN = seq_along)
      lim <- as.integer(cnt[ka])
      lim[is.na(lim)] <- 0L
      da[occ > lim, , drop = FALSE]
    } else {
      da[!ka %in% kb, , drop = FALSE]
    },
    intersect = if (q$all) {
      cnt <- table(kb)
      occ <- stats::ave(seq_along(ka), ka, FUN = seq_along)
      lim <- as.integer(cnt[ka])
      lim[is.na(lim)] <- 0L
      da[occ <= lim, , drop = FALSE]
    } else {
      da[ka %in% kb, , drop = FALSE]
    }
  )
  res <- list(data = tibble::as_tibble(d), cols = cols, ordered = TRUE)
  if (!q$all) {
    if (ncol(d) > 0 && nrow(d) > 0) {
      res <- sql_rel_rows(res, sas_order(res$data, names(res$data)))
      k <- if (ncol(res$data) > 0) keyof(res$data) else character()
      res <- sql_rel_rows(res, which(!duplicated(k)))
    }
  } else if (q$op != "union") {
    res$ordered <- isTRUE(a$ordered)
  } else {
    res$ordered <- isTRUE(a$ordered) && isTRUE(b$ordered)
  }
  res$used_outer <- used
  res
}

sql_outer_union <- function(a, b, q, eng) {
  if (!q$corr) {
    cols <- c(a$cols, b$cols)
    n1 <- nrow(a$data)
    n2 <- nrow(b$data)
    d1 <- a$data
    d2 <- b$data
    for (c in b$cols) d1[[c$id]] <- sql_blank(c, n1)
    for (c in a$cols) d2[[c$id]] <- sql_blank(c, n2)
    return(list(data = dplyr::bind_rows(d1, d2[names(d1)]), cols = cols, ordered = TRUE))
  }
  an <- vapply(a$cols, `[[`, "", "name")
  bn <- vapply(b$cols, `[[`, "", "name")
  cols <- a$cols
  for (k in seq_along(b$cols)) if (!bn[k] %in% an) cols <- c(cols, list(b$cols[[k]]))
  ids <- vapply(cols, `[[`, "", "id")
  part <- function(r, rn) {
    d <- list()
    for (c in cols) {
      j <- match(c$name, rn)
      if (is.na(j)) {
        d[[c$id]] <- sql_blank(c, nrow(r$data))
      } else {
        if (r$cols[[j]]$type != c$type) abort_syntax("OUTER UNION CORR columns of the same name must have the same type.", q$line)
        d[[c$id]] <- r$data[[r$cols[[j]]$id]]
      }
    }
    tibble::as_tibble(d, .rows = nrow(r$data))
  }
  for (k in seq_along(cols)) {
    if (cols[[k]]$type == "char") {
      j <- match(cols[[k]]$name, bn)
      if (!is.na(j)) cols[[k]]$len <- max(cols[[k]]$len, b$cols[[j]]$len)
    }
  }
  d <- dplyr::bind_rows(part(a, an), part(b, bn))
  for (c in cols) if (c$type == "char") d[[c$id]] <- sas_char(sas_rtrim(d[[c$id]]), c$len)
  list(data = d, cols = cols, ordered = TRUE)
}

# PROC SQL lengths of calls with constant arguments (verified in the function
# matrix): the value's length, or the DATA step default of 200
sql_fold_fns <- c("cat", "strip", "trim", "compbl")
sql_200_fns <- c("tranwrd")
