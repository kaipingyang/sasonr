# PROC SQL expressions: name resolution, lowering to DATA step expression
# nodes, and vectorised evaluation with the DATA step runtime (so SAS missing
# values, trailing-blank comparison, functions and formats behave exactly as
# in the DATA step).
#
# A relation (`rel`) is list(data, cols): `data` is a data frame whose columns
# are named by internal ids (".c1", ".c2", ...); `cols` describes each column:
# list(id, name, disp, quals, type, len, format, informat, label).
# Character values are stored padded to their SAS length, as in WORK.

sql_agg_fns <- c("count", "freq", "n", "sum", "avg", "mean", "min", "max", "nmiss", "std", "var", "range",
                 "css", "uss", "cv", "stderr", "sumwgt", "t", "prt", "median")

sql_new_id <- function(eng, prefix = ".c") {
  eng$sql_id <- (eng$sql_id %||% 0L) + 1L
  paste0(prefix, eng$sql_id)
}

# A summary function call (one argument, COUNT(*), or DISTINCT).
is_sql_agg <- function(e) {
  e$node == "call" && e$name %in% sql_agg_fns &&
    (length(e$args) == 1 || isTRUE(e$distinct)) && !identical(e$args[[1]]$node, "omitted")
}

# Canonical text of a node (positions removed) used to match expressions.
sql_key <- function(e) {
  strip <- function(x) {
    if (!is.list(x)) return(x)
    x$line <- NULL
    x$col <- NULL
    x$paren <- NULL
    if (!is.null(x$text) && !is.null(x$node) && x$node == "col") x$text <- NULL
    lapply(x, strip)
  }
  paste(deparse(strip(e), width.cutoff = 500L), collapse = "")
}

# Walk an SQL expression without descending into subqueries.
sql_walk <- function(e, f) {
  if (is.null(e) || !is.list(e) || is.null(e$node)) return(invisible())
  if (isFALSE(f(e))) return(invisible())
  kids <- switch(e$node,
    unary = list(e$arg),
    binary = ,
    compare = ,
    contains = list(e$lhs, e$rhs),
    like = list(e$lhs, e$rhs, e$escape),
    between = list(e$lhs, e$lo, e$hi),
    is_missing = ,
    in_subq = ,
    quant = list(e$lhs),
    in_list = c(list(e$lhs), e$items),
    call = e$args,
    case = c(list(e$operand, e$else_), unlist(lapply(e$whens, function(w) list(w$cond, w$value)), recursive = FALSE)),
    list()
  )
  for (k in kids) sql_walk(k, f)
  invisible()
}

sql_find_aggs <- function(e) {
  out <- list()
  sql_walk(e, function(x) {
    if (x$node == "call" && is_sql_agg(x)) {
      out[[sql_key(x)]] <<- x
      return(FALSE)
    }
    TRUE
  })
  out
}

sql_has_agg <- function(e) length(sql_find_aggs(e)) > 0

# Context ---------------------------------------------------------------------

sql_ctx <- function(eng, rel, outer = NULL, items = NULL, allow_alias = FALSE) {
  ctx <- new.env(parent = emptyenv())
  ctx$eng <- eng
  ctx$rel <- rel
  ctx$outer <- outer
  ctx$items <- items
  ctx$allow_alias <- allow_alias
  ctx$aggs <- NULL
  ctx$row <- NA_integer_
  ctx$qt <- new.env(parent = emptyenv()) # per query level: used_outer
  ctx$qt$used_outer <- FALSE
  ctx$calc_stack <- character()
  ctx$env <- new.env(parent = asNamespace("sasonr"))
  ctx$st <- new_state(list())
  ctx$st$context <- "sql"
  for (c in rel$cols) reg_var(ctx$st, c$id, c$type, if (c$type == "char") c$len else NA_integer_, disp = c$disp)
  for (c in rel$cols) if (!is.null(c$format)) ctx$st$formats[[c$id]] <- c$format
  ctx
}

sql_nrow <- function(rel) nrow(rel$data)

sql_col_match <- function(cols, name, table = NULL) {
  which(vapply(cols, function(c) c$name == name && (is.null(table) || table %in% c$quals), logical(1)))
}

# Bind a scalar (from an outer row) as a new variable of the context.
sql_bind_scalar <- function(ctx, value, type, len) {
  id <- sql_new_id(ctx$eng, ".o")
  assign(id, value, envir = ctx$env)
  reg_var(ctx$st, id, type, if (type == "char") len else NA_integer_)
  list(node = "var", name = id, text = id)
}

sql_bind_vector <- function(ctx, value, type, len) {
  id <- sql_new_id(ctx$eng, ".q")
  assign(id, value, envir = ctx$env)
  reg_var(ctx$st, id, type, if (type == "char") len else NA_integer_)
  list(node = "var", name = id, text = id)
}

# Resolve a column reference: this query's FROM, then enclosing queries
# (correlated reference), then (where allowed) a SELECT alias.
sql_resolve_col <- function(e, ctx) {
  if (!is.null(e$id)) {
    # a column produced by SELECT * expansion
    for (c in ctx$rel$cols) if (identical(c$id, e$id)) return(list(node = "var", name = c$id, text = c$id))
  }
  hit <- sql_col_match(ctx$rel$cols, e$name, e$table)
  if (length(hit) > 1) {
    abort_syntax(paste0("Ambiguous reference, column ", e$text %||% e$name, " is in more than one table."), e$line)
  }
  if (length(hit) == 1) {
    c <- ctx$rel$cols[[hit]]
    return(list(node = "var", name = c$id, text = c$id))
  }
  if (isTRUE(ctx$allow_alias) && is.null(e$table)) {
    it <- sql_find_item(ctx, e$name)
    if (!is.null(it)) return(sql_lower(it$expr, ctx))
  }
  o <- ctx$outer
  path <- list(ctx)
  while (!is.null(o)) {
    hit <- sql_col_match(o$rel$cols, e$name, e$table)
    if (length(hit) > 1) abort_syntax(paste0("Ambiguous reference, column ", e$name, " is in more than one table."), e$line)
    if (length(hit) == 1) {
      c <- o$rel$cols[[hit]]
      # every query level between here and the owner of the column is correlated
      for (q in path) q$qt$used_outer <- TRUE
      v <- if (is.na(o$row)) (if (c$type == "char") strrep(" ", c$len) else NA_real_) else o$rel$data[[c$id]][o$row]
      return(sql_bind_scalar(ctx, v, c$type, c$len))
    }
    path <- c(path, list(o))
    o <- o$outer
  }
  abort_syntax(paste0("The following columns were not found in the contributing tables: ", e$text %||% e$name, "."), e$line)
}

sql_find_item <- function(ctx, name) {
  for (it in ctx$items) if (!isTRUE(it$star) && identical(it$alias, name)) return(it)
  NULL
}

# Lowering --------------------------------------------------------------------

sql_fn_alias <- c(lower = "lowcase", upper = "upcase")

sql_lower <- function(e, ctx) {
  L <- function(x) sql_lower(x, ctx)
  switch(e$node,
    col = sql_resolve_col(e, ctx),
    calc = {
      if (e$name %in% ctx$calc_stack) abort_syntax(paste0("recursive CALCULATED reference to ", e$name), e$line)
      it <- sql_find_item(ctx, e$name)
      if (is.null(it)) abort_syntax(paste0("The following columns were not found as CALCULATED references in the immediate query: ", e$name, "."), e$line)
      ctx$calc_stack <- c(ctx$calc_stack, e$name)
      on.exit(ctx$calc_stack <- utils::head(ctx$calc_stack, -1L))
      L(it$expr)
    },
    unary = {
      e$arg <- L(e$arg)
      e
    },
    binary = ,
    compare = ,
    contains = {
      e$lhs <- L(e$lhs)
      e$rhs <- L(e$rhs)
      if (e$node == "compare" && isTRUE(e$colon) && sql_type_of(e$lhs, ctx)$type != "char") {
        abort_syntax("Arguments for truncated compares must be strings.", e$line)
      }
      e
    },
    between = {
      e$lhs <- L(e$lhs)
      e$lo <- L(e$lo)
      e$hi <- L(e$hi)
      e
    },
    is_missing = {
      e$lhs <- L(e$lhs)
      e
    },
    like = {
      args <- list(L(e$lhs), L(e$rhs))
      if (!is.null(e$escape)) args <- c(args, list(L(e$escape)))
      out <- list(node = "call", name = "sqllike", args = args, line = e$line, construct = TRUE)
      if (isTRUE(e$negate)) out <- list(node = "unary", op = "not", arg = out, line = e$line)
      out
    },
    in_list = {
      lhs <- L(e$lhs)
      items <- lapply(e$items, L)
      if (all(vapply(items, function(i) i$node %in% c("num", "str", "missing"), logical(1)))) {
        return(list(node = "in", lhs = lhs, items = items, negate = isTRUE(e$negate), line = e$line))
      }
      out <- list(node = "call", name = "sqlinlist", args = c(list(lhs), items), line = e$line, construct = TRUE)
      if (isTRUE(e$negate)) out <- list(node = "unary", op = "not", arg = out, line = e$line)
      out
    },
    case = sql_lower_case(e, ctx),
    call = sql_lower_call(e, ctx),
    subq = ,
    exists = ,
    in_subq = ,
    quant = sql_lower_subquery(e, ctx),
    star = abort_syntax("'*' is only valid in COUNT(*)", e$line),
    e
  )
}

sql_type_of <- function(x, ctx) expr_type(an_expr(x, ctx$st), ctx$st)

sql_lower_case <- function(e, ctx) {
  op <- if (is.null(e$operand)) NULL else sql_lower(e$operand, ctx)
  args <- list()
  vals <- list()
  for (w in e$whens) {
    cond <- if (is.null(op)) sql_lower(w$cond, ctx) else {
      list(node = "compare", op = "=", colon = FALSE, lhs = op, rhs = sql_lower(w$cond, ctx), line = e$line)
    }
    v <- sql_lower(w$value, ctx)
    args <- c(args, list(cond, v))
    vals <- c(vals, list(v))
  }
  els <- if (is.null(e$else_)) NULL else sql_lower(e$else_, ctx)
  types <- lapply(c(vals, if (!is.null(els)) list(els)), sql_type_of, ctx = ctx)
  tt <- unique(vapply(types, `[[`, "", "type"))
  if (length(tt) > 1) abort_syntax("Result of WHEN clause is not the same data type as the preceding results.", e$line)
  len <- max(vapply(types, function(t) as.integer(t$len), 1L))
  if (is.null(els)) els <- if (tt == "char") list(node = "str", value = "", line = e$line) else list(node = "missing", code = ".", line = e$line)
  list(node = "call", name = "sqlcase", args = c(args, list(els)), line = e$line, construct = TRUE,
       sql_type = list(type = tt, len = if (tt == "char") len else 8L))
}

sql_lower_call <- function(e, ctx) {
  if (is_sql_agg(e)) {
    key <- sql_key(e)
    if (is.null(ctx$aggs) || is.null(ctx$aggs[[key]])) {
      abort_syntax("Summary functions are restricted to the SELECT and HAVING clauses only.", e$line)
    }
    a <- ctx$aggs[[key]]
    return(list(node = "var", name = a$id, text = a$id))
  }
  if (e$name %in% c("put", "input")) {
    e$args <- lapply(e$args, sql_lower, ctx = ctx)
    return(e)
  }
  if (!is.null(lag_kind(e$name)) || e$name %in% c("vlength", "vlabel", "vformat", "vtype", "vname", "vinformat")) {
    abort_syntax(paste0("The ", toupper(e$name), " function is not supported in PROC SQL, it is only valid within the DATA step."), e$line)
  }
  if (e$name == "monotonic") {
    return(list(node = "call", name = "sqlmonotonic", args = list(), line = e$line, col = e$col, construct = TRUE))
  }
  if (isTRUE(e$distinct)) abort_syntax(paste0("DISTINCT is only valid in a summary function, not in ", toupper(e$name), "()"), e$line)
  name <- if (e$name %in% names(sql_fn_alias)) sql_fn_alias[[e$name]] else e$name
  args <- lapply(e$args, function(a) if (a$node == "omitted") a else sql_lower(a, ctx))
  out <- list(node = "call", name = name, args = args, line = e$line, col = e$col)
  if (name == "coalesce") {
    types <- lapply(args, sql_type_of, ctx = ctx)
    if (all(vapply(types, `[[`, "", "type") == "char")) {
      out$name <- "coalescec"
      out$sql_type <- list(type = "char", len = max(vapply(types, function(t) as.integer(t$len), 1L)))
    }
  }
  out
}

# Subqueries ------------------------------------------------------------------

# Evaluate a subquery for the current context. Uncorrelated subqueries run
# once; correlated ones run once per row (cached by the outer values used).
sql_lower_subquery <- function(e, ctx) {
  n <- sql_nrow(ctx$rel)
  run <- function(row) {
    ctx$row <- row
    on.exit(ctx$row <- NA_integer_)
    sql_query(e$query, ctx$eng, outer = ctx)
  }
  first <- run(if (n > 0) 1L else NA_integer_)
  correlated <- isTRUE(first$used_outer)
  per_row <- correlated || e$node %in% c("in_subq", "quant")
  if (!correlated || n == 0) {
    v <- sql_subq_value(e, first, ctx, NULL)
  } else {
    vals <- vector("list", n)
    vals[[1]] <- sql_subq_value(e, first, ctx, 1L)
    for (i in seq_len(n)[-1]) vals[[i]] <- sql_subq_value(e, run(i), ctx, i)
    ty0 <- attr(vals[[1]], "sql_type")
    v <- unlist(vals)
    attr(v, "sql_type") <- ty0
  }
  ty <- attr(v, "sql_type") %||% list(type = if (is.character(v)) "char" else "num", len = if (is.character(v)) max(1L, nchar(v)) else 8L)
  v <- as.vector(v)
  if (!per_row) v <- v[1]
  sql_bind_vector(ctx, v, ty$type, ty$len)
}

# The value of one subquery evaluation: a scalar, an EXISTS flag, or the
# result of IN / a quantified comparison for the (current) row.
sql_subq_value <- function(e, res, ctx, row) {
  d <- res$data
  if (e$node == "exists") return(as.numeric(nrow(d) > 0))
  if (length(res$cols) != 1 && e$node != "exists") {
    abort_syntax("A subquery used as an expression must return a single column.", e$line)
  }
  col <- res$cols[[1]]
  vals <- d[[col$id]]
  if (e$node == "subq") {
    if (length(vals) > 1) abort_runtime("Subquery evaluated to more than one row.", e$line)
    v <- if (length(vals) == 0) (if (col$type == "char") strrep(" ", col$len) else NA_real_) else vals[1]
    return(structure(v, sql_type = list(type = col$type, len = col$len)))
  }
  lhs <- sql_eval_lowered(sql_lower(e$lhs, ctx), ctx, rows = row)
  lt <- if (is.character(lhs)) "char" else "num"
  if (lt != col$type) abort_syntax("The comparison has components that are of different data types.", e$line)
  if (e$node == "in_subq") {
    hit <- sas_true(sas_in(lhs, as.list(vals)))
    return(as.numeric(if (isTRUE(e$negate)) !hit else hit))
  }
  sas_f_sqlquant(lhs, vals, e$op, e$quant)
}

# Evaluation ------------------------------------------------------------------

sql_eval_lowered <- function(low, ctx, rows = NULL) {
  low <- an_expr(low, ctx$st)
  cx <- new_cx("vec", as.list(ctx$st))
  code <- gen_expr(low, cx)
  env <- new.env(parent = ctx$env)
  d <- ctx$rel$data
  if (!is.null(rows)) d <- d[rows, , drop = FALSE]
  n <- nrow(d)
  for (id in names(d)) assign(id, d[[id]], envir = env)
  assign(".sql_n", n, envir = env)
  saved <- list(err = rt$err, notes = rt$notes, math = rt$math, mv = rt$mv)
  rt$err <- FALSE
  rt$notes <- character()
  v <- eval(parse(text = code, keep.source = FALSE), env)
  sql_function_notes(rt$notes, ctx)
  rt$err <- saved$err
  rt$notes <- saved$notes
  rt$math <- saved$math
  rt$mv <- saved$mv
  if (length(v) == 1 && n != 1) v <- rep(v, n)
  if (length(v) != n) v <- rep_len(v, n)
  v
}

# Data-error NOTEs of functions evaluated by PROC SQL.  SAS 9.4 writes
# "NOTE: Invalid argument to function MDY. Missing values may be generated."
# (no arguments, no line) where the DATA step writes "NOTE: Invalid argument to
# function MDY(2,30,2024) at line ..."; other NOTEs, and several failing calls
# or rows, are not verified in PROC SQL: fail loud instead of dropping them.
sql_function_notes <- function(notes, ctx) {
  if (!length(notes)) return(invisible())
  m <- regmatches(notes, regexec("^NOTE: Invalid argument to function ([A-Z0-9_]+)\\(", notes))
  fn <- vapply(m, function(x) if (length(x) == 2L) x[2] else NA_character_, "")
  if (anyNA(fn) || length(notes) > 1L) {
    abort_unsupported(paste0("a function data-error NOTE in PROC SQL (", sub("^NOTE: ", "", notes[1]),
                             if (length(notes) > 1L) paste0(" and ", length(notes) - 1L, " more"), ")"))
  }
  sql_note(ctx$eng, sprintf("NOTE: Invalid argument to function %s. Missing values may be generated.", fn))
  invisible()
}

sql_eval <- function(e, ctx) sql_eval_lowered(sql_lower(e, ctx), ctx)

sql_expr_type <- function(e, ctx) sql_type_of(sql_lower(e, ctx), ctx)
