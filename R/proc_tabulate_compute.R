# PROC TABULATE: the specification of a step and the cell computation.
#
# tab_spec() builds, at translation time, what the computation needs (CLASS
# variables with their MISSING / ORDER= / DESCENDING, the analysis variables,
# variable attributes, the table IRs of proc_tabulate.R, KEYLABEL texts and
# the PROC FORMAT=).  tabulate_compute(d, sp) computes the cells of every
# table from the input data (after WHERE) and returns one `sas_ods_table` per
# page of each TABLE statement:
#
#   data     one row per body row of the printed table: the hidden class
#            columns of the row dimension (raw level values, NA where the row
#            path does not restrict the variable) and one `stat` column per
#            column path (raw values; NA is an empty cell, printed as
#            MISSTEXT)
#   cols     as for every sas_ods_table; `format` is the cell format of a
#            column when it is the same in every row
#   header   the column headings: one cell per heading with `level` (first
#            heading row, 1 = top), `rowspan`, `span` (number of leaf
#            columns below the cell), `columns` (those data columns), `text`,
#            `role` ("title" heading of a class variable, "class" level or
#            ALL, "var", "stat")
#   attributes added for TABULATE:
#     row_header  tibble of the row-title cells: `row` (first body row),
#                 `nrow`, `col` (first row-title column), `ncol`, `text`,
#                 `role` (as for `header`), `heading` (TRUE: the heading of a
#                 class variable, printed on its own line above `row`)
#     formats     list (one element per stat column) of the cell formats of
#                 each body row
#     page        NULL, or list(text, role) of the page-dimension elements
#     box, misstext, table   BOX= (NULL or list(kind, value)), the MISSTEXT
#                 text (default "."), the number of the TABLE statement
#
# Statistics come from means_cell_stats() (the PROC MEANS kernel, VARDEF=DF,
# QNTLDEF=5); PCTN = n / N * 100 and PCTSUM = sum / S * 100 with N and S over
# every used row of the table (the page dimension does not change them).
# Inside one dimension only the class-level combinations that occur in the
# data become paths; between dimensions every pair is a cell, empty when no
# row matches both.

tab_stat_canon <- c(n = "n", nmiss = "nmiss", sum = "sum", mean = "mean", std = "std", stddev = "std", min = "min",
                    max = "max", range = "range", median = "median", p50 = "p50", pctn = "n", pctsum = "sum")
tab_stat_heading <- c(n = "N", nmiss = "NMiss", sum = "Sum", mean = "Mean", std = "Std", min = "Min", max = "Max",
                      range = "Range", median = "Median", p50 = "P50", pctn = "PctN", pctsum = "PctSum")
tab_default_format <- list(name = "", w = 12L, d = 2L, char = FALSE, text = "12.2")

# Specification ---------------------------------------------------------------

# `decl` from tab_declared(), `irs` the table IRs, `inp` from proc_input().
tab_spec <- function(step, decl, irs, inp) {
  st <- inp$st
  line <- step$line
  o <- step$options
  tryCatch(proc_apply_attr_stmts(step$stmts, st), sasonr_runtime_error = function(e) {
    abort_unsupported("the ERROR of a FORMAT or LABEL statement of PROC TABULATE", line)
  })
  cls <- lapply(decl$class, function(v) {
    co <- decl$class_opts[[v]]
    list(name = v, missing = isTRUE(co$missing) || isTRUE(o$missing), order = co$order %||% o$order %||% "internal",
         descending = isTRUE(co$descending))
  })
  names(cls) <- decl$class
  keylabel <- character()
  for (s in Filter(function(s) identical(s$stmt, "keylabel"), step$stmts)) keylabel[names(s$labels)] <- s$labels
  info <- proc_var_info(st, c(decl$class, decl$vars))
  # the heading of a variable is its name as stored; a different spelling in
  # the step is not measured
  for (v in c(decl$class, decl$vars)) {
    written <- if (v %in% decl$class) decl$class_text[[v]] else decl$var_text[[v]]
    if (!identical(written, info[[v]]$disp)) {
      abort_unsupported(paste0("a CLASS or VAR variable written in another case than in its data set (", toupper(v), ")"),
                        line)
    }
  }
  if (!is.null(o$format) && isTRUE(o$format$char)) abort_unsupported("PROC TABULATE FORMAT= with a character format", line)
  list(line = line, data = inp$ds$name, class = cls, vars = decl$vars, info = info, irs = irs, format = o$format,
       keylabel = keylabel)
}

# Computation -------------------------------------------------------------------

tabulate_compute <- function(d, sp) {
  line <- sp$line
  events <- character()
  n0 <- nrow(d)
  keep <- rep(TRUE, n0)
  for (cl in sp$class) if (!cl$missing) keep <- keep & !sas_is_missing(d[[cl$name]])
  d <- d[keep, , drop = FALSE]
  if (n0 > 0L && nrow(d) == 0L) {
    events <- "WARNING: A class, frequency, or weight variable is missing on every observation."
  }
  used <- unique(unlist(lapply(sp$irs, function(ir) {
    lapply(ir$dims, function(t) vapply(Filter(function(n) identical(n$kind, "class"), tab_nodes(t)), `[[`, "", "name"))
  }), use.names = FALSE))
  lv <- list()
  for (v in used) lv[[v]] <- tab_levels(d[[v]], sp$class[[v]], sp$info[[v]], line)
  used_stats <- unique(unlist(lapply(sp$irs, function(ir) {
    lapply(ir$dims, function(t) vapply(Filter(function(n) identical(n$kind, "stat"), tab_nodes(t)), `[[`, "", "name"))
  }), use.names = FALSE))
  for (s in used_stats) proc_stat_check("TABULATE", if (s == "stddev") "std" else s, line)
  msp <- list(freq = NULL, vardef = "df", exclnpwgt = FALSE, alpha = 0.05, qntldef = 5L)
  stats_of <- function(v, rows, need) {
    x <- d[[v]][rows]
    means_cell_stats(x, rep(1, length(x)), NULL, need, msp, two_sided = FALSE)
  }
  tot <- lapply(sp$vars, function(v) stats_of(v, seq_len(nrow(d)), c("n", "sum")))
  names(tot) <- sp$vars
  tables <- list()
  for (k in seq_along(sp$irs)) {
    ir <- sp$irs[[k]]
    tables <- c(tables, tab_table_pages(ir, k, d, sp, lv, tot, stats_of))
  }
  for (t in tables) validate_sas_ods_table(t)
  list(tables = tables, data = list(), events = events)
}

# Class levels of one variable (means_class_levels()) and the level index of
# every row.  ORDER=FREQ ties are taken only when every candidate order
# (internal, formatted, data) gives the same sequence.
tab_levels <- function(x, cl, info, line) {
  if (identical(cl$order, "freq") && length(x)) {
    base <- means_class_levels(x, rep(1, length(x)), utils::modifyList(cl, list(order = "internal", descending = FALSE)),
                               info)
    idx <- match(base$key, base$keys)
    cnt <- tabulate(idx, length(base$keys))
    if (anyDuplicated(cnt)) {
      first <- match(seq_along(base$keys), idx)
      txt <- proc_value_text(base$values, info)
      for (cv in unique(cnt[duplicated(cnt)])) {
        tied <- which(cnt == cv)
        if (cl$descending || !identical(order(first[tied]), seq_along(tied)) ||
            !identical(order(txt[tied], method = "radix"), seq_along(tied))) {
          abort_unsupported("PROC TABULATE ORDER=FREQ with tied counts in different internal, formatted and data order", line)
        }
      }
    }
  }
  l <- means_class_levels(x, rep(1, length(x)), cl, info)
  l$idx <- match(l$key, l$keys)
  l
}

# Number the nodes of a tree (an id per node, depth first).
tab_number <- function(tree, env) {
  lapply(tree, function(n) {
    env$id <- env$id + 1L
    n$id <- env$id
    n$children <- tab_number(n$children, env)
    n
  })
}

# Paths of a dimension tree over the rows `rows` of the data: a list of
# list(elems = list(list(node, level)...), rows).  A class node yields one
# path per level that occurs in `rows`, in level order.
tab_expand <- function(tree, rows, lv) {
  out <- list()
  for (n in tree) {
    ch <- n$children
    n$children <- NULL
    if (identical(n$kind, "class")) {
      li <- lv[[n$name]]$idx[rows]
      for (k in sort(unique(li))) out <- c(out, tab_expand_below(list(node = n, level = k), ch, rows[li == k], lv))
    } else {
      out <- c(out, tab_expand_below(list(node = n, level = NA_integer_), ch, rows, lv))
    }
  }
  out
}

tab_expand_below <- function(el, ch, rows, lv) {
  if (!length(ch)) return(list(list(elems = list(el), rows = rows)))
  lapply(tab_expand(ch, rows, lv), function(p) {
    p$elems <- c(list(el), p$elems)
    p
  })
}

# The elements of a path that is absent (no row dimension, no page
# dimension): one path over every row.
tab_paths <- function(tree, rows, lv) {
  if (is.null(tree)) return(list(list(elems = list(), rows = rows)))
  tab_expand(tree, rows, lv)
}

# Heading texts --------------------------------------------------------------------

# Heading of a class or analysis variable (NA_character_: no heading).
tab_var_heading <- function(n, sp) {
  if (!is.na(n$label)) return(if (nzchar(n$label)) n$label else NA_character_)
  inf <- sp$info[[n$name]]
  inf$label %||% inf$disp
}

tab_node_heading <- function(n, sp, line) {
  lab <- n$label
  switch(n$kind,
    class = ,
    var = tab_var_heading(n, sp),
    all = if (!is.na(lab)) lab else if ("all" %in% names(sp$keylabel)) sp$keylabel[["all"]] else "All",
    stat = {
      if (!is.na(lab)) {
        lab
      } else if (n$name %in% names(sp$keylabel)) {
        sp$keylabel[[n$name]]
      } else if (n$name == "stddev") {
        abort_unsupported("the heading of the PROC TABULATE statistic STDDEV", line)
      } else {
        tab_stat_heading[[n$name]]
      }
    }
  )
}

# The heading of a class level.  A missing numeric level prints its formatted
# text (`.`, `A` for .A) in the row dimension; in the column and page
# dimensions it is not measured.
tab_level_text <- function(v, level, lv, sp, line, row = FALSE) {
  val <- lv[[v]]$values[level]
  if (!row && !is.character(val) && sas_is_missing(val)) {
    abort_unsupported(paste0("the heading of a missing level of the numeric CLASS variable ", toupper(v)), line)
  }
  if (is.character(val) && !nzchar(sas_rtrim(val))) return("")
  proc_value_text(val, sp$info[[v]])
}

# Tables -------------------------------------------------------------------------

tab_table_pages <- function(ir, k, d, sp, lv, tot, stats_of) {
  line <- ir$line
  env <- new.env(parent = emptyenv())
  env$id <- 0L
  dims <- lapply(ir$dims, function(t) if (is.null(t)) NULL else tab_number(t, env))
  all_rows <- seq_len(nrow(d))
  pages <- tab_paths(dims$page, all_rows, lv)
  rpaths <- tab_paths(dims$row, all_rows, lv)
  cpaths <- tab_paths(dims$col, all_rows, lv)
  if (nrow(d) == 0L) {
    col_class <- any(vapply(tab_nodes(dims$col), function(n) identical(n$kind, "class"), logical(1)))
    if (!is.null(dims$page) || is.null(dims$row) || length(rpaths) || col_class) {
      abort_unsupported("an empty PROC TABULATE table with an ALL row, a page dimension or class columns", line)
    }
  }
  if (!is.null(dims$page)) {
    for (pg in pages) for (p in c(rpaths, cpaths)) {
      if (!length(intersect(pg$rows, p$rows))) {
        abort_unsupported("a row or column of PROC TABULATE without observations on a page of the page dimension", line)
      }
    }
    for (n in tab_nodes(dims$page)) {
      if (!is.null(n$format)) abort_unsupported("F= in the page dimension of PROC TABULATE", line)
    }
  }
  other <- any(vapply(c(tab_nodes(dims$page), tab_nodes(dims$row)), function(n) n$kind %in% c("stat", "var"), logical(1)))
  header <- tab_col_header(cpaths, other, sp, lv, line)
  stat_names <- header$names
  row_class <- unique(vapply(Filter(function(n) identical(n$kind, "class"), tab_nodes(dims$row)), `[[`, "", "name"))
  all_names <- make.unique(c(row_class, stat_names), sep = "_")
  stat_names <- all_names[length(row_class) + seq_along(stat_names)]
  header$cells$columns <- lapply(header$cells$columns, function(cc) stat_names[match(cc, header$names)])
  rh <- tab_row_header(rpaths, sp, lv, line)
  pct_denom <- function(v, what) {
    t <- tot[[v]][[what]]
    if (is.na(t) || t == 0) abort_unsupported("PCTN or PCTSUM with a denominator of 0 or missing", line)
    t
  }
  out <- list()
  for (pg in pages) {
    nr <- length(rpaths)
    data <- list()
    for (v in row_class) {
      vals <- lv[[v]]$values
      col <- vctrs::vec_init(vals, nr)
      for (i in seq_len(nr)) {
        hit <- Filter(function(e) identical(e$node$kind, "class") && identical(e$node$name, v), rpaths[[i]]$elems)
        if (length(hit)) col[i] <- vals[hit[[1]]$level]
      }
      data[[v]] <- col
    }
    fmts <- vector("list", length(cpaths))
    for (j in seq_along(cpaths)) {
      cp <- cpaths[[j]]
      vals <- rep(NA_real_, nr)
      fj <- vector("list", nr)
      for (i in seq_len(nr)) {
        rp <- rpaths[[i]]
        elems <- c(pg$elems, rp$elems, cp$elems)
        rows <- intersect(intersect(pg$rows, rp$rows), cp$rows)
        fj[i] <- list(tab_path_format(cp$elems) %||% tab_path_format(rp$elems) %||% sp$format %||% tab_default_format)
        vals[i] <- tab_cell_value(elems, rows, stats_of, pct_denom, line)
      }
      data[[stat_names[j]]] <- vals
      fmts[[j]] <- fj
    }
    tb <- tibble::new_tibble(data, nrow = nr)
    uniform <- lapply(fmts, function(fj) if (length(fj) && all(vapply(fj, identical, logical(1), fj[[1]]))) fj[[1]])
    cols <- ods_cols(
      name = c(row_class, stat_names),
      format = c(rep(list(NULL), length(row_class)), uniform),
      hidden = c(rep(TRUE, length(row_class)), rep(FALSE, length(stat_names))),
      role = c(rep("class", length(row_class)), rep("stat", length(stat_names))),
      var = c(row_class, vapply(cpaths, function(cp) tab_path_var(cp$elems) %||% NA_character_, "")),
      stat = c(rep(NA_character_, length(row_class)), header$stats),
      info = c(unname(sp$info[row_class]), rep(list(NULL), length(stat_names)))
    )
    t <- new_sas_ods_table(tb, name = "Table", label = paste("Table", k), path = "Tabulate.Report.Table",
                           proc = "Tabulate", template = "base.tabulate", cols = cols, header = header$cells)
    attr(t, "row_header") <- rh
    attr(t, "formats") <- fmts
    attr(t, "page") <- if (is.null(dims$page)) NULL else tab_page_text(pg$elems, sp, lv, line)
    attr(t, "box") <- ir$box
    attr(t, "misstext") <- ir$misstext %||% "."
    attr(t, "table") <- k
    out <- c(out, list(t))
  }
  out
}

tab_path_format <- function(elems) {
  f <- NULL
  for (e in elems) if (!is.null(e$node$format)) f <- e$node$format
  f
}

tab_path_var <- function(elems) {
  for (e in elems) if (identical(e$node$kind, "var")) return(e$node$name)
  NULL
}

# The value of one cell: `elems` the elements of its page, row and column
# paths, `rows` the matching rows.  NA is an empty cell.
tab_cell_value <- function(elems, rows, stats_of, pct_denom, line) {
  v <- tab_path_var(elems)
  stat <- NULL
  for (e in elems) if (identical(e$node$kind, "stat")) stat <- e$node$name
  stat <- stat %||% (if (is.null(v)) "n" else "sum")
  if (!length(rows)) return(NA_real_)
  if (is.null(v)) {
    if (stat != "n") abort_unsupported(paste0("PROC TABULATE ", toupper(stat), " without an analysis variable"), line)
    return(as.numeric(length(rows)))
  }
  s <- stats_of(v, rows, unique(c("n", tab_stat_canon[[stat]])))
  switch(stat,
    pctn = s[["n"]] / pct_denom(v, "n") * 100,
    pctsum = if (s[["n"]] == 0) NA_real_ else s[["sum"]] / pct_denom(v, "sum") * 100,
    unname(s[[tab_stat_canon[[stat]]]])
  )
}

# Column headings of the column paths: list(cells = <header tibble>, names =
# the data column names, stats = the statistic of each column).
tab_col_header <- function(cpaths, other_stat_var, sp, lv, line) {
  names <- paste0("_C", seq_along(cpaths), "_")
  slots <- lapply(cpaths, function(p) {
    out <- list()
    key <- ""
    row <- 1L
    add <- function(k, text, role, rowspan) {
      out[[length(out) + 1L]] <<- list(key = k, text = text, role = role, level = row, rowspan = rowspan)
      row <<- row + rowspan
    }
    for (e in p$elems) {
      n <- e$node
      if (n$kind %in% c("class", "all") && !is.na(n$label) && !nzchar(n$label)) {
        abort_unsupported("an empty label on a CLASS variable or ALL in the column dimension of PROC TABULATE", line)
      }
      here <- paste0(key, "/", n$id)
      if (n$kind == "class") {
        add(here, tab_var_heading(n, sp), "title", 1L)
        key <- paste0(here, ":", e$level)
        add(key, tab_level_text(n$name, e$level, lv, sp, line), "class", 1L)
      } else {
        key <- here
        h <- tab_node_heading(n, sp, line)
        if (!is.na(h) && !(n$kind %in% c("var", "stat") && !is.na(n$label) && !nzchar(n$label))) {
          add(key, h, if (n$kind == "all") "class" else n$kind, 1L)
        }
      }
    }
    # the default statistic of a column without a statistic (N, or SUM with an
    # analysis variable) prints its heading at the bottom; a statistic or an
    # analysis variable in another dimension is not measured
    if (!any(vapply(p$elems, function(e) identical(e$node$kind, "stat"), logical(1)))) {
      if (other_stat_var) {
        abort_unsupported("a PROC TABULATE column without a statistic when the rows give a statistic or an analysis variable", line)
      }
      dflt <- if (is.null(tab_path_var(p$elems))) "n" else "sum"
      add(paste0(key, "/default"), tab_node_heading(list(kind = "stat", name = dflt, label = NA_character_), sp, line),
          "stat", 1L)
    }
    list(cells = out, depth = row - 1L)
  })
  # a column with fewer heading rows than the deepest one: its top heading
  # cell reaches down over the missing rows (text at the bottom), the cells
  # below it move down; a top cell shared with a deeper column is not measured
  depth <- vapply(slots, `[[`, 1L, "depth")
  big <- max(c(0L, depth))
  for (j in which(depth < big)) {
    s <- slots[[j]]$cells
    if (!length(s)) abort_unsupported("a PROC TABULATE column without headings next to columns with headings", line)
    top <- s[[1]]$key
    shared <- vapply(slots[depth != depth[j]], function(o) length(o$cells) > 0L && identical(o$cells[[1]]$key, top), NA)
    if (any(shared)) {
      abort_unsupported("column headings of PROC TABULATE with a different number of heading rows below a shared heading", line)
    }
    extra <- big - depth[j]
    s[[1]]$rowspan <- s[[1]]$rowspan + extra
    for (q in seq_along(s)[-1]) s[[q]]$level <- s[[q]]$level + extra
    slots[[j]]$cells <- s
  }
  cells <- list()
  for (j in seq_along(slots)) for (c in slots[[j]]$cells) {
    id <- paste0(c$level, "|", c$key)
    m <- Position(function(x) identical(x$id, id) && identical(x$last_col, j - 1L), cells)
    if (!is.na(m)) {
      cells[[m]]$columns <- c(cells[[m]]$columns, names[j])
      cells[[m]]$last_col <- j
    } else {
      cells[[length(cells) + 1L]] <- list(id = id, level = c$level, rowspan = c$rowspan, text = c$text, role = c$role,
                                          columns = names[j], last_col = j)
    }
  }
  ord <- order(vapply(cells, `[[`, 1L, "level"))
  cells <- cells[ord]
  hd <- ods_header(level = vapply(cells, `[[`, 1L, "level"), text = lapply(cells, `[[`, "text"),
                   span = vapply(cells, function(x) length(x$columns), 1L), columns = lapply(cells, `[[`, "columns"),
                   role = vapply(cells, `[[`, "", "role"))
  hd$rowspan <- vapply(cells, `[[`, 1L, "rowspan")
  stats <- vapply(cpaths, function(p) {
    s <- NA_character_
    for (e in p$elems) if (identical(e$node$kind, "stat")) s <- e$node$name
    if (is.na(s)) s <- if (is.null(tab_path_var(p$elems))) "n" else "sum"
    s
  }, "")
  list(cells = hd, names = names, stats = stats)
}

# Row-title cells of the row paths (see the header of this file).
tab_row_header <- function(rpaths, sp, lv, line) {
  empty <- tibble::tibble(row = integer(), nrow = integer(), col = integer(), ncol = integer(), text = list(),
                          role = character(), heading = logical())
  if (!length(rpaths) || !length(rpaths[[1]]$elems)) return(empty)
  slots <- lapply(rpaths, function(p) {
    out <- list()
    key <- ""
    idkey <- ""
    for (e in p$elems) {
      n <- e$node
      idkey <- paste0(idkey, "/", n$id)
      if (n$kind == "class") {
        h <- tab_var_heading(n, sp)
        col <- length(Filter(function(x) !x$heading, out)) + 1L
        if (!is.na(h)) out[[length(out) + 1L]] <- list(key = paste0("h", idkey), text = h, role = "title", col = col,
                                                         heading = TRUE)
        key <- paste0(key, "/", n$id, ":", e$level)
        out[[length(out) + 1L]] <- list(key = key, text = tab_level_text(n$name, e$level, lv, sp, line, row = TRUE),
                                        role = "class", col = col, heading = FALSE)
      } else {
        key <- paste0(key, "/", n$id)
        h <- tab_node_heading(n, sp, line)
        if (is.na(h) || (!is.na(n$label) && !nzchar(n$label))) next
        col <- length(Filter(function(x) !x$heading, out)) + 1L
        out[[length(out) + 1L]] <- list(key = key, text = h, role = if (n$kind == "all") "class" else n$kind, col = col,
                                        heading = FALSE)
      }
    }
    out
  })
  ncols <- max(vapply(slots, function(s) length(Filter(function(x) !x$heading, s)), 1L))
  cells <- list()
  for (i in seq_along(slots)) {
    s <- slots[[i]]
    last_col <- max(c(0L, vapply(Filter(function(x) !x$heading, s), `[[`, 1L, "col")))
    for (c in s) {
      m <- Position(function(x) identical(x$key, c$key) && identical(x$last_row, i - 1L), cells)
      if (!is.na(m)) {
        cells[[m]]$nrow <- cells[[m]]$nrow + 1L
        cells[[m]]$last_row <- i
      } else {
        ncol <- if (!c$heading && c$col == last_col) ncols - c$col + 1L else 1L
        cells[[length(cells) + 1L]] <- list(key = c$key, row = i, nrow = 1L, col = c$col, ncol = ncol, text = c$text,
                                            role = c$role, heading = c$heading, last_row = i)
      }
    }
  }
  tibble::tibble(row = vapply(cells, `[[`, 1L, "row"), nrow = vapply(cells, `[[`, 1L, "nrow"),
                 col = vapply(cells, `[[`, 1L, "col"), ncol = vapply(cells, `[[`, 1L, "ncol"),
                 text = lapply(cells, `[[`, "text"), role = vapply(cells, `[[`, "", "role"),
                 heading = vapply(cells, `[[`, NA, "heading"))
}

# The heading of one page: the text of each page-dimension element.
tab_page_text <- function(elems, sp, lv, line) {
  text <- character()
  role <- character()
  for (e in elems) {
    n <- e$node
    if (n$kind == "class") {
      h <- tab_var_heading(n, sp)
      if (!is.na(h)) {
        text <- c(text, h)
        role <- c(role, "title")
      }
      text <- c(text, tab_level_text(n$name, e$level, lv, sp, line))
      role <- c(role, "class")
    } else {
      h <- tab_node_heading(n, sp, line)
      if (is.na(h) || (!is.na(n$label) && !nzchar(n$label))) next
      text <- c(text, h)
      role <- c(role, if (n$kind == "all") "class" else n$kind)
    }
  }
  list(text = text, role = role)
}
