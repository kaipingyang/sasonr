# PROC TABULATE: parser, TABLE expression IR and the checks of a step.
#
# The step is parsed into
#   list(type = "proc", name = "tabulate", line, options, stmts, titles, unsupported)
# where `unsupported` holds every feature outside the implemented scope (each
# list(msg, line)); translate_proc_tabulate() reports them together (sas_check()
# lists each one).  A TABLE statement is parsed into an AST:
#   concat(items = <cross>...)  blank-separated elements (concatenation)
#   cross(items = <factor>...)  `*`-separated factors (crossing)
#   factor: elem(name, text, label, denom, line, col) | paren(inner = concat) | fmt(fmt)
# and normalised into the table IR (tab_table_ir()):
#   list(dims = list(page, row, col), box, misstext, line)
# where each dimension is a tree (NULL when absent): a list of sibling nodes
#   node = list(kind = "class" | "var" | "all" | "stat" | "unknown", name, text,
#               label (NA: default, "": none), format (NULL or a parsed format),
#               children = <tree>, line, col)
# `(A B)*C` becomes `A*C B*C`; `A*(B C)` stays A with children B and C; equal
# siblings are kept apart; an F= factor sets the format of the leaves of its
# crossing.  A root-to-leaf path of a tree is a term; a cell is one term of
# each present dimension.
#
# Computation and output are not implemented: a step that passes every check
# fails loud.

# T1 statistics (the table cells): N NMISS SUM MEAN STD/STDDEV MIN MAX RANGE
# MEDIAN P50 PCTN PCTSUM.
tab_stats_supported <- c("n", "nmiss", "sum", "mean", "std", "stddev", "min", "max", "range", "median", "p50",
                         "pctn", "pctsum")
tab_stats_pct_denom <- c("colpctn", "rowpctn", "reppctn", "pagepctn", "colpctsum", "rowpctsum", "reppctsum",
                         "pagepctsum")
tab_stat_keywords <- unique(c(means_stat_names, names(means_stat_alias), "pctn", "pctsum", tab_stats_pct_denom))
# names that a CLASS or VAR variable shadows (WARNING "conflicts with the keyword")
tab_keywords <- c("all", tab_stat_keywords)

# Spelling of a statistic in the nesting path of an ERROR (measured for MEAN and SUM).
tab_path_stat <- c(mean = "Mean", sum = "Sum")

# Parser ------------------------------------------------------------------------

parse_proc_tabulate <- function(p, line) {
  advance(p)
  uns <- new.env(parent = emptyenv())
  uns$list <- list()
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC TABULATE", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC TABULATE"), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o == "data") {
        opts$data <- parse_ds_ref(p, "input")
      } else if (o == "format") {
        if (!is_format_ahead(p)) tab_skip_value(p) else opts$format <- parse_format(p)
        if (is.null(opts$format)) tab_note(uns, "PROC TABULATE FORMAT= without a format", oline)
      } else if (o == "order") {
        v <- tolower(parse_opt_value(p, o, oline))
        if (v == "unformatted") v <- "internal"
        if (!v %in% c("internal", "formatted", "freq", "data")) {
          tab_note(uns, paste0("PROC TABULATE ORDER=", toupper(v)), oline)
        } else {
          opts$order <- v
        }
      } else {
        tab_skip_value(p)
        tab_note(uns, paste0("PROC TABULATE option ", toupper(o), "="), oline)
      }
      next
    }
    if (o == "missing") {
      opts$missing <- TRUE
    } else {
      tab_note(uns, paste0("PROC TABULATE option ", toupper(o)), oline)
    }
    advance(p)
  }
  advance(p)
  body <- proc_parse_body(p, "tabulate", function(p, kw, sline) {
    switch(kw,
      class = parse_tabulate_class(p, uns),
      var = {
        s <- parse_items_stmt_opts(p, "var", uns)
        s
      },
      table = ,
      tables = parse_tabulate_table(p, uns),
      keylabel = parse_tabulate_keylabel(p, uns),
      where = parse_where(p),
      format = parse_format_stmt(p),
      label = parse_label(p),
      {
        tab_note(uns, paste0("PROC TABULATE statement ", toupper(tk_val(p))), sline)
        tab_skip_stmt(p)
        NULL
      }
    )
  })
  list(type = "proc", name = "tabulate", line = line, options = opts, stmts = body$stmts, titles = body$titles,
       unsupported = uns$list)
}

tab_note <- function(uns, msg, line) {
  uns$list[[length(uns$list) + 1L]] <- list(msg = msg, line = line)
  invisible()
}

# Skip to the end of the statement (past its semicolon).
tab_skip_stmt <- function(p) {
  while (!at_eof(p) && tk_type(p) != "semi") advance(p)
  if (!at_eof(p)) advance(p)
  invisible()
}

# Skip one option value: a token, or a bracketed group ({...}, [...], (...)).
tab_skip_value <- function(p) {
  if (tk_type(p) %in% c("lbrace", "lparen")) {
    depth <- 0L
    repeat {
      if (at_eof(p)) break
      t <- tk_type(p)
      if (t %in% c("lbrace", "lparen")) depth <- depth + 1L
      if (t %in% c("rbrace", "rparen")) depth <- depth - 1L
      advance(p)
      if (depth == 0L) break
    }
  } else if (!tk_type(p) %in% c("semi", "eof")) {
    advance(p)
  }
  invisible()
}

# VAR var ... [/ options];
parse_items_stmt_opts <- function(p, what, uns) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  while (tk_type(p) == "name") items <- c(items, list(tab_varlist_item(p)))
  if (is_op(p, "/")) {
    tab_note(uns, paste0(toupper(what), " statement options"), line)
    tab_skip_stmt(p)
    return(list(stmt = what, items = items, line = line))
  }
  expect_semi(p, toupper(what))
  list(stmt = what, items = items, line = line)
}

# A variable list item that keeps the spelling of a single name.
tab_varlist_item <- function(p) {
  text <- tk_val(p)
  it <- parse_varlist_item(p)
  if (identical(it$kind, "name")) it$text <- text
  it
}

parse_tabulate_class <- function(p, uns) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  while (tk_type(p) == "name") items <- c(items, list(tab_varlist_item(p)))
  o <- list()
  if (is_op(p, "/")) {
    advance(p)
    while (tk_type(p) == "name") {
      k <- tk_low(p)
      if (is_op(p, "=", 1L)) {
        advance(p, 2L)
        if (k == "order") {
          v <- tolower(parse_opt_value(p, k, line))
          if (v == "unformatted") v <- "internal"
          if (!v %in% c("internal", "formatted", "freq", "data")) {
            tab_note(uns, paste0("CLASS option ORDER=", toupper(v)), line)
          } else {
            o$order <- v
          }
        } else {
          tab_skip_value(p)
          tab_note(uns, paste0("CLASS option ", toupper(k), "="), line)
        }
        next
      }
      if (k %in% c("missing", "descending", "ascending")) {
        o[[k]] <- TRUE
      } else {
        tab_note(uns, paste0("CLASS option ", toupper(k)), line)
      }
      advance(p)
    }
  }
  if (tk_type(p) != "semi") {
    tab_note(uns, "a CLASS statement that does not parse", line)
    tab_skip_stmt(p)
  } else {
    advance(p)
  }
  list(stmt = "class", items = items, opts = o, line = line)
}

# KEYLABEL keyword='text' ...;
parse_tabulate_keylabel <- function(p, uns) {
  line <- tk_line(p)
  advance(p)
  labels <- character()
  while (tk_type(p) == "name" && is_op(p, "=", 1L) && tk_type(p, 2L) %in% c("str", "str_macro")) {
    labels[tk_low(p)] <- tk_val(p, 2L)
    advance(p, 3L)
  }
  if (tk_type(p) != "semi") {
    tab_note(uns, "a KEYLABEL statement that does not parse", line)
    tab_skip_stmt(p)
  } else {
    advance(p)
  }
  bad <- setdiff(names(labels), tab_keywords)
  for (b in bad) tab_note(uns, paste0("KEYLABEL for ", toupper(b)), line)
  list(stmt = "keylabel", labels = labels, line = line)
}

# TABLE [[page,] row,] column [/ options];
parse_tabulate_table <- function(p, uns) {
  line <- tk_line(p)
  advance(p)
  bad <- function() {
    tab_note(uns, "a TABLE statement that does not parse", line)
    tab_skip_stmt(p)
    list(stmt = "table", dims = list(), box = NULL, misstext = NULL, line = line, failed = TRUE)
  }
  res <- tryCatch({
    dims <- list(tab_parse_concat(p, uns))
    while (tk_type(p) == "comma") {
      advance(p)
      dims <- c(dims, list(tab_parse_concat(p, uns)))
    }
    dims
  }, tab_parse_error = function(e) NULL)
  if (is.null(res)) return(bad())
  if (length(res) > 3L) {
    tab_note(uns, "a TABLE statement with more than three dimensions", line)
  }
  box <- NULL
  misstext <- NULL
  if (is_op(p, "/")) {
    advance(p)
    while (tk_type(p) == "name") {
      k <- tk_low(p)
      oline <- tk_line(p)
      if (!is_op(p, "=", 1L)) {
        tab_note(uns, paste0("TABLE option ", toupper(k)), oline)
        advance(p)
        next
      }
      advance(p, 2L)
      if (k == "box" && tk_type(p) %in% c("str", "str_macro")) {
        box <- list(kind = "text", value = tk_val(p))
        advance(p)
      } else if (k == "box" && tk_type(p) == "name" && tk_low(p) != "_page_") {
        box <- list(kind = "var", value = tk_low(p))
        advance(p)
      } else if (k == "misstext" && tk_type(p) %in% c("str", "str_macro")) {
        misstext <- tk_val(p)
        advance(p)
      } else {
        what <- if (k == "box" && is_kw(p, "_page_")) "TABLE option BOX=_PAGE_" else paste0("TABLE option ", toupper(k), "=")
        tab_skip_value(p)
        tab_note(uns, what, oline)
      }
    }
  }
  if (tk_type(p) != "semi") return(bad())
  advance(p)
  list(stmt = "table", dims = res, box = box, misstext = misstext, line = line)
}

tab_parse_fail <- function() {
  rlang::abort("TABLE statement does not parse", class = "tab_parse_error")
}

tab_starts_factor <- function(p) {
  tk_type(p) %in% c("name", "lparen", "lbrace")
}

tab_parse_concat <- function(p, uns) {
  items <- list()
  while (tab_starts_factor(p)) items <- c(items, list(tab_parse_cross(p, uns)))
  if (!length(items)) tab_parse_fail()
  list(node = "concat", items = items)
}

tab_parse_cross <- function(p, uns) {
  items <- list(tab_parse_factor(p, uns))
  while (is_op(p, "*")) {
    advance(p)
    items <- c(items, list(tab_parse_factor(p, uns)))
  }
  list(node = "cross", items = items)
}

tab_parse_factor <- function(p, uns) {
  line <- tk_line(p)
  col <- tk_col(p)
  if (tk_type(p) == "lbrace") {
    # *[STYLE=...] / *{STYLE=...}
    tab_note(uns, "STYLE= on a TABLE element", line)
    tab_skip_value(p)
    return(list(node = "style", line = line, col = col))
  }
  if (tk_type(p) == "lparen") {
    advance(p)
    inner <- tab_parse_concat(p, uns)
    if (tk_type(p) != "rparen") tab_parse_fail()
    advance(p)
    return(list(node = "paren", inner = inner, line = line, col = col))
  }
  if (tk_type(p) != "name") tab_parse_fail()
  low <- tk_low(p)
  if (low %in% c("f", "format") && is_op(p, "=", 1L)) {
    pos <- p$pos
    advance(p, 2L)
    if (is_format_ahead(p)) {
      return(list(node = "fmt", fmt = parse_format(p), line = line, col = col))
    }
    p$pos <- pos
  }
  if (low %in% c("style", "s") && is_op(p, "=", 1L) && tk_type(p, 2L) == "lbrace") {
    advance(p, 2L)
    tab_note(uns, "STYLE= on a TABLE element", line)
    tab_skip_value(p)
    return(list(node = "style", line = line, col = col))
  }
  if ((is_op(p, ":", 1L) && tk_adjacent(p, 0L, 1L)) || is_op(p, "-", 1L)) {
    tab_note(uns, "a variable list in a TABLE statement", line)
    advance(p)
    while (is_op(p, c(":", "-"))) advance(p)
    if (tk_type(p) == "name" && is_op(p, "-", -1L)) advance(p)
    return(list(node = "style", line = line, col = col))
  }
  el <- list(node = "elem", name = low, text = tk_val(p), label = NA_character_, denom = NULL,
             line = line, col = col)
  advance(p)
  if (is_op(p, "<")) {
    advance(p)
    el$denom <- tab_parse_concat(p, uns)
    if (!is_op(p, ">")) tab_parse_fail()
    advance(p)
  }
  if (is_op(p, "=") && tk_type(p, 1L) %in% c("str", "str_macro")) {
    el$label <- tk_val(p, 1L)
    advance(p, 2L)
  }
  el
}

# IR --------------------------------------------------------------------------------

# Declared CLASS and VAR variables of a step: names (lower case) and their
# spelling; a second CLASS declaration of a variable is ignored (WARNING).
tab_declared <- function(step) {
  class <- character()
  class_text <- character()
  class_opts <- list()
  dup <- character()
  vars <- character()
  var_text <- character()
  var_line <- NA_integer_
  item_names <- function(items) {
    ok <- vapply(items, function(it) identical(it$kind, "name"), logical(1))
    list(ok = all(ok), names = vapply(items[ok], `[[`, "", "name"),
         text = vapply(items[ok], function(it) it$text %||% it$name, ""))
  }
  lists <- FALSE
  for (s in step$stmts) {
    if (identical(s$stmt, "class")) {
      it <- item_names(s$items)
      if (!it$ok) lists <- TRUE
      for (k in seq_along(it$names)) {
        v <- it$names[k]
        if (v %in% class) {
          dup <- c(dup, it$text[k])
          next
        }
        class <- c(class, v)
        class_text[v] <- it$text[k]
        class_opts[[v]] <- s$opts
      }
    } else if (identical(s$stmt, "var")) {
      it <- item_names(s$items)
      if (!it$ok) lists <- TRUE
      if (is.na(var_line)) var_line <- s$line
      for (k in seq_along(it$names)) {
        v <- it$names[k]
        if (v %in% vars) next
        vars <- c(vars, v)
        var_text[v] <- it$text[k]
      }
    }
  }
  list(class = class, class_text = class_text, class_opts = class_opts, dup = dup, vars = vars,
       var_text = var_text, var_line = var_line, lists = lists)
}

tab_elem_kind <- function(name, decl) {
  if (name %in% decl$class) return("class")
  if (name %in% decl$vars) return("var")
  if (name == "all") return("all")
  if (name %in% tab_stat_keywords) return("stat")
  "unknown"
}

# The IR of one TABLE statement (see the header of this file).
tab_table_ir <- function(stmt, decl) {
  dims <- lapply(stmt$dims, tab_tree_concat, decl = decl)
  k <- length(dims)
  get <- function(i) if (i >= 1L && i <= k) dims[[i]] else NULL
  list(dims = list(page = if (k == 3L) get(1L), row = if (k >= 2L) get(k - 1L), col = get(k)),
       box = stmt$box, misstext = stmt$misstext, line = stmt$line)
}

tab_tree_concat <- function(concat, decl) {
  out <- list()
  for (it in concat$items) out <- c(out, tab_tree_cross(it$items, decl))
  out
}

tab_tree_cross <- function(factors, decl) {
  fmts <- Filter(function(f) identical(f$node, "fmt"), factors)
  rest <- Filter(function(f) !f$node %in% c("fmt", "style"), factors)
  build <- function(fs) {
    if (!length(fs)) return(list())
    head <- fs[[1]]
    sub <- build(fs[-1])
    if (identical(head$node, "paren")) return(tab_graft(tab_tree_concat(head$inner, decl), sub))
    list(list(kind = tab_elem_kind(head$name, decl), name = head$name, text = head$text, label = head$label,
              format = NULL, denom = head$denom, children = sub, line = head$line, col = head$col))
  }
  tree <- build(rest)
  if (length(fmts)) tree <- tab_set_format(tree, fmts[[length(fmts)]]$fmt)
  tree
}

# Cross every leaf of `tree` with `sub`; the format of a leaf moves down to
# the leaves of its crossing (an F= covers the cells of its crossing).
tab_graft <- function(tree, sub) {
  lapply(tree, function(n) {
    if (length(n$children)) {
      n$children <- tab_graft(n$children, sub)
    } else if (length(sub)) {
      n$children <- if (is.null(n$format)) sub else tab_set_format(sub, n$format)
      n$format <- NULL
    }
    n
  })
}

# The format of the leaves of a crossing that have none from an inner F=.
tab_set_format <- function(tree, fmt) {
  lapply(tree, function(n) {
    if (length(n$children)) {
      n$children <- tab_set_format(n$children, fmt)
    } else if (is.null(n$format)) {
      n$format <- fmt
    }
    n
  })
}

# Root-to-leaf paths of a tree: a list of terms, each a list of nodes.
tab_terms <- function(tree) {
  out <- list()
  for (n in tree) {
    ch <- n$children
    n$children <- NULL
    if (!length(ch)) {
      out <- c(out, list(list(n)))
    } else {
      for (t in tab_terms(ch)) out <- c(out, list(c(list(n), t)))
    }
  }
  out
}

# Every node of a tree, depth first.
tab_nodes <- function(tree) {
  out <- list()
  for (n in tree) out <- c(out, list(n), tab_nodes(n$children))
  out
}

# Checks ------------------------------------------------------------------------

# Features of the IR outside the implemented scope: list(msg, line) each.
tab_ir_unsupported <- function(ir) {
  out <- list()
  for (d in ir$dims) for (n in tab_nodes(d)) {
    if (!identical(n$kind, "stat")) {
      if (!is.null(n$denom)) out <- c(out, list(list(msg = "a denominator after a TABLE element that is not PCTN or PCTSUM", line = n$line)))
      next
    }
    if (!is.null(n$denom)) {
      out <- c(out, list(list(msg = paste0("a ", toupper(n$name), " denominator in a TABLE statement"), line = n$line)))
    } else if (n$name %in% tab_stats_pct_denom) {
      out <- c(out, list(list(msg = paste0("PROC TABULATE statistic ", toupper(n$name)), line = n$line)))
    } else if (!n$name %in% tab_stats_supported) {
      out <- c(out, list(list(msg = paste0("PROC TABULATE statistic ", toupper(n$name)), line = n$line)))
    }
  }
  if (!is.null(ir$box) && identical(ir$box$kind, "var")) {
    out <- c(out, list(list(msg = "TABLE option BOX= with a variable name", line = ir$line)))
  }
  out
}

# The cells of a table IR with their elements (page, row, column order) and
# the index of the column term.
tab_cells <- function(ir) {
  pick <- function(tree) if (is.null(tree)) list(list()) else tab_terms(tree)
  pg <- pick(ir$dims$page)
  rw <- pick(ir$dims$row)
  cl <- pick(ir$dims$col)
  out <- list()
  for (a in pg) for (b in rw) for (j in seq_along(cl)) {
    out <- c(out, list(list(elems = c(a, b, cl[[j]]), col = j)))
  }
  out
}

# Family N violations of one cell: character vector of kinds.
tab_cell_violations <- function(elems) {
  kinds <- vapply(elems, `[[`, "", "kind")
  v <- character()
  if (sum(kinds == "var") > 1L) v <- c(v, "analysis")
  if (sum(kinds == "stat") > 1L) v <- c(v, "statistics")
  stats <- vapply(elems[kinds == "stat"], `[[`, "", "name")
  if (!any(kinds == "var") && any(!stats %in% c("n", "pctn"))) v <- c(v, "novar")
  v
}

tab_violation_text <- c(
  analysis = "There are multiple analysis variables associated with a single table cell",
  statistics = "There are multiple statistics associated with a single table cell",
  novar = "Statistic other than N was requested without analysis variable"
)

# The nesting path of a cell in the ERROR text, or NULL when it cannot be
# written exactly (ALL, a label, an F= in the cell; a statistic other than
# MEAN or SUM; a name not written in lower case).
tab_nesting_path <- function(elems, decl) {
  parts <- character()
  for (n in elems) {
    if (!is.na(n$label) || !is.null(n$format)) return(NULL)
    if (n$kind %in% c("class", "var")) {
      decl_text <- if (n$kind == "class") decl$class_text[[n$name]] else decl$var_text[[n$name]]
      if (!identical(n$text, n$name) || !identical(decl_text, n$name)) return(NULL)
      parts <- c(parts, n$name)
    } else if (n$kind == "stat") {
      s <- tab_path_stat[n$name]
      if (is.na(s)) return(NULL)
      parts <- c(parts, unname(s))
    } else {
      return(NULL)
    }
  }
  paste(parts, collapse = " * ")
}

# A class variable crossed with itself somewhere in the table.
tab_crosses_self <- function(ir) {
  for (cell in tab_cells(ir)) {
    cls <- vapply(Filter(function(n) identical(n$kind, "class"), cell$elems), `[[`, "", "name")
    if (anyDuplicated(cls)) return(TRUE)
  }
  FALSE
}

# Raise every unsupported finding at once (sas_check() lists each one).
tab_fail_all <- function(found) {
  conds <- lapply(found, function(f) tryCatch(abort_unsupported(f$msg, f$line), sasonr_unsupported = identity))
  first <- conds[[1]]
  rlang::abort(conditionMessage(first), class = c("sasonr_unsupported", "sasonr_error"),
               sas_line = first$sas_line, sas_more = conds[-1])
}

# A program ERROR of the step: `lines` the ERROR lines (the first without its
# "ERROR: " prefix in the condition message), `before` the WARNING lines
# written before them.  `at_end`: the ERROR follows the source of the whole
# step (not the line it is about).
tab_abort_error <- function(lines, before, line, at_end = TRUE) {
  msg <- paste0("Runtime error: ", sub("^ERROR: ", "", lines[1]), " (SAS line ", line, ")")
  rlang::abort(msg, class = c("sasonr_runtime_error", "sasonr_error"), sas_line = line,
               sas_also = lines[-1], sas_before = before, sas_step_end = at_end, tab_error = TRUE)
}

# Translation -----------------------------------------------------------------------

translate_proc_tabulate <- function(step, meta, last) {
  found <- step$unsupported
  decl <- tab_declared(step)
  tables <- Filter(function(s) identical(s$stmt, "table"), step$stmts)
  if (decl$lists) found <- c(found, list(list(msg = "a variable list in a CLASS or VAR statement of PROC TABULATE", line = step$line)))
  if (!length(tables)) found <- c(found, list(list(msg = "PROC TABULATE without a TABLE statement", line = step$line)))
  both <- intersect(decl$class, decl$vars)
  if (length(both)) {
    found <- c(found, list(list(msg = paste0("a variable in both the CLASS and the VAR statement (", toupper(both[1]), ")"),
                                line = step$line)))
  }
  irs <- lapply(Filter(function(s) !isTRUE(s$failed), tables), tab_table_ir, decl = decl)
  for (ir in irs) found <- c(found, tab_ir_unsupported(ir))
  if (length(found)) tab_fail_all(found)

  inp <- proc_input(step, meta, last)
  st <- inp$st
  missing_vars <- setdiff(c(decl$class, decl$vars), st$order)
  if (length(missing_vars)) {
    abort_unsupported(paste0("a CLASS or VAR variable that is not in the input data set (", toupper(missing_vars[1]),
                             "; the ERROR text is not implemented)"), step$line)
  }
  unexact <- function(what) abort_unsupported(paste0("the log of this PROC TABULATE step (", what, ")"), step$line)

  # WARNINGs of the declarations
  conflict <- c(decl$class[decl$class %in% tab_keywords], decl$vars[decl$vars %in% tab_keywords])
  conflict_text <- c(decl$class_text, decl$var_text)[conflict]
  if (length(conflict) && !identical(unname(conflict_text), conflict)) unexact("a keyword-named variable not written in lower case")
  warn <- if (length(conflict)) paste0("WARNING: The variable ", conflict, " conflicts with the keyword of the same name.") else character()
  other_warn <- length(decl$dup) > 0L || any(vapply(irs, tab_crosses_self, logical(1)))

  # ERROR: a character analysis variable
  char_vars <- decl$vars[vapply(decl$vars, function(v) identical(st$info[[v]]$type, "char"), logical(1))]
  # ERROR: a name that is neither a CLASS / VAR variable nor a keyword (the first one)
  unknown <- NULL
  for (ir in irs) {
    for (d in ir$dims) for (n in tab_nodes(d)) if (identical(n$kind, "unknown")) {
      unknown <- n
      break
    }
    if (!is.null(unknown)) break
  }
  # ERRORs of the cells (family N): the first table that has one
  errs <- character()
  err_cols <- integer()
  err_tables <- 0L
  for (ir in irs) {
    if (!is.null(unknown)) break
    e_ir <- character()
    for (cell in tab_cells(ir)) {
      v <- tab_cell_violations(cell$elems)
      if (!length(v)) next
      if (length(v) > 1L) unexact("a table cell with more than one kind of violation")
      path <- tab_nesting_path(cell$elems, decl)
      if (is.null(path)) unexact("the nesting path of a table cell in an ERROR")
      e_ir <- c(e_ir, paste0("ERROR: ", tab_violation_text[[v]], " in the following nesting : ", path, "."))
      err_cols <- c(err_cols, cell$col)
    }
    if (length(e_ir)) {
      err_tables <- err_tables + 1L
      if (err_tables == 1L) {
        errs <- e_ir
        err_line <- ir$line
      }
    }
  }

  n_kinds <- (length(char_vars) > 0L) + !is.null(unknown) + (length(errs) > 0L)
  if (n_kinds > 1L) unexact("ERRORs of more than one kind")
  if (length(char_vars)) {
    if (length(warn) || other_warn) unexact("an ERROR after a WARNING")
    v <- char_vars[1]
    if (!identical(decl$var_text[[v]], v)) unexact("a variable not written in lower case")
    tab_abort_error(paste0("ERROR: Variable ", v, " in list does not match type prescribed for this list."),
                    character(), decl$var_line, at_end = FALSE)
  }
  if (!is.null(unknown)) {
    if (length(warn) || other_warn) unexact("an ERROR after a WARNING")
    if (!identical(unknown$text, unknown$name)) unexact("a name not written in lower case")
    tab_abort_error(paste0("ERROR: The type of name (", unknown$name, ") is unknown."), character(), unknown$line)
  }
  if (length(errs)) {
    if (other_warn) unexact("an ERROR after a WARNING of a CLASS declaration or a self-crossing")
    if (err_tables > 1L) unexact("ERRORs in more than one TABLE statement")
    if (length(errs) > 1L && length(unique(err_cols)) > 1L) unexact("ERRORs in more than one column of a table")
    ls <- rt_linesize()
    if (any(nchar(c(warn, errs)) > ls)) unexact("an ERROR or WARNING longer than the line size")
    tab_abort_error(errs, warn, err_line)
  }
  abort_unsupported("PROC TABULATE output is not implemented yet", step$line)
}
