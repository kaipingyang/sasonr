# PROC TRANSPOSE: parser, translation and run-time engine.

parse_proc_transpose <- function(p, line) {
  advance(p)
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC TRANSPOSE", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC TRANSPOSE"), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o %in% c("data", "out")) {
        opts[[o]] <- parse_ds_ref(p, if (o == "data") "input" else "output")
        next
      }
      if (!o %in% c("prefix", "suffix", "delimiter", "delim", "name", "label")) {
        abort_unsupported(paste0("PROC TRANSPOSE option ", toupper(o), "="), oline)
      }
      v <- parse_opt_value(p, o, oline)
      if (o == "delim") o <- "delimiter"
      opts[[o]] <- v
      next
    }
    if (!o %in% c("let", "noprint")) abort_unsupported(paste0("PROC TRANSPOSE option ", toupper(o)), oline)
    opts[[o]] <- TRUE
    advance(p)
  }
  advance(p)
  body <- proc_parse_body(p, "transpose", function(p, kw, sline) {
    switch(kw,
      var = parse_items_stmt(p, "var"),
      id = parse_items_stmt(p, "id"),
      idlabel = parse_items_stmt(p, "idlabel"),
      copy = parse_items_stmt(p, "copy"),
      by = parse_by(p),
      where = parse_where(p),
      format = parse_format_stmt(p),
      label = parse_label(p),
      attrib = parse_attrib(p),
      abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC TRANSPOSE ",
                          "(ERROR 180-322: Statement is not valid or it is used out of proper order)"), sline)
    )
  })
  list(type = "proc", name = "transpose", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

translate_proc_transpose <- function(step, meta, last) {
  o <- step$options
  inp <- proc_input(step, meta, last)
  st <- inp$st
  proc_apply_attr_stmts(step$stmts, st)
  of <- function(kind) Filter(function(s) identical(s$stmt, kind), step$stmts)
  vars_in <- inp$ds$out_names
  res_list <- function(kind) unique(unlist(lapply(of(kind), function(s) proc_resolve(s$items, st, s$line))))
  by <- of("by")
  by <- if (length(by) > 0) by[[length(by)]] else NULL
  if (!is.null(by)) {
    miss <- setdiff(by$vars, vars_in)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), by$line)
  }
  id <- res_list("id")
  idlabel <- res_list("idlabel")
  copy <- res_list("copy")
  if (length(idlabel) > 1) abort_syntax("Only one IDLABEL variable is allowed", step$line)
  if (length(idlabel) > 0 && length(id) == 0) abort_syntax("IDLABEL requires an ID statement", step$line)
  vars <- res_list("var")
  if (length(of("var")) == 0) {
    vars <- vars_in[vapply(vars_in, function(v) !identical(st$info[[v]]$type, "char"), TRUE)]
    vars <- setdiff(vars, c(by$vars, id, idlabel, copy))
  }
  out <- o$out
  if (is.null(out)) abort_unsupported("PROC TRANSPOSE without OUT= (DATAn naming)", step$line)
  spec <- list(
    line = step$line, data = inp$ds$name, vars = vars, by = by$vars, by_desc = by$descending, id = id,
    idlabel = idlabel, copy = copy, prefix = o$prefix, suffix = o$suffix, delimiter = o$delimiter %||% "",
    name = o$name %||% "_NAME_", label = o$label %||% "_LABEL_", let = isTRUE(o$let),
    info = proc_var_info(st, unique(c(vars, by$vars, id, idlabel, copy))), out = out$name, out_opts = out$options
  )
  code <- c(
    sprintf("# ---- PROC TRANSPOSE (SAS line %d)", as.integer(step$line)),
    paste0(".d <- ", proc_source_code(inp)),
    paste0(".r <- sas_proc_transpose(.d, ", r_lit(spec), ")"),
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC TRANSPOSE", line = step$line,
       outputs = out$name, meta = list())
}

# A SAS variable name from an ID value: invalid characters become `_`, a
# leading digit gets a `_` in front, at most 32 characters.
transpose_valid_name <- function(s) {
  ch <- strsplit(s, "")[[1]]
  ok <- grepl("[A-Za-z0-9_]", ch)
  ch[!ok] <- "_"
  s <- paste(ch, collapse = "")
  if (!nzchar(s)) s <- "_"
  if (grepl("^[0-9]", s)) s <- paste0("_", s)
  substr(s, 1L, 32L)
}

#' PROC TRANSPOSE engine
#' @param d Input data (after WHERE).
#' @param sp Specification built by the translator.
#' @export
#' @keywords internal
sas_proc_transpose <- function(d, sp) {
  info <- sp$info
  vars <- sp$vars
  groups <- if (length(sp$by) > 0) proc_by_groups(d, sp$by, sp$by_desc, sp$data, info) else list(seq_len(nrow(d)))
  if (length(groups) == 0) groups <- list(integer())
  any_char <- any(vapply(vars, function(v) info[[v]]$char, TRUE))
  fmts <- lapply(vars, function(v) info[[v]]$fmt)
  # values of the transposed variables (character when any of them is)
  value_of <- function(v, rows) {
    x <- d[[v]][rows]
    if (!any_char) return(x)
    if (info[[v]]$char) return(sas_rtrim(x))
    sas_format(x, info[[v]]$fmt)
  }
  col_len <- if (any_char) max(vapply(vars, function(v) {
    i <- info[[v]]
    if (i$char) i$len else (if (is.null(i$fmt)) 12L else fmt_width(i$fmt, FALSE))
  }, 1L)) else 8L
  # new column names (in order of first appearance)
  new_cols <- character()
  new_disp <- character()
  new_labels <- list()
  rows_out <- list()
  has_label <- any(vapply(vars, function(v) !is.null(info[[v]]$label), TRUE))
  for (g in groups) {
    n_g <- length(g)
    if (length(sp$id) > 0) {
      ids <- vapply(seq_len(n_g), function(j) {
        r <- g[j]
        parts <- vapply(sp$id, function(v) {
          x <- d[[v]][r]
          if (sas_is_missing(x)) abort_unsupported("a missing value of an ID variable")
          t <- proc_value_text(x, info[[v]])
          if (!info[[v]]$char && is.null(info[[v]]$fmt) && (x < 0 || x != trunc(x))) {
            abort_unsupported("a negative or non-integer numeric ID value without a format")
          }
          t
        }, "")
        paste(parts, collapse = sp$delimiter)
      }, "")
      nm_disp <- vapply(ids, function(s) transpose_valid_name(paste0(sp$prefix %||% "", s, sp$suffix %||% "")), "",
                        USE.NAMES = FALSE)
    } else {
      nm_disp <- if (n_g > 0) paste0(sp$prefix %||% "COL", seq_len(n_g), sp$suffix %||% "") else character()
    }
    nm <- tolower(nm_disp)
    # duplicate ID values within a BY group
    dup <- duplicated(nm)
    if (any(dup)) {
      if (!sp$let) abort_runtime(paste0("The ID value \"", nm_disp[dup][1], "\" occurs twice in the ",
                                        if (length(sp$by) > 0) "same BY group." else "input data set."))
      rt$proc_notes <- c(rt$proc_notes, paste0("WARNING: The ID value \"", nm_disp[dup][1], "\" occurs twice in the ",
                                               if (length(sp$by) > 0) "same BY group." else "input data set."))
    }
    for (j in seq_along(nm)) {
      if (!nm[j] %in% new_cols) {
        new_cols <- c(new_cols, nm[j])
        new_disp <- c(new_disp, nm_disp[j])
        if (length(sp$idlabel) > 0) new_labels[[nm[j]]] <- proc_value_text(d[[sp$idlabel]][g[j]], info[[sp$idlabel]])
      }
    }
    n_rows <- max(length(vars), if (length(sp$copy) > 0) n_g else 0L, if (length(sp$copy) > 0) 1L else 0L)
    if (length(vars) == 0 && length(sp$copy) == 0) n_rows <- 0L
    for (i in seq_len(n_rows)) {
      r <- list()
      for (v in sp$by) r[[v]] <- if (n_g > 0) d[[v]][g[1]] else NULL
      for (v in sp$copy) r[[v]] <- if (i <= n_g) d[[v]][g[i]] else (if (info[[v]]$char) "" else NA_real_)
      if (i <= length(vars)) {
        v <- vars[i]
        r$.name <- info[[v]]$disp
        r$.label <- info[[v]]$label %||% ""
        vals <- value_of(v, g)
        cells <- list()
        for (j in seq_len(n_g)) cells[[nm[j]]] <- vals[j]
        r$.cells <- cells
      } else {
        r$.name <- ""
        r$.label <- ""
        r$.cells <- list()
      }
      rows_out[[length(rows_out) + 1L]] <- r
    }
  }
  # columns
  col_of <- function(v) {
    i <- info[[v]]
    list(name = v, disp = i$disp, char = i$char, len = i$len, fmt = i$fmt, informat = i$informat, label = i$label)
  }
  name_len <- max(8L, nchar(vapply(vars, function(v) info[[v]]$disp, "")))
  cols <- c(lapply(sp$by, col_of), lapply(sp$copy, col_of))
  if (length(vars) > 0 || length(sp$copy) > 0) {
    cols <- c(cols, list(list(name = tolower(sp$name), disp = sp$name, char = TRUE, len = name_len, label = "NAME OF FORMER VARIABLE")))
    if (has_label) {
      lab_len <- max(40L, nchar(vapply(vars, function(v) info[[v]]$label %||% "", "")))
      cols <- c(cols, list(list(name = tolower(sp$label), disp = sp$label, char = TRUE, len = lab_len, label = "LABEL OF FORMER VARIABLE")))
    }
  }
  same_fmt <- length(unique(lapply(fmts, function(f) f$text))) == 1
  if (!same_fmt && !any_char) abort_unsupported("PROC TRANSPOSE of variables with different formats")
  cfmt <- if (!any_char && length(fmts) > 0) fmts[[1]] else NULL
  for (k in seq_along(new_cols)) {
    cols <- c(cols, list(list(name = new_cols[k], disp = new_disp[k], char = any_char, len = col_len, fmt = cfmt,
                              label = new_labels[[new_cols[k]]])))
  }
  nmc <- vapply(cols, `[[`, "", "name")
  if (anyDuplicated(nmc)) abort_unsupported(paste0("PROC TRANSPOSE output variable name collision (", nmc[duplicated(nmc)][1], ")"))
  data <- lapply(cols, function(c) {
    v <- lapply(rows_out, function(r) {
      if (c$name %in% sp$by || c$name %in% sp$copy) return(r[[c$name]])
      if (c$name == tolower(sp$name)) return(r$.name)
      if (has_label && c$name == tolower(sp$label)) return(r$.label)
      r$.cells[[c$name]]
    })
    if (c$char) vapply(v, function(x) if (is.null(x)) "" else as.character(x), "") else vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 1)
  })
  names(data) <- nmc
  data <- tibble::as_tibble(data)
  res <- proc_out_options(data, proc_out_meta(cols), sp$out_opts, sp$line)
  out <- list()
  out[[sp$out]] <- res
  list(out = out)
}
