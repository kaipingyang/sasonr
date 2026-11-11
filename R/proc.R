# PROC steps (SORT, PRINT) and global statements (TITLE, OPTIONS).
#
# PROC IR: list(type = "proc", name, line, options = named list, stmts = list)
# Every other PROC, option or statement fails loud.

options_ignorable <- c(
  "center", "nocenter", "date", "nodate", "number", "nonumber", "ls", "linesize",
  "ps", "pagesize", "notes", "nonotes", "nosource", "nosource2",
  "nomprint", "nomlogic", "nosymbolgen", "nomprintnest", "nomlogicnest",
  "formdlim", "pageno", "stimer", "nostimer",
  "fullstimer", "nofullstimer", "msglevel", "errors"
)

parse_global_stmt <- function(p) {
  line <- tk_line(p)
  low <- tk_low(p)
  sp <- split_trailing_digits(low)
  if (sp$base == "title" && (is.na(sp$width) || sp$width <= 10)) {
    advance(p)
    parts <- character()
    while (tk_type(p) %in% c("str", "str_macro")) {
      parts <- c(parts, tk_val(p))
      advance(p)
    }
    if (tk_type(p) != "semi") abort_unsupported("TITLE options", line)
    advance(p)
    return(list(type = "global", kind = "title", level = if (is.na(sp$width)) 1L else sp$width,
                text = paste(parts, collapse = " "), line = line))
  }
  if (low == "filename") {
    advance(p)
    if (tk_type(p) != "name") abort_syntax("expected a fileref after FILENAME", line)
    ref <- tk_low(p)
    advance(p)
    if (!tk_type(p) %in% c("str", "str_macro")) abort_unsupported("FILENAME without a quoted physical path", line)
    path <- tk_val(p)
    advance(p)
    while (tk_type(p) == "name" && tk_low(p) %in% c("encoding", "lrecl", "recfm", "termstr")) {
      advance(p)
      if (is_op(p, "=")) advance(p, 2L)
    }
    if (tk_type(p) != "semi") abort_unsupported("FILENAME options", line)
    advance(p)
    return(list(type = "global", kind = "filename", ref = ref, path = path, line = line))
  }
  if (low == "options" || low == "option") {
    advance(p)
    opts <- character()
    while (tk_type(p) != "semi") {
      if (at_eof(p)) abort_syntax("missing semicolon after OPTIONS", line)
      if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in OPTIONS"), line)
      opt <- tk_low(p)
      opts <- c(opts, opt)
      if (!opt %in% options_ignorable) abort_unsupported(paste0("system option ", toupper(opt)), line)
      advance(p)
      if (is_op(p, "=")) {
        advance(p)
        if (tk_type(p) %in% c("name", "num", "str", "str_macro")) advance(p) else abort_syntax("invalid OPTIONS value", line)
      }
    }
    advance(p)
    return(list(type = "global", kind = "options", opts = opts, line = line))
  }
  abort_unsupported(paste0("global statement ", toupper(tk_val(p))), line)
}

proc_end_reached <- function(p) {
  at_eof(p) || (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L))
}

parse_proc_step <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name") abort_syntax("expected a procedure name after PROC", line)
  name <- tk_low(p)
  if (name == "format") return(parse_proc_format(p, line))
  if (!name %in% c("sort", "print")) abort_unsupported(paste0("PROC ", toupper(tk_val(p))), line)
  advance(p)
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC statement", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC", toupper(name)), line)
    opt <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (opt == "data") {
        opts$data <- parse_ds_ref(p, "input")
      } else if (opt == "out" && name == "sort") {
        opts$out <- parse_ds_ref(p, "output")
      } else if (name == "print" && opt %in% c("width", "contents") ||
                 name == "sort" && opt %in% c("sortsize")) {
        advance(p)
      } else {
        abort_unsupported(paste0("PROC ", toupper(name), " option ", toupper(opt), "="), oline)
      }
      next
    }
    ok <- switch(name,
      sort = opt %in% c("nodupkey", "nodupkeys", "noduprecs", "noduprec", "nodup", "force", "tagsort", "threads", "nothreads", "equals", "overwrite"),
      print = opt %in% c("noobs", "double", "uniform")
    )
    if (!ok) abort_unsupported(paste0("PROC ", toupper(name), " option ", toupper(opt)), oline)
    opts[[opt]] <- TRUE
    advance(p)
  }
  advance(p)
  stmts <- list()
  titles <- list()
  repeat {
    if (proc_end_reached(p)) break
    if (tk_type(p) == "semi") {
      advance(p)
      next
    }
    if (is_kw(p, c("run", "quit")) && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    }
    sline <- tk_line(p)
    low <- tk_low(p)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC", toupper(name)), sline)
    if (startsWith(low, "title")) {
      titles <- c(titles, list(parse_global_stmt(p)))
      next
    }
    s <- switch(paste(name, low),
      "sort by" = ,
      "print by" = {
        b <- parse_by(p)
        if (name == "print") abort_unsupported("BY statement in PROC PRINT", sline)
        b
      },
      "sort where" = ,
      "print where" = parse_where(p),
      "print var" = {
        advance(p)
        items <- list()
        while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
        expect_semi(p, "VAR")
        list(stmt = "var", items = items, line = sline)
      },
      abort_unsupported(paste0("PROC ", toupper(name), " statement ", toupper(tk_val(p))), sline)
    )
    stmts <- c(stmts, list(s))
  }
  list(type = "proc", name = name, line = line, options = opts, stmts = stmts, titles = titles)
}

# Translation -----------------------------------------------------------------

# Analyse the DATA= reference of a PROC: PDV of the input data set plus WHERE.
proc_input <- function(step, meta, last) {
  ref <- step$options$data
  if (is.null(ref)) {
    if (is.null(last)) abort_runtime("There is no default input data set (_LAST_ is _NULL_)", step$line)
    ref <- list(name = last, options = list(), line = step$line)
  }
  st <- new_state(meta)
  rd <- an_reader(list(stmt = "set", datasets = list(ref), line = step$line), st)
  known <- st$order
  wheres <- utils::tail(Filter(function(s) s$stmt == "where", step$stmts), 1) # the last WHERE wins
  conds <- lapply(wheres, function(w) {
    e <- an_expr(w$cond, st)
    extra <- setdiff(st$order, known)
    if (length(extra) > 0) abort_runtime(paste0("Variable ", extra[1], " is not on file WORK.", toupper(ref$name)), w$line)
    e
  })
  list(ds = rd$datasets[[1]], st = st, wheres = conds)
}

proc_meta <- function(st, vars, out_names = vars) {
  type <- lapply(vars, function(v) st$info[[v]]$type)
  len <- lapply(vars, function(v) if (identical(st$info[[v]]$type, "char")) st$info[[v]]$len else as.integer(st$numlen[[v]] %||% 8L))
  fmts <- lapply(vars, function(v) st$formats[[v]])
  infs <- lapply(vars, function(v) st$informats[[v]])
  labs <- lapply(vars, function(v) if (v %in% names(st$labels)) st$labels[[v]] else NULL)
  disp <- Map(function(v, on) if (on == v) st$info[[v]]$disp %||% v else on, vars, out_names)
  names(type) <- names(len) <- names(fmts) <- names(disp) <- names(infs) <- names(labs) <- out_names
  list(vars = out_names, type = type, len = len, formats = Filter(Negate(is.null), fmts), disp = disp,
       labels = Filter(Negate(is.null), labs), informats = Filter(Negate(is.null), infs))
}

proc_source_code <- function(inp, obs_col = FALSE) {
  ds_code(inp$ds, list(arrays = list()), obs_col = obs_col, wheres = inp$wheres)
}

translate_proc <- function(step, meta, last = NULL) {
  if (step$name == "format") return(translate_proc_format(step))
  inp <- proc_input(step, meta, last)
  vars <- inp$ds$out_names
  header <- sprintf("# ---- PROC %s (SAS line %d)", toupper(step$name), as.integer(step$line))
  if (step$name == "sort") {
    by <- Filter(function(s) s$stmt == "by", step$stmts)
    if (length(by) != 1) abort_syntax("PROC SORT requires exactly one BY statement", step$line)
    by <- by[[1]]
    if (identical(by$vars, "_all_")) {
      by$vars <- vars
      by$descending <- rep(by$descending, length(vars))
    }
    miss <- setdiff(by$vars, vars)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found"), by$line)
    out <- step$options$out %||% list(name = inp$ds$name, options = list())
    out_vars <- vars
    out_names <- vars
    o <- out$options
    if (!is.null(o$keep)) out_vars <- intersect(out_vars, resolve_ds_items(o$keep, out_vars, step$line))
    if (!is.null(o$drop)) out_vars <- setdiff(out_vars, resolve_ds_items(o$drop, out_vars, step$line))
    if (!is.null(o$where)) abort_unsupported("WHERE= on an output data set", step$line)
    out_names <- out_vars
    if (!is.null(o$rename)) {
      hit <- match(names(o$rename), out_vars)
      out_names[hit[!is.na(hit)]] <- unname(o$rename[!is.na(hit)])
    }
    sel <- select_code(list(vars = out_vars, out_names = out_names))
    code <- c(
      header,
      paste0(".d <- ", proc_source_code(inp)),
      sprintf("work$%s <- sas_sort_df(.d, %s, %s, nodupkey = %s, noduprecs = %s) |>\n  %s", r_name(out$name),
              r_chr_vec(by$vars), r_lit(by$descending), isTRUE(step$options[["nodupkey"]]) || isTRUE(step$options[["nodupkeys"]]),
              isTRUE(step$options[["noduprecs"]]) || isTRUE(step$options[["noduprec"]]) || isTRUE(step$options[["nodup"]]), sel)
    )
    m <- proc_meta(inp$st, out_vars, out_names)
    res_meta <- stats::setNames(list(m), out$name)
    return(list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC SORT",
                line = step$line, outputs = out$name, meta = res_meta))
  }
  # PROC PRINT
  var_stmts <- Filter(function(s) s$stmt == "var", step$stmts)
  pvars <- vars
  if (length(var_stmts) > 0) {
    pvars <- unlist(lapply(var_stmts, function(s) {
      tmp <- new_state(list())
      tmp$order <- vars
      tmp$info <- inp$st$info[vars]
      resolve_items(s$items, tmp, s$line, register = FALSE)
    }))
    miss <- setdiff(pvars, vars)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found"), var_stmts[[1]]$line)
  }
  if (length(var_stmts) > 0 && length(pvars) == 0) {
    return(list(code = paste(c(header, "# no variables to print"), collapse = "\n"), level = "PROC",
                reasons = "PROC PRINT", line = step$line, outputs = character(), meta = list()))
  }
  spec <- lapply(pvars, function(v) {
    inf <- inp$st$info[[v]]
    list(name = v, disp = inf$disp %||% v, char = identical(inf$type, "char"),
         fmt = if (is.null(inp$st$formats[[v]])) NULL else inp$st$formats[[v]][c("name", "w", "d", "char", "text")])
  })
  code <- c(
    header,
    paste0(".d <- ", proc_source_code(inp, obs_col = TRUE)),
    sprintf(".lst <- c(.lst, sas_print_listing(.d, %s, noobs = %s, titles = .titles))",
            r_lit(spec), isTRUE(step$options[["noobs"]]))
  )
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC PRINT",
       line = step$line, outputs = character(), meta = list())
}

# Runtime -------------------------------------------------------------------------

#' PROC SORT: stable sort (missing values first) with optional NODUPKEY
#' @export
sas_sort_df <- function(d, by, descending = rep(FALSE, length(by)), nodupkey = FALSE, noduprecs = FALSE) {
  d <- d[sas_order(d, by, descending), , drop = FALSE]
  if (nodupkey && nrow(d) > 0) {
    n0 <- nrow(d)
    d <- d[do.call(key_change, unname(as.list(d[by]))), , drop = FALSE]
    rt$proc_notes <- c(rt$proc_notes, sprintf("NOTE: %d observations with duplicate key values were deleted.", n0 - nrow(d)))
  }
  if (noduprecs && nrow(d) > 1) {
    n0 <- nrow(d)
    same <- Reduce(`&`, lapply(d, function(v) c(FALSE, same_value(v[-1L], v[-nrow(d)]))))
    d <- d[!same, , drop = FALSE]
    rt$proc_notes <- c(rt$proc_notes, sprintf("NOTE: %d duplicate observations were deleted.", n0 - nrow(d)))
  }
  tibble::as_tibble(d)
}

#' Apply FIRSTOBS= / OBS= to a data frame (after WHERE)
#' @export
sas_obs_window <- function(d, firstobs = NULL, obs = NULL) {
  from <- firstobs %||% 1
  to <- min(obs %||% nrow(d), nrow(d))
  d[seq_len(max(0, to - from + 1)) + from - 1, , drop = FALSE]
}

# Default PROC PRINT rendering of an unformatted numeric column: BEST7. of the
# magnitude (one position is kept for the sign), with the decimals aligned to
# the largest number of decimals in the column.
print_num_column <- function(x) {
  out <- character(length(x))
  code <- sas_miss_code(x)
  miss <- !is.na(code)
  out[miss] <- miss_label(code[miss])
  v <- x[!miss]
  if (length(v) == 0) return(out)
  if (all(v == trunc(v) & abs(v) < 1e12)) {
    out[!miss] <- sprintf("%.0f", v)
    return(out)
  }
  s <- trimws(sas_fmt_best(abs(v), 7L))
  has_e <- grepl("E", s, fixed = TRUE)
  dec <- ifelse(grepl(".", s, fixed = TRUE) & !has_e, nchar(sub("^[^.]*\\.", "", s)), 0L)
  d <- max(dec)
  if (d > 0 && !any(has_e)) s <- sprintf("%.*f", d, sas_f_round(abs(v), 10^-d))
  out[!miss] <- paste0(ifelse(v < 0, "-", ""), s)
  out
}

#' Render a PROC PRINT listing
#' @param d Data with an `.obs` column (physical observation numbers).
#' @param spec List of columns: `name`, `disp`, `char`, `fmt`.
#' @export
sas_print_listing <- function(d, spec, noobs = FALSE, titles = character()) {
  if (nrow(d) == 0) return(character())
  cols <- list()
  heads <- character()
  right <- logical()
  if (!noobs) {
    cols <- list(as.character(d$.obs))
    heads <- "Obs"
    right <- TRUE
  }
  for (s in spec) {
    x <- d[[s$name]]
    txt <- if (s$char) {
      if (is.null(s$fmt)) sas_rtrim(x) else sas_rtrim(sas_format(x, s$fmt))
    } else if (is.null(s$fmt)) {
      print_num_column(x)
    } else {
      trimws(sas_format(x, s$fmt))
    }
    cols <- c(cols, list(txt))
    heads <- c(heads, s$disp)
    right <- c(right, !s$char)
  }
  # headers wider than the data are split (see sql_header_lines())
  hl <- lapply(seq_along(cols), function(k) {
    dw <- max(nchar(cols[[k]]))
    x <- sql_header_lines(heads[k], max(dw, 8L))
    x$width <- max(dw, nchar(x$lines))
    x
  })
  widths <- vapply(hl, `[[`, 1L, "width")
  nl <- max(vapply(hl, function(x) length(x$lines), 1L))
  hrows <- lapply(seq_len(nl), function(j) vapply(hl, function(x) c(rep("", nl - length(x$lines)), x$lines)[j], ""))
  pad <- function(x, w, r) formatC(x, width = if (r) w else -w)
  row_text <- function(vals) sas_rtrim(paste(mapply(pad, vals, widths, right), collapse = "    "))
  body <- vapply(seq_len(nrow(d)), function(i) row_text(vapply(cols, `[`, "", i)), "")
  c(titles, "", vapply(hrows, row_text, ""), "", body)
}

# PROC FORMAT ------------------------------------------------------------------------

proc_format_options <- c("library", "lib", "fmtlib", "nofmtlib", "maxlablen", "maxselen", "mxlablen")

parse_proc_format <- function(p, line) {
  fmtlib_opt <- FALSE
  advance(p)
  while (tk_type(p) != "semi") {
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC FORMAT"), line)
    o <- tk_low(p)
    if (o %in% c("cntlin", "cntlout")) abort_unsupported(paste0("PROC FORMAT ", toupper(o), "="), line)
    if (!o %in% proc_format_options) abort_syntax(paste0("Unrecognized PROC FORMAT option ", toupper(o)), line)
    if (o %in% c("library", "lib") && is_op(p, "=", 1L) && !tk_low(p, 2L) %in% c("work")) {
      abort_unsupported(paste0("PROC FORMAT LIBRARY=", toupper(tk_val(p, 2L))), line)
    }
    if (o == "fmtlib") fmtlib_opt <- TRUE
    advance(p)
    if (is_op(p, "=")) advance(p, 2L)
  }
  advance(p)
  defs <- list()
  repeat {
    if (proc_end_reached(p)) break
    if (tk_type(p) == "semi") {
      advance(p)
      next
    }
    if (is_kw(p, c("run", "quit")) && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    }
    sline <- tk_line(p)
    kw <- tk_low(p)
    if (kw %in% c("value", "invalue")) {
      defs <- c(defs, list(parse_format_def(p, kw, sline)))
      next
    }
    if (kw == "picture") abort_unsupported("PROC FORMAT PICTURE", sline)
    abort_unsupported(paste0("PROC FORMAT statement ", toupper(tk_val(p))), sline)
  }
  if (fmtlib_opt && length(defs) == 0 && length(ls(rt$catalog)) == 0) abort_runtime("Catalog WORK.FORMATS does not exist", line)
  if (fmtlib_opt) abort_unsupported("PROC FORMAT FMTLIB listing", line)
  list(type = "proc", name = "format", line = line, options = list(), stmts = list(), titles = list(), defs = defs)
}

# One range endpoint: number, string, `.`/`.A`, LOW, HIGH, OTHER.
parse_fmt_point <- function(p, line) {
  t <- tk_type(p)
  if (t == "name" && tk_low(p) %in% c("low", "high", "other")) {
    v <- tk_low(p)
    advance(p)
    return(list(kind = v))
  }
  if (t %in% c("str", "str_macro")) {
    v <- tk_val(p)
    advance(p)
    return(list(kind = "str", value = v))
  }
  sign <- 1
  if (is_op(p, "-") && tk_type(p, 1L) == "num") {
    sign <- -1
    advance(p)
    t <- tk_type(p)
  }
  if (t == "num") {
    v <- sign * as.numeric(tk_val(p))
    advance(p)
    return(list(kind = "num", value = v))
  }
  if (t %in% c("missing", "dot")) {
    code <- if (t == "dot") "." else tk_val(p)
    advance(p)
    return(list(kind = "miss", code = code))
  }
  abort_syntax(paste("expected a value or range in PROC FORMAT but found", tok_desc(p)), line)
}

parse_format_def <- function(p, kind, line) {
  advance(p)
  char <- FALSE
  if (is_op(p, "$")) {
    char <- TRUE
    advance(p)
  }
  if (tk_type(p) != "name") abort_syntax(paste("expected a format name after", toupper(kind)), line)
  name <- tk_low(p)
  builtin <- if (kind == "invalue") (if (char) name %in% char_informats else name %in% c(num_informats, "f")) else
    (if (char) name %in% c(char_fmt_names, "char") else (name == "f" || !is.null(fmt_base_name(name))))
  if (builtin) abort_syntax(paste0("Format ", toupper(name), " could not be written because it has the same name as a SAS-supplied format"), line)
  if (grepl("[0-9]$", name)) abort_syntax(paste0("The format name ", toupper(name), " ends in a number, which is invalid"), line)
  advance(p)
  opts <- list()
  if (tk_type(p) == "lparen") {
    advance(p)
    while (tk_type(p) != "rparen") {
      if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in format options"), line)
      o <- tk_low(p)
      advance(p)
      if (is_op(p, "=")) {
        advance(p)
        opts[[o]] <- tk_val(p)
        advance(p)
      } else {
        opts[[o]] <- TRUE
      }
    }
    advance(p)
    bad <- setdiff(names(opts), c("default", "min", "max", "fuzz", "upcase", "just", "notsorted"))
    if (length(bad) > 0) abort_unsupported(paste0("format option ", toupper(bad[1])), line)
  }
  entries <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax(paste("missing semicolon after", toupper(kind)), line)
    keys <- list()
    repeat {
      lo <- parse_fmt_point(p, line)
      key <- list(lo = lo, hi = NULL, lo_excl = FALSE, hi_excl = FALSE)
      if (is_op(p, "<") && is_op(p, "-", 1L)) {
        key$lo_excl <- TRUE
        advance(p, 2L)
        if (is_op(p, "<")) {
          key$hi_excl <- TRUE
          advance(p)
        }
        key$hi <- parse_fmt_point(p, line)
      } else if (is_op(p, "-")) {
        advance(p)
        if (is_op(p, "<")) {
          key$hi_excl <- TRUE
          advance(p)
        }
        key$hi <- parse_fmt_point(p, line)
      }
      keys <- c(keys, list(key))
      if (tk_type(p) == "comma") {
        advance(p)
        next
      }
      break
    }
    if (!is_op(p, "=")) abort_syntax(paste("expected '=' in", toupper(kind), "but found", tok_desc(p)), line)
    advance(p)
    lab <- list()
    if (tk_type(p) %in% c("str", "str_macro")) {
      lab$label <- tk_val(p)
      advance(p)
    } else if (tk_type(p) == "lbrace") {
      advance(p)
      lab$nested <- parse_format(p)
      lab$label <- ""
      if (!tk_type(p) %in% c("rbrace")) abort_syntax("expected ']' after a nested format", line)
      advance(p)
    } else if (kind == "invalue" && tk_type(p) == "name" && tk_low(p) %in% c("_same_", "_error_")) {
      lab$action <- tk_low(p)
      advance(p)
    } else if (tk_type(p) == "num" || (is_op(p, "-") && tk_type(p, 1L) == "num")) {
      neg <- is_op(p, "-")
      if (neg) advance(p)
      lab$label <- tk_val(p)
      lab$num <- (if (neg) -1 else 1) * as.numeric(tk_val(p))
      if (neg) lab$label <- paste0("-", lab$label)
      advance(p)
    } else if (tk_type(p) %in% c("missing", "dot")) {
      lab$label <- if (tk_type(p) == "dot") "." else tk_val(p)
      lab$num <- if (tk_type(p) == "dot") NA_real_ else sas_missing(tk_val(p))
      advance(p)
    } else {
      abort_syntax(paste("expected a label in", toupper(kind), "but found", tok_desc(p)), line)
    }
    for (k in keys) entries <- c(entries, list(c(k, lab)))
  }
  advance(p)
  list(kind = kind, name = name, char = char, opts = opts, entries = entries, line = line)
}

# Turn a parsed VALUE/INVALUE into the run-time catalogue entry.
format_def_value <- function(d) {
  ents <- lapply(d$entries, function(e) {
    out <- list(other = identical(e$lo$kind, "other"), lo_excl = e$lo_excl, hi_excl = e$hi_excl)
    pt <- function(x) switch(x$kind, num = x$value, str = x$value, miss = NA_real_, NULL)
    if (!out$other) {
      if (identical(e$lo$kind, "miss")) {
        out$miss <- sas_miss_code(if (e$lo$code == ".") NA_real_ else sas_missing(e$lo$code))
      }
      out$lo_low <- identical(e$lo$kind, "low")
      out$lo <- pt(e$lo)
      if (is.null(e$hi)) {
        if (identical(e$lo$kind, "high")) {
          out$lo_low <- FALSE
          out$hi_high <- TRUE
          out$lo <- Inf
          out$hi <- Inf
        } else if (!d$char && !out$lo_low) {
          out$hi <- out$lo
        } else if (out$lo_low) {
          out$hi <- -Inf
        }
      } else {
        out$hi_high <- identical(e$hi$kind, "high")
        out$hi <- pt(e$hi)
        if (out$lo_low && !d$char) out$lo <- -Inf
      }
      out$num_key <- identical(e$lo$kind, "num") || identical(e$hi$kind, "num")
      if (d$kind == "value" && !d$char && identical(e$lo$kind, "str")) {
        abort_syntax(paste0("Character range for numeric format ", toupper(d$name)), d$line)
      }
    }
    if (!is.null(e$action)) out$action <- e$action
    if (!is.null(e$nested)) out$nested <- e$nested
    if (d$kind == "invalue") {
      out$value <- if (d$char) e$label else if (!is.null(e$num)) e$num else {
        r <- read_std_num(e$label %||% "", NA_integer_)
        r$v
      }
    }
    out$label <- e$label %||% ""
    out
  })
  labs <- vapply(ents, function(e) if (!is.null(e$nested)) fmt_width(e$nested) %|NA|% nchar(e$label) else nchar(e$label), 1L)
  dw <- if (!is.null(d$opts$default)) as.integer(d$opts$default) else max(c(1L, labs))
  if (d$kind == "value") dw <- min(dw, 40L)
  list(kind = d$kind, name = d$name, char = d$char, entries = ents, default_w = dw,
       upcase = isTRUE(d$opts$upcase))
}

translate_proc_format <- function(step) {
  defs <- lapply(step$defs, format_def_value)
  for (d in defs) sas_define_format(d)
  code <- c(sprintf("# ---- PROC FORMAT (SAS line %d)", as.integer(step$line)),
            vapply(defs, function(d) paste0("sas_define_format(", r_lit(d), ")"), ""))
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC FORMAT", line = step$line,
       outputs = character(), meta = list())
}
