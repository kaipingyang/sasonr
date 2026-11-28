# PROC steps (SORT, PRINT) and global statements (TITLE, OPTIONS).
#
# PROC IR: list(type = "proc", name, line, options = named list, stmts = list)
# Every other PROC, option or statement fails loud.

# System options OPTIONS accepts.  LINESIZE, NOTES, SYNTAXCHECK and the
# listing layout options (CENTER, DATE, NUMBER, PAGESIZE, FORMDLIM) are kept in
# the session (engine_global()); procedure output written under CENTER, DATE
# or NUMBER, under a FORMDLIM= other than a blank (from the second output on)
# or longer than one page of a finite PAGESIZE= fails loud
# (engine_add_listing(), run.R).
# PAGENO= only changes the page numbers, which are written only under NUMBER.
# SOURCE / NOSOURCE turn the source echo of the rich log views on and off
# (sas_log()); MPRINT writes the statements macros generate (log_mprint());
# the others do not change what sasonr writes (timing ...).
options_ignorable <- c(
  "center", "nocenter", "date", "nodate", "number", "nonumber", "ls", "linesize",
  "ps", "pagesize", "notes", "nonotes", "source", "nosource", "nosource2",
  "mprint", "nomprint", "nomlogic", "nosymbolgen", "nomprintnest", "nomlogicnest",
  "formdlim", "pageno", "stimer", "nostimer",
  "fullstimer", "nofullstimer", "msglevel", "errors",
  # NOSYNTAXCHECK: after an ERROR the following steps still run (sas_run(on_error = "sas"))
  "nosyntaxcheck", "syntaxcheck"
)

# `open_code`: the statement stands outside a step (sas_parse()); its SAS
# ERRORs are raised with abort_global() for the error model (errors.R).
parse_global_stmt <- function(p, open_code = FALSE) {
  line <- tk_line(p)
  low <- tk_low(p)
  g_err <- function(msg, what = "syntax") abort_global(msg, line, what, open_code)
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
  if (low == "ods") {
    # `ODS HTML;` and `ODS HTML CLOSE;` open and close the HTML destination of
    # the result (sas_html()); file options and other destinations fail loud
    if (is_kw(p, "html", 1L) && tk_type(p, 2L) == "semi") {
      advance(p, 3L)
      return(list(type = "global", kind = "ods_html", open = TRUE, line = line))
    }
    if (is_kw(p, "html", 1L) && is_kw(p, "close", 2L) && tk_type(p, 3L) == "semi") {
      advance(p, 4L)
      return(list(type = "global", kind = "ods_html", open = FALSE, line = line))
    }
    if (is_kw(p, "output", 1L)) return(parse_ods_output(p, line))
    # `ODS RTF FILE="path";` and `ODS RTF CLOSE;` (default style only)
    if (is_kw(p, "rtf", 1L) && is_kw(p, "close", 2L) && tk_type(p, 3L) == "semi") {
      advance(p, 4L)
      return(list(type = "global", kind = "ods_rtf", open = FALSE, line = line))
    }
    if (is_kw(p, "rtf", 1L) && is_kw(p, "file", 2L) && is_op(p, "=", 3L) && tk_type(p, 4L) %in% c("str", "str_macro") &&
        tk_type(p, 5L) == "semi") {
      file <- tk_val(p, 4L)
      advance(p, 6L)
      return(list(type = "global", kind = "ods_rtf", open = TRUE, file = file, line = line))
    }
    # `ODS HTML FILE="path" <STYLE=HTMLBlue>;`: the document of the outputs
    # until ODS HTML CLOSE; written to that file
    if (is_kw(p, "html", 1L) && (is_kw(p, c("file", "body"), 2L)) && is_op(p, "=", 3L) && tk_type(p, 4L) %in% c("str", "str_macro")) {
      file <- tk_val(p, 4L)
      advance(p, 5L)
      if (is_kw(p, "style") && is_op(p, "=", 1L) && tk_type(p, 2L) == "name") {
        if (tk_low(p, 2L) != "htmlblue") abort_unsupported(paste0("ODS HTML STYLE=", toupper(tk_val(p, 2L)), " (supported: HTMLBlue)"), line)
        advance(p, 3L)
      }
      if (tk_type(p) != "semi") abort_unsupported("ODS HTML options other than FILE= and STYLE=HTMLBlue", line)
      advance(p)
      return(list(type = "global", kind = "ods_html", open = TRUE, file = file, line = line))
    }
    if (is_kw(p, "html", 1L)) {
      abort_unsupported("ODS HTML options other than FILE= and STYLE=HTMLBlue", line)
    }
    # ODS TRACE ON; / ODS TRACE OFF;
    if (is_kw(p, "trace", 1L) && is_kw(p, c("on", "off"), 2L) && tk_type(p, 3L) == "semi") {
      on <- tk_low(p, 2L) == "on"
      advance(p, 4L)
      return(list(type = "global", kind = "ods_trace", on = on, line = line))
    }
    abort_unsupported(paste0("ODS ", toupper(tk_val(p, 1L)), " statement"), line)
  }
  if (low == "filename") {
    advance(p)
    # `FILENAME 'text';` is not an ERROR: the log gets "WARNING: No logical
    # assign for filename 'text'." (with the quotes written, "..." for a
    # double-quoted name) and SYSFILRC is set to -20004; `col` locates the
    # quote (engine_quote_at())
    if (tk_type(p) %in% c("str", "str_macro") && tk_type(p, 1L) == "semi" && !grepl("['\"]", tk_val(p))) {
      text <- tk_val(p)
      col <- tk_col(p)
      advance(p, 2L)
      return(list(type = "global", kind = "filename_noref", text = text, col = col, line = line))
    }
    if (tk_type(p) != "name") g_err("expected a fileref after FILENAME")
    ref <- tk_low(p)
    advance(p)
    if (!tk_type(p) %in% c("str", "str_macro")) abort_unsupported("FILENAME without a quoted physical path", line)
    path <- tk_val(p)
    # SYSFILRC is supported for paths of an existing or missing file or directory
    if (!nzchar(path) || grepl("[*?]", path)) abort_unsupported("FILENAME with an empty or wildcard physical path", line)
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
    ls <- NULL
    ps <- NULL
    formdlim <- NULL
    while (tk_type(p) != "semi") {
      if (at_eof(p)) g_err("missing semicolon after OPTIONS")
      if (tk_type(p) != "name") g_err(paste("unexpected", tok_desc(p), "in OPTIONS"))
      opt <- tk_low(p)
      opts <- c(opts, opt)
      if (!opt %in% options_ignorable) abort_unsupported(paste0("system option ", toupper(opt)), line)
      advance(p)
      if (is_op(p, "=")) {
        advance(p)
        if (opt %in% c("linesize", "ls")) {
          v <- tk_low(p)
          ls <- if (identical(v, "max")) 256L else suppressWarnings(as.integer(v))
          if (is.na(ls) || ls < 64L || ls > 256L) {
            # sasonr models LINESIZE= as follows: a name other than
            # MAX (LINESIZE=ABC) is ERROR 18-12; other invalid values (a
            # number outside 64-256 ...) fail loud (engine_global_error())
            if (tk_type(p) == "name") {
              g_err("Option value for SAS option LINESIZE must be between 64 and 256. (ERROR 18-12)", "linesize")
            }
            g_err("invalid LINESIZE= value")
          }
        }
        if (opt %in% c("pagesize", "ps")) {
          v <- tk_low(p)
          ps <- if (identical(v, "max")) Inf else suppressWarnings(as.numeric(v))
          if (is.na(ps) || ps != round(ps) || ps < 15) abort_unsupported(paste0("OPTIONS ", toupper(opt), "=", toupper(tk_val(p))), line)
        }
        if (opt == "formdlim") {
          if (!tk_type(p) %in% c("str", "str_macro")) abort_unsupported("OPTIONS FORMDLIM= without a quoted value", line)
          formdlim <- tk_val(p)
        }
        # the data-error dump limit is fixed at the SAS default ERRORS=20
        if (opt == "errors" && !identical(tk_low(p), "20")) {
          abort_unsupported(paste0("OPTIONS ERRORS=", toupper(tk_val(p)), " (only the default ERRORS=20 is implemented)"), line)
        }
        if (tk_type(p) %in% c("name", "num", "str", "str_macro")) advance(p) else g_err("invalid OPTIONS value")
      }
    }
    # the options take effect after the statement (its last line: the source echo)
    end_line <- tk_line(p)
    advance(p)
    return(list(type = "global", kind = "options", opts = opts, ls = ls, ps = ps, formdlim = formdlim, line = line,
                end_line = end_line))
  }
  # LIBNAME ref <XPORT|BASE|V9> 'path' <ACCESS=READONLY>; and LIBNAME ref CLEAR;
  # (read-only libraries, spec 01 P0-9)
  if (low == "libname") {
    advance(p)
    if (tk_type(p) != "name") abort_unsupported("LIBNAME without a libref", line)
    ref <- tk_low(p)
    advance(p)
    if (is_kw(p, "clear") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      return(list(type = "global", kind = "libname_clear", ref = ref, line = line))
    }
    engine <- "V9"
    if (tk_type(p) == "name") {
      if (!tk_low(p) %in% c("xport", "base", "v9")) abort_unsupported(paste0("the LIBNAME engine ", toupper(tk_val(p))), line)
      engine <- if (tk_low(p) == "xport") "XPORT" else "V9"
      advance(p)
    }
    if (!tk_type(p) %in% c("str", "str_macro")) abort_unsupported("LIBNAME without a quoted physical path", line)
    path <- tk_val(p)
    advance(p)
    if (is_kw(p, "access") && is_op(p, "=", 1L) && is_kw(p, "readonly", 2L)) advance(p, 3L)
    if (tk_type(p) != "semi") abort_unsupported("LIBNAME options other than ACCESS=READONLY", line)
    advance(p)
    if (ref %in% c("work", "sashelp", "sasuser", "dictionary")) abort_unsupported(paste0("LIBNAME ", toupper(ref)), line)
    # a libref has at most 8 characters (a longer one is an ERROR of the statement)
    if (nchar(ref) > 8L) abort_unsupported(paste0("the ERROR of LIBNAME ", toupper(ref), " (a libref longer than 8 characters)"), line)
    return(list(type = "global", kind = "libname", ref = ref, engine = engine, path = path, line = line))
  }
  # a word that starts no statement SAS knows (measured in open code)
  if (open_code && !low %in% global_stmt_known && !split_trailing_digits(low)$base %in% global_stmt_known) {
    abort_global("Statement is not valid or it is used out of proper order. (ERROR 180-322)", line, "unknown_stmt",
                 open_code, col = tk_col(p), token = tk_val(p))
  }
  abort_unsupported(paste0("global statement ", toupper(tk_val(p))), line)
}

# Global statements of SAS that sasonr does not implement (they fail loud;
# any other word in open code is ERROR 180-322).
global_stmt_known <- c("ods", "dm", "endsas", "goptions", "axis", "symbol", "legend", "pattern", "catname",
                       "lock", "skip", "page", "resetline", "sasfile", "x", "systask", "waitfor", "missing",
                       "%include", "include", "sysecho", "datalines", "cards", "run", "quit", "data", "proc",
                       "abort", "comment", "endrsubmit", "rsubmit", "signon", "signoff", "killtask", "listtask",
                       "ods", "title", "footnote", "options", "libname", "filename", "%let", "infile", "file")

proc_end_reached <- function(p) {
  at_eof(p) || (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L))
}

parse_proc_step <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name") abort_syntax("expected a procedure name after PROC", line)
  name <- tk_low(p)
  if (name == "format") return(parse_proc_format(p, line))
  if (name %in% c("means", "summary")) return(parse_proc_means(p, line, name))
  if (name == "tabulate") return(parse_proc_tabulate(p, line))
  if (name == "univariate") return(parse_proc_univariate(p, line))
  if (name == "freq") return(parse_proc_freq(p, line))
  if (name == "transpose") return(parse_proc_transpose(p, line))
  if (name == "contents") return(parse_proc_contents(p, line))
  if (name == "datasets") return(parse_proc_datasets(p, line))
  if (name == "append") return(parse_proc_append(p, line))
  if (name == "delete") return(parse_proc_delete(p, line))
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
      } else if (name == "print" && opt %in% c("obs", "split")) {
        if (!tk_type(p) %in% c("str", "str_macro")) abort_unsupported(paste0("PROC PRINT option ", toupper(opt), "= without a quoted string"), oline)
        opts[[opt]] <- tk_val(p)
        advance(p)
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
      print = opt %in% c("noobs", "uniform", "n", "label")
    )
    if (!ok) {
      proc_option_error(name, p)
      abort_unsupported(paste0("PROC ", toupper(name), " option ", toupper(opt)), oline)
    }
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
        if (name == "print" && any(b$descending)) abort_unsupported("BY DESCENDING in PROC PRINT", sline)
        b
      },
      "print id" = ,
      "print sum" = ,
      "print sumby" = {
        kw <- low
        advance(p)
        items <- list()
        while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
        expect_semi(p, toupper(kw))
        if (length(items) == 0) abort_syntax(paste(toupper(kw), "statement without variables"), sline)
        list(stmt = kw, items = items, line = sline)
      },
      "sort where" = ,
      "print where" = parse_where(p),
      "print format" = parse_format_stmt(p),
      "print var" = {
        advance(p)
        items <- list()
        while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
        if (is_op(p, "/")) abort_unsupported("VAR statement options in PROC PRINT", sline)
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
  if (!is.null(ref$options$in_var)) {
    if (step$name == "print") abort_syntax("Invalid option name IN. (ERROR 22-7)", step$line)
    abort_unsupported(paste0("IN= on PROC ", toupper(step$name), " DATA="), step$line)
  }
  st <- new_state(meta)
  rd <- an_reader(list(stmt = "set", datasets = list(ref), line = step$line), st)
  known <- st$order
  w <- slot_where_effective(Filter(function(s) s$stmt == "where", step$stmts))
  cond <- NULL
  if (!is.null(w)) {
    cond <- an_where(w$cond, st)
    extra <- setdiff(st$order, known)
    if (length(extra) > 0) abort_runtime(paste0("Variable ", extra[1], " is not on file WORK.", toupper(ref$name)), w$line)
  }
  list(ds = rd$datasets[[1]], st = st, where = cond)
}

proc_meta <- function(st, vars, out_names = vars) {
  type <- lapply(vars, function(v) st$info[[v]]$type)
  len <- lapply(vars, function(v) if (identical(st$info[[v]]$type, "char")) st$info[[v]]$len else vctrs::vec_cast(st$numlen[[v]] %||% 8L, integer()))
  fmts <- lapply(vars, function(v) st$formats[[v]])
  infs <- lapply(vars, function(v) st$informats[[v]])
  labs <- lapply(vars, function(v) if (v %in% names(st$labels)) st$labels[[v]] else NULL)
  disp <- Map(function(v, on) if (on == v) st$info[[v]]$disp %||% v else on, vars, out_names)
  names(type) <- names(len) <- names(fmts) <- names(disp) <- names(infs) <- names(labs) <- out_names
  list(vars = out_names, type = type, len = len, formats = Filter(Negate(is.null), fmts), disp = disp,
       labels = Filter(Negate(is.null), labs), informats = Filter(Negate(is.null), infs))
}

proc_source_code <- function(inp, obs_col = FALSE) {
  cgi_text(ds_code(inp$ds, list(arrays = list()), obs_col = obs_col, where = inp$where))
}

translate_proc <- function(step, meta, last = NULL) {
  if (step$name == "format") return(translate_proc_format(step))
  if (step$name %in% c("means", "summary")) return(translate_proc_means(step, meta, last))
  if (step$name == "tabulate") return(translate_proc_tabulate(step, meta, last))
  if (step$name == "univariate") return(translate_proc_univariate(step, meta, last))
  if (step$name == "freq") return(translate_proc_freq(step, meta, last))
  if (step$name == "transpose") return(translate_proc_transpose(step, meta, last))
  if (step$name == "contents") return(translate_proc_contents(step, meta, last))
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
    plan <- ds_out_plan(vars, out$options, out$name, step$line)
    out_vars <- plan$vars
    out_names <- plan$names
    sel <- select_code(list(vars = out_vars, out_names = out_names))
    code <- c(
      header,
      paste0(".df <- ", proc_source_code(inp)),
      sprintf("work$%s <- sas_sort_df(.df, %s, %s, nodupkey = %s, noduprecs = %s) |>\n  %s", r_name(out$name),
              r_chr_vec(by$vars), r_lit(by$descending), isTRUE(step$options[["nodupkey"]]) || isTRUE(step$options[["nodupkeys"]]),
              isTRUE(step$options[["noduprecs"]]) || isTRUE(step$options[["noduprec"]]) || isTRUE(step$options[["nodup"]]), sel)
    )
    m <- proc_meta(inp$st, out_vars, out_names)
    if (all(by$vars %in% out_vars)) {
      m$sortedby <- list(vars = out_names[match(by$vars, out_vars)], descending = by$descending,
                         nodupkey = isTRUE(step$options[["nodupkey"]]) || isTRUE(step$options[["nodupkeys"]]),
                         noduprec = isTRUE(step$options[["noduprecs"]]) || isTRUE(step$options[["noduprec"]]) || isTRUE(step$options[["nodup"]]))
    }
    res_meta <- stats::setNames(list(m), out$name)
    return(list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC SORT",
                line = step$line, outputs = out$name, meta = res_meta, compile_notes = plan$warnings))
  }
  # PROC PRINT
  # FORMAT statements attach formats for this step only
  for (fs in Filter(function(s) identical(s$stmt, "format"), step$stmts)) for (sp in fs$specs) check_format(sp$fmt, fs$line)
  proc_apply_attr_stmts(step$stmts, inp$st)
  for (v in names(inp$st$formats)) {
    f <- inp$st$formats[[v]]
    t <- inp$st$info[[v]]$type
    if (is.null(f) || is.null(t)) next
    if (t == "num" && isTRUE(f$char)) abort_syntax(paste0("The character format ", f$text, " cannot be used with numeric variable ", toupper(v)), step$line)
    if (t == "char" && !isTRUE(f$char)) abort_syntax(paste0("The format $", toupper(f$name), " was not found or could not be loaded"), step$line)
  }
  resolve_stmts <- function(kw) {
    ss <- Filter(function(s) s$stmt == kw, step$stmts)
    if (length(ss) == 0) return(NULL)
    out <- vctrs::list_unchop(lapply(ss, function(s) {
      tmp <- new_state(list())
      tmp$order <- vars
      tmp$info <- inp$st$info[vars]
      resolve_items(s$items, tmp, s$line, register = FALSE)
    }), ptype = character(), name_spec = rlang::zap())
    miss <- setdiff(out, vars)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found"), ss[[1]]$line)
    unique(out)
  }
  by_stmts <- Filter(function(s) s$stmt == "by", step$stmts)
  if (length(by_stmts) > 1) abort_unsupported("more than one BY statement in PROC PRINT", by_stmts[[2]]$line)
  by <- if (length(by_stmts) == 1) by_stmts[[1]]$vars else character()
  miss <- setdiff(by, vars)
  if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found"), by_stmts[[1]]$line)
  ids <- resolve_stmts("id") %||% character()
  sums <- resolve_stmts("sum") %||% character()
  sumby <- resolve_stmts("sumby") %||% character()
  pvars <- resolve_stmts("var")
  has_var <- !is.null(pvars)
  if (!has_var) pvars <- setdiff(vars, c(by, ids))
  if (has_var && length(pvars) == 0) {
    return(list(code = paste(c(header, "# no variables to print"), collapse = "\n"), level = "PROC",
                reasons = "PROC PRINT", line = step$line, outputs = character(), meta = list()))
  }
  o <- step$options
  # outside the measured rules: fail loud
  if (length(intersect(ids, pvars)) > 0) abort_unsupported("a PROC PRINT ID variable that is also in the VAR list", step$line)
  if (length(ids) > 1) abort_unsupported("more than one PROC PRINT ID variable", step$line)
  if (length(intersect(ids, by)) > 0) abort_unsupported("a PROC PRINT ID variable that is also a BY variable", step$line)
  if (length(sums) > 0) {
    bad <- setdiff(sums, pvars)
    if (length(bad) > 0) abort_unsupported(paste0("a PROC PRINT SUM variable that is not printed (", toupper(bad[1]), ")"), step$line)
    if (any(vapply(sums, function(v) identical(inp$st$info[[v]]$type, "char"), TRUE))) abort_unsupported("SUM of a character variable in PROC PRINT", step$line)
    if (length(by) > 1) abort_unsupported("PROC PRINT SUM with more than one BY variable", step$line)
    if (isTRUE(o[["n"]])) abort_unsupported("PROC PRINT option N together with SUM", step$line)
  }
  if (length(sumby) > 0 && !(length(by) == 1 && identical(sumby, by) && length(sums) > 0)) {
    abort_unsupported("PROC PRINT SUMBY other than the BY variable with SUM", step$line)
  }
  if (isTRUE(o[["uniform"]]) && length(by) > 0) abort_unsupported("PROC PRINT option UNIFORM with BY", step$line)
  use_label <- isTRUE(o[["label"]]) || !is.null(o[["split"]])
  col_spec <- function(v) {
    inf <- inp$st$info[[v]]
    lab <- if (use_label && v %in% names(inp$st$labels)) inp$st$labels[[v]] else NULL
    list(name = v, disp = inf$disp %||% v, label = lab, char = identical(inf$type, "char"),
         fmt = if (is.null(inp$st$formats[[v]])) NULL else inp$st$formats[[v]][c("name", "w", "d", "char", "text")])
  }
  info <- lapply(by, function(v) list(disp = inp$st$info[[v]]$disp %||% v, fmt = inp$st$formats[[v]]))
  names(info) <- by
  spec <- list(
    line = step$line, data = inp$ds$name, cols = lapply(pvars, col_spec), id = if (length(ids)) col_spec(ids) else NULL, id_n = length(ids),
    noobs = isTRUE(o[["noobs"]]), obs_label = o[["obs"]] %||% "Obs", n = isTRUE(o[["n"]]), split = o[["split"]],
    by = by, by_info = info, sums = sums
  )
  # a data set without variables: "No variables in data set" (log_emit_reads()), no listing
  code <- c(
    header,
    paste0(".df <- ", proc_source_code(inp, obs_col = TRUE)),
    if (length(vars) > 0) c(
      paste0(".r <- sas_rt_proc(\"print\", .df, ", r_lit(spec), ")"),
      ".lst <- sas_lst_c(.lst, sas_titles_once(.titles, .r$listing))",
      ".ods <- c(.ods, .r$tables)"
    )
  )
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC PRINT",
       line = step$line, outputs = character(), meta = list())
}

# Runtime -------------------------------------------------------------------------

#' PROC SORT: stable sort (missing values first) with optional NODUPKEY
#' @export
#' @keywords internal
sas_sort_df <- function(df, by, descending = rep(FALSE, length(by)), nodupkey = FALSE, noduprecs = FALSE) {
  rt <- rt_diag()
  df <- df[sas_order(df, by, descending), , drop = FALSE]
  if (nodupkey && nrow(df) > 0) {
    n0 <- nrow(df)
    df <- df[rlang::exec(key_change, !!!unname(as.list(df[by]))), , drop = FALSE]
    rt$proc_notes <- c(rt$proc_notes, sprintf("NOTE: %d observations with duplicate key values were deleted.", n0 - nrow(df)))
  }
  if (noduprecs && nrow(df) > 1) {
    n0 <- nrow(df)
    same <- Reduce(`&`, lapply(df, function(v) c(FALSE, same_value(v[-1L], v[-nrow(df)]))))
    df <- df[!same, , drop = FALSE]
    rt$proc_notes <- c(rt$proc_notes, sprintf("NOTE: %d duplicate observations were deleted.", n0 - nrow(df)))
  }
  tibble::as_tibble(df)
}

#' Apply FIRSTOBS= / OBS= to a data frame (after WHERE)
#' @export
#' @keywords internal
sas_obs_window <- function(df, firstobs = NULL, obs = NULL) {
  from <- firstobs %||% 1
  to <- min(obs %||% nrow(df), nrow(df))
  df[seq_len(max(0, to - from + 1)) + from - 1, , drop = FALSE]
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
  # the field: the widest value without its sign, one position more when a
  # value is negative (proc_print.R places the heading) [measured,
  # print_layout p08-p10]
  lay <- function(out, txt) {
    attr(out, "sfw") <- max(nchar(sub("^-", "", txt))) + any(startsWith(txt, "-"))
    attr(out, "hdec") <- any(grepl(".", txt, fixed = TRUE))
    out
  }
  if (all(v == trunc(v) & abs(v) < 1e12)) {
    out[!miss] <- sprintf("%.0f", v)
    return(lay(out, out[!miss]))
  }
  # ordinary magnitudes: a common number of decimals, as many as the values
  # need but no more than 6 minus the widest integer part, and at least 2
  # when that is less (1253 / 11350.7 / 3098.63 -> 1253.00; 42 / 3.14159 /
  # 123456789012 -> 42.00)
  ip <- nchar(sprintf("%.0f", trunc(abs(v))))
  if (max(ip) <= 15L) {
    dmax <- max(2L, 6L - max(ip))
    need <- vapply(v, function(x) {
      for (dd in 0:dmax) if (abs(x - round(x, dd)) <= 1e-9 * max(1, abs(x))) return(dd)
      dmax
    }, 1L)
    d <- max(need)
    out[!miss] <- vapply(v, display_fixed, "", d = d)
    # the decimals held at 2 (more than 4 integer digits) and a value rounded
    # to them: the heading is laid out as over a formatted column
    # (listing_base_print())
    return(lay(structure(out, rounded = 6L - max(ip) < 2L && any(abs(v - round(v, d)) > 1e-9 * pmax(1, abs(v))), dec = d), out[!miss]))
  }
  s <- trimws(sas_fmt_best(abs(v), 7L))
  has_e <- grepl("E", s, fixed = TRUE)
  dec <- ifelse(grepl(".", s, fixed = TRUE) & !has_e, nchar(sub("^[^.]*\\.", "", s)), 0L)
  d <- max(dec)
  if (d > 0 && !any(has_e)) s <- vapply(abs(v), display_fixed, "", d = d)
  out[!miss] <- paste0(ifelse(v < 0, "-", ""), s)
  # (not measured: the layout of a rounded column of large magnitudes)
  structure(out, rounded = NA, dec = d)
}

# PROC PRINT splits a mixed-case variable name that is wider than its data at
# a lower-to-upper case boundary into two lines (ToadName -> Toad / Name,
# MeanDaysRain -> Mean / DaysRain: the shortest widest line, the earliest
# split on ties); other names follow sql_header_lines().
print_header_lines <- function(h, dw) {
  n <- nchar(h)
  ch <- strsplit(h, "")[[1]]
  cut <- which(ch[-n] %in% letters & ch[-1] %in% LETTERS)
  # (measured once: x_SumWgt over a narrow column is written x_ / Sum / Wgt)
  if (n > dw && length(cut) > 0 && grepl("_", h, fixed = TRUE)) {
    abort_unsupported(paste0("the heading of the variable ", h, " (an underscore and a lowercase-uppercase change, wider than its values)"))
  }
  # one lowercase-uppercase change: split there when the name is longer than
  # the values and than 4 (AbCd stays whole); more changes split at places
  # not yet known [measured, names of 4 to 25 characters over columns of 1 to 8]
  if (n > max(dw, 4L) && length(cut) > 1L && !grepl("_", h, fixed = TRUE)) {
    abort_unsupported(paste0("the heading of the variable ", h, " (several lowercase-uppercase changes, wider than its values)"))
  }
  if (n > max(dw, 4L) && length(cut) > 0 && !grepl("_", h, fixed = TRUE)) {
    m <- vapply(cut, function(i) max(i, n - i), 1L)
    i <- cut[which.min(m)]
    return(list(lines = c(substr(h, 1L, i), substr(h, i + 1L, n)), width = max(dw, min(m))))
  }
  w <- max(dw, 8L)
  lines <- print_split_underscore(h, w)
  if (length(lines) <= 2L) return(sql_header_lines(h, w))
  if (dw > 8L) abort_unsupported(paste0("the heading of the variable ", h, " (more than two lines over a column wider than 8)"))
  list(lines = lines, width = max(w, nchar(lines)))
}

# A name without a lowercase-uppercase change splits like a PROC SQL header
# (sql_header_lines), and each piece longer than the column without its
# trailing underscore splits again (abcd_efgh_ijkl: abcd_ / efgh_ / ijkl)
# [measured, names of 4 to 16 characters over columns of 1 to 8].
print_split_underscore <- function(h, w) {
  if (nchar(sub("_+$", "", h)) <= w) return(h)
  r <- sql_header_lines(h, 0L)
  if (length(r$lines) < 2L) return(h)
  c(print_split_underscore(r$lines[1], w), print_split_underscore(r$lines[2], w))
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
    if (kw == "picture") {
      defs <- c(defs, list(parse_format_def(p, kw, sline)))
      next
    }
    abort_unsupported(paste0("PROC FORMAT statement ", toupper(tk_val(p))), sline)
  }
  if (fmtlib_opt && length(defs) == 0 && length(ls(sas_rt_active()$formats %||% emptyenv())) == 0) abort_runtime("Catalog WORK.FORMATS does not exist", line)
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
        if (o == "default" && !tk_type(p) %in% c("num", "op")) {
          abort_syntax("expecting one of the following: a numeric constant, a datetime constant.", line)
        }
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
      p$fmt_no_period <- TRUE
      lab$nested <- parse_format(p)
      p$fmt_no_period <- FALSE
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
    # a PICTURE label: the template and its options in parentheses
    if (kind == "picture" && is.null(lab$nested)) {
      po <- list()
      if (tk_type(p) == "lparen") {
        advance(p)
        while (tk_type(p) != "rparen") {
          if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in picture options"), line)
          o <- tk_low(p)
          advance(p)
          if (is_op(p, "=")) {
            advance(p)
            neg <- is_op(p, "-")
            if (neg) advance(p)
            po[[o]] <- if (tk_type(p) == "num") (if (neg) -1 else 1) * as.numeric(tk_val(p)) else tk_val(p)
            advance(p)
          } else {
            po[[o]] <- TRUE
          }
        }
        advance(p)
      }
      if (!is.null(po$multiplier)) po$mult <- po$multiplier
      po$multiplier <- NULL
      bad <- setdiff(names(po), c("prefix", "mult", "noedit"))
      if (length(bad)) abort_unsupported(paste0("the PICTURE option ", toupper(bad[1])), line)
      lab$picture <- po
    }
    for (k in keys) entries <- c(entries, list(c(k, lab)))
  }
  advance(p)
  if (kind == "picture") {
    if (char) abort_syntax(paste0("a PICTURE format is numeric (", toupper(name), ")"), line)
    if (length(setdiff(names(opts), "default"))) abort_unsupported(paste0("the PICTURE format option ", toupper(setdiff(names(opts), "default")[1])), line)
  }
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
    if (!is.null(e$picture)) out$picture <- e$picture
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
  # a PICTURE is a numeric format of the catalogue's VALUE kind
  kind <- if (d$kind == "picture") "value" else d$kind
  list(kind = kind, name = d$name, char = d$char, entries = ents, default_w = dw,
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

# The syntax ERROR of an unknown option word in the PROC SORT, PRINT and
# MEANS statements: 22-322 with the procedure's expecting list, then 202-322,
# both at the word; SYSERR and SYSCC 3000.  The expecting lists name the
# options; a word in the list, or one of the documented short forms that the
# lists leave out, is a valid option that sasonr does not implement (fail
# loud, the caller).
proc_option_expecting <- list(
  sort = paste(
    "Syntax error, expecting one of the following: ;, (, ASCII, BUFFNO, DANISH, DATA, DATECOPY, DETAILS, DIAG,",
    "DUPOUT, EBCDIC, EQUALS, FINNISH, FORCE, IN, ISA, L, LEAVE, LIST, MESSAGE, MSG, NATIONAL, NODUP, NODUPKEY,",
    "NODUPKEYS, NODUPLICATE, NODUPLICATES, NODUPREC, NODUPRECS, NODUPS, NOEQUALS, NORWEGIAN, NOTHREADS,",
    "NOUNIKEY, NOUNIKEYS, NOUNIQUEKEY, NOUNIQUEKEYS, NOUNIQUEREC, NOUNIQUERECS, NOUNIREC, NOUNIRECS, OSA, OUT,",
    "OVERWRITE, PAGESIZE, PRESORTED, PSIZE, REVERSE, SIZE, SORTSEQ, SORTSIZE, SORTWKNO, SWEDISH, T, TAGSORT,",
    "TECH, TECHNIQUE, TESTHSI, THREADS, UNIOUT, UNIQUEOUT, WKNO, WORKNO."),
  print = paste(
    "Syntax error, expecting one of the following: ;, (, BLANKLINE, CONTENTS, DATA, DOUBLE, GRANDTOTAL_LABEL,",
    "GRANDTOT_LABEL, GRAND_LABEL, GTOTAL_LABEL, GTOT_LABEL, HEADING, LABEL, N, NOOBS, NOSUMLABEL, OBS, ROUND,",
    "ROWS, SPLIT, STYLE, SUMLABEL, UNIFORM, WIDTH."),
  means = paste(
    "Syntax error, expecting one of the following: ;, (, ALPHA, CHARTYPE, CLASSDATA, CLM, COMPLETETYPES, CSS,",
    "CV, DATA, DESCEND, DESCENDING, DESCENDTYPES, EXCLNPWGT, EXCLNPWGTS, EXCLUSIVE, FW, IDMIN, INCAS, KURTOSIS,",
    "LCLM, MAX, MAXDEC, MEAN, MEDIAN, MIN, MISSING, MODE, N, NDEC, NMISS, NOLABELS, NONOBS, NOPRINT, NOTHREADS,",
    "NOTRAP, NWAY, ORDER, P1, P10, P20, P25, P30, P40, P5, P50, P60, P70, P75, P80, P90, P95, P99, PCTLDEF,",
    "PRINT, PRINTALL, PRINTALLTYPES, PRINTIDS, PRINTIDVARS, PROBT, Q1, Q3, QMARKERS, QMETHOD, QNTLDEF, QRANGE,",
    "RANGE, SKEWNESS, STACKODS, STACKODSOUTPUT, STDDEV, STDERR, SUM, SUMSIZE, SUMWGT, T, THREADS, UCLM, USS,",
    "VAR, VARDEF."))

proc_option_aliases <- list(sort = character(), print = c("l", "r", "d", "u", "h"),
                            means = c("std", "stdmean", "prt", "skew", "kurt"))

proc_option_error <- function(proc, p) {
  w <- tk_low(p)
  known <- tolower(trimws(strsplit(sub("^Syntax error, expecting one of the following: ", "", sub("\\.$", "", proc_option_expecting[[proc]])), ",")[[1]]))
  if (w %in% c(known, proc_option_aliases[[proc]])) return(invisible())
  abort_syntax_numbered(paste0(proc_option_expecting[[proc]], " (ERROR 22-322)"), tk_line(p), tk_col(p), tk_val(p),
                        also = "ERROR 202-322: The option or parameter is not recognized and will be ignored.", syserr = 3000)
}
