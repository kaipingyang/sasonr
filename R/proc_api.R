# The R interface of PROC steps (spec 11: one path).  A PROC step is an IR
# (the list the parser produces).  The verb converts its R arguments into that
# IR and the engine runs it.  `sas_run()` builds the call of a parsed step from
# its IR and converts the call's arguments back (proc_ir_via_call()), so the
# printed call is what runs; it fails loud when the round trip does not give
# the same IR.  PROC SORT and PROC PRINT so far.

#' The SAS source of a step
#'
#' Provenance of a step verb in a script: the program lines it came from and
#' its text, written as the source echo of the log. A verb called without it
#' runs the same computation; the log then echoes a SAS form of the call.
#' @param lines Integer program line numbers of the step.
#' @param text The SAS text of the step (after macro resolution).
#' @return A `sas_src` object.
#' @examples
#' sas_src(8:10, "proc sort data=adsl out=bysubj;\n  by descending age;\nrun;")
#' @export
sas_src <- function(lines, text) {
  lines <- vctrs::vec_cast(lines, integer())
  if (!length(lines) || anyNA(lines)) rlang::abort("`lines` must be line numbers.", class = "sasonr_error")
  if (!rlang::is_string(text)) rlang::abort("`text` must be a single string.", class = "sasonr_error")
  structure(list(lines = lines, text = text), class = "sas_src")
}

#' @export
print.sas_src <- function(x, ...) {
  cat("<sas_src> lines ", min(x$lines), "-", max(x$lines), "\n", x$text, "\n", sep = "")
  invisible(x)
}

# A data set reference given as a string ("adsl", "lib.adsl",
# "adsl(keep=a b where=(x > 1))"), parsed as the PROC statement parses DATA=
# and OUT= (`context`: "input" or "output").
proc_ds_arg <- function(x, context, arg) {
  if (is.null(x)) return(NULL)
  if (!rlang::is_string(x) || !nzchar(trimws(x))) {
    rlang::abort(paste0("`", arg, "` must be a data set name (a string)."), class = "sasonr_error")
  }
  p <- new_parser(sas_lex(x))
  ref <- parse_ds_ref(p, context)
  if (!at_eof(p)) abort_syntax(paste0("unexpected ", tok_desc(p), " after the data set name in `", arg, "`"))
  if (length(ref$options)) ref$text <- tok_render(p, 1L, p$n)
  ref
}

# The string of a data set reference (proc_ds_arg() reads it back).
proc_ds_text <- function(ref) {
  if (!is.null(ref[["text"]])) return(ref[["text"]])
  # options whose text was not kept: no string (the step keeps its SAS text)
  if (length(ref$options)) return(NULL)
  ref$name
}


# The procedures whose steps run as their R call, with the switches of the
# PROC statement (TRUE in the IR), its valued options (strings in the IR) and
# the statements the call takes as arguments (parse_proc_stmt()).  The
# parser accepts more options than these (WIDTH=, CONTENTS=, SORTSIZE=), but
# keeps nothing of them.
proc_api_spec <- list(
  sort = list(flags = c("nodupkey", "noduprecs", "equals", "force", "tagsort", "threads", "nothreads", "overwrite"),
              values = character(), stmts = c("by", "where")),
  print = list(flags = c("noobs", "uniform", "n", "label"), values = c("obs", "split"),
               stmts = c("var", "id", "by", "sum", "sumby", "where", "format"))
)
# Procedures whose PROC statement has its own option parser (spec 11 4b): the
# options of a call are written back as the option list and read by it.
# `stat`: the option words that are statistic keywords (one `stats` argument).
proc_means_api <- list(kind = "options", ds = "data", options = "parse_means_options", stmt = "parse_means_stmt",
                       stat = function(x) !is.na(means_canon_stat(x)),
                       stmts = c("var", "id", "class", "by", "where", "freq", "weight", "types", "ways", "output",
                                 "format", "label", "attrib"))
proc_api_spec$means <- proc_means_api
proc_api_spec$summary <- proc_means_api

# The SAS text of a statement argument: names (`c(a, desc(b))`, `a`), or
# strings with the statement's SAS text (`c("a", "descending b")`,
# `"x > 1"`).  NULL when the expression is neither.
proc_stmt_arg_text <- function(e) {
  one <- function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (is.call(x) && identical(x[[1]], as.name("desc")) && length(x) == 2L && is.symbol(x[[2]])) {
      return(paste("descending", as.character(x[[2]])))
    }
    if (is.character(x) && length(x)) return(paste(x, collapse = " "))
    NULL
  }
  # a statement built by a constructor (sas_out_stats()): its text
  if (is.call(e) && identical(e[[1]], as.name("sas_out_stats"))) e <- eval(e, asNamespace("sasonr"))
  if (inherits(e, "sas_stmt_text")) return(unclass(e))
  # a named vector of strings (FORMAT: `c(x = "8.2")`): `name value` pairs
  if (is.call(e) && identical(e[[1]], as.name("c")) && !is.null(names(e)) && all(nzchar(names(e)[-1]))) {
    vals <- as.list(e)[-1]
    if (!all(vapply(vals, rlang::is_string, NA))) return(NULL)
    return(paste(names(vals), unlist(vals), collapse = " "))
  }
  parts <- if (is.call(e) && identical(e[[1]], as.name("c"))) lapply(as.list(e)[-1], one) else list(one(e))
  if (!length(parts) || any(vapply(parts, is.null, NA))) return(NULL)
  paste(unlist(parts), collapse = " ")
}

# A statement node from a statement argument (`kw` its keyword), parsed by
# the statement parser of procedure `name`.
proc_stmt_of_arg <- function(name, kw, e) {
  text <- proc_stmt_arg_text(e)
  if (is.null(text) || !nzchar(trimws(text))) {
    rlang::abort(paste0("`", kw, "` takes variable names, desc(name) or the statement as text (strings), not `", rlang::expr_deparse(e), "`."),
                 class = "sasonr_error")
  }
  p <- new_parser(sas_lex(paste0(kw, " ", text, ";")))
  sp <- proc_api_spec[[name]]
  s <- if (identical(sp$kind, "options")) match.fun(sp$stmt)(p, kw, 1L) else parse_proc_stmt(p, name)
  if (!at_eof(p)) abort_syntax(paste0("unexpected ", tok_desc(p), " after the `", kw, "` statement"))
  s
}

# The argument of a statement node: names when the statement is plain
# variable names (BY with desc()), else its SAS text; NULL when the text was
# not kept (a token form tok_render() does not write).
proc_stmt_value <- function(s) {
  if (identical(s$stmt, "output")) return(proc_output_call(s) %||% s[["text"]])
  if (identical(s$stmt, "format")) return(proc_format_value(s) %||% s[["text"]])
  if (identical(s$stmt, "by")) {
    items <- Map(function(v, d) if (d) rlang::call2("desc", rlang::sym(v)) else rlang::sym(v), s$vars, s$descending)
    return(if (length(items) == 1L) items[[1]] else rlang::call2("c", !!!unname(items)))
  }
  plain <- (s$stmt %in% c("var", "id", "sum", "sumby") || (identical(s$stmt, "class") && !length(s$opts))) &&
    length(s$items) && all(vapply(s$items, function(it) identical(it$kind, "name") && is.null(it$to), NA))
  if (plain) {
    items <- lapply(s$items, function(it) rlang::sym(it$text %||% it$name))
    return(if (length(items) == 1L) items[[1]] else rlang::call2("c", !!!unname(items)))
  }
  s[["text"]]
}

# The IR of a step from the arguments of its verb: `opts` the PROC statement
# options (data, out, switches, valued options), `stmts` the statement
# arguments as expressions, in order (a statement given twice is two
# arguments).
proc_ir_of_args <- function(name, opts, stmts, line = 1L) {
  sp <- proc_api_spec[[name]]
  bad <- setdiff(names(stmts), sp$stmts)
  if (length(bad) || (length(stmts) && is.null(names(stmts)))) {
    rlang::abort(paste0("Unknown statement of `sas_proc_", name, "()`: ", paste(bad, collapse = ", "), "."), class = "sasonr_error")
  }
  nodes <- Map(function(kw, e) proc_stmt_of_arg(name, kw, e), names(stmts), stmts)
  if (identical(sp$kind, "options")) {
    text <- proc_opts_text(name, opts)
    p <- new_parser(sas_lex(paste0(text, ";")))
    o <- match.fun(sp$options)(p, 1L, name)
    if (!identical(tk_type(p), "semi") || p$pos != p$n) abort_syntax(paste0("unexpected ", tok_desc(p), " in the options of `sas_proc_", name, "()`"))
    return(list(type = "proc", name = name, line = vctrs::vec_cast(line, integer()), options = o, stmts = unname(nodes),
                titles = list(), opt_text = if (nzchar(text)) text))
  }
  o <- list()
  if (!is.null(opts$data)) o$data <- proc_ds_arg(opts$data, "input", "data")
  if (!is.null(opts$out)) o$out <- proc_ds_arg(opts$out, "output", "out")
  for (nm in setdiff(names(opts), c("data", "out"))) {
    v <- opts[[nm]]
    if (nm %in% sp$flags) {
      if (!rlang::is_bool(v)) rlang::abort(paste0("`", nm, "` must be TRUE or FALSE."), class = "sasonr_error")
      if (v) o[[nm]] <- TRUE
    } else if (nm %in% sp$values) {
      if (!rlang::is_string(v)) rlang::abort(paste0("`", nm, "` must be a string."), class = "sasonr_error")
      o[[nm]] <- v
    } else {
      rlang::abort(paste0("Unknown argument of `sas_proc_", name, "()`: ", nm, "."), class = "sasonr_error")
    }
  }
  bad <- setdiff(names(stmts), sp$stmts)
  if (length(bad) || (length(stmts) && is.null(names(stmts)))) {
    rlang::abort(paste0("Unknown statement of `sas_proc_", name, "()`: ", paste(bad, collapse = ", "), "."), class = "sasonr_error")
  }
  nodes <- Map(function(kw, e) proc_stmt_of_arg(name, kw, e), names(stmts), stmts)
  list(type = "proc", name = name, options = o, stmts = unname(nodes), titles = list(), line = vctrs::vec_cast(line, integer()))
}

# The R call of a step (IR -> call); NULL when the step has a part the call
# cannot hold (a statement whose text was not kept).  `src` a sas_src or NULL.
proc_call_of_ir <- function(step, src = NULL) {
  sp <- proc_api_spec[[step$name]]
  o <- step$options
  args <- list(quote(s))
  if (identical(sp$kind, "options")) {
    oa <- proc_opts_args(step$name, step[["opt_text"]])
    if (is.null(oa)) return(NULL)
    args <- c(args, oa)
    for (s in step$stmts) {
      v <- proc_stmt_value(s)
      if (is.null(v)) return(NULL)
      args <- c(args, stats::setNames(list(v), s$stmt))
    }
    if (!is.null(src)) args$.sas_src <- rlang::call2("sas_src", src$lines, src$text)
    return(rlang::call2(paste0("sas_proc_", step$name), !!!args))
  }
  for (nm in c("data", "out")) {
    if (is.null(o[[nm]])) next
    args[[nm]] <- proc_ds_text(o[[nm]])
    if (is.null(args[[nm]])) return(NULL)
  }
  for (nm in sp$flags) if (isTRUE(o[[nm]])) args[[nm]] <- TRUE
  for (nm in sp$values) if (!is.null(o[[nm]])) args[[nm]] <- o[[nm]]
  for (s in step$stmts) {
    v <- proc_stmt_value(s)
    if (is.null(v)) return(NULL)
    args <- c(args, stats::setNames(list(v), s$stmt))
  }
  if (!is.null(src)) args$.sas_src <- rlang::call2("sas_src", src$lines, src$text)
  rlang::call2(paste0("sas_proc_", step$name), !!!args)
}

# The IR of a call of a PROC verb, as the verb builds it.
proc_ir_of_call <- function(call, line) {
  name <- sub("^sas_proc_", "", as.character(call[[1]]))
  args <- rlang::call_args(call)[-1]
  args$.sas_src <- NULL
  sp <- proc_api_spec[[name]]
  is_stmt <- names(args) %in% sp$stmts
  proc_ir_of_args(name, args[!is_stmt], args[is_stmt], line)
}
# An IR without what a round trip does not keep (source positions, spans, the
# source text of a data set reference).
proc_ir_core <- function(x) {
  if (!is.list(x)) return(x)
  x$line <- NULL
  x$col <- NULL
  x$span <- NULL
  x$text <- NULL
  x$opt_text <- NULL
  x[] <- lapply(x, proc_ir_core)
  # the order of named elements (options) carries no meaning
  if (!is.null(names(x)) && all(nzchar(names(x)))) x <- x[order(names(x))]
  x
}

# A parsed step as it runs (spec 11): the IR of its R call (the call
# sas_script() prints) must be the step; a call whose IR differs from it
# fails loud.  The step then runs with the line numbers of the program (the
# call has the line of the PROC statement only).  TITLE statements inside the
# step are not part of the call; a step with one, or with a statement whose
# text was not kept, runs as parsed (and sas_script() keeps its SAS text).
proc_ir_via_call <- function(step) {
  if (length(step$titles)) return(step)
  call <- proc_call_of_ir(step)
  if (is.null(call)) return(step)
  ir <- proc_ir_of_call(call, step$line)
  if (!identical(proc_ir_core(ir), proc_ir_core(step))) {
    abort_unsupported(paste0("a PROC ", toupper(step$name), " step whose R call gives a different step"), step$line)
  }
  step
}

# The SAS text of a step IR (the source echo of a verb called without
# `.sas_src`).
proc_step_text <- function(step) {
  sp <- proc_api_spec[[step$name]]
  o <- step$options
  head <- if (identical(sp$kind, "options")) paste(c(paste("proc", step$name), step[["opt_text"]]), collapse = " ") else
    paste(c(paste("proc", step$name), if (!is.null(o$data)) paste0("data=", proc_ds_text(o$data)),
                  if (!is.null(o$out)) paste0("out=", proc_ds_text(o$out)), intersect(sp$flags, names(o)),
                  vapply(intersect(sp$values, names(o)), function(nm) paste0(nm, "='", gsub("'", "''", o[[nm]]), "'"), "")),
                collapse = " ")
  body <- vapply(step$stmts, function(s) {
    v <- proc_stmt_value(s)
    paste0("  ", s$stmt, " ", if (is.character(v)) v else proc_stmt_arg_text(v), ";")
  }, "")
  paste(c(paste0(head, ";"), body, "run;"), collapse = "\n")
}


# The span of the only step of a text from program line `line` (step_span():
# the step runs to the end of the text).
proc_text_span <- function(line) {
  c(line = as.integer(line), col = 1L, end_line = NA_integer_, end_col = NA_integer_)
}

# The arguments of an option list (`opt_text`, the parser's text of the
# options of the PROC statement; spec 11 4b): `word` -> `word = TRUE`,
# `word=value` -> `word = value` (a number, or a string), a data set option ->
# its reference as a string, statistic keywords -> one `stats` argument of
# names in their order.  NULL when an item has another form.
proc_opts_args <- function(name, opt_text) {
  if (is.null(opt_text)) return(list())
  sp <- proc_api_spec[[name]]
  p <- new_parser(sas_lex(opt_text))
  args <- list()
  stats <- character()
  while (!at_eof(p)) {
    if (tk_type(p) != "name") return(NULL)
    w <- tk_low(p)
    if (!is_op(p, "=", 1L)) {
      if (!is.null(sp$stat) && sp$stat(w)) stats <- c(stats, w) else if (!is.null(args[[w]])) return(NULL) else args[[w]] <- TRUE
      advance(p)
      next
    }
    if (!is.null(args[[w]])) return(NULL)
    advance(p, 2L)
    if (w %in% sp$ds) {
      start <- p$pos
      parse_ds_ref(p, "input")
      args[[w]] <- tok_render(p, start, p$pos - 1L)
      if (is.null(args[[w]])) return(NULL)
      next
    }
    t <- tk_type(p)
    if (t == "num") {
      args[[w]] <- as.numeric(tk_val(p))
      if (is.na(args[[w]]) || !identical(proc_num_text(args[[w]]), tk_val(p))) return(NULL)
    } else if (t == "name") {
      args[[w]] <- tk_val(p)
    } else {
      return(NULL)
    }
    advance(p)
  }
  if (length(stats)) {
    syms <- lapply(stats, rlang::sym)
    args <- c(args[intersect(names(args), sp$ds)], list(stats = if (length(syms) == 1L) syms[[1]] else rlang::call2("c", !!!syms)),
              args[setdiff(names(args), sp$ds)])
  }
  args
}

# A number as the option list writes it back (the text it was read from).
proc_num_text <- function(x) format(x, digits = 15L, scientific = FALSE, trim = TRUE)

# The option list of a call (`opts`: evaluated arguments; `stats` an
# expression of names or strings).
proc_opts_text <- function(name, opts) {
  sp <- proc_api_spec[[name]]
  out <- character()
  for (nm in names(opts)) {
    v <- opts[[nm]]
    if (identical(nm, "stats")) {
      t <- proc_stmt_arg_text(v)
      if (is.null(t)) rlang::abort("`stats` takes statistic names (`c(n, mean)`) or strings.", class = "sasonr_error")
      out <- c(out, t)
    } else if (isTRUE(v)) {
      out <- c(out, nm)
    } else if (isFALSE(v) || is.null(v)) {
      next
    } else if (is.numeric(v) && length(v) == 1L && !is.na(v)) {
      out <- c(out, paste0(nm, "=", proc_num_text(v)))
    } else if (rlang::is_string(v)) {
      out <- c(out, paste0(nm, "=", v))
    } else {
      rlang::abort(paste0("`", nm, "` must be TRUE, a number or a string."), class = "sasonr_error")
    }
  }
  paste(out, collapse = " ")
}

#' The OUTPUT statement of PROC MEANS and SUMMARY as an R argument
#'
#' Builds the `output` argument of [sas_proc_means()] and
#' [sas_proc_summary()]: the output data set and, for each statistic, the
#' names of its variables.
#' @param out The output data set (a name, with data set options in SAS form
#'   if any, as `data` of the verbs).
#' @param ... One argument per statistic, named by its keyword: names for
#'   the analysis variables in the order of VAR (`mean = c(m1, m2)`), names
#'   given to chosen variables (`std = c(sd_x = x)`, new name = variable), or
#'   `TRUE` for the default names (`n = TRUE`).
#' @param autoname,autolabel,noinherit,inherit,keeplen,levels,ways Options
#'   of the OUTPUT statement.
#' @return The OUTPUT statement as text (class `sas_stmt_text`), which the
#'   verbs read.
#' @examples
#' sas_out_stats(out = "stats", mean = c(m_x, m_y), std = c(sd_x = x), n = TRUE, autoname = TRUE)
#' @export
sas_out_stats <- function(out = NULL, ..., autoname = FALSE, autolabel = FALSE, noinherit = FALSE, inherit = FALSE,
                       keeplen = FALSE, levels = FALSE, ways = FALSE) {
  specs <- rlang::enexprs(...)
  if (length(specs) && (is.null(names(specs)) || any(!nzchar(names(specs))))) {
    rlang::abort("Every statistic of `sas_out_stats()` must be named by its keyword (`mean = c(m1, m2)`).", class = "sasonr_error")
  }
  parts <- if (!is.null(out)) paste0("out=", out) else character()
  for (k in names(specs)) parts <- c(parts, proc_output_spec_text(k, specs[[k]]))
  flags <- c(autoname = autoname, autolabel = autolabel, noinherit = noinherit, inherit = inherit, keeplen = keeplen,
             levels = levels, ways = ways)
  if (any(flags)) parts <- c(parts, "/", names(flags)[flags])
  structure(paste(parts, collapse = " "), class = "sas_stmt_text")
}

#' @export
print.sas_stmt_text <- function(x, ...) {
  cat("<sas_stmt_text> ", unclass(x), "\n", sep = "")
  invisible(x)
}

# `stat=names`, `stat(vars)=names` or `stat=` of one sas_out_stats() argument.
proc_output_spec_text <- function(k, e) {
  if (isTRUE(e)) return(paste0(k, "="))
  if (is.character(e)) return(paste0(k, "=", paste(e, collapse = " ")))
  items <- if (is.call(e) && identical(e[[1]], as.name("c"))) as.list(e)[-1] else list(e)
  nm <- names(items) %||% rep("", length(items))
  ok <- vapply(items, is.symbol, NA)
  if (!all(ok)) rlang::abort(paste0("`", k, "` of `sas_out_stats()` takes names (`c(m1, m2)`, `c(new = var)`) or TRUE."), class = "sasonr_error")
  new <- vapply(seq_along(items), function(i) if (nzchar(nm[i])) nm[i] else as.character(items[[i]]), "")
  if (all(nzchar(nm))) return(paste0(k, "(", paste(vapply(items, as.character, ""), collapse = " "), ")=", paste(new, collapse = " ")))
  if (any(nzchar(nm))) rlang::abort(paste0("`", k, "` of `sas_out_stats()`: all names given to variables (`c(new = var)`) or none."), class = "sasonr_error")
  paste0(k, "=", paste(new, collapse = " "))
}

# The sas_out_stats() call of an OUTPUT statement node, or NULL (a form it does
# not write: variable ranges, a reference whose text was not kept).
proc_output_call <- function(s) {
  args <- list()
  if (!is.null(s$out)) {
    t <- proc_ds_text(s$out)
    if (is.null(t)) return(NULL)
    args$out <- t
  }
  for (sp in s$specs) {
    disp <- sp$disp
    if (is.null(sp$vars)) {
      v <- if (!length(disp)) TRUE else if (length(disp) == 1L) rlang::sym(disp) else rlang::call2("c", !!!lapply(disp, rlang::sym))
    } else {
      plain <- all(vapply(sp$vars, function(it) identical(it$kind, "name"), NA)) && length(sp$vars) == length(disp)
      if (!plain) return(NULL)
      items <- stats::setNames(lapply(sp$vars, function(it) rlang::sym(it$text %||% it$name)), disp)
      v <- rlang::call2("c", !!!items)
    }
    args <- c(args, stats::setNames(list(v), sp$stat))
  }
  for (k in names(s$opts)) if (isTRUE(s$opts[[k]])) args[[k]] <- TRUE
  rlang::call2("sas_out_stats", !!!args)
}

# The named vector of a FORMAT statement node (`c(x = "8.2")`), or NULL when a
# format names several variables or a variable list.
proc_format_value <- function(s) {
  ok <- all(vapply(s$specs, function(sp) length(sp$vars) == 1L && identical(sp$vars[[1]]$kind, "name") && !is.null(sp$fmt$text), NA))
  if (!ok || !length(s$specs)) return(NULL)
  vals <- vapply(s$specs, function(sp) sp$fmt$text, "")
  names(vals) <- vapply(s$specs, function(sp) sp$vars[[1]]$text %||% sp$vars[[1]]$name, "")
  rlang::call2("c", !!!as.list(vals))
}
