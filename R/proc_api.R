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
  if (!is.null(ref$text)) return(ref$text)
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
  s <- parse_proc_stmt(p, name)
  if (!at_eof(p)) abort_syntax(paste0("unexpected ", tok_desc(p), " after the `", kw, "` statement"))
  s
}

# The argument of a statement node: names when the statement is plain
# variable names (BY with desc()), else its SAS text; NULL when the text was
# not kept (a token form tok_render() does not write).
proc_stmt_value <- function(s) {
  if (identical(s$stmt, "by")) {
    items <- Map(function(v, d) if (d) rlang::call2("desc", rlang::sym(v)) else rlang::sym(v), s$vars, s$descending)
    return(if (length(items) == 1L) items[[1]] else rlang::call2("c", !!!unname(items)))
  }
  plain <- s$stmt %in% c("var", "id", "sum", "sumby") &&
    all(vapply(s$items, function(it) identical(it$kind, "name") && is.null(it$to), NA))
  if (plain) {
    items <- lapply(s$items, function(it) rlang::sym(it$text %||% it$name))
    return(if (length(items) == 1L) items[[1]] else rlang::call2("c", !!!unname(items)))
  }
  s$text
}

# The IR of a step from the arguments of its verb: `opts` the PROC statement
# options (data, out, switches, valued options), `stmts` the statement
# arguments as expressions, in order (a statement given twice is two
# arguments).
proc_ir_of_args <- function(name, opts, stmts, line = 1L) {
  sp <- proc_api_spec[[name]]
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
  head <- paste(c(paste("proc", step$name), if (!is.null(o$data)) paste0("data=", proc_ds_text(o$data)),
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
