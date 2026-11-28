# The R interface of PROC steps (spec 11: one path).  A PROC step is an IR
# (the list the parser produces).  The verb converts its R arguments into that
# IR and the engine runs it.  `sas_run()` builds the call of a parsed step from
# its IR and converts the call's arguments back (proc_ir_via_call()), so the
# printed call is what runs; it fails loud when the round trip does not give
# the same IR.  PROC SORT so far.

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
  ref$name
}

# The BY variables of an argument: an expression `c(a, desc(b))` or a single
# name, or a character vector of SAS BY items (`c("a", "descending b")`).
proc_by_arg <- function(e, arg = "by") {
  vars <- character()
  desc <- logical()
  one <- function(x) {
    if (is.symbol(x)) {
      vars <<- c(vars, tolower(as.character(x)))
      desc <<- c(desc, FALSE)
    } else if (is.call(x) && identical(x[[1]], as.name("desc")) && length(x) == 2L && is.symbol(x[[2]])) {
      vars <<- c(vars, tolower(as.character(x[[2]])))
      desc <<- c(desc, TRUE)
    } else if (is.character(x) && length(x)) {
      b <- parse_by(new_parser(sas_lex(paste0("by ", paste(x, collapse = " "), ";"))))
      vars <<- c(vars, b$vars)
      desc <<- c(desc, b$descending)
    } else {
      rlang::abort(paste0("`", arg, "` takes variable names, desc(name) or strings, not `", rlang::expr_deparse(x), "`."),
                   class = "sasonr_error")
    }
  }
  if (is.call(e) && identical(e[[1]], as.name("c"))) for (x in as.list(e)[-1]) one(x) else one(e)
  if (!length(vars)) rlang::abort(paste0("`", arg, "` names no variable."), class = "sasonr_error")
  list(stmt = "by", vars = vars, descending = desc)
}

# The PROC SORT options that are switches (the IR keeps each as TRUE).
proc_sort_flags <- c("nodupkey", "noduprecs", "equals", "noequals", "force", "tagsort", "threads", "nothreads", "overwrite")

# The IR of a PROC SORT step from R arguments.  `by` is an expression.
proc_ir_sort <- function(data = NULL, out = NULL, by, flags = list(), line = 1L) {
  opts <- list()
  if (!is.null(data)) opts$data <- proc_ds_arg(data, "input", "data")
  if (!is.null(out)) opts$out <- proc_ds_arg(out, "output", "out")
  for (nm in names(flags)) {
    v <- flags[[nm]]
    if (!rlang::is_bool(v)) rlang::abort(paste0("`", nm, "` must be TRUE or FALSE."), class = "sasonr_error")
    if (v) opts[[nm]] <- TRUE
  }
  list(type = "proc", name = "sort", options = opts, stmts = list(proc_by_arg(by)), titles = list(),
       line = vctrs::vec_cast(line, integer()))
}

# The R call of a PROC SORT step (IR -> call); `src` a sas_src or NULL.
proc_call_sort <- function(step, src = NULL) {
  o <- step$options
  by <- Filter(function(s) identical(s$stmt, "by"), step$stmts)[[1]]
  items <- Map(function(v, d) if (d) rlang::call2("desc", rlang::sym(v)) else rlang::sym(v), by$vars, by$descending)
  args <- list(quote(s))
  if (!is.null(o$data)) args$data <- proc_ds_text(o$data)
  if (!is.null(o$out)) args$out <- proc_ds_text(o$out)
  args$by <- if (length(items) == 1L) items[[1]] else rlang::call2("c", !!!unname(items))
  for (nm in proc_sort_flags) if (isTRUE(o[[nm]])) args[[nm]] <- TRUE
  if (!is.null(src)) args$.sas_src <- rlang::call2("sas_src", src$lines, src$text)
  rlang::call2("sas_proc_sort", !!!args)
}

# The IR of a call of a PROC verb, as the verb builds it.
proc_ir_of_call <- function(call, line) {
  args <- rlang::call_args(call)
  args[[1]] <- NULL
  args$.sas_src <- NULL
  flags <- args[intersect(names(args), proc_sort_flags)]
  proc_ir_sort(args$data, args$out, args$by, flags, line)
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

# A parsed PROC SORT step as it runs (spec 11): the IR of its R call (the call
# sas_script() prints) must be the step; a call whose IR differs from it
# fails loud.  The step then runs with the line numbers of the program (the
# call has the line of the PROC statement only); TITLE statements inside the
# step are not part of the call (sas_script() writes them as sas_title()).
proc_ir_via_call <- function(step) {
  # a step without exactly one BY statement stops in translate_proc() (its ERROR)
  if (sum(vapply(step$stmts, function(s) identical(s$stmt, "by"), NA)) != 1L || length(step$stmts) != 1L) return(step)
  ir <- proc_ir_of_call(proc_call_sort(step), step$line)
  want <- step
  want$titles <- list()
  if (!identical(proc_ir_core(ir), proc_ir_core(want))) {
    abort_unsupported("a PROC SORT step whose R call gives a different step", step$line)
  }
  step
}

# The SAS text of a PROC SORT IR (the source echo of a verb called without
# `.sas_src`).
proc_sort_text <- function(step) {
  o <- step$options
  by <- step$stmts[[1]]
  head <- paste(c("proc sort", if (!is.null(o$data)) paste0("data=", proc_ds_text(o$data)),
                  if (!is.null(o$out)) paste0("out=", proc_ds_text(o$out)), intersect(proc_sort_flags, names(o))), collapse = " ")
  items <- ifelse(by$descending, paste("descending", by$vars), by$vars)
  paste0(head, ";\n  by ", paste(items, collapse = " "), ";\nrun;")
}

# The span of the only step of a text from program line `line` (step_span():
# the step runs to the end of the text).
proc_text_span <- function(line) {
  c(line = as.integer(line), col = 1L, end_line = NA_integer_, end_col = NA_integer_)
}
