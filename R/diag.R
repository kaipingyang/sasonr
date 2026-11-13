# Run-time diagnostics: _ERROR_, data-error NOTEs and the PDV dump.
#
# Run-time helpers flag data errors through the run-time diagnostic state of
# the active session (`rt_diag()`, session.R): invalid function arguments,
# division by zero, overflow, invalid data.
# L3 code copies the flag into `_ERROR_` after each statement that can raise
# one (sas_err_sync) and writes the SAS-style PDV dump at the end of the
# iteration (sas_iter_dump). Vectorised (L1/L2) code only needs to know that
# an error happened at all: the engine then re-runs the step as L3.
#
# Fields of the run-time state (`SasSession$rt`): per submission prx,
# prx_cache (PRXPARSE ids) and on_warn, on_error, on_note (log callbacks of
# the engine); per step evaluation (rt_reset()): err, stop_step, notes, mv,
# math, mv_rerun, reads, proc_notes, flow_note.  The format catalog, filerefs
# and the LINESIZE/NOTES options are separate session components.

rt_reset <- function(rt = rt_diag()) {
  rt$err <- FALSE
  rt$stop_step <- FALSE
  rt$notes <- character()
  rt$mv <- integer()
  rt$mv_rerun <- FALSE
  rt$math <- integer()
  rt$reads <- list()
  rt$proc_notes <- character()
  rt$flow_note <- FALSE
  invisible()
}

# Flag a data error; `note` (if any) is queued for the log.
rt_error <- function(note = NULL) {
  rt <- rt_diag()
  rt$err <- TRUE
  if (!is.null(note) && length(rt$notes) < 1000L) rt$notes <- c(rt$notes, note)
  invisible()
}

at_text <- function(.at) {
  if (is.null(.at) || length(.at) < 2 || anyNA(.at)) return("")
  sprintf(" at line %d column %d", as.integer(.at[1]), as.integer(.at[2]))
}

arg_ord <- c("first", "second", "third", "fourth", "fifth")

# A character argument in an invalid-argument NOTE: SAS shows at most 12
# characters ("'abababababab'[12 of 200 characters shown]").
note_chr <- function(v, trim = TRUE) {
  t <- if (trim) sas_rtrim(v) else v
  if (nchar(t) > 12) return(sprintf("'%s'[12 of %d characters shown]", substr(t, 1L, 12L), nchar(v)))
  paste0("'", if (nzchar(t)) gsub("'", "''", t, fixed = TRUE) else " ", "'")
}

# `args`/`bad`: the argument values and the failing elements; SAS shows the
# arguments of the failing call, e.g. "Invalid argument to function LOG(-1)".
note_invalid_arg <- function(fname, argn = NULL, .at = NULL, args = NULL, bad = NULL, numbered = FALSE, math = TRUE,
                             trim = TRUE) {
  if (numbered) {
    # "NOTE: Argument 2 to function SCAN('a b c',0) at line 3 column 7 is invalid."
    i <- if (is.null(bad)) 1L else which(bad)[1]
    vals <- vapply(args, function(a) {
      v <- a[min(i, length(a))]
      if (is.character(v)) note_chr(v) else trimws(sas_fmt_best(v, 12L))
    }, "")
    if (math) rt_count("math", .at, sum(bad %||% TRUE))
    return(rt_error(sprintf("NOTE: Argument %d to function %s(%s)%s is invalid.", argn, toupper(fname),
                            paste(vals, collapse = ","), at_text(.at))))
  }
  what <- if (is.null(argn)) "argument" else paste(arg_ord[argn], "argument")
  call <- toupper(fname)
  if (!is.null(args)) {
    i <- if (is.null(bad)) 1L else which(bad)[1]
    vals <- vapply(args, function(a) {
      v <- a[min(i, length(a))]
      if (is.null(v) || length(v) == 0) "" else if (is.character(v)) note_chr(v, trim) else trimws(sas_fmt_best(v, 12L))
    }, "")
    call <- paste0(call, "(", paste(vals, collapse = ","), ")")
    # the failed call also counts as a place where a mathematical operation
    # could not be performed (summary NOTE at the end of the step)
    if (math) rt_count("math", .at, sum(bad %||% TRUE))
  }
  rt_error(sprintf("NOTE: Invalid %s to function %s%s.", what, call, at_text(.at)))
}

# Overflow and similar: counted per place, reported at the end of the step.
note_math <- function(.at = NULL, n = 1L) {
  rt <- rt_diag()
  rt$err <- TRUE
  rt_count("math", .at, n)
}

#' Copy a pending data error into `_ERROR_` (and its NOTEs into the log)
#'
#' Called from generated L3 code after statements that can raise data errors.
#' @export
#' @keywords internal
sas_err_sync <- function(pdv) {
  rt <- rt_diag()
  if (!rt$err && length(rt$notes) == 0) return(pdv)
  env <- parent.frame()
  if (length(rt$notes) > 0) {
    if (rt_notes_on()) env$.log <- c(env$.log, rt$notes)
    rt$notes <- character()
  }
  if (rt$err) pdv[["_error_"]] <- 1
  rt$err <- FALSE
  pdv
}

#' Prepare the PDV dump of a step
#' @param vars PDV variable names in order; `names` their display names;
#'   `fmts` named list of formats.
#' @export
#' @keywords internal
sas_dump_spec <- function(vars, names, fmts = list()) {
  d <- new.env(parent = emptyenv())
  d$vars <- vars
  d$names <- names
  d$fmts <- fmts
  d$count <- 0L
  d
}

dump_value <- function(v, fmt) {
  if (is.character(v)) {
    out <- strip_both(if (is.null(fmt)) v else sas_format(v, fmt))
    return(ifelse(nzchar(out), out, " "))
  }
  trimws(sas_format(v, fmt))
}

#' End-of-iteration PDV dump when `_ERROR_` is set
#'
#' Returns the log lines: the input buffer (with the RULE line the first time
#' in a step) when an INPUT record is current, then
#' `var=value ... _ERROR_=1 _N_=n`. At most 20 dumps per step (ERRORS=20).
#' @export
#' @keywords internal
sas_iter_dump <- function(pdv, spec, inf = NULL) {
  err <- pdv[["_error_"]]
  if (is.null(err) || !sas_true(err)) return(character())
  spec$count <- spec$count + 1L
  if (spec$count > 20L) return(character())
  out <- character()
  if (!is.null(inf)) out <- input_buffer_lines(inf)
  vals <- vapply(seq_along(spec$vars), function(k) {
    v <- spec$vars[k]
    paste0(spec$names[k], "=", dump_value(pdv[[v]], spec$fmts[[v]]))
  }, "")
  items <- c(vals, paste0("_ERROR_=", dump_value(err, NULL)), paste0("_N_=", dump_value(pdv[["_n_"]], NULL)))
  c(out, dump_wrap(items, rt_linesize()))
}

# Pack `var=value` items into log lines of at most `ls` columns.  An item that
# is itself wider than the line is cut at the line size and ends the dump (SAS).
dump_wrap <- function(items, ls) {
  lines <- character()
  cur <- ""
  for (it in items) {
    if (nzchar(cur) && nchar(cur) + 1L + nchar(it) > ls) {
      lines <- c(lines, cur)
      cur <- ""
    }
    if (nchar(it) > ls) {
      # an item wider than the line continues on the next lines, cut every `ls` columns (SAS 9.4)
      if (nzchar(cur)) lines <- c(lines, cur)
      starts <- seq(1L, nchar(it), by = ls)
      chunks <- substring(it, starts, pmin(starts + ls - 1L, nchar(it)))
      lines <- c(lines, chunks[-length(chunks)])
      cur <- chunks[length(chunks)]
      next
    }
    cur <- if (nzchar(cur)) paste(cur, it) else it
  }
  c(lines, cur)
}
