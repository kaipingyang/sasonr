# Run-time diagnostics: _ERROR_, data-error NOTEs and the PDV dump.
#
# Run-time helpers flag data errors through the run-time diagnostic state of
# the active session (`rt_diag()`, session.R): invalid function arguments,
# division by zero, overflow, invalid data.
# L3 code copies the flag into `_ERROR_` after each statement that can raise
# one (sas_rt_err_sync) and writes the SAS-style PDV dump at the end of the
# iteration (sas_rt_iter_dump). Vectorised (L1/L2) code only needs to know that
# an error happened at all: the engine then re-runs the step as L3.
#
# Fields of the run-time state (`SasSession$rt`): per submission prx,
# prx_cache (PRXPARSE ids) and on_warn, on_error, on_note (log callbacks of
# the engine); per step evaluation (rt_reset()): err, stop_step, notes,
# places (mv.generated, math.failed: log_place()), mv_rerun, reads, proc_notes, flow_note.  The format catalog, filerefs
# and the LINESIZE/NOTES options are separate session components.

rt_reset <- function(rt = rt_diag()) {
  rt$err <- FALSE
  rt$stop_step <- FALSE
  rt$notes <- character()
  log_places_reset(rt)
  rt$mv_rerun <- FALSE
  rt$reads <- list()
  # a MERGE with repeats of BY values in more than one data set (log NOTE)
  rt$merge_repeats <- FALSE
  # a STOP statement ran in the step (the MERGE NOTE above is then not written)
  rt$stop_stmt <- FALSE
  # an iteration read nothing and the step stopped (looping NOTE)
  rt$looping <- FALSE
  rt$proc_notes <- character()
  rt$flow_note <- FALSE
  # ODS OUTPUT requests of the step and the NOTEs of the data sets written (ods_output.R)
  rt$ods_output <- list()
  rt$ods_notes <- list()
  rt$trace_lines <- NULL
  rt$ods_missing <- character()
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
  log_place_check(.at)
  sprintf(" at line %d column %d", as.integer(.at[1]), as.integer(.at[2]))
}

arg_ord <- c("first", "second", "third", "fourth", "fifth")

# A character argument in an invalid-argument NOTE: at most 12 characters are
# shown ("'abababababab'[12 of 200 characters shown]").
note_chr <- function(v, trim = TRUE) {
  t <- if (trim) sas_rt_rtrim(v) else v
  if (nchar(t) > 12) return(sprintf("'%s'[12 of %d characters shown]", substr(t, 1L, 12L), nchar(v)))
  paste0("'", if (nzchar(t)) gsub("'", "''", t, fixed = TRUE) else " ", "'")
}

# `args`/`bad`: the argument values and the failing elements; the NOTE shows
# the arguments of the failing call, e.g. "Invalid argument to function LOG(-1)".
note_invalid_arg <- function(fname, argn = NULL, .at = NULL, args = NULL, bad = NULL, numbered = FALSE, math = TRUE,
                             trim = TRUE) {
  if (numbered) {
    # "NOTE: Argument 2 to function SCAN('a b c',0) at line 3 column 7 is invalid."
    i <- if (is.null(bad)) 1L else which(bad)[1]
    vals <- vapply(args, function(a) {
      v <- a[min(i, length(a))]
      if (is.character(v)) note_chr(v) else trimws(sas_fmt_best(v, 12L))
    }, "")
    if (math) log_place("math.failed", at = .at, n = sum(bad %||% TRUE))
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
    if (math) log_place("math.failed", at = .at, n = sum(bad %||% TRUE))
  }
  rt_error(sprintf("NOTE: Invalid %s to function %s%s.", what, call, at_text(.at)))
}

#' Value of a WHERE condition
#'
#' sasonr evaluates a WHERE condition without data-error NOTEs (missing values
#' generated, invalid arguments of functions and of `**`, invalid numeric
#' data, mathematical operations that could not be performed) and without
#' setting `_ERROR_`: those records of the evaluation are dropped.  An ERROR
#' message of a function and a division by zero in the condition are not
#' supported.
#' @export
#' @keywords internal
sas_rt_where_cond <- function(x) {
  rt <- rt_diag()
  saved <- list(err = rt$err, notes = rt$notes, places = log_places_save(rt), mv_rerun = rt$mv_rerun)
  force(x)
  n0 <- length(saved$notes)
  added <- if (length(rt$notes) > n0) rt$notes[(n0 + 1L):length(rt$notes)] else character()
  if (any(startsWith(added, "ERROR"))) {
    abort_unsupported(paste0("an ERROR of a function in a WHERE condition (", sub("^ERROR: ", "", added[startsWith(added, "ERROR")][1]), ")"))
  }
  if (any(startsWith(added, "NOTE: Division by zero detected"))) {
    abort_unsupported("a division by zero in a WHERE condition")
  }
  rt$notes <- saved$notes
  log_places_restore(rt, saved$places)
  rt$mv_rerun <- saved$mv_rerun
  rt$err <- saved$err
  x
}

# Overflow and similar: counted per place, reported at the end of the step.
note_math <- function(.at = NULL, n = 1L) {
  rt <- rt_diag()
  rt$err <- TRUE
  log_place("math.failed", at = .at, n = n)
}

#' Copy a pending data error into `_ERROR_` (and its NOTEs into the log)
#'
#' Called from generated L3 code after statements that can raise data errors.
#' @export
#' @keywords internal
sas_rt_err_sync <- function(pdv) {
  rt <- rt_diag()
  if (!rt$err && length(rt$notes) == 0) return(pdv)
  env <- parent.frame()
  if (length(rt$notes) > 0) {
    if (rt_notes_on()) log_buffer_add(env, rt$notes)
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
sas_rt_dump_spec <- function(vars, names, fmts = list()) {
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

#' The lines of PUT _ALL_: every variable of the PDV as `name=value`, then
#' `_ERROR_` and `_N_`, as in the error dump (measured)
#' @export
#' @keywords internal
sas_rt_put_all <- function(pdv, st) {
  spec <- st$dump
  vals <- vapply(seq_along(spec$vars), function(k) {
    v <- spec$vars[k]
    paste0(spec$names[k], "=", dump_value(pdv[[v]], spec$fmts[[v]]))
  }, "")
  items <- c(vals, paste0("_ERROR_=", dump_value(pdv[["_error_"]], NULL)), paste0("_N_=", dump_value(pdv[["_n_"]], NULL)))
  dump_wrap(items, rt_linesize())
}

#' End-of-iteration PDV dump when `_ERROR_` is set
#'
#' Returns the log lines: the input buffer (with the RULE line the first time
#' in a step) when an INPUT record is current, then
#' `var=value ... _ERROR_=1 _N_=n`. At most 20 dumps per step (ERRORS=20).
#' @export
#' @keywords internal
sas_rt_iter_dump <- function(pdv, spec, inf = NULL) {
  # the state of a row-by-row step holds its dump spec
  if (is.environment(spec) && !is.null(spec$dump)) spec <- spec$dump
  err <- pdv[["_error_"]]
  if (is.null(err) || !sas_rt_true(err)) return(character())
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
# is itself wider than the line is cut at the line size and ends the dump.
dump_wrap <- function(items, ls) {
  lines <- character()
  cur <- ""
  for (it in items) {
    if (nzchar(cur) && nchar(cur) + 1L + nchar(it) > ls) {
      lines <- c(lines, cur)
      cur <- ""
    }
    if (nchar(it) > ls) {
      # an item wider than the line continues on the next lines, cut every `ls` columns
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
