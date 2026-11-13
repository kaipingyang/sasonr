# Run-time diagnostics: _ERROR_, data-error NOTEs and the PDV dump.
#
# Run-time helpers flag data errors through the package-level state `rt`
# (invalid function arguments, division by zero, overflow, invalid data).
# L3 code copies the flag into `_ERROR_` after each statement that can raise
# one (sas_err_sync) and writes the SAS-style PDV dump at the end of the
# iteration (sas_iter_dump). Vectorised (L1/L2) code only needs to know that
# an error happened at all: the engine then re-runs the step as L3.
#
# Package-global mutable state (inventory for the SasSession R6 refactor,
# CLAUDE.md §3; no new fields are added until then -- new per-run state goes
# into the engine `eng`):
#   rt (this file)  per run: catalog (PROC FORMAT), filerefs, prx, prx_cache,
#                   ls (LINESIZE), notes_on (NOTES/NONOTES), on_warn, on_error,
#                   on_note (log callbacks of the engine); per step evaluation
#                   (rt_reset()): err, stop_step, notes, mv, math, mv_rerun,
#                   reads, proc_notes, flow_note
#   ct (notes.R)    per step translation (ct_reset()): notes, mv, conv
#   registry_cache (registry.R)  read-once cache of inst/registry/*.csv and
#                   the SAS function inventory (not run state)
#   options()       sasonr.strict, sasonr.on_error, sasonr.step_notes, sasonr.sas_version

rt <- new.env(parent = emptyenv())
rt$err <- FALSE
rt$notes <- character()
# PROC SQL reports an invalid function argument with its own NOTE: no line /
# column, no argument list and no mathematical-operations summary (SAS 9.4).
rt$sql <- FALSE
rt$sql_inv <- character()
rt$catalog <- new.env(parent = emptyenv())

rt$notes_on <- TRUE
rt$ls <- 132L # log line size (OPTIONS LINESIZE=)

rt_reset <- function() {
  rt$err <- FALSE
  rt$stop_step <- FALSE
  rt$notes <- character()
  rt$mv <- integer()
  rt$mv_rerun <- FALSE
  rt$math <- integer()
  rt$reads <- list()
  rt$proc_notes <- character()
  rt$flow_note <- FALSE
  rt$sql <- FALSE
  rt$sql_inv <- character()
  invisible()
}

# Flag a data error; `note` (if any) is queued for the log.
rt_error <- function(note = NULL) {
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
  if (isTRUE(rt$sql)) {
    # PROC SQL (SAS 9.4): one NOTE per function and statement, without the
    # argument list, the line/column and the mathematical-operations summary;
    # measured with mdy(2, 30, 2024) in a CREATE TABLE ... SELECT.
    nm <- toupper(fname)
    if (!nm %in% rt$sql_inv) {
      rt$sql_inv <- c(rt$sql_inv, nm)
      msg <- sprintf("NOTE: Invalid argument to function %s. Missing values may be generated.", nm)
      rt$err <- TRUE
      # written to the log at once: PROC SQL's own NOTEs (table created, rows
      # selected) follow it in the SAS log
      if (is.function(rt$on_note)) rt$on_note(msg) else rt$notes <- c(rt$notes, msg)
    }
    return(invisible())
  }
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
  rt$err <- TRUE
  rt_count("math", .at, n)
}

#' Copy a pending data error into `_ERROR_` (and its NOTEs into the log)
#'
#' Called from generated L3 code after statements that can raise data errors.
#' @export
#' @keywords internal
sas_err_sync <- function(pdv) {
  if (!rt$err && length(rt$notes) == 0) return(pdv)
  env <- parent.frame()
  if (length(rt$notes) > 0) {
    if (isTRUE(rt$notes_on)) env$.log <- c(env$.log, rt$notes)
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
  c(out, dump_wrap(items, rt$ls %||% 132L))
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
