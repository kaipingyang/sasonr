# A step exception (an internal failure, e.g. CONSTANT('EXACTINT', 1E300)):
# sasonr writes the exception block twice, stops the DATA step and keeps the
# observations its output data sets hold so far.  sasonr supports
# it for a step that runs once without writing an observation before the
# failing statement (no SET/MERGE/UPDATE/MODIFY/INFILE/INPUT, no OUTPUT, no
# DO loop): the outputs are left with 0 observations.  Anything else fails
# loud.

# Raised from generated code (sas_rt_fcall) at the failing call.
sas_step_exception <- function(.at = NULL) {
  rlang::abort("step exception", class = c("sasonr_step_exception", "sasonr_error"), sas_line = .at[1] %||% NULL)
}

# One pass, no observation written before the failing statement.
step_runs_once <- function(step) {
  bad <- c("set", "merge", "update", "modify", "infile", "input", "datalines", "cards", "output", "do", "link", "goto")
  walk <- function(stmts) {
    for (s in stmts) {
      if (!is.list(s) || is.null(s$stmt)) next
      if (tolower(s$stmt) %in% bad) return(FALSE)
      # every nested statement: IF then/else, DO bodies, SELECT whens and otherwise
      if (!walk(stmt_children(s))) return(FALSE)
    }
    TRUE
  }
  walk(step$stmts)
}

engine_step_exception <- function(eng, step, tr, e) {
  if (!identical(step$type, "data") || !step_runs_once(step)) {
    abort_unsupported("a step exception (crash) in a DATA step that may have written observations before it", e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, "An exception has been encountered.")
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  log_boundary(eng)
  log_emit(eng, id = "step.exception", task = "DATASTEP")
  log_emit(eng, id = "step.stopped")
  for (nm in tr$outputs) {
    m <- tr$meta[[nm]]
    old <- get0(nm, envir = eng$session$work, inherits = FALSE)
        log_emit(eng, id = "ds.incomplete", name = toupper(nm), nobs = 0L, nvar = length(m$vars %||% character()))
    if (!is.null(old)) {
      log_emit(eng, id = "ds.not_replaced_stopped", name = toupper(nm))
    } else {
      engine_empty_dataset(eng, nm, m)
    }
  }
  invisible()
}
