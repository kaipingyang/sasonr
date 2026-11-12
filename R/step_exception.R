# A SAS 9.4 exception (a crash inside SAS, e.g. CONSTANT('EXACTINT', 1E300)):
# SAS writes the exception block twice, stops the DATA step and keeps its
# output data sets with the observations written so far.  sasonr reproduces
# it for a step that runs once without writing an observation before the
# failing statement (no SET/MERGE/UPDATE/MODIFY/INFILE/INPUT, no OUTPUT, no
# DO loop): the outputs are left with 0 observations.  Anything else fails
# loud.

sas_exception_lines <- function(task = "DATASTEP") {
  block <- c("ERROR:  An exception has been encountered.",
             "Please contact technical support and provide them with the following traceback information:",
             " ", sprintf("The SAS task name is [%s]", task), "Segmentation Violation", " ")
  c(block, "Traceback of the Exception:", " ", " ", block)
}

# Raised from generated code (sas_fcall) at the failing call.
sas_step_exception <- function(.at = NULL) {
  rlang::abort("SAS exception", class = c("sasonr_step_exception", "sasonr_error"), sas_line = .at[1] %||% NULL)
}

# One pass, no observation written before the failing statement.
step_runs_once <- function(step) {
  bad <- c("set", "merge", "update", "modify", "infile", "input", "datalines", "cards", "output", "do", "link", "goto")
  walk <- function(stmts) {
    for (s in stmts) {
      if (is.list(s) && !is.null(s$stmt) && tolower(s$stmt) %in% bad) return(FALSE)
      if (is.list(s) && !is.null(s$body) && !walk(s$body)) return(FALSE)
      if (is.list(s) && !is.null(s$then) && !walk(list(s$then))) return(FALSE)
      if (is.list(s) && !is.null(s$els) && !walk(list(s$els))) return(FALSE)
    }
    TRUE
  }
  walk(step$stmts)
}

engine_step_exception <- function(eng, step, tr, e) {
  if (!identical(step$type, "data") || !step_runs_once(step)) {
    abort_unsupported("a SAS exception (crash) in a DATA step that may have written observations before it", e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, "An exception has been encountered.")
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  eng$log <- c(eng$log, sas_exception_lines(), "NOTE: The SAS System stopped processing this step because of errors.")
  for (nm in tr$outputs) {
    m <- tr$meta[[nm]]
    old <- get0(nm, envir = eng$wenv, inherits = FALSE)
    eng$log <- c(eng$log, sprintf(
      "WARNING: The data set WORK.%s may be incomplete.  When this step was stopped there were 0 observations and %d variables.",
      toupper(nm), length(m$vars %||% character())))
    if (!is.null(old)) {
      eng$log <- c(eng$log, sprintf("WARNING: Data set WORK.%s was not replaced because this step was stopped.", toupper(nm)))
    } else {
      engine_empty_dataset(eng, nm, m)
    }
  }
  invisible()
}
