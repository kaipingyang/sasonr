# sas_check(): every unsupported feature of a program, found before anything runs.

#' List the unsupported features of a SAS program
#'
#' `sas_check()` answers one question: which features does this program use
#' that sasonr does not implement? It walks the program as [sas_translate()]
#' does (the macro processor runs, each step is parsed, analysed and
#' translated, and the variables a step creates are passed on to the steps
#' that follow) without running any step. A step that uses a feature sasonr
#' does not support is reported and the walk goes on with the next step, so
#' one call lists all of them, where [sas_run()] stops at the first one.
#'
#' A step whose text depends on a macro variable that only a run sets (CALL
#' SYMPUT, CALL SYMPUTX or `INTO :` in an earlier step) is not guessed: it is
#' reported with severity `"info"` and checked when the program runs.
#'
#' What `sas_check()` does not tell you:
#' * Syntax errors are not reported. They are part of the program, and
#'   [sas_run()] writes them to the log when the program runs.
#' * Some refusals depend on the data and appear only when the program runs:
#'   a value outside the supported domain of a function, a summary over rows
#'   whose order is not known, a date difference that spans years in a basis
#'   that is not implemented, for example. [sas_run()] still stops at those.
#'
#' Passing `sas_check()` therefore does not mean that the program will run.
#'
#' @inheritParams sas_translate
#' @return A tibble of class `sasonr_check`, one row per finding: `step` (the
#'   index of the step in the program), `line` and `col` (its place in the
#'   program, `NA` when unknown), `severity` (`"error"`: an unsupported
#'   feature; `"info"`: checked when the program runs), `feature` (what is not
#'   supported) and `message` (the full condition message).
#' @examples
#' chk <- sas_check("
#' data a; x = 1; run;
#' proc rank data=a out=b; var x; run;
#' ")
#' chk
#' chk$feature
#' @export
sas_check <- function(code, work = list()) {
  session <- SasSession$new()
  session_add_work(session, work)
  on.exit(session$close(), add = TRUE)
  eng <- new_engine(session, "auto", execute = FALSE)
  eng$check <- list(findings = list(), step = 0L, runtime_macro = FALSE, unresolved = character())
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  withCallingHandlers(
    tryCatch(engine_program(eng, code), error = function(e) check_program_error(eng, e)),
    # a reference the macro processor cannot resolve: remembered for the next step
    sasonr_macro_warning = function(w) {
      m <- regmatches(conditionMessage(w), regexec("symbolic reference ([A-Za-z_][A-Za-z0-9_]*) not resolved", conditionMessage(w)))[[1]]
      if (length(m) == 2L) eng$check$unresolved <- c(eng$check$unresolved, m[2])
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )
  check_result(eng$check$findings)
}

# One chunk of expanded text in a check: the step is translated (not run); a
# feature sasonr does not support is recorded and the walk goes on.
engine_check_exec <- function(eng, text, line) {
  ck <- eng$check
  step0 <- ck$step
  unresolved <- unique(ck$unresolved)
  eng$check$unresolved <- character()
  if (length(unresolved) && isTRUE(ck$runtime_macro)) {
    eng$check$step <- step0 + 1L
    check_add(eng, step0 + 1L, line, NA_integer_, "info",
              paste0("a step that uses macro variable ", paste(toupper(unresolved), collapse = ", "), " set at run time"),
              "Checked when the program runs: the step uses a macro variable set by an earlier step at run time.")
    return(invisible())
  }
  eng$cur_step <- NULL
  step_error <- function(e) {
    k <- max(eng$check$step, step0 + 1L)
    eng$check$step <- k
    check_step_error(eng, e, text, k)
  }
  tryCatch(engine_exec_steps(eng, text, line),
           sasonr_unsupported = function(e) {
             k <- max(eng$check$step, step0 + 1L)
             eng$check$step <- k
             check_add_condition(eng, k, e, line)
           },
           sasonr_syntax_error = step_error,
           sasonr_runtime_error = step_error)
  invisible()
}

# Called by engine_exec_steps() for every step it reads in a check: counts the
# steps and notes whether the step can set macro variables at run time.
engine_check_step_seen <- function(eng, step) {
  if (step$type %in% c("data", "proc", "sql_begin")) eng$check$step <- eng$check$step + 1L
  if (!isTRUE(eng$check$runtime_macro) && check_sets_macro_vars(step)) eng$check$runtime_macro <- TRUE
  check_static_step(eng, step)
}

# Refusals that sas_run() makes while a step runs but that the program text
# decides: a SASHELP member missing from the SASHELP directory (PROC SQL FROM)
# and an INTCK / INTNX interval given as a literal that is not implemented.
# The condition comes from the function that refuses at run time, so the
# message and class are the same.
check_static_step <- function(eng, step) {
  k <- max(eng$check$step, 1L)
  walk <- function(x) {
    if (!is.list(x)) return(invisible())
    if (identical(x$libref, "sashelp") && is.character(x$name) && length(x$name) == 1L && !sashelp_has(x$name)) {
      check_static_try(eng, k, x$line, sashelp_member(x$name, x$line))
    }
    if (identical(x$node, "call") && is.character(x$name) && tolower(x$name) %in% c("intck", "intnx") &&
        length(x$args) >= 1L && identical(x$args[[1]]$node, "str")) {
      fname <- tolower(x$name)
      check_static_try(eng, k, x$line, {
        iv <- interval_or_fail(x$args[[1]]$value, fname, NULL)
        if (fname == "intck" && !is.null(iv) && iv$base %in% interval_narrow && length(x$args) >= 4L) {
          abort_unsupported(paste0("INTCK with the ", toupper(iv$base), " interval and an explicit method (only the default is supported)"))
        }
      })
    }
    for (el in x) walk(el)
    invisible()
  }
  walk(step)
}

check_static_try <- function(eng, step, line, expr) {
  tryCatch(expr, sasonr_unsupported = function(e) {
    if (is.null(e$sas_line) && !is.null(line) && !is.na(line)) {
      e <- tryCatch(abort_unsupported(sub("^Unsupported: ", "", conditionMessage(e)), line), sasonr_unsupported = identity)
    }
    check_add_condition(eng, step, e, line)
  })
  invisible()
}

# An ERROR found while a step is read (outside PROC SQL): sas_run() refuses
# the program when the data sets that the ERROR leaves in WORK cannot be
# derived from the parsed step (engine_step_error(), with the default
# on_error = "sas"); for a syntax ERROR that is decided before the step runs.
check_step_error <- function(eng, e, text, step) {
  # under on_error = "abort" sas_run() stops at the ERROR itself; a run-time
  # ERROR in a check is often a consequence of an earlier refused step
  if (!identical(getOption("sasonr.on_error", "sas")[1], "sas") || !inherits(e, "sasonr_syntax_error")) return(invisible())
  if (inherits(e, "sasonr_global_error") || isTRUE(eng$sql_open)) return(invisible())
  if (!is.null(eng$cur_step) && eng$cur_step$type %in% c("sql_begin", "sql", "sql_end")) return(invisible())
  if (!is.null(err_recover_step(eng, text, e))) return(invisible())
  cnd <- tryCatch(abort_unsupported(paste0("the data sets that this ERROR leaves in WORK, or whether it starts syntax-check mode, ",
                                           "could not be derived from the parsed step (", conditionMessage(e), ")"),
                                    e$sas_line %||% NULL),
                  sasonr_unsupported = identity)
  check_add_condition(eng, step, cnd, e$sas_line %||% NA_integer_)
  invisible()
}

# Does a step's AST call SYMPUT / SYMPUTX / SYMPUTN or select INTO :?
check_sets_macro_vars <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (identical(x$stmt, "call") && is.character(x$routine) && tolower(x$routine) %in% c("symput", "symputx", "symputn")) {
    return(TRUE)
  }
  if (identical(x$node, "select") && length(x$into)) return(TRUE)
  for (el in x) if (check_sets_macro_vars(el)) return(TRUE)
  FALSE
}

# A condition that stops the macro processor itself (the rest of the program
# cannot be walked).
check_program_error <- function(eng, e) {
  k <- eng$check$step + 1L
  if (inherits(e, "sasonr_unsupported")) {
    check_add_condition(eng, k, e, NA_integer_)
  } else if (isTRUE(eng$check$runtime_macro)) {
    check_add(eng, k, e$sas_line %||% NA_integer_, NA_integer_, "info",
              "macro code that depends on a value set at run time",
              paste("Checked when the program runs:", conditionMessage(e)))
  } else if (!inherits(e, c("sasonr_syntax_error", "sasonr_runtime_error"))) {
    rlang::cnd_signal(e)
  }
  invisible()
}

check_add_condition <- function(eng, step, e, line) {
  msg <- conditionMessage(e)
  feature <- sub("^Unsupported: ", "", msg)
  feature <- sub(" \\(SAS line [0-9]+\\)$", "", feature)
  feature <- sub("; set options\\(sasonr.strict = FALSE\\) to use it.*$", "", feature)
  ln <- e$sas_line
  if (is.null(ln) || length(ln) != 1L || is.na(ln)) ln <- line
  col <- e$sas_col
  if (is.null(col) || length(col) != 1L) col <- NA_integer_
  check_add(eng, step, ln, col, "error", feature, msg)
  # further findings of the same step raised together (`sas_more`)
  for (m in e$sas_more) check_add_condition(eng, step, m, line)
}

check_add <- function(eng, step, line, col, severity, feature, message) {
  eng$check$findings[[length(eng$check$findings) + 1L]] <- list(
    step = as.integer(step), line = as.integer(line %||% NA_integer_), col = as.integer(col),
    severity = severity, feature = feature, message = message)
}

check_result <- function(findings) {
  col <- function(name, ptype) vctrs::vec_cast(unlist(lapply(findings, `[[`, name)) %||% ptype, ptype)
  out <- tibble::tibble(step = col("step", integer()), line = col("line", integer()), col = col("col", integer()),
                        severity = col("severity", character()), feature = col("feature", character()),
                        message = col("message", character()))
  class(out) <- c("sasonr_check", class(out))
  out
}

#' @export
print.sasonr_check <- function(x, ...) {
  if (nrow(x) == 0L) {
    cat("No unsupported features found.\n")
    return(invisible(x))
  }
  err <- x$severity == "error"
  if (any(err)) {
    cat(sprintf("This program uses %d unsupported feature%s:\n", sum(err), if (sum(err) == 1L) "" else "s"))
  }
  where <- function(i) if (is.na(x$line[i])) "" else sprintf(" (line %d)", x$line[i])
  for (i in which(err)) cat("- ", x$feature[i], where(i), "\n", sep = "")
  if (any(!err)) {
    cat("Checked when the program runs:\n")
    for (i in which(!err)) cat("- ", x$feature[i], where(i), "\n", sep = "")
  }
  invisible(x)
}
