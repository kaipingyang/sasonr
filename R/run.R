# Program drivers: run a SAS program (macro processor -> parser -> executor),
# or translate it to R code.

meta_from_df <- function(d, disp = names(d)) {
  vars <- tolower(names(d))
  type <- lapply(d, function(x) if (is.character(x)) "char" else "num")
  len <- lapply(d, function(x) if (is.character(x)) max(1L, nchar(x), na.rm = TRUE) else 8L)
  disp <- as.list(disp)
  names(type) <- names(len) <- names(disp) <- vars
  list(vars = vars, type = type, len = len, formats = list(), disp = disp)
}

prepare_work <- function(work) {
  if (length(work) == 0) return(list())
  if (is.null(names(work)) || any(!nzchar(names(work)))) {
    rlang::abort("`work` must be a named list of data frames.", class = "sasonr_error")
  }
  out <- lapply(work, function(d) {
    d <- tibble::as_tibble(d)
    names(d) <- tolower(names(d))
    pad_char_columns(d)
  })
  names(out) <- tolower(names(work))
  out
}

# An engine: the bookkeeping of one submission (steps translated, phase,
# syntax-check mode, ERRORs, the log and listing of this submission) against a
# SasSession (`eng$session`, session.R), which owns WORK, metadata, titles,
# macro tables, options and the diagnostic state.  `exec(text, line)` parses,
# translates and (optionally) runs one chunk of expanded text.
new_engine <- function(session, level, execute) {
  eng <- new.env(parent = emptyenv())
  eng$session <- session
  eng$level <- level
  eng$execute <- execute
  eng$steps <- list()
  eng$log <- character()
  eng$listing <- character()
  eng$sql_open <- FALSE
  eng$sql_opts <- list()
  ses <- session$macro
  ses$exec <- function(text, line) engine_exec(eng, text, line)
  ses$line <- 1L
  ses$frames <- list()
  rt <- session$rt
  # PRXPARSE ids do not outlive a submission
  rt$prx <- list()
  rt$prx_cache <- list()
  rt_reset(rt)
  # step-level NOTEs (data set sizes, observations read, missing values...)
  # and macro WARNINGs in the log; options(sasonr.step_notes = FALSE) turns them off
  eng$step_notes <- isTRUE(getOption("sasonr.step_notes", TRUE))
  rt$on_warn <- if (eng$step_notes) function(msg) eng$log <- c(eng$log, paste("WARNING:", msg))
  rt$on_error <- function(msg) eng$log <- c(eng$log, paste("ERROR:", msg))
  rt$on_note <- function(msg) eng$log <- c(eng$log, msg)
  ses$on_put <- function(line) eng$log <- c(eng$log, line)
  eng
}

engine_exec <- function(eng, text, line) {
  if (!identical(eng$on_error, "sas")) return(engine_exec_steps(eng, text, line))
  eng$phase <- "compile"
  eng$cur_step <- NULL
  tryCatch(engine_exec_steps(eng, text, line),
           sasonr_syntax_error = function(e) engine_step_error(eng, e, text),
           sasonr_runtime_error = function(e) engine_step_error(eng, e, text))
  invisible()
}

engine_exec_steps <- function(eng, text, line) {
  rt <- eng$session$rt
  prog <- sas_parse(paste0(strrep("\n", max(0L, line - 1L)), text), resolved = TRUE, sql_open = eng$sql_open)
  eng$sql_open <- isTRUE(attr(prog, "sql_open"))
  for (step in prog$steps) {
    eng$phase <- "compile"
    # the AST of the step being compiled / run (errors.R derives from it what an ERROR leaves in WORK)
    eng$cur_step <- step
    if (step$type == "global") {
      engine_global(eng, step)
      next
    }
    if (step$type == "sql_begin") {
      eng$sql_opts <- step$options
      next
    }
    if (step$type == "sql_end") {
      if (isTRUE(eng$sql_noexec)) {
        eng$log <- c(eng$log, "NOTE: The SAS System stopped processing this step because of errors.")
        eng$sql_noexec <- FALSE
      }
      next
    }
    if (step$type == "sql") {
      if (isTRUE(eng$sql_noexec) || isTRUE(eng$syntax_check)) {
        if (isTRUE(eng$sql_noexec)) eng$log <- c(eng$log, "NOTE: Statement not executed due to NOEXEC option.")
        next
      }
      eng$steps <- c(eng$steps, list(list(code = paste0("# PROC SQL ", toupper(step$stmt$kind), " (SAS line ", step$line,
                                                            "): executed by the SQL engine"),
                                          level = "SQL", reasons = "PROC SQL", line = step$line, outputs = character(), meta = list())))
      eng$phase <- "sql"
      if (eng$execute) engine_sql(eng, step$stmt)
      next
    }
    if (isTRUE(eng$syntax_check)) {
      engine_check_step(eng, step)
      next
    }
    if (step$type == "proc" && step$name == "datasets") {
      for (t in step$titles) engine_global(eng, t)
      eng$steps <- c(eng$steps, list(list(code = paste0("# PROC DATASETS (SAS line ", step$line, "): executed by the engine"),
                                          level = "PROC", reasons = "PROC DATASETS", line = step$line, outputs = character(), meta = list())))
      if (eng$execute) {
        rt_reset(rt)
        engine_datasets(eng, step)
      }
      next
    }
    if (step$type == "proc") {
      for (t in step$titles) engine_global(eng, t)
      tr <- translate_proc(step, eng$session$meta, eng$session$last)
    } else {
      tr <- translate_step(step, eng$session$meta, eng$level)
    }
    eng$session$meta[names(tr$meta)] <- tr$meta
    eng$session$unordered <- setdiff(eng$session$unordered, tr$outputs)
    if (length(tr$outputs) > 0) eng$session$last <- tr$outputs[length(tr$outputs)]
    eng$steps <- c(eng$steps, list(tr))
    if (!eng$execute) next
    vec <- step$type == "data" && tr$level %in% c("L1", "L2")
    eng$phase <- if (step$type == "data") "execute_data" else "execute"
    # run-wide state the vectorised attempt may change before a re-run (PRXPARSE ids)
    prx0 <- rt$prx
    # Vectorised code evaluates every IF/SELECT branch on every row, so a branch
    # SAS never runs can fail loud or raise a SAS exception: the step is re-run
    # row by row (L3), where only the taken branches run; a genuine unsupported
    # value or exception is raised there again.  A construct that is unsupported
    # whatever the values (sasonr_unsupported_construct) fails loud at once.
    env <- tryCatch(engine_eval(eng, tr$code, stage = vec),
                    sasonr_step_exception = function(e) if (vec) e else engine_step_exception(eng, step, tr, e),
                    sasonr_unsupported = function(e) {
                      if (vec && !inherits(e, "sasonr_unsupported_construct")) e else stop(e)
                    })
    branch <- vec && inherits(env, c("sasonr_unsupported", "sasonr_step_exception"))
    if (!branch && !is.environment(env)) next
    if (branch || (vec && (rt$err || isTRUE(rt$mv_rerun)))) {
      # A data error in vectorised code: re-run the step row by row (L3) so
      # that _ERROR_, the NOTEs and the PDV dump follow SAS exactly.
      rt$prx <- prx0
      tr <- translate_step(step, eng$session$meta, "L3")
      tr$reasons <- c(if (branch) "unsupported value or SAS exception in vectorised code (re-run row by row)" else
        "data error at run time (re-run of the vectorised step)", tr$reasons)
      eng$steps[[length(eng$steps)]] <- tr
      env <- tryCatch(engine_eval(eng, tr$code, stage = FALSE),
                      sasonr_step_exception = function(e) engine_step_exception(eng, step, tr, e))
      if (!is.environment(env)) next
    } else if (vec) {
      for (nm in tr$outputs) assign(nm, env$work[[nm]], envir = eng$session$work)
      # NOTEs that are not data errors (e.g. INPUT going to a new line)
      if (length(rt$notes) > 0 && isTRUE(eng$session$options$notes)) env$.log <- c(env$.log, rt$notes)
    }
    stopped <- step$type == "data" && isTRUE(rt$stop_step)
    if (stopped) {
      # An execution-time ERROR that stops the step (SYMGET): SAS ends the step
      # after the current observation and keeps what was written ("WARNING: The
      # data set ... may be incomplete").  Only a one-observation step is known
      # to match SAS; a longer step would have to stop part-way through.
      big <- vapply(tr$outputs, function(nm) NROW(get0(nm, envir = eng$session$work, inherits = FALSE)) > 1L, logical(1))
      if (any(big)) abort_unsupported("an execution-time ERROR that stops a DATA step with more than one observation", step$line)
      env$.log <- c(env$.log, "NOTE: The SAS System stopped processing this step because of errors.")
    }
    if (step$type == "data") engine_store_numlen(eng, tr$outputs)
    notes_on <- isTRUE(eng$session$options$notes) && eng$step_notes
    eng$log <- c(eng$log, if (notes_on) tr$compile_notes, env$.log)
    if (notes_on) {
      ds_notes <- unlist(lapply(tr$outputs, function(nm) {
        d <- get0(nm, envir = eng$session$work, inherits = FALSE)
        if (is.null(d)) NULL else if (stopped) {
          sprintf("WARNING: The data set WORK.%s may be incomplete.  When this step was stopped there were %d observations and %d variables.",
                  toupper(nm), nrow(d), ncol(d))
        } else dataset_note(nm, d)
      }))
      # PROC SORT of an empty data set reports no observations read
      if (identical(tr$reasons, "PROC SORT")) rt$reads <- Filter(function(r) r$n > 0, rt$reads)
      reads <- if (identical(tr$reasons, "PROC PRINT")) print_read_notes() else read_notes()
      eng$log <- c(eng$log, step_end_notes(), reads, rt$proc_notes, ds_notes)
    }
    engine_add_listing(eng, env$.lst, step$line)
    if (identical(eng$on_error, "sas")) err_set_macro(eng, syserr = 0)
    # output data sets whose layout is only known at run time (PROC TRANSPOSE...)
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
  }
  invisible()
}

# Syntax-check mode (after an ERROR, on_error = "sas"): the step is compiled
# but not run; a DATA step creates its output data sets with 0 observations.
engine_check_step <- function(eng, step) {
  if (step$type == "proc") {
    if (step$name != "datasets") translate_proc(step, eng$session$meta, eng$session$last)
    err_set_macro(eng, syserr = 3)
    return(invisible())
  }
  tr <- translate_step(step, eng$session$meta, eng$level)
  for (nm in tr$outputs) {
    m <- tr$meta[[nm]]
    vars <- m$vars %||% character()
    engine_empty_dataset(eng, nm, m)
    if (isTRUE(eng$session$options$notes) && eng$step_notes) {
      eng$log <- c(eng$log, sprintf("NOTE: The data set WORK.%s has 0 observations and %d variables.", toupper(nm), length(vars)))
    }
  }
  err_set_macro(eng, syserr = 3)
  invisible()
}

# Numeric variables stored with LENGTH < 8 keep only the first bytes of the
# IEEE 754 value (SAS truncates the mantissa when the observation is written).
engine_store_numlen <- function(eng, outputs) {
  for (nm in outputs) {
    m <- eng$session$meta[[nm]]
    d <- get0(nm, envir = eng$session$work, inherits = FALSE)
    if (is.null(m) || is.null(d)) next
    short <- names(m$len)[vapply(names(m$len), function(v) identical(m$type[[v]], "num") &&
                                    isTRUE(m$len[[v]] < 8L), logical(1))]
    short <- intersect(short, names(d))
    if (length(short) == 0) next
    for (v in short) d[[v]] <- sas_num_store(d[[v]], m$len[[v]])
    assign(nm, d, envir = eng$session$work)
  }
}

#' Value of a numeric variable after storage with `len` bytes (3-8)
#' @keywords internal
sas_num_store <- function(x, len) {
  ok <- !is.na(x)
  if (len >= 8L || !any(ok)) return(x)
  b <- matrix(writeBin(as.double(x[ok]), raw(), size = 8L, endian = "big"), nrow = 8L)
  b[(len + 1L):8L, ] <- as.raw(0)
  x[ok] <- readBin(as.vector(b), "double", n = sum(ok), size = 8L, endian = "big")
  x
}

# Evaluate generated code; with `stage = TRUE` output data sets go to a scratch
# copy of WORK (the caller commits them) and run-time NOTEs are discarded.
engine_eval <- function(eng, code, stage = FALSE) {
  rt <- eng$session$rt
  rt_reset(rt)
  env <- new.env(parent = asNamespace("sasonr"))
  if (stage) {
    env$work <- new.env(parent = emptyenv())
    for (nm in ls(eng$session$work, all.names = TRUE)) assign(nm, get(nm, envir = eng$session$work), envir = env$work)
  } else {
    env$work <- eng$session$work
  }
  env$.log <- character()
  env$.lst <- character()
  env$.meta <- list()
  env$.symtab <- eng$session$macro
  env$.titles <- eng$session$titles
  tryCatch(eval(parse(text = code, keep.source = FALSE), envir = env), error = function(e) {
    # surface a sasonr condition wrapped by dplyr (e.g. raised inside mutate())
    p <- e
    while (!is.null(p) && !inherits(p, "sasonr_error")) p <- p$parent
    if (!is.null(p)) stop(p)
    stop(e)
  })
  if (!stage && length(rt$notes) > 0) {
    env$.log <- c(env$.log, rt$notes)
    rt$notes <- character()
  }
  env
}

engine_global <- function(eng, g) {
  if (g$kind == "filename") eng$session$filerefs[[g$ref]] <- g$path
  if (g$kind == "options") {
    if (!is.null(g$ls)) eng$session$options$linesize <- g$ls
    if ("nonotes" %in% g$opts) eng$session$options$notes <- FALSE
    if ("notes" %in% g$opts) eng$session$options$notes <- TRUE
    if ("nosyntaxcheck" %in% g$opts) eng$session$options$nosyntaxcheck <- TRUE
    if ("syntaxcheck" %in% g$opts) eng$session$options$nosyntaxcheck <- FALSE
    o <- eng$session$options
    for (k in c("center", "date", "number")) {
      if (k %in% g$opts) o[[k]] <- TRUE
      if (paste0("no", k) %in% g$opts) o[[k]] <- FALSE
    }
    if (!is.null(g$ps)) o$pagesize <- g$ps
    if (!is.null(g$formdlim)) o$formdlim <- g$formdlim
  }
  if (g$kind == "title") {
    keep <- eng$session$titles[seq_len(min(length(eng$session$titles), g$level - 1L))]
    if (length(keep) < g$level - 1L) keep <- c(keep, rep("", g$level - 1L - length(keep)))
    eng$session$titles <- if (nzchar(g$text)) c(keep, g$text) else keep
    while (length(eng$session$titles) > 0 && !nzchar(eng$session$titles[length(eng$session$titles)])) {
      eng$session$titles <- eng$session$titles[-length(eng$session$titles)]
    }
  }
  invisible()
}

engine_program <- function(eng, code) {
  text <- paste(code, collapse = "\n")
  sink <- code_sink(eng$session$macro)
  mp_scan(mp_src(text, line_of = TRUE), eng$session$macro, sink, mode = "code")
  sink$finish()
  invisible(eng)
}

#' Translate a SAS program into R (tidyverse) code
#'
#' The macro processor runs first; steps are translated in order. Nothing is
#' executed, so macro variables created by CALL SYMPUT are not available to
#' later macro code (use [sas_run()] for that).
#'
#' @param code SAS source code.
#' @param work Named list of input data frames available as `WORK` data sets.
#' @param level `"auto"` (choose L1/L2/L3 per step) or `"L3"` (force row loops).
#' @return A `sasonr_translation` object; `$steps[[i]]$code` holds the R code.
#' @examples
#' tr <- sas_translate("data b; set a; y = round(x, 0.1); run;",
#'                     work = list(a = data.frame(x = c(1.25, 2.35))))
#' tr
#' @export
sas_translate <- function(code, work = list(), level = c("auto", "L3")) {
  level <- match.arg(level)
  session <- SasSession$new()
  session_add_work(session, work)
  on.exit(session$close(), add = TRUE)
  eng <- new_engine(session, level, execute = FALSE)
  # restore the binding first, then bind (an interrupt in between leaves nothing bound)
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  engine_program(eng, code)
  structure(list(steps = eng$steps), class = "sasonr_translation")
}

#' Run a SAS program on the R engine
#'
#' The macro processor hands complete steps to the executor one at a time
#' (at RUN; / QUIT; / step boundaries), so CALL SYMPUT(X) results are visible to
#' the macro code that follows the step, including inside macro bodies.
#'
#' @inheritParams sas_translate
#' @param on_error What a SAS ERROR does (a syntax error, an unknown function,
#'   a missing input data set, an ERROR in PROC SQL ...). `"sas"` (the
#'   default; option `sasonr.on_error`) behaves like SAS in batch: the failing
#'   step is not run, SAS's syntax-check mode (OBS=0) applies to the rest of the
#'   program (later steps are compiled but not run; global statements and the
#'   macro language still are), `SYSERR`/`SYSCC` are set and the result has
#'   `status = "error"`, the ERROR lines in `$log` and WORK as SAS leaves it.
#'   `"abort"` stops with the classed R condition instead. Features sasonr does
#'   not support (`sasonr_unsupported`) always stop the run: they are not SAS
#'   errors.
#' @param session A session from [sas_session()]: the program runs as one more
#'   submission to it (WORK, macro variables, user formats, options and titles
#'   of earlier submissions are visible; `work` data sets are added to its
#'   WORK). `NULL` (the default) runs in a fresh session that is discarded
#'   afterwards.
#' @return A `sasonr_result`: `$log` (PUT and %PUT lines), `$listing` (procedure
#'   output), `$work` (output data sets with trailing blanks removed),
#'   `$translation`, `$macro_vars` (global macro variables), `$status`
#'   (`"ok"` or `"error"`), `$errors` (SAS ERROR messages) and `$error_line`
#'   (SAS line of the first ERROR). With a `session`, `$log`, `$listing`,
#'   `$translation`, `$status` and `$errors` describe this submission, `$work`
#'   and `$macro_vars` the whole session.
#' @examples
#' res <- sas_run("
#' data class;
#'   input name $ age;
#'   length group $ 5;
#'   if age >= 13 then group = 'teen'; else group = 'child';
#'   datalines;
#' Alice 12
#' Bob 14
#' ;
#' run;
#' proc print data=class; run;
#' ")
#' res$work$class
#' cat(res$listing, sep = "\n")
#' @export
sas_run <- function(code, work = list(), level = c("auto", "L3"),
                    on_error = getOption("sasonr.on_error", c("sas", "abort")), session = NULL) {
  level <- match.arg(level)
  on_error <- match.arg(on_error, c("sas", "abort"))
  if (is.null(session)) {
    # a throw-away session, closed (SQL connection) when the run ends
    session <- SasSession$new()
    on.exit(session$close(), add = TRUE)
  } else {
    check_session(session)
  }
  # a nested submission to the same session fails here, before anything changes
  session_enter(session)
  on.exit(session_leave(session), add = TRUE)
  session_add_work(session, work)
  eng <- new_engine(session, level, execute = TRUE)
  eng$on_error <- on_error
  eng$status <- "ok"
  # bind the session for the generated code and the run-time helpers (the
  # restore is registered first: an interrupt in between leaves nothing bound)
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  on.exit(engine_flush(eng), add = TRUE)
  if (on_error == "sas") {
    # step ERRORs are handled by engine_exec(); a condition that reaches this
    # level was raised by the macro processor, whose SAS error handling
    # (ERROR text, stopping the macro, what %LET receives) is not implemented
    tryCatch(engine_program(eng, code),
             sasonr_syntax_error = function(e) macro_error_unsupported(e),
             sasonr_runtime_error = function(e) macro_error_unsupported(e))
  } else {
    engine_program(eng, code)
  }
  engine_result(eng)
}

macro_error_unsupported <- function(e) {
  msg <- sub("^(Syntax error|Runtime error): ", "", conditionMessage(e))
  rlang::abort(paste0("Unsupported: macro-time error handling (", msg, ")"),
               class = c("sasonr_unsupported", "sasonr_error"), sas_line = e$sas_line)
}

# sas_run(code, on_error = "abort") that keeps what ran before an error:
# list(result, error).  `result` is the sasonr_result of the steps run before
# the error (NULL when it cannot be built), `error` the condition (NULL on
# success).  Used by the conformance runner (matrix programs: the cases before
# a failing case need no second run).
engine_run_partial <- function(code) {
  session <- SasSession$new()
  on.exit(session$close(), add = TRUE)
  eng <- new_engine(session, "auto", execute = TRUE)
  eng$on_error <- "abort"
  eng$status <- "ok"
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  err <- tryCatch({
    engine_program(eng, code)
    NULL
  }, error = function(e) e)
  res <- if (is.null(err)) engine_result(eng) else tryCatch(engine_result(eng), error = function(e) NULL)
  list(result = res, error = err)
}

# Append procedure output to the listing of the submission.  sasonr writes
# the listing SAS writes under NOCENTER NODATE NONUMBER FORMDLIM=' '
# PAGESIZE=MAX (the options of the SAS gold runs).  Under other values SAS
# centres the lines (CENTER), writes a header line with the date or page
# number (DATE, NUMBER), a delimiter line instead of each new page after the
# first (FORMDLIM=), or breaks a long output into pages with repeated
# headings (PAGESIZE=): output written under them fails loud.  An output of at
# most PAGESIZE - 3 lines fits on one page (margin for the title lines).
engine_add_listing <- function(eng, lst, line = NULL) {
  if (length(lst) == 0) return(invisible())
  o <- eng$session$options
  bad <- c(if (isTRUE(o$center)) "CENTER", if (isTRUE(o$date)) "DATE", if (isTRUE(o$number)) "NUMBER",
           if (!identical(o$formdlim, " ") && length(eng$session$listing) + length(eng$listing) > 0)
             "FORMDLIM= other than ' '",
           if (length(lst) + 3L > o$pagesize) paste0("PAGESIZE=", o$pagesize, " (output longer than one page)"))
  if (length(bad)) {
    abort_unsupported(paste0("procedure output under OPTIONS ", paste(bad, collapse = ", "),
                             " (only the NOCENTER NODATE NONUMBER FORMDLIM=' ' PAGESIZE=MAX listing is verified)"), line)
  }
  eng$listing <- c(eng$listing, lst)
  invisible()
}

# The log and listing of a submission are appended to the session's cumulative
# log and listing when the submission ends (also after an R error).
engine_flush <- function(eng) {
  if (isTRUE(eng$flushed)) return(invisible())
  eng$flushed <- TRUE
  s <- eng$session
  s$log <- c(s$log, unlist(strsplit(eng$log, "\n", fixed = TRUE)))
  s$listing <- c(s$listing, eng$listing)
  invisible()
}

# Automatic macro variables (not listed among the user's global macro variables).
macro_auto_vars <- c("sysdate", "sysdate9", "sysday", "systime", "sysver", "sysvlong", "sysscp", "sysncpu",
                     "sysscpl", "syserr", "syscc", "sysrc", "sysmacroname", "sysindex")

# The sasonr_result of an engine.
engine_result <- function(eng) {
  out <- lapply(as.list(eng$session$work), function(d) {
    dplyr::mutate(d, dplyr::across(dplyr::where(is.character), sas_rtrim))
  })
  eng$log <- unlist(strsplit(eng$log, "\n", fixed = TRUE)) %||% character()
  mv <- as.list(eng$session$macro$globals)
  autos <- macro_auto_vars
  mv <- mv[setdiff(sort(names(mv)), autos)]
  dsn <- sort(names(out) %||% character())
  structure(
    list(log = eng$log, listing = eng$listing, work = out[dsn], meta = eng$session$meta[intersect(dsn, names(eng$session$meta))],
         unordered = intersect(eng$session$unordered, dsn),
         translation = structure(list(steps = eng$steps), class = "sasonr_translation"),
         macro_vars = mv, status = eng$status, errors = eng$errors %||% character(),
         error_line = eng$error_line %||% NA_integer_),
    class = "sasonr_result"
  )
}

#' @export
print.sasonr_translation <- function(x, ...) {
  for (s in x$steps) cat(s$code, "\n\n", sep = "")
  invisible(x)
}

#' @export
print.sasonr_result <- function(x, ...) {
  cli::cli_h2("SAS log (PUT / %PUT output)")
  if (length(x$log) > 0) cat(x$log, sep = "\n") else cli::cli_text("{.emph (empty)}")
  if (length(x$listing) > 0) {
    cli::cli_h2("Listing")
    cat(x$listing, sep = "\n")
  }
  cli::cli_h2("WORK data sets")
  if (length(x$work) == 0) cli::cli_text("{.emph (none)}")
  for (nm in names(x$work)) {
    cli::cli_text("{.strong WORK.{toupper(nm)}}: {nrow(x$work[[nm]])} obs, {ncol(x$work[[nm]])} vars")
  }
  cli::cli_text("Levels: {vapply(x$translation$steps, `[[`, '', 'level')}")
  invisible(x)
}
