# Program drivers: run a SAS program (macro processor -> parser -> executor),
# or translate it to R code.

meta_from_df <- function(d, disp = names(d)) {
  vars <- tolower(names(d))
  type <- lapply(d, function(x) if (is.character(x)) "char" else "num")
  len <- lapply(d, function(x) if (is.character(x)) max(1L, chr_bytes(x), na.rm = TRUE) else 8L)
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
  log_engine_init(eng)
  eng$listing <- character()
  eng$ods <- list()
  # the procedure outputs sent to the HTML destination (sas_html())
  eng$html <- list()
  eng$sql_open <- FALSE
  eng$sql_opts <- list()
  ses <- session$macro
  ses$exec <- function(text, line) {
    # the macro-affected lines so far: a trigger later on the line of the
    # step's last statement (after `run;`) is marked only after the step
    # ran; the step's own text on that line is not changed by it
    eng$session$rt$macro_lines <- ses$macro_lines
    eng$chunk_no <- (eng$chunk_no %||% 0L) + 1L
    engine_exec(eng, text, line)
  }
  ses$on_step_options <- function(text, line) engine_step_options(eng, text, line)
  ses$line <- 1L
  ses$frames <- list()
  # the program lines the macro processor changed (mp_consumed())
  ses$macro_lines <- integer()
  rt <- session$rt
  # PRXPARSE ids do not outlive a submission, nor the approximate features used
  # with sasonr.strict = FALSE (strict_gate())
  rt$prx <- list()
  rt$prx_cache <- list()
  log_approx_reset(rt)
  rt_reset(rt)
  # step-level NOTEs (data set sizes, observations read, missing values...)
  # and macro WARNINGs in the log; options(sasonr.step_notes = FALSE) turns them off
  eng$step_notes <- isTRUE(getOption("sasonr.step_notes", TRUE))
  log_bind_callbacks(eng, rt, ses)
  eng
}

engine_exec <- function(eng, text, line) {
  if (!is.null(eng$check)) return(engine_check_exec(eng, text, line))
  # sas_translate(): a chunk whose text depends on run-time values is kept as
  # SAS text (script.R)
  if (!eng$execute && script_defer(eng, text, line)) return(invisible())
  script_open_code(eng, text, line)
  if (!eng$execute && isTRUE(eng$sql_seen)) return(script_exec_after_sql(eng, text, line))
  if (!identical(eng$on_error, "sas")) return(engine_exec_steps(eng, text, line))
  eng$phase <- "compile"
  eng$cur_step <- NULL
  # the last program line of the chunk (the source echo after an ERROR)
  eng$chunk_end <- log_chunk_last(text, line)
  tryCatch(engine_exec_steps(eng, text, line),
           sasonr_syntax_error = function(e) engine_step_error(eng, e, text),
           sasonr_runtime_error = function(e) engine_step_error(eng, e, text))
  if (!isTRUE(eng$in_sql_step)) log_open_code(eng)
  invisible()
}

engine_exec_steps <- function(eng, text, line) {
  rt <- eng$session$rt
  prog <- sas_parse(paste0(strrep("\n", max(0L, line - 1L)), text), resolved = TRUE, sql_open = eng$sql_open)
  eng$sql_open <- isTRUE(attr(prog, "sql_open"))
  # the last program line of each step (the source echo of the rich log views)
  ends <- log_step_ends(prog$steps, text, line)
  # the RUN; after the data lines of a DATALINES step belongs to that step
  if (!length(prog$steps) && log_step_tail_extend(eng, log_chunk_last(text, line), text)) return(invisible())
  for (k in seq_along(prog$steps)) {
    step <- prog$steps[[k]]
    if (!is.null(eng$check)) engine_check_step_seen(eng, step)
    eng$phase <- "compile"
    # the AST of the step being compiled / run (errors.R derives from it what an ERROR leaves in WORK)
    eng$cur_step <- step
    # the step index of the log events (a PROC SQL step counts from PROC SQL)
    if (step$type %in% c("data", "proc", "sql_begin")) {
      log_step_start(eng, log_step_label(step), step$line, ends[k], tail = attr(ends, "tail")[[k]])
      if (!is.null(dl <- log_step_datalines(step))) log_source_skip(eng, dl$line_no)
      eng$in_sql_step <- step$type == "sql_begin"
    } else {
      # global statements between steps are open code; a PROC SQL step lasts until QUIT
      if (step$type == "global" && !isTRUE(eng$in_sql_step)) log_open_code(eng)
      if (step$type == "sql_end") eng$in_sql_step <- FALSE
      log_source_read(eng, ends[k])
    }
    if (isTRUE(eng$sc_pending) && step$type != "global") {
      # after an ERROR in a global statement syntax-check mode starts at the
      # next step, here a DATA step (also DATA _NULL_, also with a TITLE
      # statement in between; engine_global_error()), unless OPTIONS
      # NOSYNTAXCHECK came in between
      if (step$type != "data") {
        abort_unsupported("a PROC step after an ERROR in a global statement (supported: a DATA step)", step$line)
      }
      eng$sc_pending <- FALSE
      if (!isTRUE(eng$session$options$nosyntaxcheck)) {
        # the NOTE is the first thing the step writes: after its lines
        log_boundary(eng)
        log_emit(eng, id = "step.syntax_check")
        eng$syntax_check <- TRUE
      }
    }
    if (isTRUE(eng$syntax_check) && isTRUE(eng$sql_noexec) && step$type %in% c("data", "proc")) {
      abort_unsupported("a PROC SQL step in syntax-check mode that ends without QUIT (not supported)", step$line)
    }
    if (length(eng$session$ods_output) > 0 && step$type %in% c("data", "proc", "sql_begin")) {
      # ODS OUTPUT requests apply to the next PROC step (a DATA step leaves them
      # pending): PROC MEANS, or PROC PRINT of an existing data set with one
      # request (the WARNING "not created", ods_output.R)
      ok_print <- step$type == "proc" && step$name == "print" && length(eng$session$ods_output) == 1L &&
        length(eng$session$ods_output[[1]]$object) == 1L && length(step$options$data$options) == 0L && !is.null(step$options$data$name %||% eng$session$last) &&
        exists(tolower(step$options$data$name %||% eng$session$last), envir = eng$session$work, inherits = FALSE)
      ok <- step$type == "data" || (step$type == "proc" && step$name == "means") || ok_print
      if (!ok || isTRUE(eng$syntax_check)) {
        abort_unsupported(paste0("ODS OUTPUT requests at ", if (step$type == "data") "a DATA step" else
          paste("PROC", toupper(if (step$type == "sql_begin") "sql" else step$name)),
          if (isTRUE(eng$syntax_check)) " in syntax-check mode", " (supported: PROC MEANS, PROC PRINT with one request, a DATA step in between)"), step$line)
      }
    }
    if (step$type %in% c("global", "sql_begin", "sql", "sql_end")) script_record(eng, step, text, line)
    if (step$type == "global") {
      if (identical(step$kind, "filename_noref")) step$quote <- engine_quote_at(text, line, step)
      engine_global(eng, step)
      next
    }
    if (step$type == "sql_begin") {
      eng$sql_opts <- step$options
      if (!is.null(step$error)) {
        # a syntax ERROR in the PROC SQL statement: PROC SQL sets NOEXEC and
        # checks the statements that follow
        if (!identical(eng$on_error, "sas")) rlang::cnd_signal(step$error)
        engine_step_error(eng, step$error, text, sql = TRUE)
        next
      }
      if (isTRUE(eng$syntax_check)) {
        # sasonr models PROC SQL in syntax-check mode as follows: as with
        # NOEXEC no statement runs, SQLOBS and SQLRC are 0, the step ends
        # "stopped" with SYSERR 1012
        log_emit(eng, id = "sql.noexec")
        eng$sql_noexec <- TRUE
      }
      next
    }
    if (step$type == "sql_end") {
      if (isTRUE(eng$sql_noexec)) {
        # (under NOSYNTAXCHECK, or already in syntax-check mode, the step
        # ends the same way for every ERROR)
        if ("other" %in% eng$sql_error_kind && !isTRUE(eng$syntax_check) && !isTRUE(eng$session$options$nosyntaxcheck)) {
          abort_unsupported("whether this PROC SQL ERROR starts syntax-check mode (not supported)", step$line)
        }
        log_emit(eng, id = "step.stopped")
        eng$sql_noexec <- FALSE
        if ("run" %in% eng$sql_error_kind && !isTRUE(eng$syntax_check) && !isTRUE(eng$session$options$nosyntaxcheck)) {
          log_emit(eng, id = "step.syntax_check")
          eng$syntax_check <- TRUE
        }
        eng$sql_error_kind <- NULL
        # a step that stopped: SYSERR is 1012 (also without syntax-check mode)
        err_set_macro(eng, syserr = 1012)
      }
      next
    }
    if (step$type == "sql") {
      if (!is.null(step$stmt$error)) {
        # a syntax ERROR in a RESET statement is handled like one in the PROC SQL
        # statement
        if (isTRUE(eng$syntax_check)) abort_unsupported("a RESET option ERROR in syntax-check mode", step$line)
        if (!identical(eng$on_error, "sas")) rlang::cnd_signal(step$stmt$error)
        engine_step_error(eng, step$stmt$error, text, sql = TRUE)
        next
      }
      if (isTRUE(eng$sql_noexec) || isTRUE(eng$syntax_check)) {
        if (isTRUE(eng$sql_noexec)) log_emit(eng, id = "sql.not_executed")
        if (isTRUE(eng$syntax_check)) sql_set_status(eng, 0, 0)
        next
      }
      eng$steps <- c(eng$steps, list(list(code = untranslated_code(paste("PROC SQL", toupper(step$stmt$kind)), step$line),
                                          level = "SQL", reasons = "PROC SQL", line = step$line, outputs = character(), meta = list())))
      eng$phase <- "sql"
      if (eng$execute) engine_sql(eng, step$stmt)
      next
    }
    # syntax-check mode: PROC CONTENTS runs on (OUT= with 0 observations below)
    if (isTRUE(eng$syntax_check) && !engine_check_step(eng, step)) next
    if (step$type == "proc" && step$name == "datasets") {
      for (t in step$titles) engine_global(eng, t)
      eng$steps <- c(eng$steps, list(list(code = untranslated_code("PROC DATASETS", step$line),
                                          level = "PROC", reasons = "PROC DATASETS", line = step$line, outputs = character(), meta = list())))
      script_record(eng, step, text, line)
      if (eng$execute) {
        rt_reset(rt)
        engine_datasets(eng, step)
      }
      next
    }
    if (step$type == "proc") {
      for (t in step$titles) engine_global(eng, t)
      engine_check_last_view(eng, step)
      tr <- translate_proc(step, eng$session$meta, eng$session$last)
    } else {
      tr <- translate_step(step, eng$session$meta, eng$level)
      # sas_data_step(): the step runs the code of the script
      if (!is.null(eng$override)) {
        tr$exprs <- eng$override
        eng$override <- NULL
      }
    }
    eng$session$meta[names(tr$meta)] <- tr$meta
    eng$session$unordered <- setdiff(eng$session$unordered, tr$outputs)
    if (length(tr$outputs) > 0) session_set_last(eng$session, tr$outputs[length(tr$outputs)])
    eng$steps <- c(eng$steps, list(tr))
    node <- script_record(eng, step, text, line, tr)
    if (!eng$execute) next
    vec <- step$type == "data" && tr$level %in% c("L1", "L2")
    eng$phase <- if (step$type == "data") "execute_data" else "execute"
    # run-wide state the vectorised attempt may change before a re-run (PRXPARSE ids)
    prx0 <- rt$prx
    # Vectorised code evaluates every IF/SELECT branch on every row, so a branch
    # that never runs can fail loud or raise a step exception: the step is re-run
    # row by row (L3), where only the taken branches run; a genuine unsupported
    # value or exception is raised there again.  A construct that is unsupported
    # whatever the values (sasonr_unsupported_construct) fails loud at once.
    sc_empty <- eng$sc_empty %||% character()
    eng$sc_empty <- NULL
    env <- tryCatch(engine_eval(eng, tr, stage = vec, empty = sc_empty),
                    sasonr_step_exception = function(e) if (vec) e else engine_step_exception(eng, step, tr, e),
                    sasonr_unsupported = function(e) {
                      if (vec && !inherits(e, "sasonr_unsupported_construct")) e else rlang::cnd_signal(e)
                    })
    branch <- vec && inherits(env, c("sasonr_unsupported", "sasonr_step_exception"))
    if (!branch && !is.environment(env)) next
    if (branch || (vec && (rt$err || isTRUE(rt$mv_rerun)))) {
      # A data error in vectorised code: re-run the step row by row (L3) so
      # that _ERROR_, the NOTEs and the PDV dump follow the row-by-row execution.
      rt$prx <- prx0
      tr <- translate_step(step, eng$session$meta, "L3")
      tr$reasons <- c(if (branch) "unsupported value or step exception in vectorised code (re-run row by row)" else
        "data error at run time (re-run of the vectorised step)", tr$reasons)
      eng$steps[[length(eng$steps)]] <- tr
      eng$script[[node]]$tr <- tr
      eng$script[[node]]$rerun <- TRUE
      env <- tryCatch(engine_eval(eng, tr, stage = FALSE),
                      sasonr_step_exception = function(e) engine_step_exception(eng, step, tr, e))
      if (!is.environment(env)) next
    } else if (vec) {
      for (nm in tr$outputs) assign(nm, env$work[[nm]], envir = eng$session$work)
      # NOTEs that are not data errors (e.g. INPUT going to a new line)
      if (length(rt$notes) > 0 && isTRUE(eng$session$options$notes)) log_buffer_add(env, rt$notes)
    }
    if (length(sc_empty)) {
      # PROC TRANSPOSE in syntax-check mode (engine_check_step()): run on its
      # input with 0 observations
      for (nm in tr$outputs) assign(nm, env$work[[nm]], envir = eng$session$work)
    } else if (isTRUE(eng$syntax_check)) {
      # PROC CONTENTS in syntax-check mode (engine_check_step()): OUT= has 0
      # observations
      for (nm in tr$outputs) assign(nm, get(nm, envir = eng$session$work)[0, , drop = FALSE], envir = eng$session$work)
    }
    stopped <- step$type == "data" && isTRUE(rt$stop_step)
    if (stopped) {
      # An execution-time ERROR that stops the step (SYMGET): the step ends
      # after the current observation and keeps what was written ("WARNING: The
      # data set ... may be incomplete").  Only a one-observation step is
      # supported; a longer step would have to stop part-way through.
      big <- vapply(tr$outputs, function(nm) NROW(get0(nm, envir = eng$session$work, inherits = FALSE)) > 1L, logical(1))
      if (any(big)) abort_unsupported("an execution-time ERROR that stops a DATA step with more than one observation", step$line)
      log_buffer_add(env, log_render("step.stopped"))
    }
    if (step$type == "data") engine_store_numlen(eng, tr$outputs)
    # PROC PRINT with an ODS OUTPUT request pending: the table is not created
    if (step$type == "proc" && step$name == "print" && length(eng$session$ods_output) > 0) {
      rt$ods_missing <- eng$session$ods_output[[1]]$object
    }
    log_step_close(eng, if (step$type == "data") "data" else "proc", tr = tr, env = env, stopped = stopped,
                   notes = isTRUE(eng$session$options$notes) && eng$step_notes)
    engine_add_listing(eng, env$.lst, step$line)
    # the ODS OUTPUT requests end with the PROC step (no PERSIST=).  A step
    # that wrote one ODS OUTPUT data set and no other data set makes it _LAST_.
    if (length(rt$ods_notes) == 1L && length(tr$outputs) == 0L) {
      session_set_last(eng$session, tolower(rt$ods_notes[[1]]$name))
    } else if (length(rt$ods_notes) > 0L && length(tr$outputs) == 0L) {
      eng$session$last_ods <- TRUE
    }
    if (step$type == "proc") eng$session$ods_output <- NULL
    eng$ods <- c(eng$ods, env$.ods)
    if (length(env$.ods) > 0 && isTRUE(eng$session$ods_html)) {
      eng$html <- c(eng$html, list(list(proc = step$name, titles = env$.titles, footnotes = eng$session$footnotes,
                                          tables = env$.ods)))
    }
    if (identical(eng$on_error, "sas")) err_set_macro(eng, syserr = if (isTRUE(eng$syntax_check)) 3 else 0)
    # output data sets whose layout is only known at run time (PROC TRANSPOSE...)
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
  }
  # what follows the chunk (%PUT, global statements) is open code
  if (!isTRUE(eng$in_sql_step)) log_open_code(eng)
  invisible()
}

# A PROC step that reads `_LAST_` when `_LAST_` is a PROC SQL view: sasonr
# reads views only in PROC SQL, so it fails loud.  Which of several ODS OUTPUT
# data sets of one step becomes `_LAST_` is not modelled: reading `_LAST_` after
# such a step fails loud.
engine_check_last_view <- function(eng, step) {
  if (isTRUE(eng$session$last_ods) && is.null(step$options$data) &&
      step$name %in% c("sort", "print", "means", "summary", "freq", "transpose", "contents")) {
    abort_unsupported(paste0("PROC ", toupper(step$name), " reading _LAST_ after a step that wrote ODS OUTPUT data sets"), step$line)
  }
  if (isTRUE(eng$session$last_view) && is.null(step$options$data) &&
      step$name %in% c("sort", "print", "means", "summary", "freq", "transpose", "contents")) {
    abort_unsupported(paste0("PROC ", toupper(step$name), " reading _LAST_, the PROC SQL view WORK.",
                             toupper(eng$session$last)), step$line)
  }
}

# Syntax-check mode (after an ERROR, on_error = "sas"): the step is compiled
# but not run.  sasonr models that mode as follows:
#   * the global statements of a step run: TITLE in any PROC step, OPTIONS in
#     PROC DATASETS;
#   * a DATA step creates each new output data set with 0 observations and
#     the variables compiled; an existing one is not replaced ("not replaced
#     because this step was stopped"); _LAST_ names the new data set;
#   * PROC CONTENTS runs: its listing as usual, OUT= (new or existing) with 0
#     observations; it is run by the caller (TRUE is returned);
#   * PROC TRANSPOSE DATA= OUT= with only a VAR statement and a new OUT= data
#     set runs on its input with 0 observations (its OUT= has one observation
#     per VAR variable); run by the caller (TRUE is returned);
#   * PROC SORT OUT=, PROC FREQ TABLES OUT= and PROC APPEND BASE= create a new
#     data set with 0 observations and 0 variables, which becomes _LAST_; an
#     existing OUT= data set is "not replaced because new file is incomplete",
#     an existing BASE= is left alone;
#   * PROC MEANS (printing or not) stops: "stopped processing this step", an
#     OUTPUT OUT= data set as in a stopped step (new: 0 observations and 0
#     variables, "may be incomplete", it becomes _LAST_; existing: also "not
#     replaced because this step was stopped"), SYSERR 1012;
#   * PROC PRINT, printing PROC FREQ, PROC DELETE, PROC DATASETS DELETE / KILL
#     and an in-place PROC SORT change nothing, also not _LAST_;
# SYSERR is 3 after every step but PROC MEANS.  Other steps and forms (other
# forms of PROC TRANSPOSE, PROC FORMAT, PROC SUMMARY, several OUT= data sets,
# FREQ OUTPUT, data set options on OUT=, other DATASETS statements ...) fail
# loud.  PROC SQL: engine_exec_steps().
# Returns TRUE when the caller runs the step (PROC CONTENTS), FALSE otherwise.
engine_check_step <- function(eng, step) {
  unsupported <- function(what) {
    abort_unsupported(paste0(what, " in syntax-check mode (after an ERROR; not supported)"), step$line)
  }
  if (step$type == "proc") {
    if (step$name == "contents") {
      return(TRUE)
    }
    if (step$name == "transpose") {
      o <- step$options
      din <- o$data$name %||% ""
      ok <- setequal(names(o), c("data", "out")) && !length(o$data$options) && !length(o$out$options) &&
        nzchar(din) && exists(din, envir = eng$session$work, inherits = FALSE) &&
        !exists(o$out$name, envir = eng$session$work, inherits = FALSE) && !length(step$titles) &&
        all(vapply(step$stmts, function(x) identical(x$stmt, "var"), logical(1)))
      if (!ok) unsupported("this PROC TRANSPOSE (supported: DATA=, a new OUT= data set and a VAR statement)")
      eng$sc_empty <- din
      return(TRUE)
    }
    for (t in step$titles) engine_global(eng, t)
    log_boundary(eng)
    exists_ds <- function(nm) exists(nm, envir = eng$session$work, inherits = FALSE)
    notes_on <- isTRUE(eng$session$options$notes) && eng$step_notes
    # an output data set: a new one gets 0 observations and 0 variables and
    # becomes _LAST_, an existing one is kept; `stopped`: as in a stopped step
    out_ds <- function(nm, stopped = FALSE) {
      if (stopped) {
        log_emit(eng, id = "ds.incomplete", name = toupper(nm), nobs = 0L, nvar = 0L)
      } else if (notes_on) {
        log_emit(eng, id = "ds.created", name = toupper(nm), nobs = 0L, nvar = 0L)
      }
      if (exists_ds(nm)) {
        log_emit(eng, id = if (stopped) "ds.not_replaced_stopped" else "ds.not_replaced_incomplete", name = toupper(nm))
        return(invisible())
      }
      engine_empty_dataset(eng, nm, list(vars = character(), type = list(), len = list(), formats = list(), disp = list()))
      session_set_last(eng$session, nm)
    }
    syserr <- 3
    if (step$name == "datasets") {
      if (identical(step$origin, "append")) {
        s <- step$stmts[[1]]
        if (length(s$base$options) || length(s$data$options) || !exists_ds(s$data$name)) unsupported("this PROC APPEND")
        if (!exists_ds(s$base$name)) out_ds(s$base$name)
      } else if (!identical(step$origin, "delete")) {
        for (s in step$stmts) {
          if (identical(s$type, "global")) {
            engine_global(eng, s)
          } else if (!s$stmt %in% c("delete", "run")) {
            unsupported(paste("PROC DATASETS statement", toupper(s$stmt)))
          }
        }
        bad <- setdiff(names(step$options), c("nolist", "noprint", "kill", "nowarn", "nodetails", "details", "force"))
        if (length(bad)) unsupported(paste("PROC DATASETS option", toupper(bad[1])))
      }
    } else {
      tr <- translate_proc(step, eng$session$meta, eng$session$last)
      outs <- tr$outputs
      if (length(outs) > 1L) unsupported(paste0("PROC ", toupper(step$name), " with several output data sets"))
      if (step$name == "sort") {
        # in place: nothing happens
        if (!is.null(step$options$out)) {
          if (length(step$options$out$options)) unsupported("PROC SORT with data set options on OUT=")
          out_ds(outs)
        }
      } else if (step$name == "freq") {
        if (any(vapply(step$stmts, function(x) identical(x$stmt, "output"), logical(1)))) unsupported("PROC FREQ OUTPUT")
        if (any(vapply(step$stmts, function(x) length(x$opts$out$options) > 0, logical(1)))) {
          unsupported("PROC FREQ with data set options on OUT=")
        }
        if (length(outs)) out_ds(outs)
      } else if (step$name == "means") {
        if (any(vapply(step$stmts, function(x) identical(x$stmt, "output") && length(x$out$options) > 0, logical(1)))) {
          unsupported("PROC MEANS with data set options on OUT=")
        }
        log_emit(eng, id = "step.stopped")
        if (length(outs)) out_ds(outs, stopped = TRUE)
        syserr <- 1012
      } else if (step$name != "print") {
        unsupported(paste0("PROC ", toupper(step$name)))
      }
    }
    err_set_macro(eng, syserr = syserr)
    return(FALSE)
  }
  tr <- translate_step(step, eng$session$meta, eng$level)
  log_boundary(eng)
  old <- vapply(tr$outputs, function(nm) exists(nm, envir = eng$session$work, inherits = FALSE), logical(1))
  if (any(old)) {
    if (length(tr$outputs) > 1L) unsupported("a DATA step with several output data sets, one of them existing,")
  }
  for (nm in tr$outputs) {
    m <- tr$meta[[nm]]
    vars <- m$vars %||% character()
    if (isTRUE(eng$session$options$notes) && eng$step_notes) {
      log_emit(eng, id = "ds.created", name = toupper(nm), nobs = 0L, nvar = length(vars))
    }
    if (old[[nm]]) {
      log_emit(eng, id = "ds.not_replaced_stopped", name = toupper(nm))
    } else {
      engine_empty_dataset(eng, nm, m)
    }
  }
  # _LAST_: the new output data set; an existing one leaves it
  if (length(tr$outputs) && !any(old)) session_set_last(eng$session, tr$outputs[length(tr$outputs)])
  err_set_macro(eng, syserr = 3)
  FALSE
}

# Numeric variables stored with LENGTH < 8 keep only the first bytes of the
# IEEE 754 value (the mantissa is truncated when the observation is written).
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
engine_eval <- function(eng, tr, stage = FALSE, empty = character()) {
  rt <- eng$session$rt
  rt_reset(rt)
  rt$ods_output <- eng$session$ods_output %||% list()
  env <- new.env(parent = asNamespace("sasonr"))
  # `empty`: data sets the step reads with 0 observations (a staged copy)
  if (stage || length(empty)) {
    env$work <- new.env(parent = emptyenv())
    for (nm in ls(eng$session$work, all.names = TRUE)) {
      d <- get(nm, envir = eng$session$work)
      assign(nm, if (nm %in% empty) d[0, , drop = FALSE] else d, envir = env$work)
    }
  } else {
    env$work <- eng$session$work
  }
  log_buffer_init(env)
  env$.lst <- character()
  env$.ods <- list()
  env$.meta <- list()
  env$.symtab <- eng$session$macro
  env$.titles <- eng$session$titles
  # a DATA step runs its language objects (its text is for display only);
  # PROC code is still built as text
  run <- if (!is.null(tr$exprs)) cg_exec(tr$exprs) else as.expression(rlang::parse_exprs(paste(tr$code, collapse = "\n")))
  tryCatch(eval(run, envir = env), error = function(e) {
    # surface a sasonr condition wrapped by dplyr (e.g. raised inside mutate())
    p <- e
    while (!is.null(p) && !inherits(p, "sasonr_error")) p <- p$parent
    if (!is.null(p)) rlang::cnd_signal(p)
    rlang::cnd_signal(e)
  })
  if (!stage && length(rt$notes) > 0) {
    log_buffer_add(env, rt$notes)
    rt$notes <- character()
  }
  env
}

engine_global <- function(eng, g) {
  if (g$kind == "filename") {
    eng$session$filerefs[[g$ref]] <- g$path
    # SYSFILRC: 0 when the file or directory exists, 1 when it does not, also
    # when its directory does not
    assign("sysfilrc", if (file.exists(g$path)) "0" else "1", envir = eng$session$macro$globals)
  }
  if (g$kind == "filename_noref") {
    q <- g$quote %||% abort_unsupported("FILENAME with a quoted name and no physical path", g$line)
    if (eng$step_notes) log_emit(eng, id = "filename.no_assign", quote = q, name = g$text)
    assign("sysfilrc", "-20004", envir = eng$session$macro$globals)
  }
  if (g$kind == "options") {
    o <- eng$session$options
    if (!is.null(g$ls)) {
      o$linesize <- g$ls
      log_source_ls(eng, g$end_line %||% g$line, g$ls)
    }
    # the positive and negative forms apply in the order written: the last one
    # wins
    for (opt in g$opts) {
      switch(opt,
        notes = o$notes <- TRUE, nonotes = o$notes <- FALSE,
        syntaxcheck = o$nosyntaxcheck <- FALSE, nosyntaxcheck = o$nosyntaxcheck <- TRUE,
        center = o$center <- TRUE, nocenter = o$center <- FALSE,
        date = o$date <- TRUE, nodate = o$date <- FALSE,
        number = o$number <- TRUE, nonumber = o$number <- FALSE,
        source = log_source_option(eng, g$end_line %||% g$line, TRUE),
        nosource = log_source_option(eng, g$end_line %||% g$line, FALSE)
      )
    }
    if (!is.null(g$ps)) o$pagesize <- g$ps
    if (!is.null(g$formdlim)) o$formdlim <- g$formdlim
  }
  if (g$kind == "ods_html") eng$session$ods_html <- g$open
  if (g$kind == "ods_output") eng$session$ods_output <- c(eng$session$ods_output, g$requests)
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

# The quote character of the quoted string of global statement `g` (its
# `col`) in the chunk `text` that starts at program line `line`: ' or ", or
# NULL when the position does not hold one.
engine_quote_at <- function(text, line, g) {
  src <- strsplit(text, "\n", fixed = TRUE)[[1]]
  k <- g$line - line + 1L
  if (is.null(g$col) || k < 1L || k > length(src)) return(NULL)
  q <- substr(src[k], g$col, g$col)
  if (q %in% c("'", "\"")) q else NULL
}

# An OPTIONS statement inside a step takes effect when it is compiled, so the
# macro code after it in the same step runs under the new options (OPTIONS
# LINESIZE=MAX and NONOTES inside PROC DATASETS before and after a %SYSFUNC
# call that writes a NOTE, and LINESIZE=MAX inside PROC SQL).  sasonr
# runs the statements of a step when the step is complete (PROC DATASETS is
# the step that accepts OPTIONS; PROC SQL runs its statements one by one), so
# the code sink hands the statement over
# at its semicolon and the macro processor keeps the options of the step's
# OPTIONS statements so far (`step_opts`: LINESIZE and NOTES, what a macro-time
# NOTE depends on, mac_sysfunc()); the session options change when the step
# runs, as before.  Only the supported settings are kept: a statement with
# another LINESIZE= value or NOTES, or one that does not parse here, leaves
# them unknown (a macro-time NOTE then fails loud); the step reports a parse
# error when it runs.
engine_step_options <- function(eng, text, line) {
  ses <- eng$session$macro
  if (isTRUE(ses$step_opts$unknown)) return(invisible())
  st <- tryCatch(sas_parse(paste0(strrep("\n", max(0L, line - 1L)), text), resolved = TRUE)$steps,
                 error = function(e) NULL)
  g <- if (length(st) == 1L) st[[1]] else NULL
  if (is.null(g) || !identical(g$type, "global") || !identical(g$kind, "options") ||
      (!is.null(g$ls) && g$ls != 256L) || "notes" %in% g$opts) {
    ses$step_opts <- list(unknown = TRUE, line = line)
    return(invisible())
  }
  o <- ses$step_opts %||% list(unknown = FALSE, linesize = eng$session$options$linesize,
                               notes = isTRUE(eng$session$options$notes))
  if (!is.null(g$ls)) o$linesize <- g$ls
  if ("nonotes" %in% g$opts) o$notes <- FALSE
  ses$step_opts <- o
  invisible()
}

# The code of a step that the engine runs itself (PROC SQL, PROC DATASETS):
# no R translation; evaluating it fails loud.
untranslated_code <- function(what, line) {
  paste0("# ", what, " (SAS line ", line, "): run by the sasonr engine, no R translation\n",
         "sas_untranslated(", r_lit(what), ", ", as.integer(line), "L)")
}

#' Stop: a step that has no R translation
#'
#' Translated code of a step that sasonr runs itself (PROC SQL, PROC
#' DATASETS) consists of this call: evaluating it outside [sas_run()] stops
#' with a condition of class `sasonr_unsupported`.
#' @param what The step ("PROC SQL CREATE", "PROC DATASETS").
#' @param line Its SAS line.
#' @export
#' @keywords internal
sas_untranslated <- function(what, line) {
  abort_unsupported(paste0(what, " has no R translation: sasonr runs it in sas_run()"), line)
}

engine_program <- function(eng, code) {
  text <- paste(code, collapse = "\n")
  log_program(eng, text)
  sink <- code_sink(eng$session$macro)
  mp_scan(mp_src(text, line_of = TRUE), eng$session$macro, sink, mode = "code")
  sink$finish()
  if (!isTRUE(eng$deferred)) script_open_code(eng, "", length(eng$program$lines) + 1L, end = TRUE)
  if (isTRUE(eng$sc_pending)) {
    abort_unsupported("the end of a program after an ERROR in a global statement (supported: a DATA step follows)")
  }
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
#'
#' The code of a step calls base R, `dplyr::`/`tibble::` functions and the
#' run-time helpers that sasonr exports (documented as internal). It is
#' evaluated by [sas_run()] in an environment that holds `work` (an
#' environment with the WORK data sets as data frames), `.log` and `.lst`
#' (character vectors the step appends its log and listing lines to), `.meta`
#' (the variable attributes the step records), `.symtab` (the macro symbol
#' table) and `.titles` (the current TITLE lines), with the session of the
#' run bound for the log NOTEs and `_ERROR_`. Steps that sasonr runs itself
#' (PROC SQL, PROC DATASETS) have no R translation: their code is a call of
#' `sas_untranslated()`, which stops with a condition of class
#' `sasonr_unsupported`.
#' @examples
#' tr <- sas_translate("data b; set a; y = round(x, 0.1); run;",
#'                     work = list(a = data.frame(x = c(1.25, 2.35))))
#' tr
#' @export
sas_translate <- function(code, work = list(), level = c("auto", "L3")) {
  level <- rlang::arg_match(level)
  session <- SasSession$new()
  session_add_work(session, work)
  on.exit(session$close(), add = TRUE)
  eng <- new_engine(session, level, execute = FALSE)
  # restore the binding first, then bind (an interrupt in between leaves nothing bound)
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  eng$dynamic <- script_dynamic(code)
  # in a program that can depend on run-time values, macro code that fails
  # without them ends the translation: the rest is deferred (script.R)
  tryCatch(engine_program(eng, code), sasonr_runtime_error = function(e) script_defer_rest(eng, e),
           sasonr_syntax_error = function(e) script_defer_rest(eng, e))
  structure(list(steps = eng$steps, script = script_raw(eng$script, "translation", tolower(names(work)), eng$program$lines)),
            class = "sasonr_translation")
}

#' Run a SAS program on the R engine
#'
#' The macro processor hands complete steps to the executor one at a time
#' (at RUN; / QUIT; / step boundaries), so CALL SYMPUT(X) results are visible to
#' the macro code that follows the step, including inside macro bodies.
#'
#' @inheritParams sas_translate
#' @param on_error What an ERROR of the program does (a syntax error, an unknown function,
#'   a missing input data set, an ERROR in PROC SQL ...). `"sas"` (the
#'   default; option `sasonr.on_error`) uses the batch-mode error handling: the
#'   failing step is not run, syntax-check mode (OBS=0) applies to the rest of
#'   the program (later steps are compiled but not run; global statements and
#'   the macro language still are), `SYSERR`/`SYSCC` are set and the result has
#'   `status = "error"`, the ERROR lines in `$log` and WORK as the failing step
#'   leaves it.
#'   `"abort"` stops with the classed R condition instead. Features sasonr does
#'   not support (`sasonr_unsupported`) always stop the run: they are not errors of
#'   the program.
#' @param session A session from [sas_session()]: the program runs as one more
#'   submission to it (WORK, macro variables, user formats, options and titles
#'   of earlier submissions are visible; `work` data sets are added to its
#'   WORK). `NULL` (the default) runs in a fresh session that is discarded
#'   afterwards.
#' @return A `sasonr_result`: `$log` (the log: the program lines with the
#'   NOTEs, WARNINGs, ERRORs and PUT output; see [sas_log()]), `$log_events`
#'   (a tibble of the log events), `$listing` (procedure output), `$ods` (the
#'   ODS tables of the procedure output in output order, each a
#'   `sas_ods_table`; PROC MEANS and SUMMARY), `$work` (output data sets with trailing blanks removed),
#'   `$translation`, `$macro_vars` (global macro variables), `$status`
#'   (`"ok"` or `"error"`), `$errors` (the ERROR messages) and `$error_line`
#'   (SAS line of the first ERROR). With a `session`, `$log`, `$log_events`, `$listing`,
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
  level <- rlang::arg_match(level)
  on_error <- rlang::arg_match(on_error, c("sas", "abort"))
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
  eng$inputs <- tolower(names(work))
  # bind the session for the generated code and the run-time helpers (the
  # restore is registered first: an interrupt in between leaves nothing bound)
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  # the approximate-features NOTE also ends a submission that an R error
  # stops (before its events go to the session)
  on.exit(log_approx_close(eng), add = TRUE)
  on.exit(engine_flush(eng), add = TRUE)
  if (on_error == "sas") {
    # step ERRORs are handled by engine_exec(); a condition that reaches this
    # level was raised by the macro processor, whose error handling
    # (ERROR text, stopping the macro, what %LET receives) is not implemented
    tryCatch(engine_program(eng, code),
             sasonr_syntax_error = function(e) macro_error_unsupported(e),
             sasonr_runtime_error = function(e) macro_error_unsupported(e))
  } else {
    engine_program(eng, code)
  }
  log_approx_close(eng)
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
# success).  Not called inside the package: it is kept for callers that use
# it as sasonr:::engine_run_partial().
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
# the listing in the layout of NOCENTER NODATE NONUMBER FORMDLIM=' '
# PAGESIZE=MAX.  Other values centre the lines (CENTER), add a header line
# with the date or page number (DATE, NUMBER), a delimiter line instead of
# each new page after the first (FORMDLIM=), or break a long output into
# pages with repeated headings (PAGESIZE=): output written under them fails
# loud.  An output of at most PAGESIZE - 3 lines fits on one page (margin for
# the title lines).
engine_add_listing <- function(eng, lst, line = NULL) {
  if (length(lst) == 0) return(invisible())
  o <- eng$session$options
  bad <- c(if (isTRUE(o$center)) "CENTER", if (isTRUE(o$date)) "DATE", if (isTRUE(o$number)) "NUMBER",
           if (!identical(o$formdlim, " ") && length(eng$session$listing) + length(eng$listing) > 0)
             "FORMDLIM= other than ' '",
           if (length(lst) + 3L > o$pagesize) paste0("PAGESIZE=", o$pagesize, " (output longer than one page)"))
  if (length(bad)) {
    abort_unsupported(paste0("procedure output under OPTIONS ", paste(bad, collapse = ", "),
                             " (only the NOCENTER NODATE NONUMBER FORMDLIM=' ' PAGESIZE=MAX listing is supported)"), line)
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
  log_flush(eng)
  s$listing <- c(s$listing, eng$listing)
  invisible()
}

# Automatic macro variables (not listed among the user's global macro variables).
macro_auto_vars <- mac_auto_implemented

# The sasonr_result of an engine.
engine_result <- function(eng) {
  out <- lapply(as.list(eng$session$work), function(d) {
    dplyr::mutate(d, dplyr::across(dplyr::where(is.character), sas_rtrim))
  })
  mv <- as.list(eng$session$macro$globals)
  autos <- macro_auto_vars
  mv <- mv[setdiff(sort(names(mv)), autos)]
  dsn <- sort(names(out) %||% character())
  structure(
    list(log = log_view_plain(ev <- log_result_events(eng), log_echo_default), log_events = ev,
         listing = eng$listing, ods = eng$ods, work = out[dsn], meta = eng$session$meta[intersect(dsn, names(eng$session$meta))],
         unordered = intersect(eng$session$unordered, dsn),
         translation = structure(list(steps = eng$steps, script = script_raw(eng$script, "trace", eng$inputs %||% character(), eng$program$lines)),
                                 class = "sasonr_translation"),
         macro_vars = mv, status = eng$status, errors = eng$errors %||% character(),
         error_line = eng$error_line %||% NA_integer_),
    # the procedure outputs of the HTML destination (sas_html())
    html_output = eng$html,
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
  # the coloured log with the source echo in a terminal with colours
  # (sas_log(format = "ansi")); options(sasonr.log_style = "plain") turns it off
  if (!identical(getOption("sasonr.log_style"), "plain") && cli::num_ansi_colors() > 1 && !is.null(x$log_events)) {
    cli::cli_h2("Log")
    cat(sas_log(x, "ansi"), sep = "\n")
  } else {
    cli::cli_h2("Log")
    if (length(x$log) > 0) cat(x$log, sep = "\n") else cli::cli_text("{.emph (empty)}")
  }
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
