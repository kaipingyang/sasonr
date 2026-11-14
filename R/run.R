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
  ses$on_step_options <- function(text, line) engine_step_options(eng, text, line)
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
        eng$log <- c(eng$log, "NOTE: Due to ERROR(s) above, SAS set option OBS=0, enabling syntax check mode.",
                     "      This prevents execution of subsequent data modification statements.")
        eng$syntax_check <- TRUE
      }
    }
    if (isTRUE(eng$syntax_check) && isTRUE(eng$sql_noexec) && step$type %in% c("data", "proc")) {
      abort_unsupported("a PROC SQL step in syntax-check mode that ends without QUIT (not supported)", step$line)
    }
    if (step$type == "global") {
      if (identical(step$kind, "filename_noref")) step$quote <- engine_quote_at(text, line, step)
      engine_global(eng, step)
      next
    }
    if (step$type == "sql_begin") {
      eng$sql_opts <- step$options
      if (isTRUE(eng$syntax_check)) {
        # PROC SQL in syntax-check mode runs as with NOEXEC: no statement runs,
        # SQLOBS and SQLRC are 0, the step ends "stopped" with SYSERR 1012
        #
        if (sas_target_version() != "SAS 9.4") {
          abort_unsupported(paste0("PROC SQL in syntax-check mode for SAS 9.4 ", sas_target_version(), " (not supported for this release)"), step$line)
        }
        eng$log <- c(eng$log, "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.")
        eng$sql_noexec <- TRUE
      }
      next
    }
    if (step$type == "sql_end") {
      if (isTRUE(eng$sql_noexec)) {
        eng$log <- c(eng$log, "NOTE: The SAS System stopped processing this step because of errors.")
        eng$sql_noexec <- FALSE
        if (isTRUE(eng$syntax_check)) err_set_macro(eng, syserr = 1012)
      }
      next
    }
    if (step$type == "sql") {
      if (isTRUE(eng$sql_noexec) || isTRUE(eng$syntax_check)) {
        if (isTRUE(eng$sql_noexec)) eng$log <- c(eng$log, "NOTE: Statement not executed due to NOEXEC option.")
        if (isTRUE(eng$syntax_check)) sql_set_status(eng, 0, 0)
        next
      }
      eng$steps <- c(eng$steps, list(list(code = paste0("# PROC SQL ", toupper(step$stmt$kind), " (SAS line ", step$line,
                                                            "): executed by the SQL engine"),
                                          level = "SQL", reasons = "PROC SQL", line = step$line, outputs = character(), meta = list())))
      eng$phase <- "sql"
      if (eng$execute) engine_sql(eng, step$stmt)
      next
    }
    # syntax-check mode: PROC CONTENTS runs on (OUT= with 0 observations below)
    if (isTRUE(eng$syntax_check) && !engine_check_step(eng, step)) next
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
      engine_check_last_view(eng, step)
      tr <- translate_proc(step, eng$session$meta, eng$session$last)
    } else {
      tr <- translate_step(step, eng$session$meta, eng$level)
    }
    eng$session$meta[names(tr$meta)] <- tr$meta
    eng$session$unordered <- setdiff(eng$session$unordered, tr$outputs)
    if (length(tr$outputs) > 0) session_set_last(eng$session, tr$outputs[length(tr$outputs)])
    eng$steps <- c(eng$steps, list(tr))
    if (!eng$execute) next
    vec <- step$type == "data" && tr$level %in% c("L1", "L2")
    eng$phase <- if (step$type == "data") "execute_data" else "execute"
    # run-wide state the vectorised attempt may change before a re-run (PRXPARSE ids)
    prx0 <- rt$prx
    # Vectorised code evaluates every IF/SELECT branch on every row, so a branch
    # that never runs can fail loud or raise a SAS exception: the step is re-run
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
      # that _ERROR_, the NOTEs and the PDV dump follow the row-by-row execution.
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
    if (isTRUE(eng$syntax_check)) {
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
    if (identical(eng$on_error, "sas")) err_set_macro(eng, syserr = if (isTRUE(eng$syntax_check)) 3 else 0)
    # output data sets whose layout is only known at run time (PROC TRANSPOSE...)
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
    if (length(env$.meta) > 0) eng$session$meta[names(env$.meta)] <- env$.meta
  }
  invisible()
}

# A PROC step that reads `_LAST_` when `_LAST_` is a PROC SQL view: sasonr
# reads views only in PROC SQL, so it fails loud.
engine_check_last_view <- function(eng, step) {
  if (isTRUE(eng$session$last_view) && is.null(step$options$data) &&
      step$name %in% c("sort", "print", "means", "summary", "freq", "transpose", "contents")) {
    abort_unsupported(paste0("PROC ", toupper(step$name), " reading _LAST_, the PROC SQL view WORK.",
                             toupper(eng$session$last)), step$line)
  }
}

# Syntax-check mode (after an ERROR, on_error = "sas"): the step is compiled
#
#   * the global statements of a step run: TITLE in any PROC step, OPTIONS in
#     PROC DATASETS;
#   * a DATA step creates each new output data set with 0 observations and
#     the variables compiled; an existing one is not replaced ("not replaced
#     because this step was stopped"); _LAST_ names the new data set;
#   * PROC CONTENTS runs: its listing as usual, OUT= (new or existing) with 0
#     observations; it is run by the caller (TRUE is returned);
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
# SYSERR is 3 after every step but PROC MEANS.  Other steps and forms (PROC
# TRANSPOSE, which would run with 0 observations, PROC FORMAT, PROC
# SUMMARY, several OUT= data sets, FREQ OUTPUT, data set options on OUT=,
# other DATASETS statements ...) fail loud, and so does anything these rules
#
# Returns TRUE when the caller runs the step (PROC CONTENTS), FALSE otherwise.
engine_check_step <- function(eng, step) {
  unsupported <- function(what) {
    abort_unsupported(paste0(what, " in syntax-check mode (after an ERROR; not supported)"), step$line)
  }
  SAS 9.4 <- function() if (sas_target_version() != "SAS 9.4") unsupported(paste("SAS 9.4", sas_target_version()))
  if (step$type == "proc") {
    if (step$name == "contents") {
      SAS 9.4()
      return(TRUE)
    }
    if (length(step$titles)) SAS 9.4()
    for (t in step$titles) engine_global(eng, t)
    exists_ds <- function(nm) exists(nm, envir = eng$session$work, inherits = FALSE)
    notes_on <- isTRUE(eng$session$options$notes) && eng$step_notes
    # an output data set: a new one gets 0 observations and 0 variables and
    # becomes _LAST_, an existing one is kept; `stopped`: as in a stopped step
    out_ds <- function(nm, stopped = FALSE) {
      SAS 9.4()
      if (stopped) {
        eng$log <- c(eng$log, sprintf(
          "WARNING: The data set WORK.%s may be incomplete.  When this step was stopped there were 0 observations and 0 variables.",
          toupper(nm)))
      } else if (notes_on) {
        eng$log <- c(eng$log, sprintf("NOTE: The data set WORK.%s has 0 observations and 0 variables.", toupper(nm)))
      }
      if (exists_ds(nm)) {
        eng$log <- c(eng$log, sprintf("WARNING: Data set WORK.%s was not replaced because %s.", toupper(nm),
                                      if (stopped) "this step was stopped" else "new file is incomplete"))
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
            SAS 9.4()
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
        SAS 9.4()
        eng$log <- c(eng$log, "NOTE: The SAS System stopped processing this step because of errors.")
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
  old <- vapply(tr$outputs, function(nm) exists(nm, envir = eng$session$work, inherits = FALSE), logical(1))
  if (any(old)) {
    SAS 9.4()
    if (length(tr$outputs) > 1L) unsupported("a DATA step with several output data sets, one of them existing,")
  }
  for (nm in tr$outputs) {
    m <- tr$meta[[nm]]
    vars <- m$vars %||% character()
    if (isTRUE(eng$session$options$notes) && eng$step_notes) {
      eng$log <- c(eng$log, sprintf("NOTE: The data set WORK.%s has 0 observations and %d variables.", toupper(nm), length(vars)))
    }
    if (old[[nm]]) {
      eng$log <- c(eng$log, sprintf("WARNING: Data set WORK.%s was not replaced because this step was stopped.", toupper(nm)))
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
  if (g$kind == "filename") {
    eng$session$filerefs[[g$ref]] <- g$path
    # SYSFILRC: 0 when the file or directory exists, 1 when it does not, also
    # when its directory does not
    assign("sysfilrc", if (file.exists(g$path)) "0" else "1", envir = eng$session$macro$globals)
  }
  if (g$kind == "filename_noref") {
    q <- g$quote %||% abort_unsupported("FILENAME with a quoted name and no physical path", g$line)
    if (eng$step_notes) eng$log <- c(eng$log, paste0("WARNING: No logical assign for filename ", q, g$text, q, "."))
    assign("sysfilrc", "-20004", envir = eng$session$macro$globals)
  }
  if (g$kind == "options") {
    o <- eng$session$options
    if (!is.null(g$ls)) o$linesize <- g$ls
    # the positive and negative forms apply in the order written: the last one
    # wins
    for (opt in g$opts) {
      switch(opt,
        notes = o$notes <- TRUE, nonotes = o$notes <- FALSE,
        syntaxcheck = o$nosyntaxcheck <- FALSE, nosyntaxcheck = o$nosyntaxcheck <- TRUE,
        center = o$center <- TRUE, nocenter = o$center <- FALSE,
        date = o$date <- TRUE, nodate = o$date <- FALSE,
        number = o$number <- TRUE, nonumber = o$number <- FALSE
      )
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

engine_program <- function(eng, code) {
  text <- paste(code, collapse = "\n")
  sink <- code_sink(eng$session$macro)
  mp_scan(mp_src(text, line_of = TRUE), eng$session$macro, sink, mode = "code")
  sink$finish()
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
#'   default; option `sasonr.on_error`) uses the batch-mode error handling: the
#'   failing step is not run, syntax-check mode (OBS=0) applies to the rest of
#'   the program (later steps are compiled but not run; global statements and
#'   the macro language still are), `SYSERR`/`SYSCC` are set and the result has
#'   `status = "error"`, the ERROR lines in `$log` and WORK as the failing step
#'   leaves it.
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
  s$log <- c(s$log, unlist(strsplit(eng$log, "\n", fixed = TRUE)))
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
