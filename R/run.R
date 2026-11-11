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

# An engine: WORK library, metadata, titles, log and listing; `exec(text,
# line)` parses, translates and (optionally) runs one chunk of expanded text.
new_engine <- function(work, level, execute) {
  eng <- new.env(parent = emptyenv())
  inputs <- prepare_work(work)
  eng$meta <- Map(meta_from_df, inputs, lapply(work, names))
  names(eng$meta) <- names(inputs)
  eng$wenv <- new.env(parent = emptyenv())
  for (nm in names(inputs)) assign(nm, inputs[[nm]], envir = eng$wenv)
  eng$level <- level
  eng$execute <- execute
  eng$steps <- list()
  eng$log <- character()
  eng$listing <- character()
  eng$titles <- character()
  eng$last <- NULL
  eng$unordered <- character()
  eng$views <- list()
  eng$sql_open <- FALSE
  eng$sql_opts <- list()
  eng$ses <- new_macro_session(exec = function(text, line) engine_exec(eng, text, line))
  rt$catalog <- new.env(parent = emptyenv())
  rt$filerefs <- list()
  rt$prx <- list()
  rt$prx_cache <- list()
  rt_reset()
  rt$notes_on <- TRUE
  # step-level NOTEs (data set sizes, observations read, missing values...)
  # and macro WARNINGs in the log; options(sasonr.step_notes = FALSE) turns them off
  eng$step_notes <- isTRUE(getOption("sasonr.step_notes", TRUE))
  rt$on_warn <- if (eng$step_notes) function(msg) eng$log <- c(eng$log, paste("WARNING:", msg))
  eng$ses$on_put <- function(line) eng$log <- c(eng$log, line)
  eng
}

engine_exec <- function(eng, text, line) {
  prog <- sas_parse(paste0(strrep("\n", max(0L, line - 1L)), text), resolved = TRUE, sql_open = eng$sql_open)
  eng$sql_open <- isTRUE(attr(prog, "sql_open"))
  for (step in prog$steps) {
    if (step$type == "global") {
      engine_global(eng, step)
      next
    }
    if (step$type == "sql_begin") {
      eng$sql_opts <- step$options
      next
    }
    if (step$type == "sql_end") next
    if (step$type == "sql") {
      eng$steps <- c(eng$steps, list(list(code = paste0("# PROC SQL ", toupper(step$stmt$kind), " (SAS line ", step$line,
                                                            "): executed by the SQL engine"),
                                          level = "SQL", reasons = "PROC SQL", line = step$line, outputs = character(), meta = list())))
      if (eng$execute) engine_sql(eng, step$stmt)
      next
    }
    if (step$type == "proc") {
      for (t in step$titles) engine_global(eng, t)
      tr <- translate_proc(step, eng$meta, eng$last)
    } else {
      tr <- translate_step(step, eng$meta, eng$level)
    }
    eng$meta[names(tr$meta)] <- tr$meta
    eng$unordered <- setdiff(eng$unordered, tr$outputs)
    if (length(tr$outputs) > 0) eng$last <- tr$outputs[length(tr$outputs)]
    eng$steps <- c(eng$steps, list(tr))
    if (!eng$execute) next
    vec <- step$type == "data" && tr$level %in% c("L1", "L2")
    env <- engine_eval(eng, tr$code, stage = vec)
    if (vec && (rt$err || isTRUE(rt$mv_rerun))) {
      # A data error in vectorised code: re-run the step row by row (L3) so
      # that _ERROR_, the NOTEs and the PDV dump follow SAS exactly.
      tr <- translate_step(step, eng$meta, "L3")
      tr$reasons <- c("data error at run time (re-run of the vectorised step)", tr$reasons)
      eng$steps[[length(eng$steps)]] <- tr
      env <- engine_eval(eng, tr$code, stage = FALSE)
    } else if (vec) {
      for (nm in tr$outputs) assign(nm, env$work[[nm]], envir = eng$wenv)
      # NOTEs that are not data errors (e.g. INPUT going to a new line)
      if (length(rt$notes) > 0 && isTRUE(rt$notes_on)) env$.log <- c(env$.log, rt$notes)
    }
    notes_on <- isTRUE(rt$notes_on) && eng$step_notes
    eng$log <- c(eng$log, if (notes_on) tr$compile_notes, env$.log)
    if (notes_on) {
      ds_notes <- unlist(lapply(tr$outputs, function(nm) {
        d <- get0(nm, envir = eng$wenv, inherits = FALSE)
        if (is.null(d)) NULL else dataset_note(nm, d)
      }))
      # PROC SORT of an empty data set reports no observations read
      if (identical(tr$reasons, "PROC SORT")) rt$reads <- Filter(function(r) r$n > 0, rt$reads)
      reads <- if (identical(tr$reasons, "PROC PRINT")) print_read_notes() else read_notes()
      eng$log <- c(eng$log, step_end_notes(), reads, rt$proc_notes, ds_notes)
    }
    eng$listing <- c(eng$listing, env$.lst)
    # output data sets whose layout is only known at run time (PROC TRANSPOSE...)
    if (length(env$.meta) > 0) eng$meta[names(env$.meta)] <- env$.meta
    if (length(env$.meta) > 0) eng$meta[names(env$.meta)] <- env$.meta
  }
  invisible()
}

# Evaluate generated code; with `stage = TRUE` output data sets go to a scratch
# copy of WORK (the caller commits them) and run-time NOTEs are discarded.
engine_eval <- function(eng, code, stage = FALSE) {
  rt_reset()
  env <- new.env(parent = asNamespace("sasonr"))
  if (stage) {
    env$work <- new.env(parent = emptyenv())
    for (nm in ls(eng$wenv, all.names = TRUE)) assign(nm, get(nm, envir = eng$wenv), envir = env$work)
  } else {
    env$work <- eng$wenv
  }
  env$.log <- character()
  env$.lst <- character()
  env$.meta <- list()
  env$.symtab <- eng$ses
  env$.titles <- eng$titles
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
  if (g$kind == "filename") rt$filerefs[[g$ref]] <- g$path
  if (g$kind == "options") {
    if ("nonotes" %in% g$opts) rt$notes_on <- FALSE
    if ("notes" %in% g$opts) rt$notes_on <- TRUE
  }
  if (g$kind == "title") {
    keep <- eng$titles[seq_len(min(length(eng$titles), g$level - 1L))]
    if (length(keep) < g$level - 1L) keep <- c(keep, rep("", g$level - 1L - length(keep)))
    eng$titles <- if (nzchar(g$text)) c(keep, g$text) else keep
    while (length(eng$titles) > 0 && !nzchar(eng$titles[length(eng$titles)])) {
      eng$titles <- eng$titles[-length(eng$titles)]
    }
  }
  invisible()
}

engine_program <- function(eng, code) {
  text <- paste(code, collapse = "\n")
  sink <- code_sink(eng$ses)
  mp_scan(mp_src(text, line_of = TRUE), eng$ses, sink, mode = "code")
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
#' @export
sas_translate <- function(code, work = list(), level = c("auto", "L3")) {
  level <- match.arg(level)
  eng <- new_engine(work, level, execute = FALSE)
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
#' @return A `sasonr_result`: `$log` (PUT and %PUT lines), `$listing` (procedure
#'   output), `$work` (output data sets with trailing blanks removed),
#'   `$translation`, `$macro_vars` (global macro variables).
#' @export
sas_run <- function(code, work = list(), level = c("auto", "L3")) {
  level <- match.arg(level)
  eng <- new_engine(work, level, execute = TRUE)
  on.exit(sql_duck_close(eng), add = TRUE)
  engine_program(eng, code)
  out <- lapply(as.list(eng$wenv), function(d) {
    dplyr::mutate(d, dplyr::across(dplyr::where(is.character), sas_rtrim))
  })
  eng$log <- unlist(strsplit(eng$log, "\n", fixed = TRUE)) %||% character()
  mv <- as.list(eng$ses$globals)
  autos <- c("sysdate", "sysdate9", "sysday", "systime", "sysver", "sysvlong", "sysscp", "sysncpu",
             "sysscpl", "syserr", "syscc", "sysrc", "sysmacroname", "sysindex")
  mv <- mv[setdiff(sort(names(mv)), autos)]
  dsn <- sort(names(out) %||% character())
  structure(
    list(log = eng$log, listing = eng$listing, work = out[dsn], meta = eng$meta[intersect(dsn, names(eng$meta))],
         unordered = intersect(eng$unordered, dsn),
         translation = structure(list(steps = eng$steps), class = "sasonr_translation"),
         macro_vars = mv),
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
