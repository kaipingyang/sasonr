# The SAS session: every piece of mutable state a SAS program can change.
#
# A `SasSession` (R6) owns what SAS keeps between steps and between
# submissions: the WORK library and its metadata, the user format catalog,
# the macro symbol tables and macro definitions, system options, titles,
# filerefs, `_LAST_`, the SQL back-end connection, the cumulative log and
# listing, and the diagnostic state of the step being compiled (`ct`) or run
# (`rt`).  `sas_run()` drives one *submission* against a session; the
# per-submission bookkeeping (steps translated, phase, syntax-check mode,
# ERRORs of this submission) lives in the engine environment of that call
# (run.R), not in the session.
#
# Generated step code does not receive the session as an argument: the engine
# binds the active session for the duration of a submission
# (`sas_rt_active(session)`, restored with `on.exit()`), and the run-time
# helpers read it through `sas_rt_active()` / `rt_diag()` / `ct_diag()`.
# The only package-level mutable object is that binding slot (`.active`): it
# holds a reference to the bound session, never run state of its own.  There
# is no default session (spec 04 §4.7): a function-layer call outside any
# submission (`sas_f_round()` in the user's own code) runs without hidden
# state - its diagnostics go to a throw-away state (rt_diag(), ct_diag()), it
# sees no user formats and no filerefs (a fileref fails loud), uses the
# default LINESIZE (or its `.ls` argument), and PRXPARSE ids need an explicit
# `session =` (sas_f_prxparse()).
#
# Package-level objects that are *not* run state (built at load time or
# read-once caches, never written by a run): the registries
# (`registry_cache`, registry.R), the SASHELP store (`sashelp_cache`,
# sashelp.R), function/format tables (`fn_table`, `fn_policy`,
# `sysfunc_table`, `.pnt_gl` ...).  R options read at the start of a session
# (`sasonr.sas_version`) or per submission (`sasonr.step_notes`) or at use
# (`sasonr.strict`, `sasonr.on_error`, `sasonr.sashelp_dir`) are user policy,
# not SAS session state.

SasSession <- R6::R6Class("SasSession",
  cloneable = FALSE,
  public = list(
    # work: WORK library: an environment, member name (lower case) -> tibble.
    work = NULL,
    # meta: WORK metadata: member -> list(vars, type, len, formats, disp, ...).
    meta = NULL,
    # unordered: WORK members whose row order SAS does not define (PROC SQL without ORDER BY).
    unordered = NULL,
    # views: PROC SQL views: name -> query (re-run on each reference).
    views = NULL,
    # last: `_LAST_`: the most recently created data set.
    last = NULL,
    # titles: TITLE1-TITLE10 text.
    titles = NULL,
    # footnotes: FOOTNOTE1-FOOTNOTE10 text (FOOTNOTE statements still fail loud).
    footnotes = NULL,
    # formats: User format catalog (WORK.FORMATS): environment, key -> format definition.
    formats = NULL,
    # filerefs: FILENAME assignments: fileref -> path.
    filerefs = NULL,
    # macro: Macro symbol tables (global table, local frames) and macro definitions.
    macro = NULL,
    # options: System options (environment): linesize, notes, nosyntaxcheck, the listing
    #   layout (center, date, number, pagesize, formdlim) and sas_version.
    options = NULL,
    # sql: SQL back end: environment with the lazily opened DuckDB connection.
    sql = NULL,
    # log: Cumulative SAS log of all submissions.
    log = NULL,
    # listing: Cumulative procedure output of all submissions.
    listing = NULL,
    # rt: Run-time diagnostics of the step being executed (data errors, NOTE counters,
    #   observations read, PRXPARSE ids) and the log callbacks of the current submission.
    rt = NULL,
    # ct: Compile-time diagnostics of the step being translated.
    ct = NULL,
    # busy: TRUE while a submission runs (a nested submission to the same session is refused).
    busy = FALSE,

    # A new session with an empty WORK library and SAS default options.
    # `sas_version`: The SAS 9.4 release the session targets.
    initialize = function(sas_version = getOption("sasonr.sas_version", "TS1M7")) {
      self$work <- new.env(parent = emptyenv())
      self$meta <- list()
      self$unordered <- character()
      self$views <- list()
      self$titles <- character()
      self$footnotes <- character()
      self$formats <- new.env(parent = emptyenv())
      self$filerefs <- list()
      self$macro <- new_macro_session()
      self$options <- new_session_options(sas_version)
      self$sql <- new.env(parent = emptyenv())
      self$log <- character()
      self$listing <- character()
      self$rt <- new_rt_state()
      self$ct <- new_ct_state()
      invisible(self)
    },

    # Close the SQL back-end connection (it is reopened on demand).
    close = function() {
      sql_duck_close(self$sql)
      invisible(self)
    },

    # Print a short summary.
    # @param ... Unused.
    print = function(...) {
      mem <- sort(ls(self$work))
      cat("<SasSession> WORK: ", length(mem), " data set", if (length(mem) != 1L) "s", if (length(mem)) ": ",
          paste(toupper(mem), collapse = " "), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    finalize = function() self$close()
  )
)

# System options of a session, set by OPTIONS statements (engine_global()).
# The listing layout starts at the values of the SAS gold runs (NOCENTER
# NODATE NONUMBER FORMDLIM=' ' PAGESIZE=MAX); other values fail loud when a
# procedure writes output under them (engine_add_listing()).  Options whose
# only implemented value is the SAS default are not session fields: other
# values fail loud in the OPTIONS statement (YEARCUTOFF=1940, ERRORS=20,
# MISSING=.).  The session encoding (spec 04 §4.8) is not implemented: the
# gold-run values are constants where they matter (INFILE reads latin1 bytes,
# input.R; PROC CONTENTS reports UTF-8, proc_contents.R).
new_session_options <- function(sas_version) {
  o <- new.env(parent = emptyenv())
  o$linesize <- 132L
  o$notes <- TRUE
  o$nosyntaxcheck <- FALSE
  o$center <- FALSE
  o$date <- FALSE
  o$number <- FALSE
  o$pagesize <- Inf
  o$formdlim <- " "
  o$sas_version <- sas_version
  o
}

# Run-time diagnostics (diag.R, notes.R): reset per step by rt_reset().
new_rt_state <- function() {
  rt <- new.env(parent = emptyenv())
  rt$prx <- list()
  rt$prx_cache <- list()
  rt$on_warn <- NULL
  rt$on_error <- NULL
  rt$on_note <- NULL
  rt_reset(rt)
  rt
}

# Compile-time diagnostics (notes.R): reset per step by ct_reset().
new_ct_state <- function() {
  ct <- new.env(parent = emptyenv())
  ct_reset(ct)
  ct
}

# The binding slot: the session bound by the submission being run, and its
# diagnostic environments (cached for the per-row helpers).
.active <- new.env(parent = emptyenv())
.active$session <- NULL

# The active session.  Without an argument: the session bound by the
# submission being run (NULL outside a run).  With `session`: binds it (or
# unbinds with NULL) and returns the previously bound session invisibly;
# callers register the restore with on.exit() before binding.
sas_rt_active <- function(session) {
  if (missing(session)) return(.active$session)
  old <- .active$session
  .active$session <- session
  .active$rt <- if (is.null(session)) NULL else session$rt
  .active$ct <- if (is.null(session)) NULL else session$ct
  invisible(old)
}

# Fast paths for the helpers called per row (sas_err_sync, rt_error ...).
# Outside a submission each call gets a throw-away state: what a standalone
# function call records (data-error NOTEs, counters) is discarded.
rt_diag <- function() .active$rt %||% new_rt_state()
ct_diag <- function() .active$ct %||% new_ct_state()

# The current LINESIZE (log and PUT line width); the SAS default outside a run.
rt_linesize <- function() .active$session$options$linesize %||% 132L

# The current NOTES/NONOTES setting; NOTES outside a run.
rt_notes_on <- function() {
  s <- .active$session
  is.null(s) || isTRUE(s$options$notes)
}

# Start a submission on `session`.  A submission rebinds the session's macro
# executor, line counter, PRXPARSE ids and log callbacks to its own engine, so
# a second submission to the same session while one runs (sas_run() called
# from code the first one runs) would write into the inner engine and leave
# the outer one corrupted: it is refused before anything is changed.
session_enter <- function(session) {
  if (isTRUE(session$busy)) {
    rlang::abort("A SAS session cannot run a submission while another submission to it is running.",
                 class = "sasonr_error")
  }
  session$busy <- TRUE
  invisible(session)
}

# End a submission: drop the per-submission closures (they hold the engine of
# the submission) and release the session.
session_leave <- function(session) {
  m <- session$macro
  m$exec <- NULL
  m$on_put <- NULL
  m$frames <- list()
  rt <- session$rt
  rt$on_warn <- NULL
  rt$on_error <- NULL
  rt$on_note <- NULL
  session$busy <- FALSE
  invisible(session)
}

# Add data frames to a session's WORK library (names lower-cased, character
# columns padded; the metadata keeps the original variable-name case).
session_add_work <- function(session, work) {
  inputs <- prepare_work(work)
  if (length(inputs) == 0) return(invisible(session))
  meta <- Map(meta_from_df, inputs, lapply(work, names))
  names(meta) <- names(inputs)
  session$meta[names(meta)] <- meta
  for (nm in names(inputs)) assign(nm, inputs[[nm]], envir = session$work)
  invisible(session)
}
