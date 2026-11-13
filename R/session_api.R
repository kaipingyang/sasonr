# Public API of the SAS session (session.R): sas_session() and its accessors.

#' Create a persistent SAS session
#'
#' A session holds what a SAS session keeps between submissions: the WORK
#' library and its metadata, macro variables and macro definitions, user
#' formats (PROC FORMAT), system options set with `OPTIONS`, titles,
#' filerefs and `_LAST_`. Pass it to [sas_run()] several times to run a
#' program in several submissions, as in an interactive SAS session.
#' [sas_run()] without `session` runs in a fresh session that is discarded
#' afterwards.
#'
#' Each submission starts outside syntax-check mode and with its own
#' `$status`, `$errors` and `$log` in the returned result; `SYSERR`/`SYSCC`,
#' like every other macro variable, carry over. The PROC SQL connection is
#' opened on first use and closed by [sas_session_close()] (or when the
#' session is garbage collected).
#'
#' @param work Named list of data frames to load into `WORK`.
#' @return A `SasSession` object (R6).
#' @examples
#' s <- sas_session()
#' sas_run("data a; x = 1; run; %let n = 3;", session = s)
#' res <- sas_run("data b; set a; y = x * &n; run;", session = s)
#' res$work$b
#' sas_work(s)$a
#' sas_macro_vars(s)$n
#' sas_session_close(s)
#' @seealso [sas_work()], [sas_macro_vars()], [sas_log()]
#' @export
sas_session <- function(work = list()) {
  s <- SasSession$new()
  session_add_work(s, work)
  s
}

check_session <- function(session) {
  if (!inherits(session, "SasSession")) {
    rlang::abort("`session` must be a SAS session created by `sas_session()`.", class = "sasonr_error")
  }
  session
}

#' Inspect a SAS session
#'
#' Functional accessors of a [sas_session()]: the WORK data sets (trailing
#' blanks of character values removed, as in [sas_run()] results), their
#' metadata, the macro variables, the cumulative log and listing of all
#' submissions. `sas_session_close()` closes the PROC SQL connection.
#'
#' @param session A session from [sas_session()].
#' @param scope `"global"` (user-defined global macro variables) or
#'   `"automatic"` (automatic macro variables such as `SYSCC`).
#' @return `sas_work()`: named list of tibbles. `sas_meta()`: named list of
#'   variable metadata. `sas_macro_vars()`: named list of character values.
#'   `sas_log()`, `sas_listing()`: character vectors. `sas_session_close()`:
#'   the session, invisibly.
#' @examples
#' s <- sas_session()
#' sas_run("%let study = X01; data a; x = 1; put x=; run;", session = s)
#' names(sas_work(s))
#' sas_macro_vars(s)
#' sas_log(s)
#' sas_session_close(s)
#' @name sas_session_accessors
NULL

#' @rdname sas_session_accessors
#' @export
sas_work <- function(session) {
  check_session(session)
  out <- lapply(as.list(session$work), function(d) {
    dplyr::mutate(d, dplyr::across(dplyr::where(is.character), sas_rtrim))
  })
  out[sort(names(out) %||% character())]
}

#' @rdname sas_session_accessors
#' @export
sas_meta <- function(session) {
  check_session(session)
  dsn <- sort(ls(session$work))
  session$meta[intersect(dsn, names(session$meta))]
}

#' @rdname sas_session_accessors
#' @export
sas_macro_vars <- function(session, scope = c("global", "automatic")) {
  check_session(session)
  scope <- match.arg(scope)
  mv <- as.list(session$macro$globals)
  auto <- names(mv) %in% macro_auto_vars
  mv[sort(names(mv)[if (scope == "global") !auto else auto])]
}

#' @rdname sas_session_accessors
#' @export
sas_log <- function(session) {
  check_session(session)
  session$log
}

#' @rdname sas_session_accessors
#' @export
sas_listing <- function(session) {
  check_session(session)
  session$listing
}

#' @rdname sas_session_accessors
#' @export
sas_session_close <- function(session) {
  check_session(session)
  session$close()
}
