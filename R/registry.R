# Feature registries (inst/registry/functions.csv, formats.csv) and the
# run-time verification check.
#
#
#
# asks feature_check() about every function, format and informat it accepts
# (DATA step, PROC SQL and %SYSFUNC).  A feature whose status is `approximate`
#
# `mismatch` or that is missing from the registry is gated by the option
# `sasonr.strict`:
#
#                   `sasonr_unsupported`) naming the feature, its status and the
#                   opt-out
#
#
# in every mode) and `not_comparable` (clock-dependent) features always run.

registry_cache <- new.env(parent = emptyenv())

#'
#'
#' @param kind `"functions"` or `"formats"`.
#' @return A data frame, one row per function or per format/informat, as
#'
#' @keywords internal
#' @export
sas_feature_registry <- function(kind = c("functions", "formats")) {
  kind <- match.arg(kind)
  if (is.null(registry_cache[[kind]])) {
    f <- system.file("registry", paste0(kind, ".csv"), package = "sasonr")
    registry_cache[[kind]] <- if (nzchar(f)) {
      utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    } else {
      data.frame(name = character(), kind = character(), status = character(), stringsAsFactors = FALSE)
    }
  }
  registry_cache[[kind]]
}

#
#
feature_status <- function(kind, name) {
  reg <- sas_feature_registry(if (kind == "function") "functions" else "formats")
  hit <- reg$status[toupper(reg$name) == toupper(name) & reg$kind == kind]
  if (length(hit)) hit[1] else NA_character_
}

# Registry name of a format/informat specification: "w.d" (no name, numeric),
# "$" (no name, character), otherwise the upper-case name ("$CHAR", "DATE").
fmt_registry_name <- function(fmt) {
  if (!nzchar(fmt$name)) return(if (fmt$char) "$" else "w.d")
  paste0(if (fmt$char) "$", toupper(fmt$name))
}

# Registry statuses that are gated by `sasonr.strict` (NA: not in the registry).
feature_gated <- function(status) is.na(status) || status %in% c("approximate", "unsupported", "mismatch")

feature_status_text <- function(status) {
  if (is.na(status)) return("is not in the feature registry (not supported)")
  switch(status,
    approximate = "is approximate (some results differ in the last bits)",
    unsupported = "is not supported (no test cases)",
    mismatch = "has results that differ",
    paste("has status", status)
  )
}

#'
#'
#' Every function, format and informat that sasonr implements has a
#'
#'
#'
#'
#' * `TRUE` (the default): the program stops with a condition of class
#'
#'   SAS feature, its status and the SAS line. This applies to `approximate`
#'
#'   ones (no SAS case) and features missing from the registry.
#' * `FALSE`: the program runs; each such feature gives a warning of class
#'
#'
#'
#' from SAS fail loud in both modes) and clock-dependent `not_comparable`
#' functions (`TODAY()`, `DATE()`, `TIME()`, `DATETIME()`) always run.
#'
#' @name sasonr_strict
#' @aliases sasonr.strict
#' @examples
#'
#' try(sas_run("data a; y = probt(1.5, 10); run;"))
#' old <- options(sasonr.strict = FALSE)
#' res <- suppressWarnings(sas_run("data a; y = probt(1.5, 10); run;"))
#' res$work$a$y
#' options(old)
NULL

feature_check <- function(kind, name, line = NULL) {
  # internal call nodes for language constructs (PROC SQL CASE / IN / LIKE /
  # quantified comparisons, MONOTONIC, type conversion) are not SAS functions
  if (kind == "function" && (startsWith(tolower(name), "sql") || startsWith(tolower(name), "cvt_") ||
                             tolower(name) == "monotonic")) return(invisible())
  st <- feature_status(kind, name)
  if (!feature_gated(st)) return(invisible())
  what <- sprintf("%s %s %s", kind, toupper(name), feature_status_text(st))
  if (!isFALSE(getOption("sasonr.strict", TRUE))) {
    msg <- paste0("Unsupported: ", what, "; set options(sasonr.strict = FALSE) to run it anyway")
    if (!is.null(line) && length(line) == 1 && !is.na(line)) msg <- paste0(msg, " (SAS line ", line, ")")
    rlang::abort(msg, class = c("sasonr_strict_error", "sasonr_unsupported", "sasonr_error"), sas_line = line,
                 kind = kind, name = toupper(name), status = st)
  }
  warning(structure(class = c("sasonr_strict_warning", "warning", "condition"),
                    list(message = paste0("sasonr: ", what, if (!is.null(line)) sprintf(" at line %s", line) else ""),
                         call = NULL, kind = kind, name = toupper(name), status = st)))
  invisible()
}

# Argument types of a function from the SAS 9.4 inventory kept in the registry
# ("num", "char"; NA for arguments of either type), for `n` arguments; the last
# listed type repeats.  NULL when the function is not in the registry.
registry_arg_types <- function(name, n) {
  reg <- sas_feature_registry("functions")
  ty <- reg$sas_arg_types[reg$name == toupper(name)]
  if (!length(ty) || is.na(ty[1]) || !nzchar(ty[1])) return(NULL)
  ty <- strsplit(ty[1], " ", fixed = TRUE)[[1]]
  if (n > length(ty)) ty <- c(ty, rep(ty[length(ty)], n - length(ty)))
  out <- ty[seq_len(n)]
  out[!out %in% c("num", "char")] <- NA_character_
  out
}

#
# unlimited), or NULL when the function is not in the registry.
registry_arg_counts <- function(name) {
  reg <- sas_feature_registry("functions")
  r <- reg[reg$name == toupper(name), , drop = FALSE]
  if (!nrow(r) || is.na(r$sas_minarg[1]) || !nzchar(r$sas_minarg[1])) return(NULL)
  c(as.numeric(r$sas_minarg[1]), suppressWarnings(as.numeric(r$sas_maxarg[1])))
}

#
# (feature registry, from the SAS inventory); NA when unknown.
registry_defw <- function(kind, fmt) {
  reg <- sas_feature_registry("formats")
  if (!nrow(reg) || is.null(reg$sas_defw)) return(NA_integer_)
  nm <- if (!nzchar(fmt$name)) (if (fmt$char) "$" else "F") else paste0(if (fmt$char) "$", toupper(fmt$name))
  w <- reg$sas_defw[toupper(reg$name) == nm & reg$kind == kind]
  if (!length(w) || is.na(w[1]) || !nzchar(w[1])) NA_integer_ else as.integer(w[1])
}

# Is `name` a function or CALL routine of SAS 9.4 (inst/registry/sas_function_names.txt,
# names from the SAS inventory)?  Unknown names are a compile-time ERROR in SAS.
sas_known_function <- function(name) {
  if (is.null(registry_cache$fn_names)) {
    f <- system.file("registry", "sas_function_names.txt", package = "sasonr")
    registry_cache$fn_names <- if (nzchar(f)) setdiff(readLines(f), "") else character()
  }
  nm <- registry_cache$fn_names
  !length(nm) || toupper(name) %in% nm
}
