# Feature registries (inst/registry/functions.csv, formats.csv) and the
# strict-mode check.
#
# The registries are generated files (never edited by hand): one row per
# function, format or informat with its status, the contexts in which a
# function is supported and the argument counts, argument types and default
# widths of the SAS 9.4 inventory.  The analyzer asks feature_check() about
# every function, format and informat it accepts, with the context of the
# call: "data" (DATA step), "where" (WHERE statement or WHERE= option), "sql"
# (PROC SQL), "sysfunc" (%SYSFUNC/%QSYSFUNC); PUTN, PUTC, INPUTN and INPUTC
# check the format named at run time ("runtime").  A feature is gated by the
# option `sasonr.strict` when its status is `approximate` (numeric results may
# differ in the last bits), `unsupported`, any other status than `supported`,
# `partial` and `not_comparable`, or when it is missing from the registry, and
# a function also when the context of the call is not among its `contexts`
# (format results do not depend on the context):
#   TRUE (default)  fail loud: condition `sasonr_strict_error` (a subclass of
#                   `sasonr_unsupported`) naming the feature, its status or
#                   context and the opt-out
#   FALSE           run it, with a warning of class `sasonr_strict_warning`
# `supported`, `partial` and `not_comparable` (clock-dependent) features run in
# their contexts.  A `partial` feature runs only inside its supported input
# domain: its runtime guard fails loud (sasonr_unsupported) outside it in
# every mode.
#
# Internal call nodes for language constructs (PROC SQL CASE, IN lists, LIKE,
# MONOTONIC(), type conversions) are not SAS functions: the code that builds
# them marks them as constructs (`construct = TRUE` in the AST node) and the
# analyzer skips the registry for them; a SAS program cannot create one.

registry_cache <- new.env(parent = emptyenv())

#' Feature registry (functions, formats and informats) with the status of each feature
#'
#' @param kind `"functions"` or `"formats"`.
#' @return A data frame, one row per function or per format/informat: the
#'   name, the kind, the status (`supported`, `partial`, `approximate`,
#'   `unsupported`, `not_comparable`; see [sasonr_strict]), for functions the
#'   contexts in which they are supported, and the argument counts, argument
#'   types and default widths of the SAS 9.4 inventory.
#' @param sas_version The SAS 9.4 release whose rows are returned (default:
#'   the selected release, see [sasonr_sas_version]); `NULL` returns every row.
#' @keywords internal
sas_feature_registry <- function(kind = c("functions", "formats"), sas_version = sas_target_version()) {
  kind <- match.arg(kind)
  if (is.null(registry_cache[[kind]])) {
    f <- system.file("registry", paste0(kind, ".csv"), package = "sasonr")
    registry_cache[[kind]] <- if (nzchar(f)) {
      utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    } else {
      data.frame(name = character(), kind = character(), status = character(), stringsAsFactors = FALSE)
    }
  }
  registry_version_rows(registry_cache[[kind]], sas_version)
}

#'
#'
#'
#' the registries ([sas_feature_registry()]) have one block of rows per
#'
#'
#' function, format and procedure statistic by its status for that release,
#' and the code paths whose SAS behaviour differs between the releases (for
#' example the PROC MEANS moment arithmetic) follow it. Nothing is carried
#' over from one release to the other; a feature without evidence for the
#' selected release is `unsupported` there.
#'
#' A SAS session ([sas_session()], or the throw-away session of a
#' [sas_run()] call) takes the release from the option when it is created
#' and keeps it for all its submissions.
#'
#' @name sasonr_sas_version
#' @aliases sasonr.sas_version
#' @examples
#'
#' getOption("sasonr.sas_version")
#' options(old)
NULL

#
#
# Verification is recorded per SAS release (registries: column `sas_version`).
#
#
# features (strict mode), and the code paths whose SAS behaviour differs
# between the releases follow it.  Nothing is carried over from one release to
# the other: a feature without evidence for the selected release is
# `unsupported` there.
sas_versions <- c("SAS 9.4", "SAS 9.4")

sas_target_version <- function() {
  # inside a run: the release of the session (taken from the option when the
  # session was created)
  s <- .active$session
  v <- if (is.null(s)) getOption("sasonr.sas_version", "SAS 9.4") else s$options$sas_version
  if (!is.character(v) || length(v) != 1L || !toupper(v) %in% sas_versions) {
    sasonr_abort(paste0("Unsupported: option sasonr.sas_version = ", format(v)[1], " (supported: ",
                        paste(sas_versions, collapse = ", "), ")"), "sasonr_unsupported")
  }
  toupper(v)
}

# The rows of a registry for one SAS release (NULL: every row).  A registry
#
registry_version_rows <- function(reg, sas_version) {
  if (is.null(sas_version)) return(reg)
  if (is.null(reg$sas_version)) return(if (identical(sas_version, "SAS 9.4")) reg else reg[0, , drop = FALSE])
  reg[reg$sas_version == sas_version, , drop = FALSE]
}

# Status of one feature: "supported", "partial", "approximate", "unsupported",
# "not_comparable" or NA (not in the registry).
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
feature_gated <- function(status) is.na(status) || !status %in% c("supported", "partial", "not_comparable")

# Contexts in which a function is supported ("data", "where", "sql", "sysfunc", "statement").
feature_contexts <- function(name) {
  reg <- sas_feature_registry("functions")
  hit <- reg$contexts[toupper(reg$name) == toupper(name) & reg$kind == "function"]
  if (!length(hit) || is.na(hit[1])) return(character())
  strsplit(hit[1], " ", fixed = TRUE)[[1]]
}

feature_context_label <- function(context) {
  switch(context, data = "the DATA step", where = "a WHERE expression", sql = "PROC SQL",
         sysfunc = "%SYSFUNC", runtime = "a format named at run time",
         statement = "the SUBSTR statement (left of =)", context)
}

feature_status_text <- function(status) {
  if (is.na(status)) return("is not in the feature registry")
  switch(status,
    approximate = "is approximate",
    unsupported = "is unsupported",
    mismatch = "is unsupported",
    paste("has status", status)
  )
}

#' Strict mode: approximate and unsupported features fail loud
#'
#' Every function, format and informat that sasonr implements has a status
#' in the feature registry ([sas_feature_registry()]). The option
#' `sasonr.strict` decides what happens when a program uses a feature that is
#' not supported:
#'
#' * `TRUE` (the default): the program stops with a condition of class
#'   `sasonr_strict_error` (a subclass of `sasonr_unsupported`) that names the
#'   SAS feature, its status and the SAS line. This applies to `approximate`
#'   features (numeric results may differ in the last bits), `unsupported`
#'   ones, features missing from the registry, and functions called in a
#'   context (DATA step, WHERE, PROC SQL, `%SYSFUNC`) in which they are not
#'   supported.
#' * `FALSE`: the program runs; each such feature gives a warning of class
#'   `sasonr_strict_warning`.
#'
#' `supported` features, `partial` ones (they run only inside their supported
#' input domain; other inputs fail loud in both modes) and clock-dependent
#' `not_comparable` functions (`TODAY()`, `DATE()`, `TIME()`, `DATETIME()`)
#' run in the contexts in which they are supported.
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

feature_check <- function(kind, name, line = NULL, context = "data") {
  st <- feature_status(kind, name)
  if (feature_gated(st)) {
    what <- sprintf("%s %s %s", kind, toupper(name), feature_status_text(st))
  } else if (kind == "function" && !context %in% (ctx <- feature_contexts(name))) {
    what <- sprintf("function %s is unsupported in %s (supported in: %s)", toupper(name),
                    feature_context_label(context), if (length(ctx)) paste(vapply(ctx, feature_context_label, ""), collapse = ", ") else "no context")
  } else {
    return(invisible())
  }
  if (!isFALSE(getOption("sasonr.strict", TRUE))) {
    msg <- paste0(what, "; set options(sasonr.strict = FALSE) to use it")
    if (!is.null(line) && length(line) == 1 && !is.na(line)) msg <- paste0(msg, " (SAS line ", line, ")")
    rlang::abort(msg, class = c("sasonr_strict_error", "sasonr_unsupported", "sasonr_error"), sas_line = line,
                 kind = kind, name = toupper(name), status = st, context = context)
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
# Without the inventory nothing can be decided: an error (fail closed).
sas_known_function <- function(name) {
  if (is.null(registry_cache$fn_names)) registry_cache$fn_names <- sas_function_inventory()
  toupper(name) %in% registry_cache$fn_names
}

# Is `fmt` (a format, or an informat with kind = "informat") one of SAS 9.4
# (inst/registry/sas_format_names.txt, "F NAME" / "I NAME" from the SAS
# inventory)?  An unknown name is SAS's compile-time ERROR 48-59.
sas_known_format <- function(fmt, kind = "format") {
  if (is.null(registry_cache$fmt_names)) {
    registry_cache$fmt_names <- sas_function_inventory(system.file("registry", "sas_format_names.txt", package = "sasonr"))
  }
  nm <- if (nzchar(fmt$name)) paste0(if (fmt$char) "$", toupper(fmt$name)) else if (fmt$char) "$" else "F"
  paste(if (kind == "informat") "I" else "F", nm) %in% registry_cache$fmt_names
}

sas_function_inventory <- function(f = system.file("registry", "sas_function_names.txt", package = "sasonr")) {
  nm <- if (nzchar(f) && file.exists(f)) setdiff(readLines(f), "") else character()
  if (!length(nm)) {
    rlang::abort(paste0("sasonr: the SAS 9.4 inventory (", basename(f %||% ""), ") is missing"),
                 class = "sasonr_error")
  }
  nm
}
