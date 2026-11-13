# Generate the reference documentation of the SAS function implementations
# (sas_f_*) and of the internal runtime helpers.
#
# Usage (package root): Rscript tools/gen_fn_docs.R [conformance-results.csv]
#   - reads tools/fn_docs.tsv (SAS syntax, category, one-line description),
#   - removes hand-written roxygen text above `sas_f_* <- function` (keeps @export),
#   - writes R/doc_functions.R: one roxygen block per function (title = SAS
#     syntax, description + link to the SAS documentation, @param for every
#     formal, "Differences from R", "Verification" from the offline conformance
#     results, @example when inst/examples/<name>.R exists),
#   - writes R/doc_runtime.R: one internal topic listing the other exported
#     runtime helpers (keywords internal).
# The verification status comes from a tools/run_conformance.R --out CSV and
# the tags in tests/conformance/*/meta.yaml.

args <- commandArgs(trailingOnly = TRUE)
res_csv <- if (length(args)) args[1] else ""
suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
ns <- asNamespace("sasonr")
tab <- utils::read.delim("tools/fn_docs.tsv", quote = "", colClasses = "character")
fns <- sort(grep("^sas_f_", ls(ns), value = TRUE))
stopifnot(setequal(paste0("sas_f_", tab$name), fns))

docs_url <- "https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm"

param_desc <- c(
  x = "Argument (vector; character values are SAS fixed-width strings).",
  unit = "Rounding unit (default 1).", .at = "Internal: source position `c(line, column)` used in log NOTEs.",
  .len = "Internal: length of the result buffer (CATX).", "..." = "Further arguments, as in SAS (vectorised).",
  start = "Starting position.", a = "Numeric argument.", b = "Numeric argument.", n = "Count or position.",
  sep = "Delimiter.", dist = "Distribution name, as in SAS (`\"NORMAL\"`, `\"T\"`, ...).",
  p = "Probability.", df = "Degrees of freedom.", nc = "Noncentrality parameter.", s = "Source text.",
  fmt = "Format or informat (text such as `\"8.2\"` or a parsed specification).", w = "Width override.",
  d = "Decimals override.", qq = "`\"?\"` or `\"??\"` modifier (suppress NOTEs / errors).",
  align = "Alignment (`\"-l\"`, `\"-r\"`, `\"-c\"`).", interval = "Interval name, as in SAS.",
  from = "Start value.", to = "End value.", method = "`\"d\"` (discrete) or `\"c\"` (continuous).",
  pos = "Position.", len = "Length.", mods = "Modifiers, as in SAS.", chars = "Characters to remove or find.",
  dlm = "Delimiters.", word = "Word to find.", sub = "Substring.", re = "Perl regular expression or its id.",
  times = "Number of replacements (-1 for all).", k = "Index.", cond = "Condition.", m = "Value when missing.",
  basis = "Day-count basis, as in SAS.", symtab = "Internal: macro symbol table.", name = "Name.",
  y = "Numeric argument.", r = "Count.", q = "Quotation character.", opt = "Option, as in SAS.",
  end = "End position.", h = "Hours.", j = "Julian date.", desc = "Descriptor (`\"u\"`, `\"v\"`, `\"w\"`).",
  sdate = "Start date.", edate = "End date.", mode = "Mode.", values = "Values.", op = "Operator.",
  quant = "`\"any\"` or `\"all\"`.", pattern = "LIKE pattern.", escape = "Escape character.",
  a3 = "Modifiers or start position (as in SAS).", a4 = "Start position or modifiers (as in SAS).",
  excerpt = "Excerpt.", source = "Source string.", target = "Target string.", replacement = "Replacement string."
)

diff_text <- c(
  character = "SAS character values have a fixed length and are padded with blanks; trailing blanks do not count in comparisons. R strings have no fixed length.",
  numeric = "Missing arguments (including special missing values) give an ordinary missing value and are reported in the SAS log; R propagates NA/NaN silently and has no special missing values.",
  descriptive = "Missing arguments are ignored; the result is missing only when no argument is non-missing. R summary functions return NA unless `na.rm = TRUE`.",
  datetime = "SAS dates count days and SAS datetimes count seconds from 1960-01-01; R's Date and POSIXct start at 1970-01-01.",
  probability = "Arguments and parameterisation follow SAS (order of arguments, noncentrality), not R's d/p/q/r functions.",
  special = "Follows the SAS DATA step semantics (log NOTEs, `_ERROR_`), which base R has no counterpart for.",
  macro = "Reads the macro symbol table of the running SAS program.",
  internal = "Used by translated code; not a SAS function."
)
diff_specific <- list(
  round = "SAS rounds half away from zero with a fuzz of about 1e-12 relative, and returns the double nearest to the decimal multiple; `base::round()` rounds half to even on the binary value.",
  rounde = "Ties go to the even multiple as in `base::round()`, but SAS applies the same fuzz as ROUND and returns the decimal multiple.",
  roundz = "No fuzz; ties go to the even multiple; the result is the decimal multiple.",
  int = "A value within 1.5e-12 of the next integer counts as that integer; `INT(0)` is -0.",
  floor = "A value within 1.5e-12 below an integer counts as that integer.",
  ceil = "A value within 1.5e-12 above an integer counts as that integer; a zero result is -0.",
  putn = "w.d rounds the exact binary value half away from zero with a 1-ulp fuzz, writes -0.00 for negatives that round to zero and falls back to E notation as SAS does; `sprintf()` rounds half to even.",
  substr = "Invalid positions or lengths set `_ERROR_` and write a NOTE; the result length is that of the source.",
  missing = "Blank character values and special missing values (.A-.Z, ._) are missing; `is.na()` knows neither."
)

# verification per function from the offline conformance results
tag_map <- list(funcs = c("round", "rounde", "roundz", "int", "floor", "ceil", "floorz", "ceilz"),
                formats = "putn", round = "round", put = c("put", "putn"), input = c("input", "inputn"))
verif <- list()
if (nzchar(res_csv) && file.exists(res_csv) && dir.exists("tests/conformance")) {
  res <- utils::read.csv(res_csv, colClasses = "character")
  for (k in seq_len(nrow(res))) {
    if (startsWith(res$case[k], "fn_examples/")) next  # counted per call below
    mf <- file.path("tests/conformance", res$case[k], "meta.yaml")
    if (!file.exists(mf)) next
    tg <- grep("^tags:", readLines(mf), value = TRUE)
    tags <- trimws(strsplit(gsub("^tags: \\[|\\]$", "", tg), ",")[[1]])
    targets <- unique(unlist(lapply(tags, function(t) tag_map[[t]] %||% t)))
    st <- res$status[k]
    fl <- strsplit(res$failed_layers[k], "+", fixed = TRUE)[[1]]
    kn <- strsplit(res$known_fail[k], "+", fixed = TRUE)[[1]]
    if (st == "fail" && length(fl) && all(fl %in% kn)) st <- paste0("known:", paste(fl, collapse = "+"))
    for (t in intersect(targets, tab$name)) verif[[t]] <- c(verif[[t]], st)
  }
}
# reference-page examples (tools/gen_examples.R): per call, agree or not
ex_res <- if (file.exists(f <- "tests/conformance/fn_examples/results.csv")) utils::read.csv(f, colClasses = "character") else NULL
ex_note <- c(today = "clock", date = "clock", datetime = "clock", time = "clock")
ex_text <- function(nm) {
  if (is.null(ex_res)) return(NULL)
  r <- ex_res[tolower(ex_res$fn) == nm, ]
  if (nrow(r) == 0) return(NULL)
  list(ok = sum(r$status %in% c("agree", "agree_tol")), n = nrow(r))
}
# the feature registry (tools/update_registry.R) is the primary source
reg <- if (file.exists("inst/registry/functions.csv")) utils::read.csv("inst/registry/functions.csv", stringsAsFactors = FALSE) else NULL
# documentation describes the default release (TS1M7)
if (!is.null(reg) && !is.null(reg$sas_version)) reg <- reg[reg$sas_version == "TS1M7", ]
# per-function parameter descriptions (SAS semantics), else the generic ones
fn_params <- if (file.exists("tools/fn_params.tsv")) utils::read.delim("tools/fn_params.tsv", quote = "", colClasses = "character") else NULL
param_text <- function(nm, p) {
  if (!is.null(fn_params)) {
    d <- fn_params$description[fn_params$name == nm & fn_params$param == p]
    if (length(d) && nzchar(d[1])) return(d[1])
  }
  if (p %in% names(param_desc)) param_desc[[p]] else sprintf("Argument `%s` of the SAS function.", p)
}
registry_url <- "https://kaipingyang.github.io/sasonr/articles/dev-feature-registry.html"
reg_text <- function(r) {
  ctx <- if (nzchar(r$contexts_verified %||% "")) sprintf(" Contexts where every case agrees: %s.", gsub(" ", ", ", r$contexts_verified)) else ""
  head <- switch(r$status,
    verified = sprintf("verified (%d of %d SAS 9.4 cases agree bit for bit).", r$pass, r$cases),
    partial = sprintf("partial (%d of %d SAS 9.4 cases agree bit for bit; %s).%s", r$pass, r$cases, r$note,
                      if (nzchar(r$domain %||% "")) sprintf(" Verified domain: %s; other inputs fail loud (`sasonr_unsupported`).", r$domain) else ""),
    approximate = sprintf("approximate (%d of %d SAS 9.4 cases agree bit for bit; %s).", r$pass, r$cases, r$note),
    not_comparable = sprintf("not comparable (%d SAS 9.4 cases check type and range only; the value depends on the session clock).", r$cases),
    unverified = "unverified (no SAS 9.4 case yet).",
    sprintf("%s (%s).", r$status, r$note)
  )
  sprintf("%s%s See the [feature registry](%s).", head, ctx, registry_url)
}
verif_text <- function(nm) {
  if (!is.null(reg) && toupper(nm) %in% reg$name) {
    r <- reg[reg$name == toupper(nm), ][1, ]
    return(paste0(reg_text(r), " Updated ", r$updated, "."))
  }
  clock <- if (nm %in% names(ex_note)) " The value depends on the clock of the session and cannot be reproduced; the examples check its type and range only." else ""
  paste0(verif_text0(nm), clock)
}
verif_text0 <- function(nm) {
  v <- verif[[nm]]
  e <- ex_text(nm)
  if (!is.null(e)) {
    v <- v %||% character()
    ok <- sum(v %in% c("pass", "both_error"))
    kn <- sum(startsWith(v, "known:"))
    cases <- if (length(v)) sprintf(" and %d of %d offline conformance case(s)", ok + kn, length(v)) else ""
    if (e$ok == e$n && ok + kn == length(v)) {
      return(sprintf("Verified against SAS 9.4: %d example call(s)%s agree with SAS%s.", e$n,
                     if (length(v)) sprintf(" and %d offline conformance case(s)", length(v)) else "",
                     if (kn) sprintf(" (%d case(s) with a known log or listing difference)", kn) else ""))
    }
    return(sprintf("Partial: %d of %d example call(s)%s agree with SAS 9.4; the examples show only calls that agree.",
                   e$ok, e$n, cases))
  }
  if (is.null(v)) return("Not yet verified against SAS (no offline conformance case).")
  ok <- sum(v %in% c("pass", "both_error"))
  kn <- sum(startsWith(v, "known:"))
  if (ok == length(v)) return(sprintf("Verified against SAS 9.4: %d offline conformance case(s) agree.", ok))
  if (ok + kn == length(v)) {
    layers <- unique(sub("^known:", "", v[startsWith(v, "known:")]))
    return(sprintf("Values verified against SAS 9.4 in %d offline conformance case(s); %d of them have a known difference in the %s layer (log NOTE or listing layout).",
                   ok + kn, kn, paste(layers, collapse = ", ")))
  }
  sprintf("Partial: %d of %d offline conformance cases agree with SAS 9.4; inputs outside the verified range fail loud (`sasonr_unsupported`).", ok, length(v))
}

esc <- function(s) gsub("([%\\\\])", "\\\\\\1", s)

blocks <- character()
for (i in seq_len(nrow(tab))) {
  nm <- tab$name[i]
  f <- paste0("sas_f_", nm)
  fm <- names(formals(get(f, envir = ns)))
  ex <- file.path("inst", "examples", paste0(f, ".R"))
  internal <- tab$category[i] == "internal"
  b <- c(
    sprintf("#' %s", if (internal) paste("Runtime helper:", tab$description[i]) else paste0("SAS `", tab$syntax[i], "`")),
    "#'",
    sprintf("#' %s %s", esc(tab$description[i]),
            if (internal) "" else sprintf("See the [SAS documentation](%s) (Functions and CALL Routines).", docs_url)),
    "#'",
    vapply(fm, function(p) sprintf("#' @param %s %s", p, esc(param_text(nm, p))), ""),
    sprintf("#' @return %s", "A vector with one result per element (arguments are recycled), as the SAS function returns per observation."),
    sprintf("#' @section Differences from R: %s", esc(diff_specific[[nm]] %||% diff_text[[tab$category[i]]])),
    sprintf("#' @section Verification: %s", verif_text(nm)),
    if (file.exists(ex)) sprintf("#' @example %s", ex),
    sprintf("#' @family %s functions", if (internal) "internal" else tab$category[i]),
    if (internal) "#' @keywords internal",
    sprintf("#' @name %s", f),
    "NULL",
    ""
  )
  blocks <- c(blocks, b)
}
writeLines(c("# Generated by tools/gen_fn_docs.R from tools/fn_docs.tsv -- do not edit by hand.", "", blocks),
           "R/doc_functions.R")

# strip hand-written roxygen text above sas_f_* definitions (keep @export)
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (basename(file) %in% c("doc_functions.R", "doc_runtime.R")) next
  x <- readLines(file)
  defs <- grep("^sas_f_[a-z0-9_]+ <- function", x)
  drop <- integer()
  for (d in defs) {
    j <- d - 1L
    while (j >= 1L && startsWith(x[j], "#'")) {
      if (!grepl("^#' @export\\s*$", x[j])) drop <- c(drop, j)
      j <- j - 1L
    }
  }
  if (length(drop)) writeLines(x[-drop], file)
}

# internal runtime helpers: one topic with an alias for each
helpers <- setdiff(grep("^sas_", getNamespaceExports("sasonr"), value = TRUE),
                   c(fns, "sas_run", "sas_translate", "sas_lex", "sas_parse", "sas_parse_expr", "sas_analyze",
                     "sas_export_work", "sas_missing"))
helpers <- sort(helpers)
# helpers with their own documentation get @keywords internal there
has_doc <- character()
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (basename(file) %in% c("doc_functions.R", "doc_runtime.R")) next
  x <- readLines(file)
  changed <- FALSE
  for (h in helpers) {
    d <- grep(paste0("^", h, " <- function"), x)
    if (length(d) != 1L) next
    j <- d - 1L
    blk <- integer()
    while (j >= 1L && startsWith(x[j], "#'")) {
      blk <- c(j, blk)
      j <- j - 1L
    }
    if (length(blk) && !grepl("^#'\\s*@", x[blk[1]]) && nzchar(trimws(sub("^#'", "", x[blk[1]])))) {
      has_doc <- c(has_doc, h)
      if (!any(grepl("@keywords internal", x[blk]))) {
        x <- append(x, "#' @keywords internal", after = d - 1L)
        changed <- TRUE
      }
    }
  }
  if (changed) writeLines(x, file)
}
helpers <- setdiff(helpers, has_doc)
writeLines(c(
  "# Generated by tools/gen_fn_docs.R -- do not edit by hand.", "",
  "#' Runtime helpers used by translated code",
  "#'",
  "#' Translated R code calls these helpers to reproduce SAS value semantics",
  "#' (missing values, fixed-length characters, PDV, formats, log NOTEs). They",
  "#' are exported so that translated code can run on its own; they are not a",
  "#' stable user interface.",
  "#' @name sasonr-runtime",
  paste0("#' @aliases ", paste(helpers, collapse = " ")),
  "#' @keywords internal",
  "NULL"
), "R/doc_runtime.R")
cat(length(fns), "function topics;", length(helpers), "internal helpers\n")
