# Results archive of the conformance runner (tools/run_conformance.R), local
# only: <pkg>/.claude/results/<commit>/ (gitignored, like all of .claude/).
#
#   <root>-<sas_version>.csv               per-case results, strict mode
#   <root>-strict-false-<sas_version>.csv  per-case results, sasonr.strict = FALSE
#   <root>[-strict-false].summary.txt      the printed summary of the run
#   <root>-proc-columns.csv                PROC statistics evidence columns
#                                          (tools/update_proc_registry.R --archive)
#   <root>-impact.rds                      impact map: per case directory the
#                                          functions and registry rows it used,
#                                          its fingerprint and run time
#   info.txt                               commit, date, how the results were made
# <commit> is the full hash of HEAD, with the suffix "-dirty" when R/, tools/,
# inst/, tests/testthat/helper-conformance.R, DESCRIPTION or NAMESPACE have
# uncommitted changes (such results are kept for --baseline but never reused).
# <root> is "own" (tests/conformance), "internal" (the internal repository's
# cases/) or the base name of the root.  The column `source` says whether a row
# was run ("run") or inherited from the archived run of another commit
# ("inherited:<commit>": an incremental --since run).

arc_status_levels <- c("pass", "fail", "unsupported", "error", "both_error", "sas_error_unsupported", "sas_error_rerror",
                       "lenient", "no_gold", "no_program")

arc_dir <- function(pkg = ".") file.path(normalizePath(pkg), ".claude", "results")

arc_git <- function(pkg, ...) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(pkg), ...), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) return(character())
  out
}

# Archive key of the working tree: full HEAD hash, "-dirty" when the code differs.
arc_key <- function(pkg = ".") {
  sha <- arc_git(pkg, "rev-parse", "HEAD")
  if (!length(sha)) return("nogit")
  dirty <- arc_git(pkg, "status", "--porcelain", "--", "R", "tools", "inst", "tests/testthat/helper-conformance.R",
                   "DESCRIPTION", "NAMESPACE")
  if (length(dirty)) paste0(sha, "-dirty") else sha
}

# Commit-ish -> archive key (a full hash; "-dirty" keys are passed as they are).
arc_resolve <- function(pkg, x) {
  if (grepl("-dirty$", x)) return(x)
  sha <- arc_git(pkg, "rev-parse", "--verify", "--quiet", paste0(x, "^{commit}"))
  if (!length(sha)) stop("conformance archive: unknown commit ", x)
  sha
}

arc_root_name <- function(root, pkg = ".") {
  root <- normalizePath(root, mustWork = FALSE)
  if (identical(root, normalizePath(file.path(pkg, "tests", "conformance"), mustWork = FALSE))) return("own")
  if (basename(root) == "cases" && grepl("^sasonr-conformance", basename(dirname(root)))) return("internal")
  gsub("[^A-Za-z0-9_.-]", "_", basename(root))
}

# Environment the verdicts depend on besides the code and the cases: R and
# package versions, BLAS/LAPACK, thread count, locale and time zone, the
# SASHELP copy and the books root (path; the book programs themselves are part
# of the case hashes, conf_impact.R imp_unit_hash()).
arc_fingerprint <- function(pkg = ".") {
  pk <- c("dplyr", "tibble", "rlang", "cli", "purrr", "vctrs", "DBI", "duckdb", "R6", "haven")
  ver <- vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "-"), "")
  sh <- list.files(file.path(pkg, "inst", "sashelp"), recursive = TRUE, full.names = TRUE)
  fi <- file.info(sh, extra_cols = FALSE)
  br <- Sys.getenv("SASONR_BOOKS_ROOT")
  list(R = R.version.string, packages = paste(names(ver), ver, collapse = " "),
       sashelp = paste(length(sh), sum(fi$size), max(c(0, as.integer(fi$mtime)))),
       books_root = if (nzchar(br)) normalizePath(br, mustWork = FALSE) else "",
       blas = paste(extSoftVersion()[["BLAS"]], La_library(), La_version()),
       threads = Sys.getenv("OPENBLAS_NUM_THREADS"),
       locale = Sys.getlocale(), tz = paste(Sys.timezone(), Sys.getenv("TZ")))
}

# Content hash of the code the verdicts depend on (what arc_key() calls
# "dirty"): R/, inst/ (without the SASHELP copy, part of the fingerprint), the
# runner (tools/run_conformance.R, tools/conf_*.R), the conformance helper,
# DESCRIPTION and NAMESPACE.  Taken when the cases are selected and when each
# worker loads the package; finalize stops when they differ (the archive key
# names the code that ran).
arc_code_files <- function(pkg = ".") {
  pkg <- normalizePath(pkg)
  fs <- c(list.files(file.path(pkg, "R"), recursive = TRUE, full.names = TRUE, all.files = TRUE),
          list.files(file.path(pkg, "inst"), recursive = TRUE, full.names = TRUE, all.files = TRUE),
          file.path(pkg, c("tools/run_conformance.R", "tests/testthat/helper-conformance.R", "DESCRIPTION", "NAMESPACE")),
          list.files(file.path(pkg, "tools"), pattern = "^conf_.*\\.R$", full.names = TRUE))
  fs <- fs[!startsWith(fs, file.path(pkg, "inst", "sashelp", ""))]
  sort(unique(fs[file.exists(fs)]), method = "radix")
}
arc_code_hash <- function(pkg = ".") {
  fs <- arc_code_files(pkg)
  rel <- substring(fs, nchar(normalizePath(pkg)) + 2L)
  tf <- tempfile()
  on.exit(unlink(tf))
  writeLines(paste(rel, unname(tools::md5sum(fs))), tf)
  unname(tools::md5sum(tf))
}

arc_path <- function(key, root_name, strict, version, pkg = ".") {
  file.path(arc_dir(pkg), key, sprintf("%s%s-%s.csv", root_name, if (strict) "" else "-strict-false", version))
}

# Write the results of one root.  `res` = list(strict = df, nonstrict = df) (either may be NULL).
arc_write <- function(key, root_name, res, map, info, summary, pkg = ".", extra = list()) {
  d <- file.path(arc_dir(pkg), key)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # every earlier result of this root (both modes, summaries, impact map,
  # extras): a run in one mode must not leave the other mode's older verdicts
  old <- list.files(d, full.names = TRUE)
  old <- old[grepl(paste0("^", gsub(".", "\\.", root_name, fixed = TRUE), "(-|\\.)"), basename(old))]
  unlink(old)
  for (mode in names(res)) {
    df <- res[[mode]]
    if (is.null(df)) next
    strict <- mode == "strict"
    v <- df$sas_version
    v[is.na(v)] <- "unknown"
    for (ver in unique(v)) utils::write.csv(df[v == ver, , drop = FALSE], arc_path(key, root_name, strict, ver, pkg), row.names = FALSE)
    if (!is.null(summary[[mode]])) {
      writeLines(summary[[mode]], file.path(d, sprintf("%s%s.summary.txt", root_name, if (strict) "" else "-strict-false")))
    }
  }
  for (nm in names(extra)) utils::write.csv(extra[[nm]], file.path(d, sprintf("%s-%s.csv", root_name, nm)), row.names = FALSE)
  if (!is.null(map)) saveRDS(map, file.path(d, paste0(root_name, "-impact.rds")))
  # info.txt: one block per root (a second root of the same run appends)
  inf <- file.path(d, "info.txt")
  keep <- if (file.exists(inf)) readLines(inf, warn = FALSE) else character()
  blk <- cumsum(grepl("^\\[", keep))
  hd <- grepl("^\\[", keep)
  own <- blk %in% blk[hd & keep == paste0("[", root_name, "]")]
  writeLines(c(keep[!own], paste0("[", root_name, "]"), info), inf)
  invisible(d)
}

# Archived results of one root (all SAS versions), or NULL.
arc_read <- function(key, root_name, strict = TRUE, pkg = ".") {
  d <- file.path(arc_dir(pkg), key)
  fs <- list.files(d, pattern = sprintf("^%s%s-[A-Za-z0-9]+\\.csv$", root_name, if (strict) "" else "-strict-false"),
                   full.names = TRUE)
  fs <- if (strict) fs[!grepl("-strict-false-", fs)] else fs[grepl("-strict-false-", fs)]
  if (!length(fs)) return(NULL)
  do.call(rbind, lapply(fs, function(f) utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(case = "character"))))
}

arc_read_map <- function(key, root_name, pkg = ".") {
  f <- file.path(arc_dir(pkg), key, paste0(root_name, "-impact.rds"))
  if (file.exists(f)) readRDS(f) else NULL
}

# Most recent archived impact map of a root (any commit): run times for sharding.
arc_latest_map <- function(root_name, pkg = ".") {
  fs <- Sys.glob(file.path(arc_dir(pkg), "*", paste0(root_name, "-impact.rds")))
  if (!length(fs)) return(NULL)
  readRDS(fs[which.max(file.info(fs)$mtime)])
}

# Printed summary of a result table (the lines release.sh and round reports read).
arc_summary <- function(res, strict) {
  st <- arc_status_levels
  lab <- c(sas_error_unsupported = "serr_unsup", sas_error_rerror = "serr_rerr")
  hd <- ifelse(st %in% names(lab), lab[st], st)
  cnt <- function(x) paste(sprintf("%11d", vapply(st, function(s) sum(x$status == s), 1L)), collapse = "")
  out <- c(sprintf("strict = %s", if (strict) "TRUE" else "FALSE"), "== by area ==",
           sprintf("%-14s %6s %s", "area", "cases", paste(sprintf("%11s", hd), collapse = "")))
  for (a in unique(res$area)) {
    x <- res[res$area == a, ]
    out <- c(out, sprintf("%-14s %6d %s", a, nrow(x), cnt(x)))
  }
  out <- c(out, sprintf("%-14s %6d %s", "TOTAL", nrow(res), cnt(res)),
           sprintf("\nbit-identical (pass + both_error): %d / %d; fail loud: %d; other-layer mismatches (ds ok): %d",
                   sum(res$status %in% c("pass", "both_error")), sum(!res$status %in% c("no_gold", "no_program")),
                   sum(res$status %in% c("unsupported", "sas_error_unsupported")),
                   sum(res$status == "fail" & !(res$ds %in% FALSE))))
  ran <- res[res$status %in% c("pass", "fail"), ]
  out <- c(out, "\n== layers (cases sasonr ran) ==")
  for (L in c("ds", "attr", "log", "lst", "put", "files")) {
    v <- ran[[L]]
    out <- c(out, sprintf("L-%-6s compared %5d  pass %5d  fail %4d", toupper(L), sum(!is.na(v)), sum(v %in% TRUE), sum(v %in% FALSE)))
  }
  c(out, sprintf("\nsilent errors: L-DS fail = %d, lenient = %d; R errors = %d (where SAS ran: %d, where SAS stopped: %d)",
                 sum(ran$ds %in% FALSE), sum(res$status == "lenient"), sum(res$status %in% c("error", "sas_error_rerror")),
                 sum(res$status == "error"), sum(res$status == "sas_error_rerror")))
}

# Case-by-case comparison with an archived run: the cases whose verdict
# (status and failing layers) differs, plus cases only in one of the two.
arc_compare <- function(old, new, label) {
  vk <- function(d) paste(d$status, d$failed_layers %|NA|% "", sep = " ")
  if (is.null(old)) return(sprintf("baseline %s: no archived result", label))
  m <- merge(old[, c("case", "status", "failed_layers")], new[, c("case", "status", "failed_layers")], by = "case",
             all = TRUE, suffixes = c(".old", ".new"))
  ov <- ifelse(is.na(m$status.old), "-", paste(m$status.old, ifelse(is.na(m$failed_layers.old), "", m$failed_layers.old)))
  nv <- ifelse(is.na(m$status.new), "-", paste(m$status.new, ifelse(is.na(m$failed_layers.new), "", m$failed_layers.new)))
  # an incremental or selective run compares only the cases it has
  sel <- !is.na(m$status.new)
  ch <- m[sel & ov != nv, ]
  out <- sprintf("baseline %s: %d of %d case(s) changed", label, nrow(ch), sum(sel))
  if (nrow(ch)) out <- c(out, sprintf("  %s: %s -> %s", ch$case, trimws(ov[sel & ov != nv]), trimws(nv[sel & ov != nv])))
  out
}

`%|NA|%` <- function(x, y) ifelse(is.na(x), y, x)
