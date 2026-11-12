# Offline conformance report: run every case under one or more conformance
# roots through sasonr and compare with the stored SAS gold standard.
#
# Usage (from the package root; one R process at a time):
#   OPENBLAS_NUM_THREADS=1 Rscript tools/run_conformance.R [--pattern REGEX] [--out FILE.csv] [ROOT ...]
# ROOT defaults to tests/conformance.  Writes a per-case CSV (status, failing
# layers, first difference) and prints counts by area:
#   pass         every compared layer agrees
#   fail         sasonr ran, some layer differs (columns ds/attr/log/lst/put/files)
#   unsupported  sasonr_unsupported (SAS ran normally)
#   error        sasonr stopped with an error (SAS ran normally)
#   both_error   SAS ended with errors (rc >= 2) and sasonr stopped too
#   lenient      SAS reported an error but sasonr ran without one  (must be 0)
# The program text of cases with `program_ref` (book programs) is read from
# $SASONR_BOOKS_ROOT.

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  k <- match(name, args)
  if (is.na(k)) return(default)
  v <- args[k + 1L]
  args <<- args[-c(k, k + 1L)]
  v
}
pattern <- opt("--pattern", ".")
out_csv <- opt("--out", "")
keep_dir <- opt("--keep", "")  # save sasonr's outputs per case (log, lst, ds/, files/) for analysis
roots <- if (length(args)) args else "tests/conformance"

suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
env <- new.env(parent = asNamespace("sasonr"))
sys.source("tests/testthat/helper-conformance.R", envir = env)

rows <- list()
t0 <- Sys.time()
for (root in roots) {
  root <- normalizePath(root)
  cases <- env$conf_list_cases(root)
  cases <- cases[grepl(pattern, cases, perl = TRUE)]
  copies <- list()
  for (k in seq_along(cases)) {
    case <- cases[k]
    rel <- substring(case, nchar(root) + 2L)
    area <- sub("/.*$", "", rel)
    meta <- env$conf_read_meta(file.path(case, "meta.yaml"))
    rc <- NULL
    if (identical(meta$cwd, "_root")) {
      if (is.null(copies[[area]])) {
        d <- tempfile("sasonr_root_")
        dir.create(d)
        file.copy(list.files(file.path(root, area, "_root"), full.names = TRUE, all.files = TRUE, no.. = TRUE), d,
                  recursive = TRUE)
        copies[[area]] <- d
      }
      rc <- copies[[area]]
    }
    r <- tryCatch(env$conf_run_case(case, root_copy = rc, keep = if (nzchar(keep_dir)) file.path(keep_dir, rel)),
                  error = function(e) list(status = "error", layers = logical(), diffs = character(),
                                           msg = paste("[runner]", conditionMessage(e)), meta = meta))
    fails <- names(r$layers)[!r$layers]
    first <- if (length(fails)) r$diffs[[fails[1]]] else r$msg
    rows[[length(rows) + 1L]] <- data.frame(
      root = basename(dirname(root)), area = area, case = rel, status = r$status,
      failed_layers = paste(fails, collapse = "+"),
      ds = if ("ds" %in% names(r$layers)) r$layers[["ds"]] else NA,
      attr = if ("attr" %in% names(r$layers)) r$layers[["attr"]] else NA,
      log = if ("log" %in% names(r$layers)) r$layers[["log"]] else NA,
      lst = if ("lst" %in% names(r$layers)) r$layers[["lst"]] else NA,
      put = if ("put" %in% names(r$layers)) r$layers[["put"]] else NA,
      files = if ("files" %in% names(r$layers)) r$layers[["files"]] else NA,
      known_fail = paste(meta$known_fail %||% character(), collapse = "+"),
      # diagnostic only (a difference is a failure): the largest relative difference when
      # the failing layers differ only in numbers (ds, files: hex/best values, put); NA otherwise
      tol = {
        tl <- c(if ("files" %in% fails) r$tol_files %||% NA_real_, if ("put" %in% fails) r$tol_put %||% NA_real_)
        if (length(fails) && all(fails %in% c("ds", "files", "put")) && any(c("files", "put") %in% fails)) max(tl) else NA_real_
      },
      detail = gsub("[\r\n]+", " ", substr(first, 1, 300))
    )
    if (k %% 200 == 0) {
      cat(sprintf("%s: %d/%d  %.0fs\n", basename(root), k, length(cases), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      invisible(gc())
    }
  }
  for (d in copies) unlink(d, recursive = TRUE)
}
res <- do.call(rbind, rows)
if (nzchar(out_csv)) utils::write.csv(res, out_csv, row.names = FALSE)

st <- c("pass", "fail", "unsupported", "error", "both_error", "lenient", "no_gold", "no_program")
cat("\n== by area ==\n")
cat(sprintf("%-14s %6s %s\n", "area", "cases", paste(sprintf("%11s", st), collapse = "")))
for (a in unique(res$area)) {
  x <- res[res$area == a, ]
  cat(sprintf("%-14s %6d %s\n", a, nrow(x), paste(sprintf("%11d", vapply(st, function(s) sum(x$status == s), 1L)), collapse = "")))
}
cat(sprintf("%-14s %6d %s\n", "TOTAL", nrow(res), paste(sprintf("%11d", vapply(st, function(s) sum(res$status == s), 1L)), collapse = "")))
ran <- res[res$status %in% c("pass", "fail"), ]
cat("\n== layers (cases sasonr ran) ==\n")
for (L in c("ds", "attr", "log", "lst", "put", "files")) {
  v <- ran[[L]]
  cat(sprintf("L-%-6s compared %5d  pass %5d  fail %4d\n", toupper(L), sum(!is.na(v)), sum(v %in% TRUE), sum(v %in% FALSE)))
}
cat(sprintf("\nsilent errors: L-DS fail = %d, lenient = %d\n", sum(ran$ds %in% FALSE), sum(res$status == "lenient")))
