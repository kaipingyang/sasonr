# Offline conformance report: run every case under one or more conformance
# roots through sasonr and compare with the stored SAS gold standard.
#
# Usage (from the package root; one R process at a time):
#   OPENBLAS_NUM_THREADS=1 Rscript tools/run_conformance.R [options] [ROOT ...]
# Options:
#   --pattern REGEX   only cases whose path matches (matrix cases: the case id may match)
#   --out FILE.csv    per-case CSV (status, failing layers, first difference)
#   --keep DIR        save sasonr's outputs per case (log, lst, ds/, files/)
#   --strict true|false
#                     option sasonr.strict for the run (default: the package
#                     default, TRUE: approximate and unverified features fail loud)
#   --status S[,S]    only cases of functions / formats whose registry status is
#                     S (e.g. `--status approximate --strict false` measures how
#                     close the approximate functions are)
#   --since [BASE]    incremental: only the cases affected by the changes since
#                     the git commit BASE (default origin/main), see
#                     tools/affected_cases.R; a change to a shared file (lexer,
#                     parser, runtime core, runner) selects every case
# ROOT defaults to tests/conformance.  Prints counts by area:
#   pass         every compared layer agrees
#   fail         sasonr ran, some layer differs (columns ds/attr/log/lst/put/files)
#   unsupported  sasonr_unsupported (SAS ran normally)
#   error        sasonr stopped with an error (SAS ran normally)
#   both_error   SAS ended with errors (rc >= 2) and sasonr stopped too
#   lenient      SAS reported an error but sasonr ran without one  (must be 0)
# Matrix programs (fn_matrix/, fmt_matrix/: meta.yaml `layout: matrix`) are
# reported per case (`<area>/<NAME>/<case>`).  The program text of cases with
# `program_ref` (book programs) is read from $SASONR_BOOKS_ROOT.

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL, flag_value = NULL) {
  k <- match(name, args)
  if (is.na(k)) return(default)
  if (!is.null(flag_value) && (k == length(args) || startsWith(args[k + 1L], "--") || dir.exists(args[k + 1L]))) {
    args <<- args[-k]
    return(flag_value)
  }
  v <- args[k + 1L]
  args <<- args[-c(k, k + 1L)]
  v
}
pattern <- opt("--pattern", ".")
out_csv <- opt("--out", "")
keep_dir <- opt("--keep", "")
strict <- opt("--strict", "")
status_sel <- opt("--status", "")
since <- opt("--since", "", flag_value = "origin/main")
roots <- if (length(args)) args else "tests/conformance"

suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
if (nzchar(strict)) options(sasonr.strict = as.logical(toupper(strict)))
env <- new.env(parent = asNamespace("sasonr"))
sys.source("tests/testthat/helper-conformance.R", envir = env)

# case selection: matrix directories by registry status, incremental selection
sel_dirs <- NULL
if (nzchar(status_sel)) {
  st <- strsplit(status_sel, ",", fixed = TRUE)[[1]]
  f <- sas_feature_registry("functions")
  m <- sas_feature_registry("formats")
  fm <- readLines("tools/fmt_matrix.txt")
  fm <- strsplit(sub("^## ", "", grep("^## ", fm, value = TRUE)), "\\s+")
  fm_dir <- vapply(fm, `[`, "", 1L)
  fm_sas <- toupper(vapply(fm, function(p) {
    b <- if (length(p) > 3) p[4] else p[1]
    if (b == "-") "w.d" else if (p[3] == "C" && !startsWith(b, "$")) paste0("$", b) else b
  }, ""))
  fm_sas[fm_sas == "W.D"] <- "w.d"
  fm_kind <- vapply(fm, `[`, "", 2L)
  hit_f <- f$name[f$status %in% st]
  hit_m <- fm_dir[paste(fm_sas, fm_kind) %in% paste(m$name, m$kind)[m$status %in% st]]
  sel_dirs <- c(file.path("fn_matrix", hit_f), file.path("functions", hit_f), file.path("fmt_matrix", hit_m))
}
affected <- NULL
if (nzchar(since)) {
  aff_env <- new.env()
  sys.source("tools/affected_cases.R", envir = aff_env)
  affected <- aff_env$affected_cases(since, roots)
  cat(affected$summary, sep = "\n")
}

rows <- list()
row_of <- function(root, area, rel, r, meta) {
  fails <- names(r$layers)[!r$layers]
  first <- if (length(fails)) r$diffs[[fails[1]]] else r$msg
  data.frame(
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
}
t0 <- Sys.time()
done <- 0L
for (root in roots) {
  root <- normalizePath(root)
  cases <- env$conf_list_cases(root)
  rels <- substring(cases, nchar(root) + 2L)
  keep <- rep(TRUE, length(cases))
  if (!is.null(sel_dirs)) keep <- keep & (rels %in% sel_dirs | sub("/[^/]+$", "", rels) %in% sel_dirs)
  if (!is.null(affected) && !isTRUE(affected$all)) keep <- keep & cases %in% affected$cases
  cases <- cases[keep]
  copies <- list()
  for (k in seq_along(cases)) {
    case <- cases[k]
    rel <- substring(case, nchar(root) + 2L)
    area <- sub("/.*$", "", rel)
    meta <- env$conf_read_meta(file.path(case, "meta.yaml"))
    if (identical(meta$layout, "matrix")) {
      only <- NULL
      if (!grepl(pattern, case, perl = TRUE)) {
        ids <- utils::read.csv(file.path(case, "cases.csv"), colClasses = "character")$case
        only <- ids[grepl(pattern, file.path(rel, ids), perl = TRUE)]
        if (!length(only)) next
      }
      rr <- tryCatch(env$conf_run_matrix(case, keep = if (nzchar(keep_dir)) file.path(keep_dir, rel), only = only),
                     error = function(e) list(`?` = list(status = "error", layers = logical(), diffs = character(),
                                                        msg = paste("[runner]", conditionMessage(e)))))
      for (id in names(rr)) rows[[length(rows) + 1L]] <- row_of(root, area, file.path(rel, id), rr[[id]], meta)
      done <- done + length(rr)
    } else {
      if (!grepl(pattern, case, perl = TRUE)) next
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
      rows[[length(rows) + 1L]] <- row_of(root, area, rel, r, meta)
      done <- done + 1L
    }
    if (k %% 50 == 0) {
      cat(sprintf("%s: %d/%d  (%d cases)  %.0fs\n", basename(root), k, length(cases), done,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      invisible(gc())
    }
  }
  for (d in copies) unlink(d, recursive = TRUE)
}
res <- if (length(rows)) do.call(rbind, rows) else data.frame(area = character(), status = character(), ds = logical())
if (nzchar(out_csv)) utils::write.csv(res, out_csv, row.names = FALSE)

st <- c("pass", "fail", "unsupported", "error", "both_error", "lenient", "no_gold", "no_program")
cat(sprintf("\nstrict = %s\n", format(getOption("sasonr.strict", TRUE))))
cat("== by area ==\n")
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
