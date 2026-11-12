# Run the testthat suite in parallel worker processes.
#
# The test files are independent, so they can run in separate R sessions.  Each
# worker loads the package once and then runs its whole batch of files in a
# single test_dir() call (loading the package once per file is what made a
# naive version slower than serial).  Measured on this machine: the suite takes
# about 53 s serially and about 20 s with four workers.
#
# Usage:
#   Rscript tools/run_tests.R                 # all files, default number of jobs
#   Rscript tools/run_tests.R --jobs 6        # more workers
#   Rscript tools/run_tests.R --filter means  # only files matching a pattern
#   Rscript tools/run_tests.R --serial        # one process (plain testthat)
#
# Exit status is non-zero when any test fails.

args <- commandArgs(trailingOnly = TRUE)
value_of <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1L]
}

root <- normalizePath(".")
test_dir_path <- file.path(root, "tests", "testthat")
if (!dir.exists(test_dir_path)) stop("run this from the package root (tests/testthat not found)")
dir <- normalizePath(test_dir_path)

filter <- value_of("--filter")
serial <- "--serial" %in% args

files <- sort(list.files(dir, pattern = "^test-.*\\.R$"))
if (!is.null(filter)) files <- files[grepl(filter, files, ignore.case = TRUE)]
if (length(files) == 0) stop("no test files matched")

# testthat filters on the context name: file name without "test-" and ".R"
stem_of <- function(f) sub("\\.R$", "", sub("^test-", "", f))
escape_re <- function(x) gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", x)
filter_for <- function(stems) paste0("^(", paste(escape_re(stems), collapse = "|"), ")$")

jobs <- as.integer(value_of("--jobs", NA_character_))
if (is.na(jobs)) {
  detected <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  jobs <- if (is.na(detected)) 4L else max(1L, min(6L, detected - 1L))
}
jobs <- max(1L, min(jobs, length(files)))
if (serial) jobs <- 1L

# longest-first round robin, so one slow file does not define the wall time
sizes <- file.info(file.path(dir, files))$size
batches <- vector("list", jobs)
loads <- rep(0, jobs)
for (i in order(-sizes)) {
  j <- which.min(loads)
  batches[[j]] <- c(batches[[j]], files[i])
  loads[j] <- loads[j] + sizes[i]
}

cat(sprintf("running %d test files with %d job(s)\n", length(files), jobs))

run_batch <- function(batch) {
  start <- proc.time()[["elapsed"]]
  res <- testthat::test_dir(dir, filter = filter_for(stem_of(batch)), reporter = "silent",
                            load_package = "none", stop_on_failure = FALSE)
  results <- unlist(lapply(res, function(x) x$results), recursive = FALSE)
  is_fail <- vapply(results, function(r) inherits(r, "expectation_failure") ||
                      inherits(r, "expectation_error"), logical(1))
  list(
    files = batch,
    elapsed = proc.time()[["elapsed"]] - start,
    passed = sum(!is_fail),
    failed = sum(is_fail),
    skipped = sum(vapply(results, function(r) inherits(r, "expectation_skip"), logical(1))),
    messages = vapply(results[is_fail], function(r) {
      paste0(sub("\n.*$", "", r$message), "  [", paste(r$file, collapse = ", "), "]")
    }, character(1))
  )
}

started <- proc.time()[["elapsed"]]
if (jobs == 1L) {
  suppressPackageStartupMessages({
    library(testthat)
    pkgload::load_all(root, quiet = TRUE)
  })
  out <- list(run_batch(files))
} else {
  cl <- parallel::makePSOCKcluster(jobs, outfile = "")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, list("dir", "root", "filter_for", "stem_of", "run_batch", "escape_re"), envir = environment())
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages(library(testthat))
    pkgload::load_all(root, quiet = TRUE)
    NULL
  })
  out <- parallel::parLapplyLB(cl, batches, run_batch)
}
wall <- proc.time()[["elapsed"]] - started

failed <- sum(vapply(out, function(x) x$failed, integer(1)))
passed <- sum(vapply(out, function(x) x$passed, integer(1)))
skipped <- sum(vapply(out, function(x) x$skipped, integer(1)))

cat("\n--- failures ---\n")
msgs <- unlist(lapply(out, function(x) x$messages))
if (length(msgs)) {
  for (m in msgs) cat("* ", m, "\n", sep = "")
} else {
  cat("none\n")
}

cat("\n--- per job ---\n")
for (x in out) cat(sprintf("%6.1f s  %2d files  (%s)\n", x$elapsed, length(x$files),
                           paste(x$files, collapse = ", ")))

cat(sprintf("\nfiles %d, passed %d, failed %d, skipped %d, wall %.1f s\n",
            length(files), passed, failed, skipped, wall))
if (failed > 0) quit(status = 1L)
