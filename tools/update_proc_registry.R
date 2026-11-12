# Build inst/registry/proc_stats.csv (PROC statistics registry) from the SAS
# gold probes (never edited by hand).
#
# Usage (package root; one R process at a time):
#   OPENBLAS_NUM_THREADS=1 Rscript tools/update_proc_registry.R [ROOT]
#
# ROOT defaults to tests/conformance.  The specification tools/proc_stats.tsv
# lists, per procedure x statistic x condition, the evidence: `case|dataset|column`
# (the column of the SAS data set is compared bit for bit with sasonr's, run
# with options(sasonr.strict = FALSE)) or `case` (the whole case must pass all
# layers).  Status: verified (all compared values identical; `partial` when the
# statistic has a domain, outside of which sasonr fails loud), approximate
# (some values differ), unverified (no evidence).

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else "tests/conformance")
suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
env <- new.env(parent = asNamespace("sasonr"))
sys.source("tests/testthat/helper-conformance.R", envir = env)

spec <- utils::read.delim("tools/proc_stats.tsv", comment.char = "#", colClasses = "character", quote = "")
spec[is.na(spec)] <- ""
items <- lapply(strsplit(spec$evidence, ";", fixed = TRUE), function(e) e[nzchar(e)])
cases <- unique(unlist(lapply(items, function(e) sub("\\|.*$", "", e))))

run_prog <- function(case) {
  dir <- file.path(root, case)
  old <- setwd(tempdir())
  on.exit(setwd(old))
  op <- options(sasonr.strict = FALSE, sasonr.step_notes = TRUE, sasonr.freq_unverified_stats = TRUE)
  on.exit(options(op), add = TRUE)
  code <- readLines(file.path(dir, "program.sas"), warn = FALSE, encoding = "latin1")
  tryCatch(suppressWarnings(sas_run(code)), error = function(e) e)
}
options(sasonr.freq_unverified_stats = TRUE)
results <- list()
whole <- list()
for (cs in cases) {
  needs_whole <- any(vapply(items, function(e) cs %in% e, logical(1)))
  if (needs_whole) {
    r <- env$conf_run_case(file.path(root, cs))
    whole[[cs]] <- r$status %in% c("pass", "both_error")
  }
  if (any(vapply(items, function(e) any(startsWith(e, paste0(cs, "|"))), logical(1)))) results[[cs]] <- run_prog(cs)
}

col_check <- function(cs, ds, col) {
  res <- results[[cs]]
  if (inherits(res, "error")) return(c(compared = 0, same = 0))
  e <- haven::read_sas(file.path(root, cs, "expected", "datasets", paste0(ds, ".sas7bdat")))
  nm <- names(res$work)[toupper(names(res$work)) == toupper(ds)]
  if (!length(nm)) return(c(compared = 0, same = 0))
  o <- res$work[[nm]]
  j <- match(toupper(col), toupper(names(o)))
  k <- match(toupper(col), toupper(names(e)))
  if (is.na(j) || is.na(k) || nrow(o) != nrow(e)) return(c(compared = nrow(e), same = 0))
  same <- env$conf_num_eq(env$conf_sas_num(e[[k]]), env$conf_son_num(o[[j]]))
  c(compared = length(same), same = sum(same))
}

today <- format(Sys.Date())
out <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
  ev <- items[[i]]
  n <- 0
  ok <- 0
  for (e in ev) {
    p <- strsplit(e, "|", fixed = TRUE)[[1]]
    if (length(p) == 1) {
      n <- n + 1
      ok <- ok + isTRUE(whole[[p[1]]])
    } else {
      r <- col_check(p[1], p[2], p[3])
      n <- n + r[["compared"]]
      ok <- ok + r[["same"]]
    }
  }
  status <- if (n == 0) "unverified" else if (ok == n) (if (nzchar(spec$domain[i])) "partial" else "verified") else "approximate"
  note <- spec$note[i]
  if (status == "approximate") note <- paste(c(if (nzchar(note)) note,
                                               sprintf("%d of %d values bit-identical to SAS", ok, n)), collapse = "; ")
  data.frame(proc = spec$proc[i], statistic = spec$statistic[i], condition = spec$condition[i], status = status,
             domain = spec$domain[i], compared = n, identical = ok,
             evidence = paste(unique(sub("\\|.*$", "", ev)), collapse = " "), note = note, updated = today,
             stringsAsFactors = FALSE)
}))
utils::write.csv(out, "inst/registry/proc_stats.csv", row.names = FALSE)
tab <- table(paste(out$proc, out$status))
cat(paste(names(tab), tab), sep = "\n")
