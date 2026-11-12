# Offline conformance cases (tests/conformance/): one test per case, compared
# with the real-SAS gold standard stored next to the program (see
# helper-conformance.R).  Every layer must agree exactly; `known_fail` in a
# case's meta.yaml is rejected (fix the difference or make the input fail loud).
#
# The cases are local assets (not in git and not in the package build).  Which
# cases run depends on the test tier (conf_tier(), CONTRIBUTING.md):
#   none   R CMD check / CRAN, or no tests/conformance/: skipped
#   smoke  local devtools::test() (default): conf_smoke_units(), about 300 cases,
#          one per function / format matrix, per function directory and per area
#   full   SASONR_CONFORMANCE=full: every case.  The release check is
#          tools/run_conformance.R; this tier only mirrors it inside testthat.
conf_dir <- conf_root()
conf_units <- if (is.null(conf_dir) || conf_tier() == "none") list() else conf_units_for_tier(conf_dir, conf_tier())

# the verdict of one case result (conf_run_case() or one element of conf_run_matrix())
conf_expect_case <- function(r) {
  # known differences are not allowed: a difference is fixed or the input fails loud
  expect_length(r$meta$known_fail %||% character(), 0L)
  v <- conf_case_verdict(r)
  # no gold, or fail loud (also where SAS stopped with an ERROR): not a silent error
  if (v == "skip") skip(paste(r$status, r$msg, sep = ": "))
  expect_false(r$status == "lenient", info = "SAS reported an error but sasonr ran without one")
  expect_false(r$status %in% c("error", "sas_error_rerror"), info = paste(r$status, r$msg, sep = ": "))
  if (v == "pass") {
    succeed()
    return(invisible())
  }
  bad <- names(r$layers)[!r$layers]
  for (L in bad) expect_true(FALSE, label = paste0("L-", toupper(L), ": ", r$diffs[[L]]))
  succeed()
}

test_that("offline conformance cases are available", {
  skip_if(is.null(conf_dir), "tests/conformance/ not present (local conformance assets)")
  skip_if(conf_tier() == "none", "conformance tier 'none' (R CMD check)")
  skip_if_not_installed("haven")
  expect_gt(length(conf_units), 0)
})

for (u in conf_units) {
  rel <- substring(u$case, nchar(conf_dir) + 2L)
  if (is.null(u$ids)) {
    test_that(paste("SAS conformance:", rel), {
      skip_if_not_installed("haven")
      conf_expect_case(conf_run_case(u$case))
    })
  } else {
    # matrix program: the selected cases run together (one sas_run)
    rr <- NULL
    for (id in u$ids) {
      test_that(paste("SAS conformance:", file.path(rel, id)), {
        skip_if_not_installed("haven")
        if (is.null(rr)) rr <<- conf_run_matrix(u$case, only = if (u$subset) u$ids)
        conf_expect_case(rr[[id]])
      })
    }
  }
}
