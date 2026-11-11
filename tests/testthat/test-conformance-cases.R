# Offline conformance cases (tests/conformance/): one test per case, compared
# with the real-SAS gold standard stored next to the program (see
# helper-conformance.R).  Known differences are declared in the case's
# meta.yaml (`known_fail`, `known_reason`) and reported as skips.

# The cases are local assets (not in git and not in the package build): without
# tests/conformance/ these tests skip.
conf_dir <- conf_root()
conf_cases <- if (is.null(conf_dir)) character() else conf_list_cases(conf_dir)

test_that("offline conformance cases are available", {
  skip_if(length(conf_cases) == 0, "tests/conformance/ not present (local conformance assets)")
  skip_if_not_installed("haven")
  expect_gt(length(conf_cases), 0)
})

for (case in conf_cases) {
  rel <- substring(case, nchar(conf_dir) + 2L)
  test_that(paste("SAS conformance:", rel), {
    skip_on_cran()
    skip_if_not_installed("haven")
    r <- conf_run_case(case)
    known <- r$meta$known_fail %||% character()
    why <- r$meta$known_reason %||% "known difference"
    if (r$status %in% c("no_program", "no_gold")) skip(r$status)
    if (r$status == "unsupported") {
      if ("unsupported" %in% known) {
        succeed()
        return(invisible())
      }
      skip(paste("unsupported:", r$msg))
    }
    expect_false(r$status == "lenient", info = "SAS reported an error but sasonr ran without one")
    expect_false(r$status == "error", info = r$msg)
    if (r$status == "both_error") {
      succeed()
      return(invisible())
    }
    bad <- names(r$layers)[!r$layers]
    unexpected <- setdiff(bad, known)
    for (L in unexpected) expect_true(FALSE, label = paste0("L-", toupper(L), ": ", r$diffs[[L]]))
    if (length(unexpected) == 0 && length(bad) > 0) skip(paste0("known (", paste(bad, collapse = "+"), "): ", why))
    succeed()
  })
}
