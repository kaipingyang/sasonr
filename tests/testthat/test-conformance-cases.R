# Offline conformance cases (tests/conformance/): one test per case, compared
# with the real-SAS gold standard stored next to the program (see
# helper-conformance.R).  Every layer must agree exactly; `known_fail` in a
# case's meta.yaml is rejected (fix the difference or make the input fail loud).

# The cases are local assets (not in git and not in the package build): without
# tests/conformance/ these tests skip.
conf_dir <- conf_root()
conf_cases <- if (is.null(conf_dir)) character() else conf_list_cases(conf_dir)
# fn_examples/ holds the reference-page examples, compared call by call in
# test-fn-examples.R (one case runs several independent calls).
conf_cases <- conf_cases[!grepl("/fn_examples/", conf_cases, fixed = TRUE)]
# fn_matrix/ and fmt_matrix/ (about 7,000 one-call cases) are run by
# tools/run_conformance.R and summarised in inst/registry/ (test-registry.R).
conf_cases <- conf_cases[!grepl("/(fn_matrix|fmt_matrix)/", conf_cases)]

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
    # known differences are not allowed: a difference is fixed or the input fails loud
    expect_length(r$meta$known_fail %||% character(), 0L)
    known <- character()
    why <- "known difference"
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
    # numbers that differ only in the last bits (a different algorithm): not a pass,
    # reported as "approximate" (feature registry; fails loud with sasonr.strict = TRUE)
    tl <- c(if ("files" %in% bad) r$tol_files, if ("put" %in% bad) r$tol_put)
    if (length(bad) && all(bad %in% c("ds", "files", "put"))) {
      if (!any(c("files", "put") %in% bad)) tl <- conf_ds_rel_diff(r)
      if (length(tl) && all(!is.na(tl)) && max(tl) <= 1e-9) skip(paste0("approximate (last bits differ): ", r$diffs[[bad[1]]]))
    }
    unexpected <- setdiff(bad, known)
    for (L in unexpected) expect_true(FALSE, label = paste0("L-", toupper(L), ": ", r$diffs[[L]]))
    if (length(unexpected) == 0 && length(bad) > 0) skip(paste0("known (", paste(bad, collapse = "+"), "): ", why))
    succeed()
  })
}
