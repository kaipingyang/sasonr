# A word, a number or an unresolved macro call at the start of an open-code
# statement: ERROR 180-322 and syntax-check mode for the next DATA step.

oce_log <- function(code) {
  withr::local_options(sasonr.step_notes = TRUE, sasonr.on_error = "sas")
  as.character(sas_log(sas_run(code)))
}

test_that("an unknown word in open code writes ERROR 180-322 and starts syntax-check mode", {
  lg <- oce_log("data a; x=1; run;\nfoo;\ndata b; y=2; run;")
  k <- which(lg == "2          foo;")
  expect_identical(lg[k + 1:4], c("           ___", "           180", "", "ERROR 180-322: Statement is not valid or it is used out of proper order."))
  expect_true("NOTE: The data set WORK.B has 0 observations and 1 variables." %in% lg)
})

test_that("a number statement and an unresolved macro call with ; are the same ERROR", {
  expect_true(any(grepl("^ERROR 180-322", oce_log("data a; x=1; run;\n1;\ndata b; y=2; run;"))))
  lg <- suppressWarnings(oce_log("data a; x=1; run;\n%nosuch(1, 2);\ndata b; y=2; run;"))
  w <- which(lg == "WARNING: Apparent invocation of macro NOSUCH not resolved.")
  expect_identical(lg[w + 1:3], c("2          %nosuch(1, 2);", "           _", "           180"))
})

test_that("under NOSYNTAXCHECK the ERROR starts no syntax-check mode", {
  lg <- oce_log("options nosyntaxcheck;\ndata a; x=1; run;\nfoo;\ndata b; y=2; run;")
  expect_true("NOTE: The data set WORK.B has 1 observations and 1 variables." %in% lg)
})

test_that("numbered TITLE / FOOTNOTE statements are not unknown words", {
  expect_false(any(grepl("ERROR 180-322", tryCatch(oce_log("footnote2 'x';\ndata a; x=1; run;"), error = function(e) ""))))
})
