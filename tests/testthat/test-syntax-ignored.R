# A statement ignored after a syntax ERROR (76-322): the step compiles on, and
# the later statements give their own messages.

si_log <- function(code) {
  withr::local_options(sasonr.step_notes = TRUE)
  l <- as.character(sas_log(sas_run(code, on_error = "sas")))
  l[grepl("^(ERROR|WARNING|NOTE)|^ +[0-9]+:", l)]
}

test_that("PUT() without a format is ERROR 388-185 + 76-322 and the target stays numeric", {
  l <- si_log("data a;\n  x = put(1);\n  y = x + 1;\nrun;")
  expect_identical(l[1:2], c("ERROR 388-185: Expecting an arithmetic operator.",
                             "ERROR 76-322: Syntax error, statement will be ignored."))
  expect_false(any(grepl("converted", l)))
})

test_that("a later character format on the ignored target gives its WARNING and ERROR 48-59 at its line", {
  l <- si_log("data a;\n  r = put(1);\n  n = length(r);\n  put r $quote10.;\nrun;")
  expect_identical(l[1:4], c("ERROR 388-185: Expecting an arithmetic operator.",
                             "ERROR 76-322: Syntax error, statement will be ignored.",
                             "WARNING: Variable r has already been defined as numeric.",
                             "ERROR 48-59: The format QUOTE was not found or could not be loaded."))
  expect_identical(l[5:6], c("NOTE: Numeric values have been converted to character values at the places given by: (Line):(Column).",
                             "      3:14    "))
})

test_that("$w. on a numeric variable writes w. after the WARNING", {
  r <- sas_run("data a; x = 1; put 'v=' x $10.; run;")
  expect_true("v=         1" %in% as.character(sas_log(r)))
})
