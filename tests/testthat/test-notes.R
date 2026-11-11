run_notes <- function(code, level = "auto") {
  old <- options(sasonr.step_notes = TRUE)
  on.exit(options(old))
  suppressWarnings(sas_run(code, level = level))
}

test_that("DATA step NOTEs: data set size, observations read with WHERE", {
  r <- run_notes(c("data a; do i = 1 to 5; output; end; run;",
                   "data b; set a; where i > 2; run;"))
  expect_equal(r$log, c(
    "NOTE: The data set WORK.A has 5 observations and 1 variables.",
    "NOTE: There were 3 observations read from the data set WORK.A.",
    "      WHERE i>2;",
    "NOTE: The data set WORK.B has 3 observations and 1 variables."
  ))
})

test_that("observations read stop at STOP; PROC SORT NODUPKEY NOTE", {
  r <- run_notes(c("data a; input k; datalines;", "1", "1", "2", ";", "run;",
                   "data b; set a; if _n_ = 2 then stop; run;",
                   "proc sort data=a out=c nodupkey; by k; run;"))
  expect_true("NOTE: There were 2 observations read from the data set WORK.A." %in% r$log)
  expect_true("NOTE: 1 observations with duplicate key values were deleted." %in% r$log)
  expect_true("NOTE: The data set WORK.C has 2 observations and 1 variables." %in% r$log)
})

test_that("missing values generated: counts per place, compile-time folding", {
  code <- c("data _null_;", "  y = . + 3;", "  x = 2 * y;", "run;")
  for (lv in c("auto", "L3")) {
    r <- run_notes(code, lv)
    expect_equal(r$log, c(
      "NOTE: Missing values were generated as a result of performing an operation on missing values.",
      "      Each place is given by: (Number of times) at (Line):(Column).",
      "      2:9  ",
      "NOTE: Missing values were generated as a result of performing an operation on missing values.",
      "      Each place is given by: (Number of times) at (Line):(Column).",
      "      1 at 3:9  "
    ))
  }
})

test_that("guarded missing-value counts are exact (vectorised steps re-run row by row)", {
  code <- c("data a; input x; datalines;", "1", ".", ".", ";", "run;",
            "data b; set a; if x = . then y = x + 1; z = x * 2; run;")
  r1 <- run_notes(code)
  r3 <- run_notes(code, "L3")
  expect_equal(r1$log, r3$log)
  expect_true("      2 at 7:36   2 at 7:47  " %in% r1$log)
})

test_that("uninitialized variables and macro WARNINGs are logged", {
  r <- run_notes(c("data a; x = y + 1; run;", "%put &nope;"))
  expect_equal(r$log[1], "NOTE: Variable y is uninitialized.")
  expect_true("WARNING: Apparent symbolic reference NOPE not resolved." %in% r$log)
})

test_that("OPTIONS NONOTES suppresses NOTEs", {
  r <- run_notes(c("options nonotes;", "data a; x = 1; run;"))
  expect_length(r$log, 0)
})

test_that("variable attributes are kept for export", {
  r <- run_notes(c("data a; length c $5 n 4; format d date9.; informat d date9.; label c = 'Char';",
                   "c = 'x'; n = 1; d = 0; run;",
                   "proc sort data=a out=b; by c; run;"))
  dir <- tempfile()
  sas_export_work(r, dir)
  attrs <- utils::read.csv(file.path(dir, "__attr.csv"), colClasses = "character")
  expect_equal(attrs$length, rep(c("5", "4", "8"), 2))
  expect_equal(attrs$format, rep(c("", "", "DATE9."), 2))
  expect_equal(attrs$informat, rep(c("", "", "DATE9."), 2))
  expect_equal(attrs$label, rep(c("Char", "", ""), 2))
  expect_equal(readLines(file.path(dir, "A.csv")), c("c,n,d", "x,1,01JAN1960"))
})
