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

test_that("missing values generated: only the operation that produced the missing value is a place", {
  # verified against SAS 9.4 TS1M7 (the local install): a chained expression has
  # one place, at its first missing-producing operation; division, power and a
  # unary minus of an already generated missing result are not counted again,
  # while a unary minus of a missing variable is (at the operand's position)
  code <- c("data x;",
            "  a = .; b = .; c = .; d = 2;",
            "  x1 = a + b;",
            "  x2 = a + b + c;",
            "  x3 = (a + b) / 3;",
            "  x4 = a / 3;",
            "  x5 = a * d;",
            "  x6 = a + d;",
            "  x7 = -a;",
            "  x8 = a ** 2;",
            "  x9 = a > d;",
            "run;")
  for (lv in c("auto", "L3")) {
    r <- run_notes(code, lv)
    expect_equal(r$log, c(
      "NOTE: Missing values were generated as a result of performing an operation on missing values.",
      "      Each place is given by: (Number of times) at (Line):(Column).",
      "      1 at 3:10   1 at 4:10   1 at 5:11   1 at 6:10   1 at 7:10   1 at 8:10   1 at 9:9   1 at 10:10  ",
      "NOTE: The data set WORK.X has 1 observations and 13 variables."
    ))
  }
})

test_that("missing-value places: ** and function results are not generated missing values (merge of dev d95db7c)", {
  # SAS 9.4 TS1M7 gold datastep/dev_merge/mv_places_chain: +, -, *, / and the unary
  # minus skip an operand that is a generated missing value, ** always records its
  # place, and the result of ** or of a function is not a generated missing value
  code <- c("data x;",
            "  a = .; b = .; d = 2;",
            "  y1 = (a + b) ** 2;",
            "  y2 = (a ** 2) + 1;",
            "  y3 = -(a ** 2);",
            "  y4 = -abs(a);",
            "  y5 = - - a;",
            "  y6 = -a + d;",
            "run;")
  for (lv in c("auto", "L3")) {
    r <- run_notes(code, lv)
    expect_equal(r$log[3], "      1 at 3:11   1 at 3:16   1 at 4:11   1 at 4:17   2 at 5:12   2 at 6:9   1 at 7:12   1 at 8:9  ",
                 label = lv)
  }
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
