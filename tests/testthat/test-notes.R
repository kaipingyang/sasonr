run_notes <- function(code, level = "auto") {
  old <- options(sasonr.step_notes = TRUE)
  on.exit(options(old))
  suppressWarnings(sas_run(code, level = level))
}

test_that("DATA step NOTEs: data set size, observations read with WHERE", {
  r <- run_notes(c("data a; do i = 1 to 5; output; end; run;",
                   "data b; set a; where i > 2; run;"))
  expect_equal(msgs(r), c(
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
  expect_true("NOTE: There were 2 observations read from the data set WORK.A." %in% msgs(r))
  expect_true("NOTE: 1 observations with duplicate key values were deleted." %in% msgs(r))
  expect_true("NOTE: The data set WORK.C has 2 observations and 1 variables." %in% msgs(r))
})

test_that("missing values generated: counts per place, compile-time folding", {
  code <- c("data _null_;", "  y = . + 3;", "  x = 2 * y;", "run;")
  for (lv in c("auto", "L3")) {
    r <- run_notes(code, lv)
    expect_equal(msgs(r), c(
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
  expect_equal(msgs(r1), msgs(r3))
  expect_true("      2 at 7:36   2 at 7:47  " %in% msgs(r1))
})

test_that("missing values generated: only the operation that produced the missing value is a place", {
  # A chained expression has
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
    expect_equal(msgs(r), c(
      "NOTE: Missing values were generated as a result of performing an operation on missing values.",
      "      Each place is given by: (Number of times) at (Line):(Column).",
      "      1 at 3:10   1 at 4:10   1 at 5:11   1 at 6:10   1 at 7:10   1 at 8:10   1 at 9:9   1 at 10:10  ",
      "NOTE: The data set WORK.X has 1 observations and 13 variables."
    ))
  }
})

test_that("missing-value places: ** and function results are not generated missing values", {
  # +, -, *, / and the unary
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
    expect_equal(msgs(r)[3], "      1 at 3:11   1 at 3:16   1 at 4:11   1 at 4:17   2 at 5:12   2 at 6:9   1 at 7:12   1 at 8:9  ",
                 label = lv)
  }
})

test_that("missing-value places: inside function arguments every operation records its place", {
  code <- c("data x;",
            "  a = .;",
            "  y1 = (a - 1) / 7;",
            "  y2 = int((a - 1) / 7);",
            "  y3 = int(-(a - 1));",
            "run;")
  for (lv in c("auto", "L3")) {
    r <- run_notes(code, lv)
    expect_equal(msgs(r)[3], "      1 at 3:11   1 at 4:8   1 at 4:15   1 at 4:20   1 at 5:8   2 at 5:16  ", label = lv)
  }
})

test_that("uninitialized variables and macro WARNINGs are logged", {
  r <- run_notes(c("data a; x = y + 1; run;", "%put &nope;"))
  expect_equal(msgs(r)[1], "NOTE: Variable y is uninitialized.")
  expect_true("WARNING: Apparent symbolic reference NOPE not resolved." %in% msgs(r))
})

test_that("OPTIONS NONOTES suppresses NOTEs", {
  r <- run_notes(c("options nonotes;", "data a; x = 1; run;"))
  expect_length(msgs(r), 0)
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

test_that("WHERE NOTEs: MIN / MAX, one-value IN lists, chains, prefix signs", {
  where_line <- function(w) {
    r <- run_notes(c("data a; a = 1; b = 2; c = 3; run;", paste0("data b; set a; where ", w, "; run;")))
    sub("^ +", "", msgs(r)[startsWith(msgs(r), "      WHERE ")])
  }
  expect_identical(where_line("a min b < c"), "WHERE c>a><b;")
  expect_identical(where_line("a max b = c"), "WHERE c=a<>b;")
  expect_identical(where_line("a + b in (3)"), "WHERE a+b=3;")
  expect_identical(where_line("a < b < c"), "WHERE a<b and b<c;")
  expect_identical(where_line("a ^= b < c"), "WHERE a not = b and b<c;")
  expect_identical(where_line("a & + b < c"), "WHERE a and (b<c);")
  expect_identical(where_line("- - a + b < c"), "WHERE c>a+b;")
})

test_that("WHERE conditions write no missing-value NOTE and no NOTE for invalid ** arguments", {
  r <- run_notes(c("data a; a = 0; b = .; c = 0; v = 1; run;",
                   "data w; set a; where (a * b is missing) = v and (a ** c in (.)) = v; e = _error_; run;"))
  expect_false(any(grepl("Missing values were generated", msgs(r), fixed = TRUE)))
  expect_false(any(grepl("exponential operator", msgs(r), fixed = TRUE)))
  expect_identical(nrow(r$work$w), 1L)
  expect_identical(r$work$w$e, 0)
  # in an assignment the same operations are reported
  r <- run_notes("data a; a = 0; b = .; c = 0; x = a * b; y = a ** c; run;")
  expect_true(any(startsWith(msgs(r), "NOTE: Missing values were generated")))
  expect_true(any(startsWith(msgs(r), "NOTE: Invalid argument(s) to the exponential operator")))
})

test_that("WHERE conditions drop function data-error NOTEs; a division by zero fails loud", {
  withr::local_options(sasonr.strict = FALSE)
  r <- run_notes(c("data a; x = -1; v = .; run;", "data w; set a; where sqrt(x) = v; e = _error_; run;"))
  expect_false(any(grepl("Invalid argument to function SQRT", msgs(r), fixed = TRUE)))
  expect_identical(nrow(r$work$w), 1L)
  expect_identical(r$work$w$e, 0)
  expect_error(run_notes(c("data a; x = 0; v = .; run;", "data w; set a; where 1 / x = v; run;")),
               class = "sasonr_unsupported")
})

test_that("WHERE= options and PROC WHERE drop data-error NOTEs; compile-time NOTEs in WHERE fail loud", {
  r <- run_notes(c("data a; a = 0; b = .; v = 1; run;", "data w; set a(where = ((a * b is missing) = v)); run;",
                   "proc print data=a; where (a * b is missing) = v; run;"))
  expect_false(any(grepl("Missing values were generated", msgs(r), fixed = TRUE)))
  expect_identical(nrow(r$work$w), 1L)
  for (w in c("x = 1/0", "x = 0 ** 0", "x = . + 1")) {
    expect_error(run_notes(c("data a; x = 1; run;", paste0("data w; set a; where ", w, "; run;"))), class = "sasonr_unsupported")
  }
})

test_that("an ERROR of a function in a WHERE condition fails loud", {
  withr::local_options(sasonr.strict = FALSE)
  expect_error(suppressWarnings(run_notes(c("data a; nm = ' '; run;", "data w; set a; where symget(nm) = 'x'; run;"))),
               class = "sasonr_unsupported")
})

test_that("WHERE NOTEs: constant BETWEEN bounds in ascending order, colon comparisons in chains", {
  where_line <- function(w) {
    r <- run_notes(c("data a; a = 2; s = 'ab'; t = 'a'; u = 'b'; v = 1; run;", paste0("data b; set a; where ", w, "; run;")))
    sub("^ +", "", msgs(r)[startsWith(msgs(r), "      WHERE ")])
  }
  expect_identical(where_line("a between 3 and 1"), "WHERE (a>=1 and a<=3);")
  expect_identical(where_line("a not between 3 and 1"), "WHERE not (a>=1 and a<=3);")
  expect_identical(where_line("(s =: t < u) = v"), "WHERE v=s=:t and t<u;")
})

test_that("an iteration that reads nothing writes its observation, then the step stops", {
  dl <- function(...) c("  datalines;", ..., ";", "run;")
  loop_note <- "NOTE: DATA STEP stopped due to looping."
  r <- run_notes(c("data a;", "  if _n_ = 1 then input x;", dl("7", "8")))
  expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 2 observations and 1 variables."))
  expect_identical(r$work$a$x, c(7, NA))
  r <- run_notes(c("data a;", "  if _n_ <= 2 then do; input x; end;", dl("1", "2", "3")))
  expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 3 observations and 1 variables."))
  r <- run_notes(c("data a;", "  x = 1; if 0 then input y;", dl("1")))
  expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 1 observations and 2 variables."))
  r <- run_notes(c("data b; x = 1; output; x = 2; output; run;", "data a; if _n_ = 1 then do; set b; end; run;"))
  expect_identical(msgs(r)[-1], c(loop_note, "NOTE: There were 1 observations read from the data set WORK.B.",
                                  "NOTE: The data set WORK.A has 2 observations and 1 variables."))
  # a read on every iteration, or no reading statement: no NOTE
  r <- run_notes(c("data a;", "  do i = 1 to 2; input x; end;", dl("1", "2")))
  expect_identical(msgs(r), "NOTE: The data set WORK.A has 1 observations and 2 variables.")
  r <- run_notes("data a; x = 1; run;")
  expect_identical(msgs(r), "NOTE: The data set WORK.A has 1 observations and 1 variables.")
})

test_that("an iteration that reads nothing: any read condition, OUTPUT, DELETE, STOP", {
  withr::local_options(sasonr.step_notes = TRUE)
  dl <- function(...) c("  datalines;", ..., ";", "run;")
  loop_note <- "NOTE: DATA STEP stopped due to looping."
  # the first iteration without a read stops the step (lines 4 and 5 unread)
  r <- run_notes(c("data a;", "  if _n_ ne 3 then input x;", dl("1", "2", "3", "4", "5")))
  expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 3 observations and 1 variables."))
  expect_identical(r$work$a$x, c(1, 2, NA))
  # the explicit OUTPUT statements of that iteration run
  r <- run_notes(c("data a;", "  if _n_ = 1 then input x; output; output;", dl("1", "2")))
  expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 4 observations and 1 variables."))
  # STOP ends the step first: no NOTE
  r <- run_notes(c("data a;", "  if _n_ = 1 then input x; if _n_ > 1 then stop;", dl("1", "2")))
  expect_identical(msgs(r), "NOTE: The data set WORK.A has 1 observations and 1 variables.")
  # DELETE (or a subsetting IF) drops the observation; the NOTE is written
  for (del in c("if _n_ > 1 then delete;", "if _n_ = 1;")) {
    r <- run_notes(c("data a;", paste("  if _n_ = 1 then input x;", del), dl("1", "2")))
    expect_identical(msgs(r), c(loop_note, "NOTE: The data set WORK.A has 1 observations and 1 variables."))
  }
  # a read that hits the end of the data stops as usual
  r <- run_notes(c("data a;", "  input x; if _n_ = 2 then input y;", dl("1", "2")))
  expect_identical(msgs(r), "NOTE: The data set WORK.A has 1 observations and 2 variables.")
  expect_identical(r$work$a$x, 1)
})

test_that("an iteration that reads nothing fails loud for the shapes not known", {
  dl <- function(...) c("  datalines;", ..., ";", "run;")
  # a reading statement in a DO loop: the same rule
  r <- run_notes(c("data a;", "  if _n_ = 1 then do i = 1 to 1; input x; end;", dl("1", "2", "3")))
  expect_identical(msgs(r), c("NOTE: DATA STEP stopped due to looping.",
                              "NOTE: The data set WORK.A has 2 observations and 2 variables."))
  expect_identical(r$work$a$x, c(1, NA))
  for (body in c("  select; when (_n_ = 1) input x; otherwise; end;",
                 "  if _n_ = 1 then input x; if _n_ = 2 then return;")) {
    expect_error(run_notes(c("data a;", body, dl("1", "2", "3"))), class = "sasonr_unsupported")
  }
})

test_that("each SET statement has its own reader: IF 0 THEN SET, conditional SET, two SET statements, DOW loop", {
  src <- "data s; length name $8; name = 'alpha'; v = 10; output; name = 'beta'; v = 20; output; run;"
  # never executed: the variables only, no read NOTE
  r <- run_notes(c(src, "data o; if 0 then set s; x = 1; run;"))
  expect_identical(msgs(r)[-1], c("NOTE: DATA STEP stopped due to looping.",
                                  "NOTE: The data set WORK.O has 1 observations and 3 variables."))
  expect_identical(r$work$o$v, NA_real_)
  # executed once: the values read are retained
  r <- run_notes(c(src, "data o; if _n_ = 1 then set s; x = _n_; run;"))
  expect_identical(r$work$o$name, c("alpha", "alpha"))
  expect_identical(msgs(r)[3], "NOTE: There were 1 observations read from the data set WORK.S.")
  # two SET statements read in parallel; the step stops at the first end
  r <- run_notes(c("data a; x = 1; output; x = 2; output; run;",
                   "data b; x = 3; output; x = 4; output; x = 5; output; run;",
                   "data c; set a; set b; run;"))
  expect_identical(r$work$c$x, c(3, 4))
  expect_identical(msgs(r)[3:5], c("NOTE: There were 2 observations read from the data set WORK.A.",
                                   "NOTE: There were 2 observations read from the data set WORK.B.",
                                   "NOTE: The data set WORK.C has 2 observations and 1 variables."))
  # DOW loop: BY in the DO loop, LAST. tested before the first read
  r <- run_notes(c("data h; g = 'a'; v = 1; output; v = 2; output; g = 'b'; v = 10; output; run;",
                   "data w; do until (last.g); set h; by g; t = sum(t, v); end; run;"))
  expect_identical(r$work$w$t, c(3, 10))
  # DO WHILE (NOT LAST.): LAST. starts at 1, the loop never runs; the BY
  # reader has read one observation ahead
  r <- run_notes(c("data h; g = 'a'; v = 1; output; run;",
                   "data w; do while (not last.g); set h; by g; end; run;"))
  expect_identical(msgs(r)[-1], c("NOTE: DATA STEP stopped due to looping.",
                                  "NOTE: There were 1 observations read from the data set WORK.H.",
                                  "NOTE: The data set WORK.W has 1 observations and 2 variables."))
})

test_that("MERGE without BY pairs observations; a SET value takes the PDV length", {
  r <- sas_run("data a; x = 1; output; x = 2; output; x = 3; output; run; data b; y = 10; output; y = 20; output; run; data m; merge a b; run;")
  expect_identical(r$work$m$y, c(10, 20, NA))
  r <- sas_run("data a; length s $3; s = 'abc'; run; data b; length s $10; set a; t = s || 'X'; run; data c; length s $2; set a; run;")
  expect_identical(r$work$b$t, "abc       X")
  expect_identical(r$work$c$s, "ab")
})

test_that("END= and IN= start at 0, FIRST. and LAST. at 1, before the first read", {
  r <- sas_run("data a; g = 'x'; output; g = 'y'; output; run; data o; z = e + 1; pf = first.g; pi = ina; set a(in = ina) end = e; by g; run;")
  expect_identical(r$work$o$z, c(1, 1))
  expect_identical(r$work$o$pf, c(1, 1))
  expect_identical(r$work$o$pi, c(0, 1))
})
