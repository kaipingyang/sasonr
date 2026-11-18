# Literal assignments before the read (SET/INPUT): the vectorised form runs
# them right after the read; the row-wise (L3) PDV loop runs them at the top
# of every iteration. Both give the same data sets and the same log.

withr::local_options(sasonr.step_notes = TRUE)

step_levels <- function(res) vapply(res$translation$steps, `[[`, "", "level")
step_reasons <- function(res, k) res$translation$steps[[k]]$reasons

expect_before_same <- function(code, levels) {
  auto <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(step_levels(auto), levels)
  expect_identical(auto$work, rows$work)
  expect_identical(auto$log, rows$log)
  invisible(auto)
}

test_that("character and numeric literals before INPUT, with labels and @@", {
  code <- "
data d;
  label id = 'Subject' param = 'Parameter' aval = 'Value';
  param = 'Subject Died';
  flag = 1;
  input id $ aval grp $ @@;
datalines;
101 0 a 102 . b 103 1 a
104 1 b
;
run;
"
  res <- expect_before_same(code, "L1")
  expect_equal(names(res$work$d), c("id", "param", "aval", "flag", "grp"))
  expect_equal(res$work$d$param, rep("Subject Died", 4))
  expect_equal(res$work$d$flag, rep(1, 4))
})

test_that("column input with short records and a special missing literal", {
  code <- "
data ce;
  length term $10;
  term = 'Seizure';
  pre = 'Y';
  m = .A;
  input id $ 1-3 occ $ 5 dtc $ 7-16;
datalines;
101 Y 2004-05-05
102 N
103
;
run;
"
  res <- expect_before_same(code, "L1")
  expect_equal(res$work$ce$term, rep("Seizure", 3))
})

test_that("literals before SET, with a later reassignment, BY and WHERE", {
  code <- "
data a; input g x; datalines;
1 10
1 11
2 12
;
data b;
  tag = 'raw';
  n0 = 0;
  set a;
  where x > 10;
  by g;
  if x > 11 then tag = 'big';
  f = first.g;
run;
data e;
  c = 'x';
  set a;
  if x > 100;
run;
"
  res <- expect_before_same(code, c("L1", "L2", "L1"))
  expect_equal(res$work$b$tag, c("raw", "big"))
  expect_equal(nrow(res$work$e), 0L)
})

test_that("statements before the read that are not literal assignments stay L3", {
  base <- "data a; input x; datalines;\n1\n2\n;\n"
  cases <- list(
    c("data b; y = x + 1; set a; run;", "assignment of an expression that is not a literal"),
    c("data b; put 'top'; set a; run;", "PUT statement"),
    c("data b; s + 1; set a; run;", "sum statement"),
    c("data b; x = 5; set a; run;", "assignment to retained variable x"),
    c("data b; retain r; r = 1; set a; run;", "assignment to retained variable r"),
    c("data b; c = 1; input @c y; datalines;\n5\n;\nrun;", "used by the read")
  )
  for (cs in cases) {
    code <- paste0(base, cs[1])
    auto <- sas_run(code)
    rows <- sas_run(code, level = "L3")
    expect_equal(step_levels(auto)[2], "L3")
    expect_true(any(grepl(cs[2], step_reasons(auto, 2), fixed = TRUE)), info = cs[1])
    expect_identical(auto$work, rows$work)
    expect_identical(auto$log, rows$log)
  }
})
