squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x[nzchar(x)]
}

test_that("PROC SORT is stable, puts missing values first, supports DESCENDING", {
  code <- "
data d; input g $ v; datalines;
b 2
a .
b 1
a 3
. 5
a 1
;
run;
proc sort data=d out=s; by g descending v; run;
proc sort data=d; by v; run;
"
  res <- sas_run(code)
  expect_equal(res$work$s$g, c("", "a", "a", "a", "b", "b"))
  expect_equal(res$work$s$v, c(5, 3, 1, NA, 2, 1))
  expect_equal(res$work$d$v, c(NA, 1, 1, 2, 3, 5))
  expect_equal(res$work$d$g, c("a", "b", "a", "b", "a", ""))
})

test_that("PROC SORT NODUPKEY keeps the first observation of each key; WHERE filters", {
  code <- "
data d; input id x; datalines;
2 20
1 10
2 21
1 11
3 30
;
run;
proc sort data=d out=u nodupkey; by id; run;
proc sort data=d out=w; where x > 15; by descending id; run;
"
  res <- sas_run(code)
  expect_equal(res$work$u$x, c(10, 20, 30))
  expect_equal(res$work$w$x, c(30, 20, 21))
})

test_that("PROC PRINT renders a SAS-style listing", {
  code <- "
data graded;
  input USUBJID $ LBSTRESN LBORNRHI;
  ratio = LBSTRESN / LBORNRHI;
  datalines;
01-001 45 40
01-002 130 40
01-003 38 .
;
run;
proc print data=graded; var USUBJID LBSTRESN ratio; run;
title 'My title';
proc print data=graded noobs; where LBSTRESN > 40; run;
"
  res <- sas_run(code)
  expect_equal(squish(res$listing), c(
    "Obs USUBJID LBSTRESN ratio",
    "1 01-001 45 1.125", "2 01-002 130 3.250", "3 01-003 38 .",
    "My title",
    "USUBJID LBSTRESN LBORNRHI ratio",
    "01-001 45 40 1.125", "01-002 130 40 3.250"
  ))
})

test_that("PROC PRINT Obs shows physical observation numbers; decimals are aligned", {
  code <- "
data d; input name $ age; datalines;
Al 14
Bo 13
Cy 13
Di 14
;
run;
proc print data=d(firstobs=2); where age = 13 or name = 'Di'; run;
data f; x = 0.1 + 0.2; y = 1 / 3; z = 10 / 4; run;
proc print data=f; run;
"
  res <- sas_run(code)
  expect_equal(squish(res$listing), c("Obs name age", "3 Cy 13", "4 Di 14", "Obs x y z", "1 0.3 0.33333 2.5"))
})

test_that("PROC PRINT uses stored formats; empty data sets print nothing", {
  code <- "
data d; dt = '15jan2024'd; format dt date9.; n = 5; run;
data e; set d; if n > 10; run;
proc print data=d noobs; run;
proc print data=e; run;
"
  expect_equal(squish(sas_run(code)$listing), c("dt n", "15JAN2024 5"))
})

test_that("unsupported PROCs, options and statements fail loud", {
  d <- "data a; x = 1; run;"
  expect_error(sas_run(paste(d, "proc means data=a; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc print data=a; by x; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc print data=a label; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc sort data=a dupout=b; by x; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc sort data=a; by y; run;")), class = "sasonr_runtime_error")
  expect_error(sas_run(paste(d, "proc print data=a; where y > 1; run;")), class = "sasonr_runtime_error")
})

test_that("data steps after PROC SORT see the sorted data and its metadata", {
  code <- "
data a; input g $ v; datalines;
b 1
a 2
a 3
;
run;
proc sort data=a; by g; run;
data s; set a; by g; if first.g then n = 0; n + v; if last.g then output; run;
"
  res <- sas_run(code)
  expect_equal(res$work$s$n, c(5, 1))
  expect_equal(res$work$s, sas_run(code, level = "L3")$work$s)
})
