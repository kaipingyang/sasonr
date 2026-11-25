# PROC UNIVARIATE, block U1: NOPRINT with OUTPUT OUT=.

univ_data <- "data d; input v; datalines;\n10\n20\n30\n40\n50\n60\n70\n80\n;\nrun;\n"

test_that("OUTPUT OUT= writes the statistics in the measured column order with their labels", {
  r <- sas_run(paste0(univ_data, "proc univariate data=d noprint; var v; ",
                      "output out=s n=n mean=mean median=median std=std p25=q1 p75=q3 min=min max=max; run;"))
  s <- r$work$s
  expect_identical(names(s), c("n", "mean", "std", "max", "q3", "median", "q1", "min"))
  expect_identical(s$n, 8)
  expect_identical(s$median, 45)
  expect_identical(s$q1, 25)
  m <- r$meta$s
  expect_identical(unname(unlist(m$labels[c("n", "q1")])), c("number of nonmissing values, v", "the lower quartile, v"))
})

test_that("PROC UNIVARIATE writes no observations-read NOTE", {
  withr::local_options(sasonr.step_notes = TRUE)
  lg <- as.character(sas_log(sas_run(paste0(univ_data, "proc univariate data=d noprint; var v; output out=s n=n; run;"))))
  expect_false(any(grepl("observations read from the data set WORK.D", lg, fixed = TRUE)))
  expect_true("NOTE: The data set WORK.S has 1 observations and 1 variables." %in% lg)
})

test_that("what U1 does not cover fails loud", {
  fails <- function(body) expect_error(sas_run(paste0(univ_data, body)), class = "sasonr_unsupported")
  fails("proc univariate data=d; var v; run;")
  fails("proc univariate data=d noprint; var v; output out=s mean=m gini=g; run;")
  fails("proc univariate data=d noprint; freq v; var v; output out=s n=n; run;")
})

test_that("the tests for location give the measured statistics and p values", {
  a <- sasonr:::univ_location_tests(c(30, 40))
  expect_equal(c(a$t, a$m, a$s, a$p_m, a$p_s), c(7, 1, 1.5, 0.5, 0.5))
  expect_equal(round(a$p_t, 4), 0.0903)
  b <- sasonr:::univ_location_tests(c(1, 2, NA, 4, NA, 6))
  expect_equal(c(b$m, b$s, b$p_m, b$p_s), c(2, 5, 0.125, 0.125))
  expect_equal(round(b$p_t, 4), 0.0609)
})

test_that("several analysis variables, BY and CLASS: columns by statistic then variable, one row per group", {
  d <- "data a; input g $ a b; datalines;\nx 1 10\nx 2 20\ny 3 30\ny 5 50\n;\nrun;\n"
  r <- sas_run(paste0(d, "proc univariate data=a noprint; var a b; output out=o mean=ma mb n=na nb; run;"))
  expect_identical(names(r$work$o), c("na", "nb", "ma", "mb"))
  r <- sas_run(paste0(d, "proc univariate data=a noprint; by g; var a; output out=o n=cn mean=cm; run;"))
  expect_identical(r$work$o$cm, c(1.5, 4))
  r <- sas_run(paste0(d, "proc univariate data=a noprint; class g; var a; output out=o mean=cm median=md; run;"))
  expect_identical(as.character(r$work$o$g), c("x", "y"))
  expect_identical(r$meta$o$memlabel, "Selected Summary Statistics (Percentile Definition=5)")
})

test_that("WEIGHT: weighted mean and a two-pass corrected sum of squares, by VARDEF=", {
  d <- "data w; input x wt; datalines;\n10 3\n20 1\n30 1\n;\nrun;\n"
  r <- sas_run(paste0(d, "proc univariate data=w noprint; var x; weight wt; output out=s n=n mean=mean css=css var=var; run;"))
  expect_identical(c(r$work$s$mean, r$work$s$css, r$work$s$var), c(16, 320, 160))
  expect_error(sas_run(paste0(d, "proc univariate data=w noprint; var x; weight wt; output out=s median=md; run;")),
               class = "sasonr_unsupported")
})
