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
  fails("proc univariate data=d; by v; var v; run;")
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

test_that("PCTLPTS= with PCTLPRE= and PCTLNAME=: names, order and labels", {
  d <- "data d; input a b; datalines;\n1 10\n2 20\n3 30\n4 40\n;\nrun;\n"
  r <- sas_run(paste0(d, "proc univariate data=d noprint; var a b; output out=o pctlpts=2.5 33 pctlpre=a_ b_; run;"))
  expect_identical(names(r$work$o), c("a_2_5", "b_2_5", "a_33", "b_33"))
  expect_identical(r$meta$o$labels[["a_2_5"]], "the   2.5000 percentile, a")
  r <- sas_run(paste0(d, "proc univariate data=d noprint; var a; output out=o pctlpts=10 90 pctlpre=p_ pctlname=lo hi; run;"))
  expect_identical(names(r$work$o), c("p_lo", "p_hi"))
})

test_that("the printed output writes the five tables of each variable", {
  withr::local_options(sasonr.center = FALSE)
  lst <- sas_run("data d; input x; datalines;\n1\n2.5\n1000.25\n123456.789\n;\nrun; proc univariate data=d; var x; run;")$listing
  lst <- lst[nzchar(lst)]
  expect_true("Mean               31115.1348    Sum Observations    124460.539" %in% lst)
  expect_true("Median     501.38     Variance              3789990938" %in% lst)
  expect_true("Student's t    t   1.01084    Pr > |t|    0.3865" %in% lst)
  expect_true("75% Q3          62228.520" %in% lst)
  expect_true(" 123456.79        4         123456.79        4" %in% lst)
  lst <- sas_run("data d; input x; datalines;\n1\n2\n.\n.\n3\n3\n;\nrun; proc univariate data=d; var x; run;")$listing
  expect_true("      .           2       33.33      100.00" %in% lst)
  expect_false(any(grepl("Total", lst)))
  lst <- sas_run("data d; input x; datalines;\n1\n.\n.A\n3\n;\nrun; proc univariate data=d; var x; run;")$listing
  expect_true("      A           1       25.00       50.00" %in% lst)
  expect_true("  Total           2       50.00      100.00" %in% lst)
  lst <- sas_run("data d; input x; label x = 'Lab Value'; datalines;\n1\n2\n3\n;\nrun; proc univariate data=d; var x; run;")$listing
  expect_true("Variable:  x  (Lab Value)" %in% lst)
})
