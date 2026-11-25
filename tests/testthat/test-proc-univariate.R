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
  fails("proc univariate data=d noprint; var v; output out=s mean=m skewness=sk; run;")
  fails("proc univariate data=d noprint; var v; output out=s n=n mean=mean css=css std=std; run;")
  fails("proc univariate data=d noprint; class v; var v; output out=s n=n; run;")
})
