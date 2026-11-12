# sas_run(on_error = "sas"): SAS batch behaviour after an ERROR (spec 01 §4.7,
# SAS gold tests/conformance/datastep/errors)

run_sas_mode <- function(code) {
  withr::local_options(sasonr.step_notes = TRUE)
  sas_run(code, on_error = "sas")
}

test_that("a compile ERROR puts the rest of the program in syntax-check mode", {
  r <- run_sas_mode("
data a; do i = 1 to 3; x = i * 2; output; end; run;
data b; set a; y = x + ; run;
data c; set a; z = x * 10; run;
%put syserr=&syserr syscc=&syscc;
")
  expect_identical(r$status, "error")
  expect_identical(r$error_line, 3L)
  expect_identical(nrow(r$work$a), 3L)
  expect_identical(names(r$work$b), c("i", "x", "y"))
  expect_identical(nrow(r$work$b), 0L)
  expect_identical(names(r$work$c), c("i", "x", "z"))
  expect_identical(nrow(r$work$c), 0L)
  expect_true("NOTE: The SAS System stopped processing this step because of errors." %in% r$log)
  expect_true("NOTE: Due to ERROR(s) above, SAS set option OBS=0, enabling syntax check mode." %in% r$log)
  expect_true("NOTE: The data set WORK.C has 0 observations and 3 variables." %in% r$log)
  expect_true("syserr=3 syscc=1012" %in% r$log)
})

test_that("a failing step does not replace an existing data set", {
  r <- run_sas_mode("data a; do i = 1 to 4; output; end; run; data a; set a; j = i * ; run;")
  expect_identical(nrow(r$work$a), 4L)
  expect_true("WARNING: Data set WORK.A was not replaced because this step was stopped." %in% r$log)
})

test_that("an ERROR in PROC SQL sets NOEXEC for that PROC SQL only", {
  r <- run_sas_mode("
data a; x = 1; output; x = 2; output; run;
proc sql; create table q1 as select x, nosuchcol from a; create table q2 as select x from a; quit;
data c; set a; run;
%put syserr=&syserr syscc=&syscc;
")
  expect_identical(r$status, "error")
  expect_false("q1" %in% names(r$work))
  expect_false("q2" %in% names(r$work))
  expect_identical(nrow(r$work$c), 2L)
  expect_true("NOTE: Statement not executed due to NOEXEC option." %in% r$log)
  expect_true("syserr=0 syscc=1012" %in% r$log)
})

test_that("global statements and macro code still run after an ERROR", {
  r <- run_sas_mode("data b; x = nosuchfn(2); run; %let after = yes; %put after=&after; data _null_; put 'not written'; run;")
  expect_true("after=yes" %in% r$log)
  expect_false("not written" %in% r$log)
  expect_true(any(startsWith(r$log, "ERROR 68-185: The function NOSUCHFN is unknown")))
})

test_that("on_error = 'abort' keeps the classed condition; fail loud always aborts", {
  expect_error(sas_run("data b; y = x + ; run;", on_error = "abort"), class = "sasonr_syntax_error")
  expect_error(sas_run("data b; x = 1; run; proc nosuchproc; run;", on_error = "sas"), class = "sasonr_unsupported")
  r <- sas_run("options nosyntaxcheck; data b; y = x + ; run; data c; z = 1; run;", on_error = "sas")
  expect_identical(nrow(r$work$c), 1L)
  r <- sas_run("data a; x = 1; run;", on_error = "sas")
  expect_identical(r$status, "ok")
})
