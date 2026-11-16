# sas_run(on_error = "sas"): the batch-mode error handling after an ERROR.

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
  expect_true("NOTE: sasonr stopped processing this step because of errors." %in% msgs(r))
  expect_true("NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode." %in% msgs(r))
  expect_true("NOTE: The data set WORK.C has 0 observations and 3 variables." %in% msgs(r))
  expect_true("syserr=3 syscc=1012" %in% msgs(r))
})

test_that("a failing step does not replace an existing data set", {
  r <- run_sas_mode("data a; do i = 1 to 4; output; end; run; data a; set a; j = i * ; run;")
  expect_identical(nrow(r$work$a), 4L)
  expect_true("WARNING: Data set WORK.A was not replaced because this step was stopped." %in% msgs(r))
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
  expect_true("NOTE: Statement not executed due to NOEXEC option." %in% msgs(r))
  expect_true("syserr=0 syscc=1012" %in% msgs(r))
})

test_that("global statements and macro code still run after an ERROR", {
  r <- run_sas_mode("data b; x = nosuchfn(2); run; %let after = yes; %put after=&after; data _null_; put 'not written'; run;")
  expect_true("after=yes" %in% msgs(r))
  expect_false("not written" %in% msgs(r))
  expect_true(any(startsWith(msgs(r), "ERROR 68-185: The function NOSUCHFN is unknown")))
})

test_that("on_error = 'abort' keeps the classed condition; fail loud always aborts", {
  expect_error(sas_run("data b; y = x + ; run;", on_error = "abort"), class = "sasonr_syntax_error")
  expect_error(sas_run("data b; x = 1; run; proc nosuchproc; run;", on_error = "sas"), class = "sasonr_unsupported")
  r <- sas_run("options nosyntaxcheck; data b; y = x + ; run; data c; z = 1; run;", on_error = "sas")
  expect_identical(nrow(r$work$c), 1L)
  r <- sas_run("data a; x = 1; run;", on_error = "sas")
  expect_identical(r$status, "ok")
})

test_that("an assignment without its semicolon: the rest of the statement adds its variables", {
  r <- run_sas_mode("data one;\n  x = 1\n  y = z + 2;\nrun;")
  expect_identical(r$status, "error")
  expect_identical(names(r$work$one), c("x", "y", "z"))
  expect_identical(nrow(r$work$one), 0L)
  expect_true(paste("WARNING: The data set WORK.ONE may be incomplete.  When this step was stopped there were",
                    "0 observations and 3 variables.") %in% msgs(r))
  expect_identical(names(run_sas_mode("data one;\n  x = 1\n  y = 2;\nrun;")$work$one), c("x", "y"))
  expect_identical(names(run_sas_mode("data one;\n  x = 1\n  y;\nrun;")$work$one), c("x", "y"))
  expect_identical(names(run_sas_mode("data one;\n  x = 1\n  2 w;\nrun;")$work$one), c("x", "w"))
  # the rest starts a statement: nothing of it is added
  expect_identical(names(run_sas_mode("data one;\n  x = 1\n  if x then w = 3;\nrun;")$work$one), "x")
  # a rest with a character constant is not supported
  expect_error(run_sas_mode("data one;\n  x = 1\n  y = 'abc';\nrun;"), class = "sasonr_unsupported")
})

test_that("missing semicolons: remainders with IF / THEN, two names, parentheses, several assignments", {
  vars <- function(rest) names(run_sas_mode(paste0("data one;\n  x = 1\n  ", rest, ";\nrun;"))$work$one)
  expect_identical(vars("y = 2 if z"), c("x", "y", "z"))
  expect_identical(vars("y = 2 then w"), c("x", "y", "w"))
  expect_identical(vars("y z"), "x")
  expect_identical(vars("y = (z) * 2"), c("x", "y", "z"))
  expect_identical(vars("y = 2\n  z = 3\n  w = 4"), c("x", "y", "z", "w"))
  # an array name or a statement keyword in the remainder is not supported
  expect_error(run_sas_mode("data one;\n  array arr{2} a1 a2;\n  x = 1\n  arr = 2;\nrun;"), class = "sasonr_unsupported")
  expect_error(run_sas_mode("data one;\n  x = 1\n  y = 2 output;\nrun;"), class = "sasonr_unsupported")
})

# the steps that a stopped step leaves to run: the NOTE that starts
# syntax-check mode, and the observations of the next DATA step
sc_note <- "NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode."
sc_pre <- "data a;\n  input x;\n  datalines;\n1\n2\n3\n;\nrun;\n"
sc_after <- function(code) run_sas_mode(paste0(sc_pre, code, "data check;\n  set a;\nrun;\n"))

test_that("a stopped DATA _NULL_ step does not start syntax-check mode", {
  r <- sc_after("data _null_;\n  set nosuch;\n  put x;\nrun;\n")
  expect_identical(r$status, "error")
  expect_false(sc_note %in% msgs(r))
  expect_identical(nrow(r$work$check), 3L)
})

test_that("a stopped PROC MEANS, SUMMARY, FREQ or PRINT does not start syntax-check mode", {
  for (code in c("proc means data=a;\n  frobnicate;\n  var x;\nrun;\n",
                 "proc summary data=a;\n  frobnicate;\n  var x;\nrun;\n",
                 "proc freq data=a;\n  frobnicate;\n  tables x;\nrun;\n",
                 "proc print data=a;\n  format x $8.;\nrun;\n")) {
    r <- sc_after(code)
    expect_identical(r$status, "error", info = code)
    expect_false(sc_note %in% msgs(r), info = code)
    expect_identical(nrow(r$work$check), 3L, info = code)
  }
  # the numbered ERROR of PROC MEANS: no blank line after it
  r <- sc_after("proc means data=a;\n  frobnicate;\n  var x;\nrun;\n")
  i <- match("ERROR 180-322: Statement is not valid or it is used out of proper order.", r$log)
  expect_identical(r$log[i + 1L], "11           var x;")
})

test_that("a stopped PROC SORT or DATA step starts syntax-check mode, one NOTE per step", {
  for (code in c("proc sort data=a out=sorted;\n  by nosuchvar;\nrun;\n",
                 "proc sort data=a;\n  by nosuchvar;\nrun;\n",
                 "data b;\n  set a;\n  y = x + ;\n  z = x * ;\nrun;\n")) {
    r <- sc_after(code)
    expect_identical(sum(msgs(r) == sc_note), 1L, info = code)
    expect_identical(nrow(r$work$check), 0L, info = code)
  }
})

test_that("an ERROR in OPTIONS: every later step has 0 observations", {
  r <- run_sas_mode("options ls=abc;\ndata a;\n  x = 1; output; x = 2; output;\nrun;\ndata check;\n  set a;\nrun;\n")
  expect_identical(sum(msgs(r) == sc_note), 1L)
  expect_identical(c(nrow(r$work$a), nrow(r$work$check)), c(0L, 0L))
})

test_that("another stopped PROC, or the OUT= data sets of a stopped PROC MEANS or FREQ, fail loud", {
  for (code in c("proc transpose data=a out=t;\n  frobnicate;\nrun;\n",
                 "proc means data=a;\n  frobnicate;\n  output out=m mean=mx;\nrun;\n",
                 "proc freq data=a;\n  frobnicate;\n  tables x / out=f;\nrun;\n")) {
    expect_error(sc_after(code), "syntax-check mode.*SAS line", class = "sasonr_unsupported", info = code)
  }
})

test_that("what a stopped PROC MEANS leaves unknown fails loud", {
  # SYSERR and SYSCC after a stopped PROC MEANS are not implemented
  expect_error(sas_run("data a; x = 1; run;\nproc means data=a frobnicate; var x; run;\n%put &syscc;", on_error = "sas"),
               class = "sasonr_unsupported")
  # an ERROR found after the statements were read (a variable that is not there)
  expect_error(sas_run("data a; x = 1; run;\nproc means data=a; var nosuchvar; run;", on_error = "sas"),
               class = "sasonr_unsupported")
  # _NULL_ together with another output name
  expect_error(sas_run("data a _null_; x = ; run;", on_error = "sas"), class = "sasonr_unsupported")
})

test_that("a PROC SQL ERROR while a statement runs starts syntax-check mode at QUIT", {
  r <- sas_run("data t; x = 1; output; output; run;\nproc sql;\n  create table r1 as select (select x from t) as y from t;\nquit;\ndata after; z = 1; run;",
               on_error = "sas")
  expect_true(any(grepl("enabling syntax check mode", sas_log(r, echo = FALSE))))
  expect_identical(nrow(r$work$after), 0L)
})
