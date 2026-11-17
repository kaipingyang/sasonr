test_that("sas_check() lists every unsupported feature with its step and line", {
  chk <- sas_check("
data a; x = 1; output; x = 2; output; run;
proc rank data=a out=b; var x; run;
data c; set a; link sub; return; sub: y = 1; return; run;
data d; set c; z = 2; run;
proc gplot data=a; plot x*x; run;
")
  expect_s3_class(chk, "sasonr_check")
  expect_named(chk, c("step", "line", "col", "severity", "feature", "message"))
  expect_equal(nrow(chk), 3L)
  expect_equal(chk$step, c(2L, 3L, 5L))
  expect_equal(chk$line, c(3L, 4L, 6L))
  expect_equal(chk$severity, rep("error", 3))
  expect_equal(chk$feature[c(1, 3)], c("PROC RANK", "PROC GPLOT"))
  expect_match(chk$feature[2], "LINK")
  expect_output(print(chk), "This program uses 3 unsupported features:")
  expect_output(print(chk), "- PROC RANK (line 3)", fixed = TRUE)
})

test_that("sas_check() finds nothing in a supported program", {
  chk <- sas_check("data a; x = 1; run; proc print data=a; run;")
  expect_equal(nrow(chk), 0L)
  expect_output(print(chk), "No unsupported features found.", fixed = TRUE)
})

test_that("a step that uses a macro variable set at run time is reported as info", {
  chk <- sas_check("
data _null_; call symputx('ds', 'a'); run;
data b; set &ds; run;
", work = list(a = data.frame(x = 1)))
  expect_equal(nrow(chk), 1L)
  expect_equal(chk$severity, "info")
  expect_equal(chk$step, 2L)
  expect_match(chk$feature, "DS")
  expect_output(print(chk), "Checked when the program runs:")
})

test_that("sas_check() runs nothing: WORK passed in is unchanged and nothing is logged", {
  w <- list(a = data.frame(x = 1:2))
  expect_silent(chk <- sas_check("data a; x = 5; run; proc print data=a; run; %put hello;", work = w))
  expect_equal(w, list(a = data.frame(x = 1:2)))
  expect_equal(nrow(chk), 0L)
})

test_that("sas_check() reports a SASHELP member missing from the SASHELP directory", {
  withr::local_options(sasonr.sashelp_dir = tempfile("nosashelp"))
  code <- "
data a; x = 1; run;
proc sql;
  create table b as select * from sashelp.class;
quit;
"
  chk <- sas_check(code)
  expect_equal(nrow(chk), 1L)
  expect_equal(chk$line, 4L)
  expect_match(chk$feature, "SASHELP.CLASS (not in the SASHELP directory", fixed = TRUE)
  run_msg <- tryCatch(sas_run(code), sasonr_unsupported = conditionMessage)
  expect_equal(chk$message, run_msg)
})

test_that("sas_check() reports an INTCK / INTNX interval literal that is not supported", {
  code <- "
data a;
  d = '01jan2020'd;
  n = intck('weekday7w', d, d + 10);
  m = intnx('month', d, 1);
run;
"
  chk <- sas_check(code)
  expect_equal(nrow(chk), 1L)
  expect_equal(chk$line, 4L)
  expect_equal(chk$severity, "error")
  expect_match(chk$feature, "INTCK interval 'weekday7w' (weekend days)", fixed = TRUE)
  expect_error(sas_run(code), "INTCK interval 'weekday7w'", class = "sasonr_unsupported")
  expect_equal(nrow(sas_check("data a; d = 1; m = intnx('month', d, 1); n = intck('qtr', d, 400); run;")), 0L)
})

test_that("sas_check() reports an ERROR whose effect on WORK cannot be derived from the step", {
  code <- "
data a; x = 1; run;
proc means data=a noprint; var x; output out=s mean=m; bogus; run;
"
  withr::local_options(sasonr.on_error = "sas")
  chk <- sas_check(code)
  expect_equal(nrow(chk), 1L)
  expect_equal(chk$line, 3L)
  expect_match(chk$feature, "the data sets that this ERROR leaves in WORK", fixed = TRUE)
  expect_error(sas_run(code), "the data sets that this ERROR leaves in WORK", class = "sasonr_unsupported")
  # an ERROR whose effect is known is not a finding
  expect_equal(nrow(sas_check("data a; x = 1; y = ; run;")), 0L)
})
