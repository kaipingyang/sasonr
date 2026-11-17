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
