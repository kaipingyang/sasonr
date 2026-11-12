# Round 13 (D): R errors in the internal corpus became passes, SAS ERRORs or
# classed `sasonr_unsupported` conditions.  Invented data only.

put_lines <- function(r) r$log[!grepl("^(NOTE|WARNING|ERROR)", r$log)]

test_that("DIF of a character value converts it to numeric", {
  old <- options(sasonr.step_notes = TRUE)
  on.exit(options(old))
  r <- sas_run("data _null_; c = 'abc'; d = dif(c); put d= _error_=; run;", on_error = "sas")
  expect_true(any(grepl("Character values have been converted to numeric", r$log)))
  expect_true(any(grepl("d=. _ERROR_=1", r$log, fixed = TRUE)))
})

test_that("a character user format on a numeric PUT item is SAS ERROR 48-59", {
  r <- sas_run("proc format; value $yn 'Y'='Yes'; run;\ndata _null_; x = 5; put x $yn.; run;", on_error = "sas")
  expect_equal(r$status, "error")
  expect_match(r$errors[1], "format YN was not found", fixed = TRUE)
})

test_that("PUT(char, name.) applies the character user format $NAME", {
  r <- sas_run("proc format; value $armf 'A'='Placebo'; run;\ndata _null_; a = put('A', armf.); put a=; run;", on_error = "sas")
  expect_true("a=Placebo" %in% r$log)
})

test_that("%SYSFUNC keeps blanks quoted with %STR()", {
  r <- sas_run("%let r = %sysfunc(tranwrd(a b c,%str( ),_));\n%put r=&r;", on_error = "sas")
  expect_true("r=a_b_c" %in% r$log)
})

test_that("FILE into a directory that does not exist is a SAS ERROR, not an R error", {
  expect_error(sas_file_write(file.path(tempfile(), "no", "x.txt"), "a"), class = "sasonr_runtime_error")
})

test_that("%SUBSTR with a length past the end warns and returns the rest", {
  old <- options(sasonr.step_notes = TRUE)
  on.exit(options(old))
  r <- suppressWarnings(sas_run("%put [%substr(HELLO,2,%length(HELLO))] [%substr(ABCDEFG,2,5-2)];", on_error = "sas"))
  expect_true("[ELLO] [BCD]" %in% r$log)
  expect_true(any(grepl("Argument 3 to macro function %SUBSTR is out of range", r$log, fixed = TRUE)))
})

test_that("date, time and datetime literals in %SYSEVALF and %SYSFUNC", {
  r <- sas_run("%let a = %sysevalf('15JAN2020'd + 1);\n%let b = %sysfunc(day('15JAN2020'd));\n%let c = %sysevalf('12:00't);\n%let d = %sysevalf('15JAN2020:12:00:00'dt);\n%put &a &b &c &d;", on_error = "sas")
  expect_true("21930 15 43200 1894708800" %in% r$log)
})

test_that("a very long %DO loop fails loud", {
  expect_error(sas_run("%macro m; %do i = 1 %to 20000; %end; %mend; %m", on_error = "sas"), class = "sasonr_unsupported")
})

test_that("macro-time errors fail loud instead of escaping as R errors", {
  expect_error(sas_run("%let a = %eval(9999999999 * 9999999999);", on_error = "sas"), class = "sasonr_unsupported")
  expect_error(sas_run("%let a = %eval(1 +);", on_error = "sas"), class = "sasonr_unsupported")
})

test_that("hexadecimal numeric constants", {
  r <- sas_run("data _null_; a = 0fx; b = 1Fx; c = 9x; d = 0b0ax; max = 100; xx = max + 1; put a= b= c= d= xx=; run;", on_error = "sas")
  expect_true("a=15 b=31 c=9 d=2826 xx=101" %in% r$log)
})

test_that("colon modifiers on mnemonic operators and IN:", {
  r <- sas_run("data _null_; x = 'TUMOR'; q = (x ge: 'TU'); e = (x eq: 'TUX'); y1 = x in: ('TU', 'ZZ'); y2 = x not in: ('TU'); put q= e= y1= y2=; run;", on_error = "sas")
  expect_true("q=1 e=0 y1=1 y2=0" %in% r$log)
  expect_error(sas_run("data _null_; array a{2} (1 2); f = 1 in a; run;", on_error = "sas"), class = "sasonr_unsupported")
})

test_that("negative array bounds", {
  r <- sas_run("data _null_; array b{-2:2} b1-b5 (10 20 30 40 50); x = b{-2}; y = b{2}; l = lbound(b); put x= y= l=; run;", on_error = "sas")
  expect_true("x=10 y=50 l=-2" %in% r$log)
})

test_that("an array of existing character variables without $ is a character array", {
  r <- sas_run("data _null_; length a $4 b $6; array ch{*} a b; a = 'x'; b = 'y'; n = vname(ch{2}); put n=; run;", on_error = "sas")
  expect_true(any(grepl("n=b", r$log, fixed = TRUE)))
})

test_that("OBS= / FIRSTOBS= values with K suffix, MIN and MAX", {
  r <- sas_run("data f; do i = 1 to 5; output; end; run;
data a; set f(obs=2k); run; data b; set f(firstobs=max); run; data c; set f(obs=min); run;")
  expect_equal(nrow(r$work$a), 5L)
  expect_equal(nrow(r$work$b), 0L)
  expect_equal(nrow(r$work$c), 0L)
})

test_that("a zero-padded numbered data set list keeps the padding", {
  r <- sas_run("data d08; y = 8; run; data d09; y = 9; run; data d10; y = 10; run; data all; set d08-d10; run;", on_error = "sas")
  expect_equal(r$work$all$y, c(8, 9, 10))
})

test_that("MIN / MAX word operators in PROC SQL fail loud", {
  expect_error(sas_run("data e; a = 1; b = 2; run; proc sql; select * from e where (a min b) = 1; quit;"),
               class = "sasonr_unsupported")
})

test_that("WHERE= on a variable not on the input data set is a SAS ERROR", {
  r <- sas_run("data d; x = 1; run; data b; set d(where=(nope > 1)); run;", on_error = "sas")
  expect_equal(r$status, "error")
  expect_match(r$errors[1], "Variable nope is not on file WORK.D.", fixed = TRUE)
  r <- sas_run("data d; x = 1; run; proc print data=d(where=(nope > 1)); run;", on_error = "sas")
  expect_equal(r$status, "error")
})

test_that("RENAME= onto an existing variable is a SAS ERROR", {
  r <- sas_run("data d; a = 1; b = 2; run; data _null_; set d(rename=(a=b)); run;", on_error = "sas")
  expect_equal(r$status, "error")
  expect_match(r$errors[1], "Variable b already exists on file WORK.D.", fixed = TRUE)
})

test_that("an OF list that matches no variable is SAS ERROR 71-185", {
  r <- sas_run("data _null_; y = 1; s = sum(of zz:); run;", on_error = "sas")
  expect_equal(r$status, "error")
  expect_match(r$errors[1], "ERROR 71-185", fixed = TRUE)
})
