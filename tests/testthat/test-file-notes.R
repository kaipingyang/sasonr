# Behaviour verified against SAS 9.4 in the offline conformance cases
# (tests/conformance/functions/...): FILE statement, LINESIZE, log NOTEs.

# run in a fresh temporary working directory (removed when the test ends)
withr_local_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(d, .local_envir = env)
  normalizePath(d)
}

# log NOTEs are switched off for most tests (helper-options.R)
notes_on <- function(env = parent.frame()) withr::local_options(sasonr.step_notes = TRUE, .local_envir = env)

test_that("FILE writes PUT output to an external file; MOD appends", {
  d <- withr_local_dir()
  r <- sas_run("data _null_; x = 1; file 'out.txt'; put 'a' x; run;
data _null_; file 'out.txt' mod lrecl=1000; put 'b'; run;")
  expect_equal(readLines(file.path(d, "out.txt")), c("a1", "b"))
  expect_false(any(c("a1", "b") %in% r$log))
  # FILE LOG keeps the log as destination
  expect_true("c" %in% sas_run("data _null_; file log; put 'c'; run;")$log)
})

test_that("FILE inside a condition, FILE PRINT and unknown options fail loud", {
  withr_local_dir()
  expect_error(sas_run("data _null_; if 1 then file 'a.txt'; put 'x'; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; file print; put 'x'; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; file 'a.txt' recfm=v; put 'x'; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; put 'x'; file 'a.txt'; run;"), class = "sasonr_unsupported")
})

test_that("OPTIONS LINESIZE= sets the log line size (PUT wrapping, NOTE layout)", {
  notes_on()
  long <- strrep("x", 100)
  code <- sprintf("data _null_; a = '%s'; b = '%s'; put a b; run;", long, long)
  expect_length(sas_run(code)$log, 2)
  expect_length(sas_run(paste("options linesize=max;", code))$log, 1)
  log <- sas_run("options linesize=max; data _null_; x = input('abc', 8.); run;")$log
  expect_true(any(startsWith(log, "NOTE: Mathematical operations could not be performed at the following places. The results of the operations have been set to missing values.")))
})

test_that("implicit conversions of function arguments write the SAS NOTEs", {
  notes_on()
  log <- sas_run("data _null_;\n  r = catx(1,'a','b');\n  s = round('3.5');\nrun;")$log
  k <- which(startsWith(log, "NOTE: Numeric values have been converted to character values"))
  expect_length(k, 1)
  expect_equal(trimws(log[k + 1]), "2:12")
  k <- which(startsWith(log, "NOTE: Character values have been converted to numeric values"))
  expect_equal(trimws(log[k + 1]), "3:13")
  log <- sas_run("data _null_;\n  r = input(123, 8.);\nrun;")$log
  expect_true(any(startsWith(log, "NOTE: Numeric values have been converted to character values")))
})

test_that("missing arguments of ROUND/INT/INTCK/INTNX are counted in the missing-value NOTE", {
  notes_on()
  log <- sas_run("data _null_;\n  r = round(5.5, .);\n  s = intck('month', ., 1);\nrun;")$log
  k <- which(startsWith(log, "NOTE: Missing values were generated"))
  expect_length(k, 1)
  expect_match(log[k + 2], "1 at 2:7")
  expect_match(log[k + 2], "1 at 3:7")
})

test_that("INPUT() with invalid data is listed in the mathematical-operations NOTE", {
  notes_on()
  log <- sas_run("data _null_;\n  x = input('abc', 8.);\nrun;")$log
  expect_true(any(log == "NOTE: Invalid argument to function INPUT at line 2 column 7."))
  k <- which(startsWith(log, "NOTE: Mathematical operations could not be performed"))
  expect_match(paste(log[k:(k + 3)], collapse = " "), "1 at 2:7")
})

test_that("an invalid PRX pattern is an ERROR, as in SAS", {
  notes_on()
  log <- sas_run("data _null_;\n  rx = prxparse('/(abc/');\nrun;")$log
  expect_true("ERROR: The regular expression passed to the function PRXPARSE contains a syntax error." %in% log)
  expect_true(any(startsWith(log, "NOTE: Argument 1 to function PRXPARSE('/(abc/') at line 2 column 8 is invalid.")))
})

test_that("%SYSFUNC(CATX) with an empty argument reports the %SYSEVALF ERROR and continues", {
  notes_on()
  r <- sas_run("%let r = %sysfunc(catx(%str( ),a,,c)); %put [&r];")
  expect_equal(r$log, c("ERROR: %SYSEVALF function has no expression to evaluate.", "[a c]"))
})

test_that("the PDV dump writes a blank character value as one blank and wraps at the line size", {
  notes_on()
  log <- sas_run("data _null_; a = ' '; b = substr('abc', 9); run;")$log
  expect_true("a=  b=  _ERROR_=1 _N_=1" %in% log)
  items <- c(paste0("s=", strrep("ab", 100)), "r= ", "_ERROR_=1", "_N_=1")
  # SAS 9.4 continues a value wider than the line on the next lines (cut every LINESIZE columns)
  expect_equal(dump_wrap(items, 132L), c(substr(items[1], 1, 132), paste(substring(items[1], 133), "r=  _ERROR_=1 _N_=1")))
  expect_equal(dump_wrap(c("a=1", "b=2"), 132L), "a=1 b=2")
})

test_that("PROC EXPORT records longer than LRECL=32767 are cut (sas_export_work)", {
  d <- withr_local_dir()
  r <- sas_run("data a; length s $32767; s = repeat('x', 32765); n = 1; run;")
  sas_export_work(r, "ds")
  x <- readLines(file.path(d, "ds", "A.csv"))
  expect_equal(nchar(x[2]), 32767L)
  expect_true(endsWith(x[2], ","))
})

test_that("HEX16. informat reads IEEE 754 bit patterns; invalid digits are invalid data", {
  notes_on()
  r <- sas_run("data a; x = input('3FF8000000000000', hex16.); y = input('8000000000000000', hex16.);
  z = input('400921FB54442D18', hex16.); i = input('FF', hex2.); run;")
  expect_equal(r$work$a$x, 1.5)
  expect_identical(1 / r$work$a$y, -Inf)
  expect_equal(r$work$a$z, pi)
  expect_equal(r$work$a$i, 255)
  log <- sas_run("data _null_;\n  x = input('3FF800000000000G', hex16.);\nrun;")$log
  expect_true(any(startsWith(log, "NOTE: Invalid argument to function INPUT")))
  expect_error(sas_run("data _null_; x = input('3FF8', hex16.); run;"), class = "sasonr_unsupported")
})

test_that("ABS/SIGN return an ordinary missing for special missing; all-missing MIN/SUM are counted", {
  notes_on()
  r <- sas_run("data a;\n  x = .A;\n  y = abs(x);\n  s = sum(., .);\n  t = sum(0, .);\nrun;")
  expect_equal(sas_miss_code(r$work$a$y), 1L)
  k <- which(startsWith(r$log, "NOTE: Missing values were generated"))
  expect_match(r$log[k + 2], "1 at 3:7")
  expect_match(r$log[k + 2], "1 at 4:7")
  expect_false(grepl("5:7", r$log[k + 2]))
})

test_that("PROC SQL writes no missing-value NOTE", {
  notes_on()
  r <- sas_run("data t; x = .; run; proc sql; create table u as select x + 1 as y from t; quit;")
  expect_false(any(startsWith(r$log, "NOTE: Missing values were generated")))
})
