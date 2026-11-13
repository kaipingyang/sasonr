# Round 13: display rounding from the ulp-by-ulp SAS 9.4 scans
# (tests/conformance/rounding/scan/), CONSTANT('EXACTINT', 1E300), MDY NOTEs.
hexd <- function(h) readBin(as.raw(strtoi(substring(h, seq(1, 15, 2), seq(2, 16, 2)), 16L)), "double", size = 8L, endian = "big")

test_that("0 decimals: half away from zero on fl(|x| * (1 + 2^-52))", {
  f <- function(x) trimws(sas_f_putn(x, "32."))
  expect_equal(f(hexd("4003FFFFFFFFFFFF")), "3") # 2.5 - 1 ulp: SAS 9.4 3
  expect_equal(f(hexd("4003FFFFFFFFFFFE")), "2") # 2.5 - 2 ulps: SAS 9.4 2
  expect_equal(f(hexd("400BFFFFFFFFFFFE")), "4") # 3.5 - 2 ulps: SAS 9.4 4 (mantissa 1.75: 2-ulp adjustment)
  expect_equal(f(hexd("4011FFFFFFFFFFFE")), "4") # 4.5 - 2 ulps: SAS 9.4 4
})

test_that("decimals: values 1 to 2 ulps below a half point fail loud (former silent results)", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  # SAS 9.4 writes 4.7 (the former rule wrote 4.8) and 0.1 for 0.049999999999999975 (former 0.0)
  expect_error(f(hexd("4012FFFFFFFFFFFF"), "32.1"), class = "sasonr_unsupported")
  expect_error(f(hexd("3FA9999999999996"), "32.1"), class = "sasonr_unsupported")
  expect_equal(trimws(f(hexd("4013000000000000"), "32.1")), "4.8")   # 4.75: SAS 9.4 4.8
  expect_equal(trimws(f(0.15, "32.1")), "0.2")                       # the double nearest to 0.15: SAS 9.4 0.2
  expect_equal(trimws(f(hexd("4012FFFFFFFFFFFC"), "32.1")), "4.7")   # 4.75 - 4 ulps: SAS 9.4 4.7
})

test_that("w.d shows at most 15 significant digits (former exact digits)", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(hexd("3FF0147AE147AE13"), "32.30"), "1.004999999999990000000000000000") # SAS 9.4
  expect_equal(f(hexd("3FF0147AE147AE14"), "32.30"), "1.005000000000000000000000000000") # 1.005: SAS 9.4
  expect_equal(f(hexd("3FF0147AE147AE12"), "32.16"), "              1.0049999999999900")
  expect_equal(trimws(f(2922024225.7520895, "32.7")), "2922024225.7520800")
  expect_error(f(0.285, "32.30"), class = "sasonr_unsupported") # below 1: the digit limit is not verified
  expect_equal(trimws(f(939586291501115008, "32.")), "939586291501115008") # integers: exact digits
  expect_equal(trimws(f(.Machine$double.xmax, "32.")), "1.7976931348623E308")
})

test_that("BESTw. above 16 columns: the first 15 digits of the display value, truncated", {
  f <- function(x) trimws(sas_f_putn(x, "BEST32."))
  expect_equal(f(hexd("3FF0147AE147AE13")), "1.00499999999999") # SAS 9.4
  expect_equal(f(hexd("3FF0147AE147AE14")), "1.005")            # SAS 9.4
  expect_equal(f(hexd("4012416C16C16C17")), "4.56388888888888") # former fail-loud window: SAS 9.4 truncates
})

test_that("E notation with 14 digits: truncated up to 0.888 of the 14th digit, fail loud above", {
  f <- function(x) sas_f_putn(x, "E32.")
  expect_equal(f(23082174.53712922), " 2.3082174537129000000000000E+07")
  expect_error(f(pi), class = "sasonr_unsupported")
})

test_that("CONSTANT('EXACTINT', 1E300): SAS 9.4 exception, the step stops with 0 observations", {
  r <- sas_run("data a; x = constant('EXACTINT', 1e300); run;")
  expect_true(any(grepl("ERROR:  An exception has been encountered.", r$log, fixed = TRUE)))
  expect_equal(nrow(r$work$a), 0L)
  expect_equal(names(r$work$a), "x")
  expect_error(sas_run("data a; do i = 1 to 2; x = constant('EXACTINT', 1e300); output; end; run;", on_error = "sas"),
               class = "sasonr_unsupported")
})

test_that("MDY: %SYSFUNC NOTE wrapped at LINESIZE, PROC SQL NOTE without arguments", {
  withr::local_options(list(sasonr.step_notes = TRUE))
  r <- sas_run("%let d = %sysfunc(mdy(2, 30, 2024));")
  # LINESIZE 132 (SAS 9.4 gold, opensas/opendisk): the line breaks after "have been set"
  expect_identical(r$log[1:2], c("NOTE: Mathematical operations could not be performed during %SYSFUNC function execution. The result of the operations have been set",
                                 "      to a missing value."))
  r <- sas_run("data one; x = 1; run; proc sql; create table t as select mdy(2, 30, 2024) as d from one; quit;")
  expect_true("NOTE: Invalid argument to function MDY. Missing values may be generated." %in% r$log)
})
