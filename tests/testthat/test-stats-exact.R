# Moment statistics as SAS 9.4 computes them (R/stats_exact.R; SAS gold
# tests/conformance/proc_stats/)

hex <- function(x) paste(rev(as.character(writeBin(x, raw()))), collapse = "")

# SAS 9.4 gold values, printed with `put ... hex16.` by the probe programs
# (local TS1M2; the same numbers are in tests/conformance/proc_stats/).
unhex <- function(h) {
  unname(vapply(h, function(s) {
    readBin(as.raw(strtoi(substring(s, seq(1, 15, 2), seq(2, 16, 2)), 16L)), "double", 1,
            size = 8, endian = "big")
  }, 0))
}

test_that("the emulated fused multiply-add is exactly rounded", {
  expect_identical(stats_fma(0.1, 10, -1), 5.551115123125783e-17)
  expect_identical(stats_fma(3, 1 / 3, -1), -2^-54) # 3 * (1/3 rounded) - 1, exactly
  set.seed(1)
  a <- runif(200, -1e3, 1e3)
  b <- runif(200, -1e3, 1e3)
  p <- a * b
  # a * b - round(a * b) is the exact rounding error of the product
  e <- stats_fma(a, b, -p)
  expect_true(all(abs(e) <= abs(p) * 2^-53))
})

test_that("sequential sums are plain double additions", {
  x <- c(1e16, 1, 1, -1e16)
  expect_identical(stats_seq_sum(x), Reduce(`+`, x))
  expect_identical(stats_seq_sum(numeric()), 0)
})

test_that("PROC SQL MEAN uses the first value as shift and an FMA", {
  # SAS 9.4: mean of 9259.2549 and -628322.4 is C112E46E4A4A8C15 (sum / 2 is ...16)
  m <- stats_sql_moment("mean", c(9259.2549, -628322.4))
  expect_identical(hex(m), "c112e46e4a4a8c15")
  expect_identical(stats_sql_moment("css", c(2, 4, 6)), 8)
  expect_identical(stats_sql_moment("uss", c(2, 4, 6)), 56)
})

test_that("order-free domain: integer values with exact sums", {
  expect_true(stats_order_free("sum", c(1, 2, 3)))
  expect_false(stats_order_free("sum", c(0.1, 0.2)))
  expect_false(stats_order_free("css", c(1, 2, 3)))
})

test_that("PROC SQL GROUP BY on data not flagged as sorted fails loud for order-dependent statistics", {
  data <- "data a; input g x; datalines;\n1 0.1\n2 0.2\n1 0.7\n;\nrun;\n"
  q <- "proc sql; create table s as select g, sum(x) as s from a group by g; quit;"
  expect_error(sas_run(paste0(data, q)), class = "sasonr_unsupported")
  ints <- "data a; input g x; datalines;\n1 1\n2 2\n1 7\n;\nrun;\n"
  expect_identical(sas_run(paste0(ints, q))$work$s$s, c(8, 2))
  sorted <- sas_run(paste0(data, "proc sort data=a; by g; run;\n", q))
  expect_identical(sorted$work$s$s, c(0.1 + 0.7, 0.2))
})

test_that("PROC MEANS CSS: shifted sums finalised about the reported mean", {
  # SAS 9.4: data constant to within 1E-14 relative give the uncorrected
  # sum of squares (the mean is taken to be the first value)
  x <- c(1, 1 + 2^-52, 1 + 3 * 2^-52)
  expect_identical(hex(stats_means_css(x)), "39a4000000000000")
  # otherwise sdd - md * (2 * sd - n * md) with md = SUM/n - c0
  expect_identical(hex(stats_means_css(c(0.1, 0.2, 0.3))), "3f947ae147ae147a")
  expect_identical(hex(stats_means_css(c(1, 2, 3, 4))), "4014000000000000")
  # one value: no corrected sum of squares
  expect_identical(stats_means_css(5), 0)
})

test_that("PROC MEANS STD is fuzzed to 0 for constant data", {
  # SAS 9.4: a variance within 2^-86 of MEAN^2 gives STD = 0 ...
  expect_identical(stats_means_std(10 * 2^-104, 1), 0)
  # ... but STDERR (which is not fuzzed) stays: sqrt(VAR / n)
  expect_identical(sqrt((10 * 2^-104 / 2) / 3), unhex("3cb4a7e9cb8a3491"))
  # ordinary data: sqrt(VAR)
  expect_identical(stats_means_std(2, 1), sqrt(2))
  expect_true(is.na(stats_means_std(NA_real_, 1)))
})

test_that("PROC MEANS: CSS, VAR, STD, STDERR, CV and USS are bit-identical to SAS", {
  code <- paste0(
    "data a; input g x; datalines;\n",
    "1 0.1\n1 0.2\n1 0.3\n2 1\n2 2\n2 3\n2 4\n",
    "3 1\n3 1.0000000000000002\n3 1.0000000000000006\n;\nrun;\n",
    "proc means data=a noprint; by g; var x;",
    " output out=o n=n mean=m css=css var=v std=sd stderr=se cv=cv uss=uss; run;"
  )
  r <- sas_run(code) # strict mode: every statistic below must be verified
  o <- r$work$o
  expect_identical(o$m, unhex(c("3fc999999999999b", "4004000000000000", "3ff0000000000001")))
  expect_identical(o$css, unhex(c("3f947ae147ae147a", "4014000000000000", "39a4000000000000")))
  expect_identical(o$v, unhex(c("3f847ae147ae147a", "3ffaaaaaaaaaaaab", "3994000000000000")))
  expect_identical(o$sd, unhex(c("3fb9999999999999", "3ff4a7e9cb8a3491", "0000000000000000")))
  expect_identical(o$se, unhex(c("3fad8f7208e6b82c", "3fe4a7e9cb8a3491", "3cb4a7e9cb8a3491")))
  expect_identical(o$cv, unhex(c("4048ffffffffffff", "4049d1e43e6cc1b5", "0000000000000000")))
  expect_identical(o$uss, unhex(c("3fc1eb851eb851ed", "403e000000000000", "4008000000000004")))
})

test_that("MOD and MODZ: exact remainders inside the verified domain, fail loud outside", {
  expect_identical(sas_f_mod(10, 3), 1)
  expect_identical(sas_f_mod(5.5, 2), 1.5)
  expect_identical(sas_f_mod(0.3, 0.1), 0)
  expect_error(sas_f_mod(10, 0.3), class = "sasonr_unsupported")
  # SAS 9.4: MODZ(0.3, 0.1) is 3FB9999999999998
  expect_identical(hex(sas_f_modz(0.3, 0.1)), "3fb9999999999998")
  expect_error(sas_f_modz(1.2, -0.006), class = "sasonr_unsupported")
})

test_that("HARMEAN sums the reciprocals in argument order", {
  expect_identical(sas_f_harmean(0.1, 0.2, 0.3), 3 / Reduce(`+`, 1 / c(0.1, 0.2, 0.3)))
})
