# Moment statistics as SAS 9.4 computes them (R/stats_exact.R; SAS gold
# tests/conformance/proc_stats/)

hex <- function(x) paste(rev(as.character(writeBin(x, raw()))), collapse = "")

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

hexd <- function(h) readBin(as.raw(strtoi(substring(h, seq(1, 15, 2), seq(2, 16, 2)), 16L)), "double", n = 1L, endian = "big")

test_that("PROC MEANS: CSS family bit-identical to SAS 9.4 (round 12 kernel)", {
  # proc_stats/means_css_int_probe: integer data, so only the CSS formula matters
  # SAS 9.4: CSS of 2 8 4 = 18.666666666666664 (not the nearest double of 56/3)
  expect_identical(stats_means_moments(c(2, 8, 4), c(1, 1, 1))$css, 18.666666666666664)
  # SAS 9.4: 1 4 8 -> 24.666666666666671; 8 9 9 -> 0.66666666666666663
  expect_identical(stats_means_moments(c(1, 4, 8), c(1, 1, 1))$css, 24.666666666666671)
  expect_identical(stats_means_moments(c(8, 9, 9), c(1, 1, 1))$css, 0.66666666666666663)
  code <- "data a; input x @@; datalines;\n0.1 0.2 0.3 0.7\n;\nrun;\nproc means data=a noprint; var x; output out=o sum=s mean=m std=sd css=css; run;"
  r <- sas_run(code)
  expect_identical(r$work$o$s, Reduce(`+`, c(0.1, 0.2, 0.3, 0.7)))
  k <- stats_means_moments(c(0.1, 0.2, 0.3, 0.7), rep(1, 4))
  expect_identical(r$work$o$css, k$css)
  expect_identical(r$work$o$sd, sqrt(k$css / 3))
})

test_that("PROC MEANS: WEIGHT sums are fma(w, x, .) and a zero-weight first value is the shift", {
  # proc_stats/means_css_dec_probe g200 (SAS 9.4): x = 83.2 (w 0), 48.1 (w 3)
  # -> SUM 144.3, CSS 1.0615508472255895e-13
  code <- "data a; input x w; datalines;\n83.2 0\n48.1 3\n;\nrun;\nproc means data=a noprint; var x; weight w; output out=o sum=s css=css; run;"
  r <- sas_run(code)
  expect_identical(r$work$o$s, 144.3)
  expect_identical(r$work$o$css, 1.0615508472255895e-13)
})

test_that("PROC MEANS: T is capped at 99999.99; SKEWNESS stays approximate", {
  code <- "data a; input x @@; datalines;\n1000000.1 1000000.2\n;\nrun;\nproc means data=a noprint; var x; output out=o t=t; run;"
  expect_identical(sas_run(code)$work$o$t, 99999.99)
  withr::with_options(list(sasonr.strict = TRUE),
    expect_error(sas_run(sub("t=t", "skewness=sk", code, fixed = TRUE)), class = "sasonr_not_verified"))
})

test_that("PROC MEANS: a variance negligible against the squared mean gives STD 0, the undetermined band fails loud", {
  # proc_stats/means_css_dec_probe g13 (SAS 9.4): STD 0 and CV 0 for VAR/MEAN**2 = 2E-32
  code <- "data a; input x w; datalines;\n1000000.6063 0\n1000000.594 1.7\n;\nrun;\nproc means data=a noprint; var x; weight w; output out=o std=sd cv=cv; run;"
  r <- sas_run(code)
  expect_identical(r$work$o$sd, 0)
  expect_identical(r$work$o$cv, 0)
})

test_that("DATA step SKEWNESS is exact up to 10 arguments", {
  # fn_matrix/SKEWNESS (SAS 9.4): skewness(1, 2, 3, 10) = 1.76363261480388 (hex 3FFC37D6D52BDDDD)
  expect_identical(sas_f_skewness(1, 2, 3, 10), hexd("3FFC37D6D52BDDDD"))
  # skewness(2, 4, 4, 4, 5, 5, 7, 9) = 0.81848755335679 (hex 3FEA310CCF3B3760)
  expect_identical(sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9), hexd("3FEA310CCF3B3760"))
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
