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

test_that("PROC MEANS: STD is approximate (strict fails loud), SUM and MEAN are exact", {
  code <- "data a; input x @@; datalines;\n0.1 0.2 0.3\n;\nrun;\nproc means data=a noprint; var x; output out=o sum=s mean=m; run;"
  r <- sas_run(code)
  expect_identical(r$work$o$s, Reduce(`+`, c(0.1, 0.2, 0.3)))
  expect_identical(r$work$o$m, Reduce(`+`, c(0.1, 0.2, 0.3)) / 3)
  withr::with_options(list(sasonr.strict = TRUE),
    expect_error(sas_run(sub("sum=s mean=m", "std=sd", code, fixed = TRUE)), class = "sasonr_not_verified"))
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
