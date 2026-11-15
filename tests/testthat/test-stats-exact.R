# Moment statistics (R/stats_exact.R).

hex <- function(x) paste(rev(as.character(writeBin(x, raw()))), collapse = "")

# hex16 text -> double, for the exact expectations below.
unhex <- function(h) {
  vapply(h, function(one) {
    b <- as.raw(strtoi(substring(one, seq(1L, 15L, 2L), seq(2L, 16L, 2L)), 16L))
    readBin(b, "double", size = 8L, endian = "big")
  }, 0, USE.NAMES = FALSE)
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
  # the mean of 9259.2549 and -628322.4 is C112E46E4A4A8C15 (sum / 2 is ...16)
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
hexds <- function(...) vapply(c(...), hexd, 0, USE.NAMES = FALSE)
# a data step reading the values exactly (hex) and weights
hex_data <- function(x, w = rep(1, length(x))) {
  paste0("data a; input xh :$16. w; x = input(xh, hex16.); drop xh; datalines;\n",
         paste(x, w, collapse = "\n"), "\n;\nrun;\n")
}
means_out <- function(data, stmt, out, opts = "") {
  sas_run(paste0(data, "proc means data=a noprint ", opts, "; var x; ", stmt, " output out=o ", out, "; run;"))$work$o
}

test_that("PROC MEANS: the CSS family of the moment kernel", {
  # integer data, so only the CSS formula matters
  # CSS of 2 8 4 = 18.666666666666664 (not the nearest double of 56/3)
  expect_identical(stats_moments(c(2, 8, 4), "means", need = "css")$css, 18.666666666666664)
  # 1 4 8 -> 24.666666666666671; 8 9 9 -> 0.66666666666666663
  expect_identical(stats_moments(c(1, 4, 8), "means", need = "css")$css, 24.666666666666671)
  expect_identical(stats_moments(c(8, 9, 9), "means", need = "css")$css, 0.66666666666666663)
  # decimal data (exact hex values)
  o <- means_out(hex_data(c("C00BB22D0E560419", "BFFC49BA5E353F7D", "3FFD5810624DD2F2")), "",
                 "sum=s css=css std=sd uss=uss")
  expect_identical(o$s, hexd("C00B2B020C49BA5F"))
  expect_identical(o$css, hexd("402D42D7B634DAD3"))
  expect_identical(o$sd, hexd("4005A32D885EC1BA"))
  expect_identical(o$uss, hexd("4032798E10CF5B1D"))
})

test_that("PROC MEANS: nearly constant data use a zero mean shift", {
  # two values 12 ulps apart: the CSS is the shifted sum of squares itself
  # (144 ulp**2), not 72 ulp**2;
  # STD and CV are cut to 0, STDERR and T kept.
  o <- means_out(hex_data(c("4038851EB851EB93", "4038851EB851EB9F")), "",
                 "mean=m css=css var=v std=sd stderr=se cv=cv t=t uss=uss")
  expect_identical(c(o$m, o$css, o$v, o$sd, o$se, o$cv, o$t, o$uss),
                   hexds("4038851EB851EB99", "3A62000000000000", "3A62000000000000", "0000000000000000",
                         "3D20F876CCDF6CD9", "0000000000000000", "40F869FFD70A3D71", "4092C9D7DBF4881B"))
  # the same with weights other than 1: not supported
  expect_error(means_out(hex_data(c("4038851EB851EB93", "4038851EB851EB9F"), c(2, 1)), "weight w;", "css=css"),
               class = "sasonr_unsupported")
  # a mean shift between 9.98E-15 and 1.0006E-14 of the first value: not supported
  expect_error(stats_moments(c(1, 1 + 2e-14), "means", need = "css"), class = "sasonr_unsupported")
})

test_that("PROC MEANS: a negative CSS is missing (with USS, VAR, STD ...)", {
  # with WEIGHT:
  # missing CSS, USS, VAR, STD, STDERR and CV, MEAN C07DABF7CED91688
  o <- means_out(hex_data(c("C07D525E353F7CEE", rep("C07DABF7CED91688", 4)), c(0, 0.5, 1, 2.25, 0.5)), "weight w;",
                 "mean=m css=css uss=uss var=v std=sd stderr=se cv=cv")
  expect_identical(o$m, hexd("C07DABF7CED91688"))
  expect_identical(c(o$css, o$uss, o$v, o$sd, o$se, o$cv), rep(NA_real_, 6))
})

test_that("PROC MEANS: the STD cut-off region fails loud outside the supported domain", {
  # VAR / MEAN**2 = 1.28E-26: between the known limits
  expect_error(means_out(hex_data(c("3FF0000000000000", toupper(hex(1 + 1.6e-13)))), "", "std=sd"),
               class = "sasonr_unsupported")
  # VAR / MEAN**2 = 4.9E-34, below the smallest known ratio
  # (2.27E-33): not extrapolated to STD 0
  tiny <- paste0("data a; do i = 1 to 99; x = 1; output; end; x = 1 + 2**-52; output; run;\n")
  expect_error(means_out(tiny, "", "std=sd"), class = "sasonr_unsupported")
  expect_identical(means_out(tiny, "", "var=v")$v, 2^-104 / 99)
  # cut-off region with FREQ: STD fails loud, VAR still runs
  fd <- "data a; input x f; datalines;\n1 1\n1.0000000000000002 1\n;\nrun;\n"
  expect_error(means_out(fd, "freq f;", "std=sd"), class = "sasonr_unsupported")
  expect_identical(means_out(fd, "freq f;", "var=v")$v, 2^-104)
  # PRT in the cut-off region without WEIGHT
  withr::with_options(list(sasonr.strict = FALSE),
    expect_error(suppressWarnings(means_out(hex_data(c("4038851EB851EB93", "4038851EB851EB9F")), "", "prt=p")),
                 class = "sasonr_unsupported"))
})

test_that("PROC MEANS: overflow or underflow inside the kernel fails loud (never a missing value)", {
  # d**4 overflows: fail loud instead of a missing KURTOSIS
  big <- "data a; input x @@; datalines;\n1e100 -1e100 3e100 5e100\n;\nrun;\n"
  expect_error(means_out(big, "", "kurtosis=ku"), class = "sasonr_unsupported")
  # (d**3) * d underflows: fail loud instead of KURTOSIS from a zero sum
  tiny <- "data a; input x @@; datalines;\n1e-100 2e-100 4e-100 8e-100\n;\nrun;\n"
  expect_error(means_out(tiny, "", "kurtosis=ku"), class = "sasonr_unsupported")
  # values beyond the supported magnitudes
  expect_error(means_out("data a; input x @@; datalines;\n1e150 2\n;\nrun;\n", "", "mean=m"),
               class = "sasonr_unsupported")
})

test_that("PROC MEANS: EXCLNPWGT fails loud", {
  d <- "data a; input x w; datalines;\n1 1\n2 0\n3 2\n;\nrun;\n"
  expect_error(means_out(d, "weight w;", "mean=m", opts = "exclnpwgt"), class = "sasonr_unsupported")
})

test_that("the emulated fma fails loud outside its exact range, missing stays missing", {
  # no fallback to a * b + c (Inf, or 1 for an underflowed product)
  expect_error(stats_fma(1e200, 1e200, 0), class = "sasonr_unsupported")
  expect_error(stats_fma(1e-200, 1e-200, 1), class = "sasonr_unsupported")
  expect_error(stats_fma(Inf, 1, 1), class = "sasonr_unsupported")
  expect_identical(stats_fma(c(NA, 2), 3, 1), c(NA, 7))
  expect_error(stats_fma_accumulate(c(1, 1e200), c(1, 1e200)), class = "sasonr_unsupported")
})

test_that("stats_fma_accumulate() is stats_fma() step by step", {
  set.seed(3)
  for (k in 1:20) {
    a <- runif(50, -1e3, 1e3) * 10^sample(-5:5, 50, TRUE)
    b <- runif(50, -1e3, 1e3) * 10^sample(-5:5, 50, TRUE)
    expect_identical(stats_fma_accumulate(a, b), Reduce(function(acc, i) stats_fma(a[i], b[i], acc), seq_along(a), 0))
  }
})


test_that("PROC MEANS: WEIGHT sums are fma(w, x, .) and a zero-weight first value is the shift", {
  # x = 83.2 (w 0), 48.1 (w 3)
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
    expect_error(sas_run(sub("t=t", "skewness=sk", code, fixed = TRUE)), class = "sasonr_strict_error"))
})
test_that("DATA step moment functions: exact up to 9 values, guards fail loud", {
  # skewness(1, 2, 3, 10) = 1.76363261480388 (hex 3FFC37D6D52BDDDD)
  expect_identical(sas_f_skewness(1, 2, 3, 10), hexd("3FFC37D6D52BDDDD"))
  # skewness(2, 4, 4, 4, 5, 5, 7, 9) = 0.81848755335679 (hex 3FEA310CCF3B3760)
  expect_identical(sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9), hexd("3FEA310CCF3B3760"))
  # a symmetric list: the result is 0, the kernel leaves a residue below
  # 1E-12: the guard fails loud
  expect_error(sas_f_skewness(hexd("C062E33333333333"), hexd("C06AF66666666666"), hexd("C060F66666666666"),
                              hexd("C069099999999999")),
               class = "sasonr_unsupported")
  expect_error(sas_f_skewness(2, 0, 4, 4, 5, 5, 7, 9), class = "sasonr_unsupported")
  # 10 values: the CSS is 40BA860E1130538C, the kernel's one bit off:
  # 10 values fail loud
  x <- hexds("C072B2923A29C77A", "C06EC3C1BDA5119E", "C07242923A29C77A", "C0716343958105E9", "C06C252474538EF4",
             "C06D452474538EF4", "C07222923A29C77A", "C06F9EBE0DED288E", "C06D052474538EF4", "C070F5C56D5CFAAD")
  expect_error(do.call(sas_f_css, as.list(x)), class = "sasonr_unsupported")
  expect_identical(do.call(sas_f_uss, as.list(x)), Reduce(`+`, x * x))
  # magnitudes beyond the supported domain
  expect_error(sas_f_std(1e150, 1), class = "sasonr_unsupported")
})

test_that("MOD and MODZ: exact remainders inside the supported domain, fail loud outside", {
  expect_identical(sas_f_mod(10, 3), 1)
  expect_identical(sas_f_mod(5.5, 2), 1.5)
  expect_identical(sas_f_mod(0.3, 0.1), 0)
  expect_error(sas_f_mod(10, 0.3), class = "sasonr_unsupported")
  # MODZ(0.3, 0.1) is 3FB9999999999998
  expect_identical(hex(sas_f_modz(0.3, 0.1)), "3fb9999999999998")
  expect_error(sas_f_modz(1.2, -0.006), class = "sasonr_unsupported")
})

test_that("HARMEAN sums the reciprocals in argument order", {
  expect_identical(sas_f_harmean(0.1, 0.2, 0.3), 3 / Reduce(`+`, 1 / c(0.1, 0.2, 0.3)))
})

test_that("PROC MEANS: CSS, VAR, STD, STDERR, CV and USS are exact", {
  code <- paste0(
    "data a; input g x; datalines;\n",
    "1 0.1\n1 0.2\n1 0.3\n2 1\n2 2\n2 3\n2 4\n",
    "3 1\n3 1.0000000000000002\n3 1.0000000000000006\n;\nrun;\n",
    "proc means data=a noprint; by g; var x;",
    " output out=o n=n mean=m css=css var=v std=sd stderr=se cv=cv uss=uss; run;"
  )
  r <- sas_run(code) # strict mode: every statistic below must be supported
  o <- r$work$o
  expect_identical(o$m, unhex(c("3fc999999999999b", "4004000000000000", "3ff0000000000001")))
  expect_identical(o$css, unhex(c("3f947ae147ae147a", "4014000000000000", "39a4000000000000")))
  expect_identical(o$v, unhex(c("3f847ae147ae147a", "3ffaaaaaaaaaaaab", "3994000000000000")))
  expect_identical(o$sd, unhex(c("3fb9999999999999", "3ff4a7e9cb8a3491", "0000000000000000")))
  expect_identical(o$se, unhex(c("3fad8f7208e6b82c", "3fe4a7e9cb8a3491", "3cb4a7e9cb8a3491")))
  expect_identical(o$cv, unhex(c("4048ffffffffffff", "4049d1e43e6cc1b5", "0000000000000000")))
  expect_identical(o$uss, unhex(c("3fc1eb851eb851ed", "403e000000000000", "4008000000000004")))
})


