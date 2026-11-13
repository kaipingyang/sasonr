# ROUND family, INT/FLOOR/CEIL and w.d-type formats: expectations taken from
# the SAS 9.4 rounding gold standard (sasonr-conformance rounding_gold;
# offline cases tests/conformance/rounding/).

hex <- function(x) toupper(paste(as.character(writeBin(x, raw(), size = 8L, endian = "big")), collapse = ""))

test_that("ROUND returns the double nearest to the decimal multiple", {
  expect_identical(sas_f_round(0.1 + 0.2, 0.1), 0.3)
  expect_identical(sas_f_round(0.1 + 0.2, 1e-9), 0.3)
  expect_identical(sas_f_round(2.675, 0.01), 2.68)
  expect_identical(sas_f_round(-2.5), -3)
  expect_identical(sas_f_round(10 / 3, 1 / 3), 10 / 3)
  expect_identical(sas_f_round(1e20, 0.1), 1e20)
})

test_that("ROUND fuzz: about 1e-12 relative below a half, capped at 2e-6 in units", {
  expect_identical(sas_f_round(2.5 - 1e-13), 3)
  expect_identical(sas_f_round(0.5 - 2^-30), 0) # far outside the window
  expect_identical(sas_f_round(1000000000.5 - 2^-24), 1000000001) # 6e-8 below: inside
})

test_that("ROUND signs: a negative value rounding to zero gives -0, -0 and subnormals give +0", {
  expect_identical(hex(sas_f_round(-0.05)), "8000000000000000")
  expect_identical(hex(sas_f_round(-0)), "0000000000000000")
  expect_identical(hex(sas_f_round(-5e-324)), "0000000000000000")
  expect_true(is.na(sas_f_round(sas_missing(".A"))))
})

test_that("a unit that is not a normal double is an invalid argument (SAS 9.4 TS1M2)", {
  # TS1M7 has no gold for a subnormal unit: fail loud
  expect_error(sas_f_round(1, 2.2e-308), class = "sasonr_unsupported")
  # TS1M2 (ROUND_047): 2.3e-308 is normal, 2.2e-308 is subnormal: SAS takes
  # the first and rejects the second with "Argument 2 ... is invalid"
  withr::local_options(sasonr.sas_version = "TS1M2")
  expect_identical(sas_f_round(1, 2.3e-308), 1)
  expect_true(is.na(suppressWarnings(sas_f_round(1, 2.2e-308))))
  expect_true(is.na(suppressWarnings(sas_f_rounde(1, 1e-308))))
  expect_true(is.na(suppressWarnings(sas_f_roundz(1, 1e-308))))
})

test_that("a subnormal literal: an ERROR in TS1M7, kept as-is in TS1M2", {
  # TS1M7 (opensas/best_fmt_subnormal): ERROR: Invalid number '1E-310'
  expect_error(sas_run("data a; x = 1e-310; run;"), class = "sasonr_syntax_error")
  # TS1M2 (ROUND_044) does not run denormals-are-zero: 1e-310 is stored as-is
  # and ordinary arithmetic on it follows IEEE.
  withr::local_options(sasonr.sas_version = "TS1M2")
  res <- suppressWarnings(sas_run("data a; x = 1e-310; y = x * 1e10; run;"))
  expect_identical(hex(res$work$a$x), "000012688B70E62B")
  expect_identical(hex(res$work$a$y), "01A56E1FC2F8F346")
})

test_that("ROUND fails loud where the gold standard does not determine the result", {
  # round 15: floor(fl(fl(|q| + 0.5) + min(1E-12 |q|, 1E-6))) (r15_rnd, SAS 9.4 gold)
  expect_identical(sas_f_round(1267.7744999989193, 0.001), 1267.774)
  expect_identical(sas_f_round(0.03499999999996535, 0.01), 0.04)     # 0.99E-12 |q| below the half point: up
  expect_identical(sas_f_round(-0.034999999999964386, 0.01), -0.03)  # 1.02E-12 |q| below: down
  expect_error(sas_f_round(507561523905906.125, 0.25), class = "sasonr_unsupported") # |x/unit| >= 2^49
  expect_error(sas_f_round(1e15 + 0.5, 0.2), class = "sasonr_unsupported")
})

test_that("ROUNDE and ROUNDZ tie to even; ROUNDZ has no fuzz", {
  expect_identical(sas_f_rounde(0.15, 0.1), 0.2)
  expect_identical(sas_f_rounde(0.25, 0.1), 0.2)
  expect_identical(sas_f_rounde(2.5 - 1e-13), 2)
  expect_identical(sas_f_roundz(2.5), 2)
  expect_identical(sas_f_roundz(0.35, 0.1), 0.3)
  expect_identical(sas_f_roundz(2.675, 0.01), 2.68)
})

test_that("INT/FLOOR/CEIL: absolute fuzz 1.5e-12; signs of zero as in SAS", {
  expect_identical(sas_f_int(1 - 1e-12), 1)
  expect_identical(sas_f_floor(2 - 2e-12), 1)
  expect_identical(sas_f_floor(-1e-13), 0)
  expect_identical(hex(sas_f_ceil(1e-13)), "8000000000000000")
  expect_identical(hex(sas_f_int(0)), "8000000000000000")
  expect_identical(hex(sas_f_floor(-0)), "0000000000000000")
  expect_identical(hex(sas_f_floorz(-5e-324)), "8000000000000000")
  expect_identical(sas_f_floorz(1 - 1e-15), 0)
  expect_identical(sas_f_int(1e15), 1e15)
})

test_that("w.d: at or above the half point up, the double nearest to it up, 3 ulps below down", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(1.005, "4.2"), "1.01")
  expect_equal(f(2.675, "8.2"), "    2.68")
  # SAS 9.4 writes 1.00 here (round 15: (1.0049999999999997 + 0.005) (1 + 2^-52)
  # stays below 1.01)
  expect_equal(f(1.0049999999999997, "4.2"), "1.00")
  expect_equal(f(1.0049999999999992, "4.2"), "1.00")
})

test_that("w.d: minus sign for negatives that round to zero, missing in the units column", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(-0.05, "2."), "-0")
  expect_equal(f(-0.001, "8.2"), "   -0.00")
  expect_equal(f(NA_real_, "4.2"), " .  ")
  expect_equal(f(sas_missing(".A"), "8.2"), "     A  ")
  expect_equal(f(-0.05, "1."), "0")
})

test_that("narrow widths drop decimals, then use E notation", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(-1.045, "4.2"), "-1.0")
  expect_equal(f(1234567.891, "6.2"), "1.23E6")
  expect_equal(f(123456789012.5, "6.2"), " 123E9")
  expect_equal(f(-12345.6789, "Z5."), "-12E3")
  expect_equal(f(1e10, "Z8.2"), "00001E10")
  expect_equal(f(1e12, "12."), "        1E12")
})

test_that("COMMA and DOLLAR drop separators and the dollar sign before decimals", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(12345.6789, "COMMA10.2"), " 12,345.68")
  expect_equal(f(7890.715, "COMMA5.1"), " 7891")
  expect_equal(f(-99.95, "DOLLAR6.2"), "-99.95")
  expect_equal(f(1234567.891, "DOLLAR10.2"), "1234567.89")
})

test_that("PERCENT keeps three columns for the sign and percent sign", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(99.95, "PERCENT8.1"), "  9995% ")
  expect_equal(f(-1.5, "PERCENT8.1"), "(150.0%)")
  expect_equal(f(9999.5, "PERCENT8.1"), "   1E6% ")
  expect_equal(f(NA_real_, "PERCENT8.1"), "    .   ")
})

test_that("BESTw.: -0 is 0, wide widths keep 15 digits, small widths fail loud", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(-0, "BEST12."), "           0")
  expect_equal(trimws(f(2 / 3, "BEST32.")), "0.66666666666666")
  expect_equal(trimws(f(2 / 3, "BEST12.")), "0.6666666667")
  expect_equal(trimws(f(.Machine$double.xmax, "BEST32.")), "1.7976931348623E308")
  expect_error(f(0.05, "BEST3."), class = "sasonr_unsupported")
})

test_that("Ew.: positive values use the sign column for a decimal in E7.", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(f(0.5, "E7."), "5.0E-01")
  expect_equal(f(-0.5, "E7."), "-5.E-01")
  expect_equal(f(0.125, "E8."), " 1.3E-01")
})
