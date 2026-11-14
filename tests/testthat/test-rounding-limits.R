# Rounding and display limits: values outside the supported domain of w.d,
# BESTw., Ew. and ROUND / ROUNDE / ROUNDZ fail loud.

test_that("w.d integer zeros: only inside the supported domain and at the known points", {
  f <- function(x, fmt) trimws(sas_f_putn(x, fmt))
  # 16 digits with 4 decimals (1234567890123456.0000) is outside the supported domain
  expect_error(sas_f_putn(1234567890123456, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "DOLLAR32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "Z32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(939586291501115008, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(-1e15, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; x = 1234567890123456; put x 32.4; run;"), class = "sasonr_unsupported")
  # the known points
  expect_identical(f(1e15, "COMMA32.4"), "1,000,000,000,000,000.0000")
  expect_identical(f(1e15, "DOLLAR32.4"), "$1,000,000,000,000,000.0000")
  expect_identical(f(939586291501115008, "32.15"), "939586291501115008.0000000000000")
  expect_identical(f(26745296926266265600, "32.20"), "26745296926266265600.00000000000")
  # supported domain: below 1E15 any decimals, 3 decimals below 2^53
  expect_identical(f(123456789012345, "32.10"), "123456789012345.0000000000")
  expect_identical(f(1234567890123456, "32.3"), "1234567890123456.000")
})

test_that("ROUNDE and ROUNDZ fail loud from |x/unit| = 2^49 as ROUND does", {
  # ROUND(507561523905906.125, 0.25) = 507561523905906 (not the nearest multiple)
  expect_error(sas_f_round(507561523905906.125, 0.25), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(507561523905906.125, 0.25), class = "sasonr_unsupported")
  expect_error(sas_f_roundz(507561523905906.125, 0.25), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; y = rounde(507561523905906.125, 0.25); run;"), class = "sasonr_unsupported")
})

test_that("integers from 1E18 to 1E21 without decimals: the known point only", {
  drt <- sasonr:::display_round_text
  expect_error(drt(1e18, 0L), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1e18, "32."), class = "sasonr_unsupported")
  expect_identical(trimws(sas_f_putn(26745296926266265600, "32.")), "26745296926266265600")
  expect_identical(drt(939586291501115008, 0L), "939586291501115008")
  expect_identical(trimws(sas_f_putn(1e18, "8.")), "1E18")                # no fixed form fits: E notation
})

test_that("ROUNDE: the undetermined window fails loud from 1.1E-12 |q| > 1E-6", {
  # 1.5E-6 below 1000000.5: outside the 0.8-1.25 W band
  expect_error(sas_f_rounde(1000000.5 - 1.5e-6, 1), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(1000000.5 - 1e-9, 1), class = "sasonr_unsupported")
  expect_equal(sas_f_rounde(1000.5 - 1.5e-6, 1), 1000) # small |q|: unchanged rule
  expect_equal(sas_f_rounde(1000000.5, 1), 1000000)    # exact tie: even
})

test_that("wide BEST E form: BEST16.-BEST19. and k = 13 fail loud outside the known forms", {
  # 1.2345678901234E-6 takes 18 columns; the width-derived number of mantissa
  # decimals k is not determined for BEST16.-BEST19.: fail loud
  expect_error(sas_f_putn(1.2345678901234567e-6, "BEST16."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(-1.2345678901234567e-6, "BEST16."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1.23456789e-20, "BEST19."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1.2345e-7, "BEST17."), class = "sasonr_unsupported")
  # k = 13 at BEST20. (-1.5E-12: 20 - 1 - 3 - 1 - 2 = 13 mantissa decimals)
  expect_error(sas_f_putn(-1.2345678901234567e-12, "BEST20."), class = "sasonr_unsupported")
  # BEST16.: mantissas of at most 4 significant digits
  expect_identical(sas_f_putn(c(-1e15, 1e16, -1e16, 1e20, -1e20, 1.234e-7, -1.234e-7, 1e-10, -1e-10), "BEST16."),
                   formatC(c("-1E15", "1E16", "-1E16", "1E20", "-1E20", "1.234E-7", "-1.234E-7", "1E-10", "-1E-10"), width = 16))
  # BEST32. (k >= 14) unchanged
  expect_identical(trimws(sas_f_putn(1.2345678901234567e-6, "BEST32.")), "1.2345678901234E-6")
})

test_that("an exact tie at the 15th digit fails loud when it decides the E digits", {
  # 123456789012345: 1.2345678901234|5 exactly; E19. (12 decimals) depends on the tie
  expect_error(sas_f_putn(123456789012345, "E19."), class = "sasonr_unsupported")
  expect_error(sasonr:::sci_dec_text(123456789012345, 12L), class = "sasonr_unsupported")
  # 2^-21 = 4.76837158203125E-7: a tie whose direction does not change E10.
  expect_identical(trimws(sas_f_putn(2^-21, "E10.")), "4.768E-07")
})

test_that("ROUND above 2^49: only exact multiples are exempt", {
  # x integer, unit 0.3, x/unit not an integer: not exempt
  expect_error(sas_f_round(600000000000001, 0.3), class = "sasonr_unsupported")
  expect_equal(sas_f_round(123456789012.5, 1e-4), 123456789012.5) # x/unit an integer
})
