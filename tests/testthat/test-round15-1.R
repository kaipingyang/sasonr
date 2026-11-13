# Round 15.1: the independent review of round 15 (review_081e9ac.md; one
# regression test per finding, each fails on 081e9ac).

test_that("w.d integer zeros: only the verified domain and the gold points (finding 1)", {
  f <- function(x, fmt) trimws(sas_f_putn(x, fmt))
  # 081e9ac wrote 1234567890123456.0000 etc. silently: 16 digits with 4 decimals is not verified
  expect_error(sas_f_putn(1234567890123456, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "DOLLAR32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "Z32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(939586291501115008, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(-1e15, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; x = 1234567890123456; put x 32.4; run;"), class = "sasonr_unsupported")
  # the gold points
  expect_identical(f(1e15, "COMMA32.4"), "1,000,000,000,000,000.0000")   # SAS 9.4 (fmt_matrix COMMA_ec3f12)
  expect_identical(f(1e15, "DOLLAR32.4"), "$1,000,000,000,000,000.0000") # SAS 9.4 (fmt_matrix DOLLAR_ef9d86)
  expect_identical(f(939586291501115008, "32.15"), "939586291501115008.0000000000000")  # SAS 9.4 (wd_digits D0172)
  expect_identical(f(26745296926266265600, "32.20"), "26745296926266265600.00000000000") # SAS 9.4 (wd_digits D0186)
  # verified domain: below 1E15 any decimals, 3 decimals below 2^53
  expect_identical(f(123456789012345, "32.10"), "123456789012345.0000000000")
  expect_identical(f(1234567890123456, "32.3"), "1234567890123456.000")
})

test_that("ROUNDE and ROUNDZ fail loud from |x/unit| = 2^49 as ROUND does (finding 2)", {
  # SAS 9.4: ROUND(507561523905906.125, 0.25) = 507561523905906 (not the nearest multiple)
  expect_error(sas_f_round(507561523905906.125, 0.25), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(507561523905906.125, 0.25), class = "sasonr_unsupported") # 081e9ac: silent
  expect_error(sas_f_roundz(507561523905906.125, 0.25), class = "sasonr_unsupported") # 081e9ac: silent
  expect_error(sas_run("data _null_; y = rounde(507561523905906.125, 0.25); run;"), class = "sasonr_unsupported")
})

test_that("integers from 1E18 to 1E21 without decimals: gold point only (finding 3)", {
  drt <- sasonr:::display_round_text
  expect_error(drt(1e18, 0L), class = "sasonr_unsupported")               # 081e9ac: "1000000000000000000"
  expect_error(sas_f_putn(1e18, "32."), class = "sasonr_unsupported")
  expect_identical(trimws(sas_f_putn(26745296926266265600, "32.")), "26745296926266265600") # SAS 9.4 (D0186)
  expect_identical(drt(939586291501115008, 0L), "939586291501115008")     # SAS 9.4 (D0172)
  expect_identical(trimws(sas_f_putn(1e18, "8.")), "1E18")                # no fixed form fits: E notation
})

test_that("ROUNDE: the refuted window fails loud from 1.1E-12 |q| > 1E-6 (finding 4)", {
  # 1.5E-6 below 1000000.5: outside the old 0.8-1.25 W band, 081e9ac rounded it down silently
  expect_error(sas_f_rounde(1000000.5 - 1.5e-6, 1), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(1000000.5 - 1e-9, 1), class = "sasonr_unsupported")
  expect_equal(sas_f_rounde(1000.5 - 1.5e-6, 1), 1000) # small |q|: unchanged rule
  expect_equal(sas_f_rounde(1000000.5, 1), 1000000)    # exact tie: even
})

test_that("wide BEST E form: mantissa decimals from the width (finding 5)", {
  # 081e9ac wrote 1.2345678901234E-6 (18 columns) in BEST16.
  s <- trimws(sas_f_putn(1.2345678901234567e-6, "BEST16."))
  expect_lte(nchar(s), 16L)
  expect_identical(s, sasonr:::sci_dec_text(1.2345678901234567e-6, 11L)$text)
  expect_lte(nchar(trimws(sas_f_putn(-1.2345678901234567e-6, "BEST16."))), 16L)
})

test_that("an exact tie at the 15th digit fails loud when it decides the E digits (finding 6)", {
  # 123456789012345: 1.2345678901234|5 exactly; E19. (12 decimals) depends on the tie
  expect_error(sas_f_putn(123456789012345, "E19."), class = "sasonr_unsupported") # 081e9ac: 1.234567890123E+14
  expect_error(sasonr:::sci_dec_text(123456789012345, 12L), class = "sasonr_unsupported")
  # 2^-21 = 4.76837158203125E-7: a tie whose direction does not change E10.
  expect_identical(trimws(sas_f_putn(2^-21, "E10.")), "4.768E-07")
})

test_that("ROUND above 2^49: only exact multiples are exempt (finding 7)", {
  # x integer, unit 0.3, x/unit not an integer: 081e9ac exempted it (no gold)
  expect_error(sas_f_round(600000000000001, 0.3), class = "sasonr_unsupported")
  expect_equal(sas_f_round(123456789012.5, 1e-4), 123456789012.5) # x/unit an integer: SAS 9.4 (round_b01)
})
