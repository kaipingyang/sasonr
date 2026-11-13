# Platform invariants.  These tests must give the same verdict on every OS:
# the display engine must not depend on a C library detail.  In particular,
# glibc's printf prints the exact binary value at any precision, while the
# Windows UCRT is only correctly rounded up to about 17 significant digits
# (123456789012345 = 1.23456789012345E14 prints as ...4499999... beyond that),
# so a sprintf-based E-notation tie detection silently rounded on Windows
# (found by running test-round15-1.R on Windows, fixed in the exact-bit
# expansion of R/rounding.R).

test_that("the exact decimal expansion of a double is the same on every platform", {
  sig <- sasonr:::exact_significant_digits

  # 123456789012345 = 1.23456789012345E14: a 15-digit exact value ending in 5
  expect_identical(sig(123456789012345)$digits, "123456789012345")
  expect_identical(sig(123456789012345)$ex, 14L)

  # 2^-21 = 4.76837158203125E-7: another 15-digit exact value, E10. is unambiguous
  expect_identical(sig(2^-21)$digits, "476837158203125")
  expect_identical(sig(2^-21)$ex, -7L)

  # 0.1: the binary value is 0.1000000000000000055511151231257827...; the old
  # sprintf("%.800e") tail check and this expansion must agree with the exact
  # decimal representation, not with a platform's printf
  expect_identical(sig(0.1)$digits,
                   "1000000000000000055511151231257827021181583404541015625")
  expect_identical(sig(0.1)$ex, -1L)

  # subnormal: exact expansion of the smallest positive double (about 751 digits)
  expect_identical(sig(5e-324)$ex, -324L)
  expect_identical(nchar(sig(5e-324)$digits), 751L)

  # the integer branch (p >= 0) and the fraction branch (p < 0) agree with the
  # exact equality of the reconstructed value
  expect_identical(sig(1e300)$ex, 300L)
  expect_true(startsWith(sig(1e300)$digits, "10000000000000000525047602552"))
})

test_that("an exact tie at the 15th digit fails loud on every platform", {
  # Old behaviour on Windows: the tie was not detected and E19. of
  # 123456789012345 silently wrote 1.234567890123E+14 instead of failing loud.
  expect_error(sas_f_putn(123456789012345, "E19."), class = "sasonr_unsupported")
  expect_error(sasonr:::sci_dec_text(123456789012345, 12L), class = "sasonr_unsupported")
  expect_error(sasonr:::sci_dec_text(123456789012345, 13L), class = "sasonr_unsupported")

  # 2^-21 is also an exact 15-digit tie, but rounding to E10. gives the same
  # digits in both directions: SAS's output is unambiguous
  expect_identical(trimws(sas_f_putn(2^-21, "E10.")), "4.768E-07")
})
