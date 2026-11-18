# Function details: CAT/CATT/CATS of numbers, COUNTC and INDEXC modifiers, JULDATE,
# the tails of LOGSDF/SDF/LOGCDF, noncentral F and t, zero-length STRIP/TRIMN,
# SYMGET names and blank literals.

run1 <- function(stmts, setup = "") {
  res <- sas_run(paste0("data d; ", setup, " ", stmts, " e = _error_; run;"))
  res$work$d
}

test_that("CAT/CATT/CATS write numbers without leading blanks", {
  expect_equal(sas_f_cat("x", 1), "x1")
  expect_equal(sas_f_cat("a", NA_real_, "b"), "a.b")
  expect_equal(sas_f_catt("x", 3), "x3")
  d <- run1("r1 = cat('x', 1); r2 = cat('a', ., 'b'); r3 = cat('a', '', 'b'); r4 = catt('x', 3);")
  expect_equal(c(d$r1, d$r2, d$r3, d$r4), c("x1", "a.b", "a b", "x3"))
})

test_that("COUNTC V modifier counts characters not in the list; INDEXC takes several excerpts", {
  expect_equal(sas_f_countc("abc", "ab", "v"), 1)
  expect_equal(sas_f_indexc("abc", "x", "yz"), 0)
  expect_equal(sas_f_indexc("abc", "x", "c"), 3)
  d <- run1("r = indexc('abc', 'x', 'c');")
  expect_equal(d$r, 3)
})

test_that("JULDATE uses the YEARCUTOFF window for yyddd", {
  expect_equal(sas_f_juldate(c(23450, 0, -21915)), c(24075, 60001, 1899365))
})

test_that("LOGSDF/SDF/LOGCDF use the tails directly", {
  expect_equal(lenient(sas_f_logsdf("NORMAL", 10)), -53.23128515051247, tolerance = 1e-14)
  expect_equal(lenient(sas_f_sdf("NORMAL", 1.96)), 0.024997895148220439, tolerance = 1e-12)
})

test_that("noncentral F: accurate distribution function and quantile", {
  expect_equal(lenient(sas_f_probf(1, 2, 10, 1)), 0.4502696915707326, tolerance = 1e-14)
  expect_equal(lenient(sas_f_finv(0.95, 2, 10, 1)), 5.9925880338395423, tolerance = 1e-12)
})

test_that("PROBT: noncentral t beyond the range of R's pnt (1e-13)", {
  # probt(2,10,1) = 0.80761156253037503
  expect_equal(lenient(sas_f_probt(2, 10, 1)), 0.80761156253037503, tolerance = 1e-12)
  # R's pt() returns 2.3e-13 here; the true value is 7.5e-88, so compare the ratio.
  expect_equal(lenient(sas_f_probt(-40, 100, 5)) / 7.5280904809890335e-88, 1, tolerance = 1e-11)
  expect_equal(lenient(sas_f_probt(-5, 100, 1)) / 7.5915155338165518e-09, 1, tolerance = 1e-12)
  expect_equal(lenient(sas_f_probt(2, 10)), 0.96330598261462985, tolerance = 1e-15)
  expect_true(is.na(lenient(sas_f_probt(2, 10, NA_real_)))) # missing nc gives missing
  expect_warning(v <- lenient(sas_f_probt(2, 0)), class = "sasonr_data_error") # df must be positive
  expect_true(is.na(v))
})

test_that("zero-length results of STRIP and TRIMN have LENGTHC 0", {
  expect_equal(sas_f_lengthc(sas_f_strip("   ")), 0)
  expect_equal(sas_f_lengthc(sas_f_trimn("   ")), 0)
  d <- run1("r1 = lengthc(strip('   ')); r2 = lengthc(trimn('   ')); r3 = lengthc(trim('   '));")
  expect_equal(c(d$r1, d$r2, d$r3), c(0, 0, 1))
})

test_that("SYMGET: a name with a leading blank is invalid", {
  res <- sas_run("%let mv = hello;\ndata d; r1 = symget('mv'); r2 = symget(' mv '); e = _error_; run;")
  expect_equal(res$work$d$r1, "hello")
  expect_equal(res$work$d$r2, "")
  expect_equal(res$work$d$e, 1)
  expect_true(any(grepl("Invalid argument to function SYMGET(' mv ')", msgs(res), fixed = TRUE)))
})

test_that("blank literals: QUOTE and TRANWRD", {
  expect_equal(sas_f_quote(" "), "\" \"")
  expect_equal(sas_f_tranwrd("abc", "b", " "), "a c")
})

test_that("STRIP and TRIM: blanks only, all-blank values, missing values, other characters kept", {
  x <- c("  ab  ", "", "   ", strrep(" ", 5000), NA, "\tx\t", "ab \n")
  expect_identical(sas_f_strip(x), c("ab", "", "", "", NA, "\tx\t", "ab \n"))
  # bytes outside ASCII (UTF-8 session): blanks are removed by bytes
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    expect_identical(sas_f_strip("stra\u00dfe  "), "stra\u00dfe")
    expect_identical(sas_f_trim("\u00e9t\u00e9 "), "\u00e9t\u00e9")
  }
  expect_identical(sas_f_trim(c("ab  ", "", "   ", strrep(" ", 5000), NA, " a\t ")),
                   c("ab", " ", " ", " ", NA, " a\t"))
  expect_identical(sas_f_strip(c(a = " x ")), c(a = "x"))
})

test_that("COUNT: non-overlapping matches, empty and missing substrings, T and I modifiers", {
  expect_identical(sas_f_count(c("aaa", "abab", NA, "", "AbA "), "a"), c(3, 2, NA, 0, 0))
  expect_identical(sas_f_count("aaaa", "aa"), 2)
  expect_identical(sas_f_count(c(NA, "x", "   "), ""), c(0, 0, 0)) # empty substring before missing x
  expect_identical(sas_f_count("AbA ", "a ", "it"), 2)
  expect_identical(sas_f_count(c("a a ", "a a "), "a ", c("", "t")), c(2, 2))
  # the I modifier folds ASCII letters only
  if (isTRUE(l10n_info()[["UTF-8"]])) expect_identical(sas_f_count("stra\u00dfe", "SS", "i"), 0)
  expect_identical(sas_f_count(strrep(" ", 3000), "  "), 1500)
  expect_error(sas_f_count("abc", NA_character_), class = "sasonr_unsupported")
})

test_that("COMPRESS with a character list and modifiers; LENGTHC counts bytes", {
  expect_identical(sas_f_compress(c("a b\tc", "", "   ")), c("ab\tc", "", ""))
  expect_identical(sas_f_compress(c("A1b2", "x"), "", "kd"), c("12", ""))
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    expect_identical(sas_f_compress(c("Stra\u00dfe 1", "a"), "a"), c("Str\u00dfe 1", ""))
    expect_identical(sas_f_compress("ab", "\u00df"), "ab")
  }
  expect_identical(sas_f_compress(c("aAbB", "AA"), "a", "i"), c("bB", ""))
  expect_identical(sas_f_compress(c("a]b^c-d\\e", "x"), "]^-\\"), c("abcde", "x"))
  expect_identical(sas_f_compress(c("a\tb c", " "), "", "s"), c("abc", ""))
  expect_identical(sas_f_lengthc(c("", "  ", "stra\u00dfe", "e\u0301", NA)), c(0, 2, 7, 3, NA))
})

test_that("TRANWRD with one target: blanks, empty and missing values", {
  expect_identical(sas_f_tranwrd(c("a b a", "", NA, "aa  "), "a", "xy"), c("xy b xy", "", NA, "xyxy  "))
  if (isTRUE(l10n_info()[["UTF-8"]])) expect_identical(sas_f_tranwrd(c(n = "stra\u00dfe"), "\u00df", "ss"), "strasse")
  expect_identical(sas_f_tranwrd(c("ab", "b"), "b", ""), c("a", ""))
})
