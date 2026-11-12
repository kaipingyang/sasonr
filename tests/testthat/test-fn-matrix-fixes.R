# Differences found by the reference-page examples and the verification matrix
# (tests/conformance/fn_examples, fn_matrix), fixed against the SAS 9.4 gold
# standard.

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
  expect_equal(sas_f_logsdf("NORMAL", 10), -53.23128515051247, tolerance = 1e-14)
  expect_equal(sas_f_sdf("NORMAL", 1.96), 0.024997895148220439, tolerance = 1e-12)
})

test_that("noncentral F: accurate distribution function and quantile", {
  expect_equal(sas_f_probf(1, 2, 10, 1), 0.4502696915707326, tolerance = 1e-14)
  expect_equal(sas_f_finv(0.95, 2, 10, 1), 5.9925880338395423, tolerance = 1e-12)
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
  expect_true(any(grepl("Invalid argument to function SYMGET(' mv ')", res$log, fixed = TRUE)))
})

test_that("blank literals: QUOTE and TRANWRD", {
  expect_equal(sas_f_quote(" "), "\" \"")
  expect_equal(sas_f_tranwrd("abc", "b", " "), "a c")
})
