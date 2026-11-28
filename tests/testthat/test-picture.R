# PROC FORMAT PICTURE: digit selectors, multiplier, prefix, message characters.

pic_put <- function(fmt_stmt, values, fmt) {
  r <- sas_run(paste0("proc format; ", fmt_stmt, " run; data _null_; do x = ", values, "; put '[' x ", fmt, " ']'; end; run;"))
  grep("^\\[", as.character(sas_log(r, echo = FALSE)), value = TRUE)
}

test_that("digit selectors: zero suppression, selectors 1-9, truncation, overflow keeps the last digits", {
  expect_identical(pic_put("picture p low-high='000.0';", "0, 7, 0.5, 12.345, 1000, -42, ., .A", "p10."),
                   c("[          ]", "[       7.0]", "[         5]", "[      12.3]", "[          ]", "[      42.0]", "[         .]", "[         A]"))
  expect_identical(pic_put("picture p low-high='0,000,009';", "7, 1000", "p10."), c("[         7]", "[     1,000]"))
  expect_identical(pic_put("picture p low-high='1999';", "7", "p10."), "[      0007]")
  expect_identical(pic_put("picture p low-high='009';", "12345", "p3."), "[345]")
})

test_that("MULT=, PREFIX=, message characters, NOEDIT, ranges and a nested format", {
  expect_identical(pic_put("picture p low-high='009.9' (mult=100);", "0.049, 1.25", "p10."), c("[       0.4]", "[      12.5]"))
  expect_identical(pic_put("picture p low-<0='009.9' (prefix='-') 0-high='009.9';", "-1.5, 1.5", "p10."), c("[      -1.5]", "[       1.5]"))
  expect_identical(pic_put("picture p low-high='(009)';", "7", "p10."), "[        7)]")
  expect_identical(pic_put("picture p low-high='009' (noedit);", "7", "p10."), "[       009]")
  expect_identical(pic_put("value v low-<10='small'; picture p 0-<10='9.99' other=[v];", "-1, 5", "p10."), c("[small     ]", "[      5.00]"))
  expect_error(sas_run("proc format; picture p low-high='009' (fill='*'); run;"), class = "sasonr_unsupported")
})
