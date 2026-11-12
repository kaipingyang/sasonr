test_that("missing values sort below numbers, with special missings ordered", {
  expect_equal(sas_lt(NA_real_, -1e300), 1)
  expect_equal(sas_lt(sas_missing("._"), NA_real_), 1)
  expect_equal(sas_lt(sas_missing(".A"), NA_real_), 0)
  expect_equal(sas_miss_code(c(1, NA, sas_missing(".Z"))), c(NA, 1L, 27L))
})

test_that("character comparison pads with blanks", {
  expect_equal(sas_eq("ab", "ab   "), 1)
  expect_equal(sas_lt("a", "a!"), 1)
  expect_equal(sas_cmp_trunc("=", "ABC", "AB"), 1)
  expect_equal(sas_char("abcd", 2L), "ab")
  expect_equal(sas_char("hi", 4L), "hi  ")
})

test_that("SAS functions ignore missing values", {
  expect_equal(sas_f_sum(NA_real_, 1), 1)
  expect_true(is.na(sas_f_sum(NA_real_, NA_real_)))
  expect_equal(sas_f_min(NA_real_, 3), 3)
  expect_equal(sas_f_mod(-7, 3), -1)
  expect_equal(sas_f_round(2.675, 0.01), 2.68)
  expect_equal(sas_f_round(-2.5), -3)
  expect_equal(sas_f_cats(" a ", 1, "b "), "a1b")
})

test_that("dates use the 1960 epoch and SAS formats", {
  d <- sas_parse_dt_literal("15MAR2026", "date")
  expect_equal(sas_format(d, list(name = "date", w = 9L, d = NA)), "15MAR2026")
  expect_equal(sas_format(d, list(name = "yymmdd", w = 10L, d = NA)), "2026-03-15")
  expect_equal(sas_put_value(37800, list(name = "time", w = 5L, d = NA)), "10:30")
  expect_equal(sas_f_intck("month", sas_parse_dt_literal("01JAN2026", "date"), d), 2)
  expect_equal(sas_f_intnx("month", d, 1, "e"), sas_parse_dt_literal("30APR2026", "date"))
})

test_that("BEST12. formatting", {
  expect_equal(trimws(sas_fmt_best(c(8, -4, 3.5, 1 / 3, 1e15))), c("8", "-4", "3.5", "0.3333333333", "1E15"))
  expect_equal(sas_put_value(sas_missing(".A")), "A")
})

test_that("LAG queues only advance when executed", {
  st <- new.env()
  expect_true(is.na(sas_lag(st, "k", 1)))
  expect_equal(sas_lag(st, "k", 3), 1)
  expect_equal(sas_lag_vec(c(1, 2, 3)), c(NA, 1, 2))
})

test_that("ROUND is vectorised over x", {
  expect_equal(sas_f_round(c(1.234, 2.345, -0.125), 0.01), c(1.23, 2.35, -0.13))
  res <- sas_run("data a; input x; y = round(x, 0.1); datalines;\n1.24\n2.25\n;\nrun;")
  expect_equal(res$work$a$y, c(1.2, 2.3))
})

test_that("FIRSTOBS=/OBS= count observations after WHERE", {
  code <- "data a; input x; datalines;\n1\n2\n3\n4\n5\n;\nrun;
data b; set a(firstobs=2 obs=3); where x > 1; run;
data c; set a(firstobs=2 where=(x ne 2)); run;"
  res <- sas_run(code)
  expect_equal(res$work$b$x, c(3, 4))
  expect_equal(res$work$c$x, c(3, 4, 5))
  expect_equal(res$work, sas_run(code, level = "L3")$work)
})

test_that("PUT: no blank after literals; formatted items keep their field", {
  log <- sas_run("data _null_; x = 14; y = 3.14159; put 'x=' x ' y=' y; put 'b=[' y best8. ']'; put x= y= 5.2; run;")$log
  expect_equal(log, c("x=14  y=3.14159", "b=[ 3.14159]", "x=14 y=3.14"))
})

test_that("arithmetic on special missing values and overflow yields ordinary missing", {
  # EXP is approximate (last bits): strict mode off for the overflow case
  log <- withr::with_options(list(sasonr.strict = FALSE), suppressWarnings(
    sas_run("data _null_; a = .A + 1; b = -.B; c = exp(1000); d = . ** 0; put a= b= c= d=; run;")))$log
  # EXP overflow is a data error: _ERROR_ is set and the PDV is dumped
  expect_equal(grep("^NOTE", log, value = TRUE, invert = TRUE), c("a=. b=. c=. d=.", "a=. b=. c=. d=. _ERROR_=1 _N_=1"))
})

test_that("SUBSTR: nonpositive length returns the remainder, invalid position a blank", {
  log <- sas_run("data _null_; a = substr('hello', 2, -1); b = substr('hello', 0, 2); c = substr('hello', 9); d = substr('hello', 2, 9);
    put '[' a '][' b '][' c '][' d ']'; run;")$log
  # a blank character value is dumped as one blank (SAS: "b=  c=")
  expect_equal(grep("^NOTE", log, value = TRUE, invert = TRUE), c("[ello ][ ][ ][ello ]", "a=ello b=  c=  d=ello _ERROR_=1 _N_=1"))
})

test_that("BESTw. chooses E notation as SAS does", {
  expect_equal(trimws(sas_fmt_best(c(1e-7, 12345678901234.5, 1 / 3 * 1e-8, 0.000123, 1 / 3), 12)),
               c("1E-7", "1.2345679E13", "3.3333333E-9", "0.000123", "0.3333333333"))
})

test_that("short date format widths and invalid widths", {
  log <- sas_run("data _null_; d = '17mar2013'd; put d mmddyy6. ' ' d yymmdd5. ' ' d mmddyy4. ' ' d ddmmyy8.; run;")$log
  expect_equal(log, "031713 13-03 0317 17/03/13")
  expect_error(sas_run("data _null_; d = 1; put d date4.; run;"), class = "sasonr_syntax_error")
})

test_that("MERGE/SET use the first data set's length for character BY variables", {
  code <- "data a; length k $2; k = 'AB'; x = 1; run;
data b; length k $3; k = 'ABC'; y = 2; run;
data m; merge a b; by k; run;"
  res <- sas_run(code)
  expect_equal(res$work$m$k, "AB")
  expect_equal(res$work$m$y, 2)
})

test_that("RENAME statement applies before output DROP=; data set WHERE= beats WHERE", {
  code <- "data s; input lane weight; datalines;\n1 410\n3 630\n;\nrun;
data r(drop=kg); set s; rename weight=kg; run;
data w; set s(where=(lane=1)); where lane=3; run;"
  res <- sas_run(code)
  expect_equal(names(res$work$r), "lane")
  expect_equal(res$work$w$lane, 1)
})

test_that("invalid programs SAS rejects fail loud", {
  expect_error(sas_run("data _null_; x = 1e400; run;"), class = "sasonr_syntax_error")
  expect_error(sas_run("data a; length c $0; run;"), class = "sasonr_syntax_error")
  expect_error(sas_run("data a; length n 2; run;"), class = "sasonr_syntax_error")
  expect_error(sas_run("data a; a = 'x'; format a best8.; run;"), class = "sasonr_syntax_error")
  expect_error(sas_run("data a; x = 1; run; data b; set a(drop=nothere); run;"), class = "sasonr_runtime_error")
  expect_error(sas_run("data _null_; x = 1; select (x); otherwise; when (1); end; run;"), class = "sasonr_syntax_error")
})

test_that("TIMEw.d writes fractional seconds", {
  expect_equal(sas_run("data _null_; t = 37845.9; put t time12.3; run;")$log, "10:30:45.900")
})
