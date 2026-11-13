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

test_that("MERGE walks observations when both inputs repeat a BY group", {
  # Verified against SAS 9.4: a many-to-many MERGE pairs the two inputs
  # observation by observation (the shorter input falls back to missing) and a
  # common non-BY variable keeps the value from the last data set named.
  code <- "data a; input k x; datalines;
1 10
1 11
2 20
;
run;
data b; input k y; datalines;
1 100
1 101
2 200
;
run;
data m; merge a b; by k; run;
data c; input k v; datalines;
1 10
1 11
;
run;
data d; input k v; datalines;
1 100
1 101
;
run;
data m2; merge c d; by k; run;"
  res <- sas_run(code)
  expect_equal(res$work$m$k, c(1, 1, 2))
  expect_equal(res$work$m$x, c(10, 11, 20))
  expect_equal(res$work$m$y, c(100, 101, 200))
  expect_equal(res$work$m2$k, c(1, 1))
  expect_equal(res$work$m2$v, c(100, 101))
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

test_that("left-hand SUBSTR replaces part of a variable (SAS semantics)", {
  res <- sas_run("data d; length t $6; t='abcdef'; substr(t,2,2)='XY'; r=t; run;")
  expect_equal(res$work$d$t, "aXYdef")
  # a replacement longer than the room left is cut to the room
  res2 <- sas_run("data d; length t $6; t='abcdef'; substr(t,5)='WXYZ'; r=t; run;")
  expect_equal(res2$work$d$t, "abcdWX")
  # an invalid position is a data error: NOTE, _ERROR_, variable unchanged
  res3 <- sas_run("data d; length t $6; t='abcdef'; substr(t,8)='Z'; e=_error_; r=t; run;")
  expect_equal(res3$work$d$t, "abcdef")
  expect_equal(res3$work$d$e, 1)
  expect_true(any(grepl("Invalid second argument to function SUBSTR", res3$log)))
})
test_that("SUBSTR assignment to an undeclared variable creates it with length 8", {
  # TS1M7 has no gold for it: fail loud
  expect_error(sas_run("data d; substr(t,2,2)='XY'; run;", on_error = "abort"), class = "sasonr_unsupported")
  # SAS 9.4 TS1M2 (cases/ts1m2/datastep/substr_undeclared) compiles the
  # left-hand SUBSTR target with a default length of 8
  withr::local_options(sasonr.sas_version = "TS1M2", sasonr.strict = FALSE)
  res <- sas_run("data d; substr(t,2,2)='XY'; v=vlength(t); l=lengthn(t); r=t; rv=vlength(r); run;")
  expect_equal(res$work$d$v, 8)
  expect_equal(res$work$d$l, 3)     # " XY" after the trailing blanks are dropped
  expect_equal(res$work$d$rv, 8)    # the assignment result inherits the length
  # a declared variable keeps its own length
  res2 <- sas_run("data d; length c $4; substr(c,2,2)='XY'; v=vlength(c); run;")
  expect_equal(res2$work$d$v, 4)
  # a longer replacement is cut to the length argument
  res3 <- sas_run("data d; substr(t,2,2)='XYZ'; l=lengthn(t); run;")
  expect_equal(res3$work$d$l, 3)
})
test_that("the sign of zero in SUM / MEAN / MIN / MAX and PROC MEANS", {
  hex <- function(x) paste(rev(as.character(writeBin(x, raw()))), collapse = "")
  res <- sas_run("data f;
    p = input('0000000000000000', hex16.);
    n = input('8000000000000000', hex16.);
    m = .;
    sm_mn = sum(m,n); sm_nm = sum(n,m); sm_nn = sum(n,n);
    me_pn = mean(p,n); me_np = mean(n,p); me_npm = mean(n,p,m);
    mn_pn = min(p,n); mn_np = min(n,p);
  run;")
  f <- res$work$f
  # a missing argument counts as +0 for the sign (gold cases/ts1m2/proc/zero_sign)
  expect_identical(hex(f$sm_mn), "0000000000000000")
  expect_identical(hex(f$sm_nm), "0000000000000000")
  expect_identical(hex(f$sm_nn), "8000000000000000")
  # MEAN returns the first non-missing argument's sign when every value is zero
  expect_identical(hex(f$me_pn), "0000000000000000")
  expect_identical(hex(f$me_np), "8000000000000000")
  expect_identical(hex(f$me_npm), "8000000000000000")
  # MIN / MAX keep the first non-missing argument's sign on ties
  expect_identical(hex(f$mn_pn), "0000000000000000")
  expect_identical(hex(f$mn_np), "8000000000000000")
  # PROC MEANS: SUM and MEAN are always +0, MIN / MAX keep the first value's sign
  r2 <- sas_run("data d; length hx $16; input hx $; x = input(hx, hex16.); datalines;
8000000000000000
0000000000000000
;
proc means data=d noprint; var x; output out=o sum=s mean=mn min=mi max=ma; run;")
  expect_identical(hex(r2$work$o$s), "0000000000000000")
  expect_identical(hex(r2$work$o$mn), "0000000000000000")
  expect_identical(hex(r2$work$o$mi), "8000000000000000")  # first value is -0
  expect_identical(hex(r2$work$o$ma), "8000000000000000")
})