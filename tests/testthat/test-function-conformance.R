# Behaviour checked against real SAS 9.4 in the function pilot (SUBSTR, SCAN,
# COMPRESS, INDEX, CATX, ROUND, INPUT, PUT, INTNX, INTCK).

run1 <- function(stmts, setup = "") {
  res <- sas_run(paste0("data d; ", setup, " ", stmts, " e = _error_; run;"))
  res$work$d
}

test_that("$HEXw. and HEX16. write the bytes of the value", {
  d <- run1("length r $6 h $12; r = 'bcd'; h = putc(r, '$hex12.'); x = 1; n = put(x, hex16.); z = put(-0.4 * 0, hex16.);")
  expect_equal(d$h, "626364202020")
  # a new variable gets the length of PUTC's first argument (SAS: putc('abc', '$hex6.') = '616')
  d2 <- run1("r = putc('abc', '$hex6.'); l = vlength(r);")
  expect_equal(d2$r, "616")
  expect_equal(d2$l, 3)
  expect_equal(d$n, "3FF0000000000000")
  expect_equal(sasonr:::fmt_hex_num(-0, 16L), "8000000000000000")
  expect_error(sas_run("data d; x = 1; n = put(x, hex8.); run;"), class = "sasonr_unsupported")
  # an unknown character format applied at run time fails loud instead of passing the value through
  expect_error(sas_run("data d; r = putc('a', '$nosuch8.'); run;"), class = "sasonr_unsupported")
})

test_that("SCAN: result length of the first argument, B and Q modifiers, invalid n", {
  res <- sas_run("data d; r = scan('a b c', 1); l = vlength(r); run;")
  expect_equal(res$work$d$l, 5)
  d <- run1("r = scan('one two three', 1, ' ', 'b'); q = scan('a \"b c\" d', 2, ' ', 'q');")
  expect_equal(d$r, "three")
  expect_equal(d$q, "\"b c\"")
  d <- run1("r = scan('a b c', 1.9);")
  expect_equal(d$r, "")
  expect_equal(d$e, 1)
  d <- run1("r = scan('a b c', 4);")
  expect_equal(d$e, 0)
})

test_that("mismatched argument types are converted (BEST12. / numeric read)", {
  d <- run1("i = index('a3b', 3); c = compress('a.b', .); s = substr('abcdef', '2', 2);")
  expect_equal(d$i, 0)
  expect_equal(d$c, "ab")
  expect_equal(d$s, "bc")
  d <- run1("r = round('2.5');")
  expect_equal(d$r, 3)
  expect_equal(d$e, 0)
  d <- run1("r = round('abc');")
  expect_true(is.na(d$r))
  expect_equal(d$e, 1)
  d <- run1("r = input(123, 8.);")
  expect_true(is.na(d$r))
})

test_that("too many arguments to a function with a known SAS maximum is a syntax error", {
  expect_error(sas_run("data d; r = substr('abc', 1, 2, 3); run;"), class = "sasonr_syntax_error")
  expect_error(sas_run("data d; r = round(1, 2, 3); run;"), class = "sasonr_syntax_error")
})

test_that("ROUND: invalid unit, special missing, large magnitudes, fuzz", {
  d <- run1("a = round(5, 0); b = round(.A); c = round(1e15 + 0.5); f = round(2**53 + 1); g = round(123.456, 1e-10);")
  expect_true(is.na(d$a))
  expect_equal(d$e, 1)
  expect_identical(sasonr:::sas_miss_code(d$b), sasonr:::sas_miss_code(NA_real_))
  expect_identical(d$c, 1e15 + 1)
  expect_identical(d$f, 2^53)
  expect_identical(d$g, 123.456)
  expect_equal(sasonr:::sas_f_round(c(0.285, 8.675, 1.005), 0.01), c(0.29, 8.68, 1.01))
  expect_identical(sasonr:::sas_f_round(1e300, 1e-300), 1e300)
})

test_that("CATX: numeric items, missing items kept, buffer overflow", {
  d <- run1("r = catx('-', 1e15, 1e-5, -0, 1/3, ., .A);")
  expect_equal(d$r, "1000000000000000-0.00001-0-0.3333333333-.-A")
  d <- run1("length r2 $5; r2 = catx('-', 'abc', 'def');")
  expect_equal(d$r2, "")
  expect_equal(d$e, 1)
  d <- run1("r = catx(1, 'a', 'b');")
  expect_equal(d$r, "a           1b")
})

test_that("INTNX / INTCK: floor of increment, WEEK.d, continuous counts, invalid arguments", {
  d <- run1("a = intnx('month', '15JAN2020'd, -1.5); b = intnx('week.2', '15JAN2020'd, 1);
    c = intck('week.2', '05JAN2020'd, '06JAN2020'd); f = intck('month', '01MAR2020'd, '15JAN2020'd, 'c');
    g = intck('month', '15JAN2020'd, '15MAR2020'd, 'cont');")
  expect_equal(d$a, as.numeric(as.Date("2019-11-01") - as.Date("1960-01-01")))
  expect_equal(d$b, as.numeric(as.Date("2020-01-20") - as.Date("1960-01-01")))
  expect_equal(c(d$c, d$f, d$g), c(1, -2, 2))
  expect_equal(d$e, 0)
  for (src in c("r = intck('month', '15JAN2020'd, '15MAR2020'd, 'x');", "r = intck('month', 0, 1e7);",
                "r = intnx('month', '15JAN2020:10:00:00'dt, 1);", "r = intnx('year', 0, 1e6);",
                "r = intnx('month', '15JAN2020'd, 1, 'x');")) {
    d <- run1(src)
    expect_true(is.na(d$r))
    expect_equal(d$e, 1)
  }
})

test_that("PUT: w. overflow, BEST32. of tiny values, $QUOTE. width, $w./w. on the other type", {
  d <- run1("a = put(123456789, 5.); b = put(1e10, 8.); c = put(1e-300, best32.); q = put('abc', $quote.);
    n = put(123, $5.); s = put('abc', 8.);")
  expect_equal(d$a, "123E6")
  expect_equal(d$b, "    1E10")
  expect_equal(trimws(d$c), "1E-300")
  expect_equal(d$q, "\"abc\"")
  expect_equal(d$n, "  123")
  expect_equal(d$s, "abc")
  expect_equal(sasonr:::sas_format(strrep("x", 10), list(name = "quote", w = 6L, d = NA_integer_, char = TRUE)), "\"xxxx\"")
})

test_that("%SYSFUNC: COMPRESS with three arguments, INTCK and INTNX", {
  res <- sas_run("%let a = %sysfunc(compress(a1b2,,d)); %let b = %sysfunc(intck(month,0,31));
    %let c = %sysfunc(intnx(month,0,1,e));")
  expect_equal(res$macro_vars$a, "ab")
  expect_equal(res$macro_vars$b, "1")
  expect_equal(res$macro_vars$c, "59")
})

test_that("INTCK / INTNX support WEEKDAY, SEMIMONTH and TENDAY (SAS gold)", {
  res <- sas_run("
data d;
  a = intck('weekday', '17JAN2020'd, '20JAN2020'd);
  b = intck('semimonth', '14JAN2020'd, '16JAN2020'd);
  c = intck('tenday', '09JAN2020'd, '11JAN2020'd);
  e = intnx('weekday', '17JAN2020'd, 1);
  f = intnx('tenday', '15FEB2020'd, 1);
  g = intnx('semimonth', '20JAN2020'd, 1);
run;")
  expect_equal(res$work$d$a, 1)       # Fri -> Mon crosses one weekday
  expect_equal(res$work$d$b, 1)       # 14th (1-15) -> 16th (16-end)
  expect_equal(res$work$d$c, 1)       # 9th (1-10) -> 11th (11-20)
  expect_equal(res$work$d$e, 21934)   # Fri 17 Jan 2020 + 1 weekday = Mon 20 Jan
  expect_equal(res$work$d$f, 21966)   # 15 Feb -> start of 21-29 block
  expect_equal(res$work$d$g, 21946)   # 20 Jan -> start of 1-15 block of February
})
test_that("EXIST() reports WORK and SASHELP members", {
  res <- sas_run("data work.present; x = 1; run;
data work.check;
  e1 = exist('work.present');
  e2 = exist('work.absent');
  e3 = exist('present');
  e4 = exist('work.present', 'DATA');
run;")
  expect_equal(res$work$check$e1, 1)
  expect_equal(res$work$check$e2, 0)
  expect_equal(res$work$check$e3, 1)
  expect_equal(res$work$check$e4, 1)
  # a member of SASHELP (inst/sashelp) and an absent one
  res2 <- sas_run("data c; e = exist('sashelp.class'); f = exist('sashelp.nope'); run;")
  expect_equal(res2$work$c$e, 1)
  expect_equal(res2$work$c$f, 0)
  # unverified shapes fail loud instead of guessing
  expect_error(sas_run("data c; e = exist('other.ds'); run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data c; e = exist('work.x', 'VIEW'); run;"), class = "sasonr_unsupported")
})