# Function semantics of SUBSTR, SCAN, COMPRESS, INDEX, CATX, ROUND, INPUT, PUT,
# INTNX, INTCK and EXIST.

run1 <- function(stmts, setup = "") {
  res <- sas_run(paste0("data d; ", setup, " ", stmts, " e = _error_; run;"))
  res$work$d
}

test_that("$HEXw. and HEX16. write the bytes of the value", {
  d <- run1("length r $6 h $12; r = 'bcd'; h = putc(r, '$hex12.'); x = 1; n = put(x, hex16.); z = put(-0.4 * 0, hex16.);")
  expect_equal(d$h, "626364202020")
  # a new variable gets the length of PUTC's first argument (putc('abc', '$hex6.') = '616')
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

test_that("INTCK / INTNX support WEEKDAY, SEMIMONTH and TENDAY", {
  res <- sas_run("
data d;
  a = intck('weekday', '17JAN2020'd, '20JAN2020'd);
  b = intck('semimonth', '14JAN2020'd, '16JAN2020'd);
  c = intck('tenday', '09JAN2020'd, '11JAN2020'd);
  e = intnx('weekday', '17JAN2020'd, 1);
  f = intnx('tenday', '15FEB2020'd, 1);
run;")
  expect_equal(res$work$d$a, 1)       # Fri -> Mon crosses one weekday
  expect_equal(res$work$d$b, 1)       # 14th (1-15) -> 16th (16-end)
  expect_equal(res$work$d$c, 1)       # 9th (1-10) -> 11th (11-20)
  expect_equal(res$work$d$e, 21934)   # Fri 17 Jan 2020 + 1 weekday = Mon 20 Jan
  expect_equal(res$work$d$f, 21966)   # 15 Feb -> start of 21-29 block
})
test_that("EXIST() reports WORK and SASHELP members", {
  # 1 / 0 as EXIST returns them
  res <- sas_run("data work.present; x = 1; run;
data work.check;
  e1 = exist('work.present');
  e2 = exist('work.absent');
  e3 = exist('present');
  e4 = exist('work.present', 'DATA');
  e5 = exist('WORK.PRESENT');
  e6 = exist('work.present', 'ANY');
run;")
  expect_identical(unlist(res$work$check), c(e1 = 1, e2 = 0, e3 = 1, e4 = 1, e5 = 1, e6 = 0))
  # PROC SQL
  r2 <- sas_run("data work.present; x = 1; run; proc sql; create table t as select exist('work.present') as e from present; quit;")
  expect_identical(r2$work$t$e, 1)
  # a SASHELP member missing from the SASHELP directory fails loud (it may still be a SASHELP member)
  expect_error(sas_run("data c; f = exist('sashelp.nope'); run;", on_error = "abort"), class = "sasonr_unsupported")
  # unsupported shapes fail loud instead of guessing
  expect_error(sas_run("data c; e = exist('other.ds'); run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data c; e = exist('work.x', 'VIEW'); run;"), class = "sasonr_unsupported")
})
test_that("VLENGTH of a numeric variable is its storage length", {
  vl <- function(code, ...) {
    r <- sas_run(code, ...)
    sub("^L ", "", grep("^L ", r$log, value = TRUE))
  }
  expect_identical(vl("data _null_; length x3 3 x4 4 x7 7 x8 8; x3 = 1; l3 = vlength(x3); l4 = vlength(x4);
    l7 = vlength(x7); l8 = vlength(x8); l = vlength(z); z = 1; put 'L ' l3= l4= l7= l8= l=; run;"),
    "l3=3 l4=4 l7=7 l8=8 l=8")
  expect_identical(vl("data _null_; attrib x length=5; x = 1; l = vlength(x); put 'L ' l=; run;", level = "L3"), "l=5")
  # the length travels with the data set into a later step, also through an array
  expect_identical(vl("data a; length x 4; x = 1; run; proc sort data=a out=b; by x; run;
    data _null_; set b; array q{2} x y; l1 = vlength(q{1}); l2 = vlength(q{2}); l = vlength(x); put 'L ' l1= l2= l=; run;"),
    "l1=4 l2=8 l=4")
  expect_error(sas_run("data _null_; length x 6; v = 'x'; l = vlengthx(v); run;", on_error = "abort"),
               class = "sasonr_unsupported")
})

test_that("YEAR, MONTH, DAY, QTR, WEEKDAY: 1582, 1960, year 4000 (no 29 February), 19950, missing values", {
  y <- c(1582, 1582, 1959, 1960, 3999, 4000, 19950, 20000)
  m <- c(10, 10, 12, 1, 12, 2, 6, 12)
  dd <- c(14, 15, 31, 1, 31, 28, 30, 31)
  d <- lenient(sas_f_mdy(m, dd, y))
  expect_identical(lenient(sas_f_year(d)), y)
  expect_identical(lenient(sas_f_month(d)), m)
  expect_identical(lenient(sas_f_day(d)), dd)
  expect_identical(lenient(sas_f_qtr(d)), c(4, 4, 4, 1, 4, 1, 2, 4))
  expect_identical(lenient(sas_f_weekday(c(0, -1, -0.5, 6, 0.9))), c(6, 5, 5, 5, 6))
  feb28 <- lenient(sas_f_mdy(2, 28, 4000))
  expect_identical(lenient(sas_f_mdy(3, 1, 4000)) - feb28, 1)
  expect_identical(lenient(c(sas_f_month(feb28 + 1), sas_f_day(feb28 + 1))), c(3, 1))
  miss <- c(NA, sas_missing(".A"), sas_missing("._"))
  for (f in list(sas_f_year, sas_f_month, sas_f_day, sas_f_qtr, sas_f_weekday)) {
    expect_identical(lenient(f(miss)), rep(NA_real_, 3))
  }
})
