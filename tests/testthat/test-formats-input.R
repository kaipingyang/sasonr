# Formats, informats, PUT()/INPUT(), PROC FORMAT, INPUT statement, INFILE,
# _ERROR_ and the PDV dump.

run_log <- function(code, ...) {
  log <- sas_run(code, ...)$log
  grep("^NOTE", log, value = TRUE, invert = TRUE)
}
fmt <- function(x, text) sas_format(x, sas_fmt_text(text))

test_that("numeric formats follow the SAS width ladder", {
  expect_equal(fmt(3.14159, "8.2"), "    3.14")
  expect_equal(fmt(0.5, "3.2"), ".50")
  # too narrow: integer mantissa + exponent (SAS: PUT(123456789, 5.) is 123E6)
  expect_equal(fmt(123456, "4."), "12E4")
  expect_equal(fmt(2.675, "8.2"), "    2.68")
  expect_equal(fmt(-12, "z5."), "-0012")
  expect_equal(fmt(1234567, "comma10."), " 1,234,567")
  expect_equal(fmt(-1234.5, "dollar10.2"), "$-1,234.50")
  expect_equal(fmt(0.1234, "percent8.1"), "  12.3% ")
  expect_equal(fmt(-0.05, "percent8.1"), "(  5.0%)")
  expect_equal(fmt(-1234, "negparen8."), " (1,234)")
  expect_equal(fmt(123.456, "e12."), " 1.23456E+02")
  expect_equal(fmt(0.00001, "pvalue6.4"), "<.0001")
  expect_equal(fmt(NA_real_, "8.2"), "       .")
})

test_that("date, time and datetime formats", {
  d <- sas_parse_dt_literal("15JAN2024", "date")
  expect_equal(fmt(d, "date9."), "15JAN2024")
  expect_equal(fmt(d, "date7."), "15JAN24")
  expect_equal(fmt(d, "yymmdd10."), "2024-01-15")
  expect_equal(fmt(d, "yymmddn8."), "20240115")
  expect_equal(fmt(d, "yymmdds10."), "2024/01/15")
  expect_equal(fmt(d, "mmddyy10."), "01/15/2024")
  expect_equal(fmt(d, "ddmmyy10."), "15/01/2024")
  expect_equal(fmt(d, "e8601da."), "2024-01-15")
  expect_equal(fmt(d, "monyy7."), "JAN2024")
  expect_equal(trimws(fmt(d, "worddate.")), "January 15, 2024")
  expect_equal(trimws(fmt(d, "weekdate.")), "Monday, January 15, 2024")
  dt <- d * 86400 + 37800
  expect_equal(fmt(dt, "datetime20."), "  15JAN2024:10:30:00")
  expect_equal(fmt(dt, "datetime."), "15JAN24:10:30:00")
  expect_equal(fmt(dt, "e8601dt."), "2024-01-15T10:30:00")
  expect_equal(fmt(37845.9, "time12.3"), "10:30:45.900")
  expect_equal(fmt(11700, "tod8."), "03:15:00")
  expect_equal(fmt(11700, "tod4."), "3:15")
  expect_equal(fmt(59400, "hour."), "17")
})

test_that("informats read SAS text", {
  rd <- function(s, text) sas_informat_read(s, sas_fmt_text(text))$v
  expect_equal(rd("15JAN2024", "date9."), 23390)
  expect_equal(rd("2024-01-15", "yymmdd10."), 23390)
  expect_equal(rd("01/15/2024", "mmddyy10."), 23390)
  expect_equal(rd("15JAN24", "date7."), 23390)
  expect_equal(rd("16JAN26", "date7."), 24122) # YEARCUTOFF window
  expect_equal(rd("2024-01-15T10:30:00", "e8601dt."), 23390 * 86400 + 37800)
  expect_equal(rd("1:30:00 PM", "time11."), 48600)
  expect_equal(rd("$1,234.50", "comma10."), 1234.5)
  expect_equal(rd("(12%)", "percent6."), -0.12)
  expect_equal(rd("12345", "8.2"), 123.45)
  expect_equal(rd("01/15/2024", "anydtdte."), 23390) # month first (DATESTYLE=MDY)
  expect_equal(rd("15JAN2024", "anydtdte."), 23390)
  expect_false(sas_informat_read("12 45", sas_fmt_text("8."))$ok)
})

test_that("PUT() and INPUT() functions", {
  log <- run_log("data _null_;
    length s $12;
    d = input('2024-01-15', yymmdd10.);
    s = put(d, date9.);
    n = input('abc', ?? 8.);
    c = put(42, 5.);
    l = put(42, 5. -l);
    put s= n= '[' c $char5. ']' '[' l $char5. ']';
  run;")
  expect_equal(log, "s=15JAN2024 n=. [   42][42   ]")
})

test_that("INPUT() with invalid data sets _ERROR_ and dumps the PDV", {
  log <- run_log("data _null_; x = input('abc', 8.); y = 1; run;")
  expect_equal(log, "x=. y=1 _ERROR_=1 _N_=1")
  # `?` suppresses the NOTE only, `??` also _ERROR_
  expect_equal(run_log("data _null_; x = input('abc', ?? 8.); run;"), character())
  full <- sas_run("data _null_; x = input('abc', ? 8.); run;")$log
  expect_false(any(grepl("^NOTE", full)))
  expect_equal(full, "x=. _ERROR_=1 _N_=1")
})

test_that("_ERROR_ is live inside the step and the step falls back to row-wise code", {
  log <- run_log("data _null_;
    a = substr('hello', 0, 2); if _error_ then put 'caught';
    _error_ = 0; b = sqrt(4); if _error_ then put 'bad'; else put 'clean';
  run;")
  expect_equal(log, c("caught", "clean"))
  r <- sas_run("data a; input x; datalines;\n1\n0\n2\n;\nrun; data b; set a; y = 1 / x; run;")
  expect_equal(r$work$b$y, c(1, NA, 0.5))
  expect_equal(r$translation$steps[[2]]$level, "L3")
  expect_equal(grep("^NOTE", r$log, invert = TRUE, value = TRUE), "x=0 y=. _ERROR_=1 _N_=2")
  # constant division by zero is folded at compile time: no _ERROR_
  expect_equal(run_log("data _null_; z = 5 / 0; if _error_ then put 'err'; run;"), character())
})

test_that("PROC FORMAT VALUE and INVALUE", {
  log <- run_log("proc format;
      value agef low-<18 = 'Child' 18-64 = 'Adult' 65-high = 'Senior' . = 'Missing';
      value $sexf 'M' = 'Male' 'F' = 'Female' other = 'Unknown';
      invalue yn 'Y' = 1 'N' = 0 other = .;
    run;
    data _null_;
      do age = 5, 18, 64, 70, .;
        a = put(age, agef.);
        put a=;
      end;
      s = put('M', $sexf.); t = put('X', $sexf.);
      y = input('Y', yn.); n = input('Q', yn.);
      put s= t= y= n=;
    run;")
  expect_equal(log, c("a=Child", "a=Adult", "a=Adult", "a=Senior", "a=Missing", "s=Male t=Unknown y=1 n=."))
  expect_error(sas_run("proc format; value date 1 = 'x'; run;"), class = "sasonr_syntax_error")
  # a FORMAT statement attaches a user format to PUT and PROC PRINT
  r <- sas_run("proc format; value g 1 = 'one' 2 = 'two'; run;
    data a; x = 1; output; x = 2; output; format x g.; run;
    proc print data=a noobs; run;")
  expect_equal(trimws(r$listing[r$listing != ""]), c("x", "one", "two"))
})

test_that("INPUT pointer controls, column and formatted input", {
  log <- run_log("data _null_;
    input @3 a $2. +1 b 2. @'ID=' id $3. #2 c 1-3 / d;
    put a= b= id= c= d=;
  datalines;
  XY 12 ID=abc
 77
5
;
run;")
  expect_equal(log, "a=XY b=12 id=abc c=77 d=5")
  log <- run_log("data _null_; input x @@; put x=; datalines;\n1 2\n3\n;\nrun;")
  expect_equal(log, c("x=1", "x=2", "x=3"))
  log <- run_log("data _null_; input type $ @; if type = 'A' then input x; else input y 3-4; put type= x= y=;
datalines;
A 5
B 77
;
run;")
  expect_equal(log, c("type=A x=5 y=.", "type=B x=. y=77"))
  log <- run_log("data _null_; input name & $20. age; put name= age=; datalines;\nJohn Smith  30\n;\nrun;")
  expect_equal(log, "name=John Smith age=30")
})

test_that("INFILE DATALINES options", {
  log <- run_log("data _null_; infile datalines dsd; input a b $ c; put a= b= c=;
datalines;
1,\"x,y\",3
4,,6
;
run;")
  expect_equal(log, c("a=1 b=x,y c=3", "a=4 b= c=6"))
  log <- run_log("data _null_; infile datalines missover; input a b c; put a= b= c=;
datalines;
1 2
3 4 5
;
run;")
  expect_equal(log, c("a=1 b=2 c=.", "a=3 b=4 c=5"))
  log <- run_log("data _null_; infile datalines dlm='|' firstobs=2 obs=3; input a b; put a= b=;
datalines;
9|9
1|2
3|4
5|6
;
run;")
  expect_equal(log, c("a=1 b=2", "a=3 b=4"))
})

test_that("INFILE reads an external file; LOST CARD is a data error", {
  f <- tempfile(fileext = ".txt")
  writeLines(c("a  1", "bb"), f)
  log <- run_log(sprintf("data _null_; infile '%s' truncover; input s $ 1-2 n 4-5; put s= n=; run;", f))
  expect_equal(log, c("s=a n=1", "s=bb n=."))
  r <- sas_run("data _null_; input a b; put a= b=; datalines;\n1 2\n3\n;\nrun;")
  expect_true("NOTE: LOST CARD." %in% r$log)
  expect_true("a=3 b=. _ERROR_=1 _N_=2" %in% r$log)
})

test_that("new character functions", {
  log <- run_log("data _null_;
    a = find('Hello World', 'world', 'i');
    b = countw('a b  c');
    c = compress('a1b2c3', , 'd');
    d = compbl('a   b');
    e = scan('a,b,,c', 3, ',', 'm');
    f = propcase('hello-world');
    g = prxchange('s/(\\w+) (\\w+)/$2 $1/', -1, 'ab cd');
    h = prxmatch('/[0-9]+/', 'ab123');
    i = substrn('abc', 0, 2);
    j = verify('123a', '0123456789');
    k = quote('it''s');
    put a= b= c= d= e= f= g= h= i= j= k=;
  run;")
  expect_equal(log, "a=7 b=3 c=abc d=a b e= f=Hello-World g=cd ab h=3 i=a j=4 k=\"it's\"")
})

test_that("new numeric and date functions", {
  log <- run_log("data _null_;
    a = rounde(2.5); b = std(1, 2, 3); c = largest(2, 5, 9, 1); d = ifn(0, 1, 2);
    e = intnx('month', '15JAN2024'd, 1, 'e'); f = intck('month', '31JAN2024'd, '01FEB2024'd);
    g = yrdif('15JUN1980'd, '01MAR2024'd, 'AGE'); h = datdif('01JAN2024'd, '01MAR2024'd, 'ACT/ACT');
    i = dhms('01JAN2024'd, 1, 2, 3) - '01JAN2024:00:00:00'dt; j = mod(0.3, 0.1);
    put a= b= c= d= e= date9. f= g= 8.4 h= i= j=;
  run;")
  expect_equal(log, "a=2 b=1 c=5 d=2 e=29FEB2024 f=1 g=43.7104 h=60 i=3723 j=0")
})

test_that("PUT wraps at the 132-column log line size", {
  vars <- paste0("v", 1:30, " = 12345;", collapse = " ")
  log <- sas_run(paste("data _null_;", vars, "put", paste0("v", 1:30, "=", collapse = " "), "; run;"))$log
  expect_equal(length(log), 3L)
  expect_true(all(nchar(log) <= 132))
})

test_that("CALL MISSING / SORTN, STOP, INFORMAT and ATTRIB statements", {
  log <- run_log("data _null_;
    array a{4} (3 . 1 2);
    call sortn(of a{*});
    put a1= a2= a3= a4=;
    x = 1; s = 'q';
    call missing(x, s);
    put x= s=;
  run;")
  expect_equal(log, c("a1=. a2=1 a3=2 a4=3", "x=. s="))
  r <- sas_run("data a; do i = 1 to 10; output; if i = 3 then stop; end; run;")
  expect_equal(r$work$a$i, 1:3)
  log <- run_log("data _null_;
    informat d date9.;
    attrib n length=8 format=z4. label='Number';
    input d n;
    put d= date9. n= ;
  datalines;
15JAN2024 7
;
run;")
  expect_equal(log, "d=15JAN2024 n=0007")
})
