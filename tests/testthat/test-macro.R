mlog <- function(code, ...) msgs(suppressWarnings(sas_run(code, ...)))

test_that("macro variables resolve with &, &var. and multi-pass &&", {
  expect_equal(mlog("%let a=1; %let b=a; %put R=&&&b;"), "R=1")
  expect_equal(mlog("%let i=2; %let v2=hello; %put R=&&v&i;"), "R=hello")
  expect_equal(mlog("%let x=ab; %put &x.c &x..d;"), "abc ab.d")
  expect_equal(mlog("%let p=v; %let n=3; %let v3=deep; %put &&&p&n;"), "deep")
  expect_equal(mlog("%let x=5; %put &=x;"), "X=5")
})

test_that("%LET strips unquoted blanks; %STR keeps them", {
  expect_equal(mlog("%let x =   a b   ; %put [&x];"), "[a b]")
  expect_equal(mlog("%let x = %str(  a  ); %put [&x];"), "[  a  ]")
  expect_equal(mlog("%let x = %str(a;b); %put &x;"), "a;b")
  expect_equal(mlog("%put %str(it%'s);"), "it's")
})

test_that("double quotes resolve, single quotes do not (compiler text)", {
  code <- "%let x=1; data _null_; a = \"&x\"; b = '&x'; put a= b=; run;"
  expect_equal(mlog(code), "a=1 b=&x")
})

test_that("%NRSTR, %SUPERQ, %BQUOTE and %UNQUOTE", {
  expect_equal(mlog("%put %nrstr(&nothere %nomacro);"), "&nothere %nomacro")
  expect_equal(mlog("%let a = %nrstr(&b); %let b = B; %put %superq(a) &a;"), "&b &b")
  expect_equal(mlog("%let a = %nrstr(&b); %let b = B; %put %unquote(&a);"), "B")
  expect_equal(mlog("%let x = a,b; %put %bquote(&x);"), "a,b")
})

test_that("macro definitions: positional, keyword, PARMBUFF, no-paren calls", {
  expect_equal(
    mlog("%macro p(a, b=2, c=); %put R=&a|&b|&c|; %mend p; %p(1) %p(x, c=z, b=) %p"),
    c("R=1|2||", "R=x||z|", "R=|2||")
  )
  expect_equal(mlog("%macro q / parmbuff; %put R=&syspbuff; %mend; %q(1, 2) %q"), c("R=(1, 2)", "R="))
  expect_equal(mlog("%macro r(a); %put &a; %mend; %r( spaced  )"), "spaced")
  expect_error(sas_run("%macro r(a); %mend; %r(1, 2)"), class = "sasonr_syntax_error")
  expect_error(sas_run("%macro r(a); %mend; %r(b=1)"), class = "sasonr_syntax_error")
})

test_that("%IF / %ELSE and %DO blocks", {
  code <- "
%macro t(v);
  %if &v > 3 %then %put big; %else %put small;
  %if &v = 1 %then %do; %put one; %put uno; %end;
  %else %do; %put not one; %end;
%mend;
%t(5) %t(1)"
  expect_equal(mlog(code), c("big", "not one", "small", "one", "uno"))
  expect_equal(mlog("%let f=1; %if &f %then %do; %put yes; %end; %else %do; %put no; %end;"), "yes")
  expect_error(sas_run("%if 1 %then %put x;"), class = "sasonr_syntax_error")
})

test_that("%DO loops: iterative (incl. negative %BY), %WHILE, %UNTIL", {
  expect_equal(mlog("%macro m; %do i = 5 %to 1 %by -2; %put &i; %end; %put after=&i; %mend; %m"),
               c("5", "3", "1", "after=-1"))
  expect_equal(mlog("%macro w; %let k=1; %do %while(&k < 4); %put k=&k; %let k=%eval(&k+1); %end; %mend; %w"),
               c("k=1", "k=2", "k=3"))
  expect_equal(mlog("%macro u; %let k=10; %do %until(&k > 5); %put k=&k; %let k=%eval(&k+1); %end; %mend; %u"),
               "k=10")
  expect_equal(mlog("%macro n; %do i=1 %to 0; %put never; %end; %put i=&i; %mend; %n"), "i=1")
  expect_error(sas_run("%do i=1 %to 2; %end;"), class = "sasonr_syntax_error")
})

test_that("%EVAL is integer arithmetic; comparisons are numeric only for integers", {
  expect_equal(mlog("%put %eval(7/2) %eval(-7/2) %eval(2**3) %eval(1+2*3) %eval((1+2)*3);"), "3 -3 8 7 9")
  expect_equal(mlog("%put %eval(10 > 9) %eval(10.0 > 9) %eval(abc = abc) %eval(1 and 0 or 1);"), "1 0 1 1")
  expect_equal(mlog("%put %eval(not 0) %eval(3 ne 3) %eval(a < b);"), "1 0 1")
  expect_error(sas_run("%put %eval(1.5 + 1);"), class = "sasonr_runtime_error")
  expect_error(sas_run("%put %eval(1/0);"), class = "sasonr_runtime_error")
  expect_error(sas_run("%macro m; %if abc %then %put x; %mend; %m"), class = "sasonr_runtime_error")
})

test_that("%SYSEVALF uses floating point and conversion types", {
  expect_equal(mlog("%put %sysevalf(7/2) %sysevalf(2.5e2 + 1) %sysevalf(1e10) %sysevalf(10.5 > 9);"),
               "3.5 251 1e10 1")
  expect_equal(mlog("%put %sysevalf(2.4, ceil) %sysevalf(-2.6, integer) %sysevalf(0, boolean);"), "3 -2 0")
})

test_that("character macro functions", {
  code <- "%let s = Hello World;
%put %upcase(&s)|%lowcase(&s)|%scan(&s, 2)|%scan(&s, -1)|%substr(&s, 1, 5)|%substr(&s, 7)|%index(&s, World)|%length(&s)|%length();"
  expect_equal(mlog(code), "HELLO WORLD|hello world|World|World|Hello|World|7|11|0")
  expect_equal(mlog("%put %scan(a-b-c, 2, -);"), "b")
  expect_equal(mlog("%put %cmpres(  a   b  )|%left(  a)|%trim(a  )|;"), "a b|a|a|")
  expect_error(sas_run("%put %substr(abc, 5);"), class = "sasonr_runtime_error")
})

test_that("%SYSFUNC supports a small function set and formats", {
  today <- toupper(format(Sys.Date(), "%d%b%Y"))
  # TODAY() is supported in the macro context (function registry)
  expect_identical(mlog("%put %sysfunc(today(), date9.);"), today)
  # a function not yet supported in the macro context is gated
  expect_error(mlog("%put %sysfunc(anyalpha(abc));"), class = "sasonr_strict_error")
  expect_equal(mlog("%put %sysfunc(putn(5, z3.)) %sysfunc(compress(a b c)) %sysfunc(sum(1, 2.5));"), "005 abc 3.5")
  expect_equal(mlog("%put %sysfunc(cats(a, b, c))|%sysfunc(upcase(x))|%sysfunc(strip(  y  ));"), "abc|X|y")
  expect_error(sas_run("%put %sysfunc(rand(uniform));"), class = "sasonr_unsupported")
})

test_that("%LET writes to the nearest table that has the variable; %LOCAL shadows", {
  code <- "
%let x = global;
%macro inner; %let x = inner; %let fresh = f; %mend;
%macro outer; %local x; %let x = outer; %inner %put o=&x; %put s=%symexist(fresh); %mend;
%outer
%put g=&x; %put s=%symexist(fresh);"
  expect_equal(mlog(code), c("o=inner", "s=0", "g=global", "s=0"))
  expect_equal(mlog("%macro m; %global gg; %let gg = 1; %mend; %m %put &gg;"), "1")
  expect_error(sas_run("%local x;"), class = "sasonr_syntax_error")
})

test_that("CALL SYMPUT(X) results are visible after the step, with SAS scoping", {
  expect_equal(mlog("data _null_; call symputx('n', 3); run; %put n=&n;"), "n=3")
  expect_equal(mlog("data _null_; call symput('n', 3); run; %put [&n];"), "[           3]")
  # inside a macro with a nonempty local table -> local
  code <- "%macro m(p); data _null_; call symputx('loc', 'L'); run; %put in=&loc; %mend;
%m(1) %put out=%symexist(loc);"
  expect_equal(mlog(code), c("in=L", "out=0"))
  # empty local table -> global; 'G' forces global
  expect_equal(mlog("%macro m2; data _null_; call symputx('g1', 'A'); run; %mend; %m2 %put &g1;"), "A")
  expect_equal(mlog("%macro m3(p); data _null_; call symputx('g2', 'B', 'G'); run; %mend; %m3(1) %put &g2;"), "B")
  # the step before an implicit boundary runs before the next step's text is resolved
  expect_equal(mlog("data _null_; call symputx('a', 7); data _null_; put \"a=&a\"; run;"), "a=7")
  res <- sas_run("data _null_; call symputx('kept', 'yes'); run;")
  expect_equal(res$macro_vars$kept, "yes")
})

test_that("macro-generated steps run in order (t11/t12 pattern)", {
  code <- "%macro m; %do i=1 %to 3; data d&i; x=&i; run; %end; %mend m;
%m
data all; set d1-d3; run;
%macro chk;
  data _null_; set all end=e; s+x; if e then call symputx('tot', s); run;
  %if &tot > 5 %then %put R=BIG; %else %put R=SMALL;
%mend chk;
%chk"
  res <- sas_run(code)
  expect_equal(msgs(res), "R=BIG")
  expect_equal(res$work$all$x, c(1, 2, 3))
})

test_that("DATALINES are not macro-resolved; comments", {
  code <- "%let x=1;\ndata a; input s $; datalines;\n&x\n%put\n;\nrun;\ndata _null_; set a; put s=; run; %* a %put no; ;"
  expect_equal(mlog(code), c("s=&x", "s=%put"))
  expect_equal(mlog("/* %put hidden; */ %put shown;"), "shown")
})

test_that("unresolved references warn and stay as text; unsupported features fail loud", {
  expect_warning(r <- sas_run("%put A=&nope;"), class = "sasonr_macro_warning")
  expect_equal(msgs(r), "A=&nope")
  expect_error(sas_run("%include 'x.sas';"), class = "sasonr_unsupported")
  expect_error(sas_run("%put _user_;"), class = "sasonr_unsupported")
  expect_error(sas_run("%undefined_macro"), class = "sasonr_unsupported")
  expect_error(sas_run("%macro m / store; %mend;"), class = "sasonr_unsupported")
})

test_that("sas_translate expands macros without executing", {
  tr <- sas_translate("%macro m(n); data d&n; x = &n; run; %mend; %m(4)")
  expect_equal(tr$steps[[1]]$outputs, "d4")
  expect_match(tr$steps[[1]]$code, "x = 4", fixed = TRUE)
})

test_that("%THEN text drops the terminating semicolon; macro bodies lose line breaks", {
  code <- "%macro c(n);
  %if &n > 0 %then %str(pos; ok);
  %else %str(neg; bad);
%mend;
data _null_; length s $20; s = \"%c(5)\"; put s; s = \"%c(-1)\"; put s; run;
%macro e;
  %do i=1 %to 5;x&i.%if &i = 2 %then %let i = 99;%end;final=&i
%mend;
data _null_; put \"[%e]\"; run;"
  expect_equal(mlog(code), c("pos; ok", "neg; bad", "[x1x2final=100]"))
})

test_that("statement comments do not treat quotes specially", {
  expect_equal(mlog("* it's a comment;\ndata _null_; put 'ok'; run;"), "ok")
})

test_that("SYSINDEX counts macro invocations; %SYSMACEXIST", {
  expect_equal(mlog("%macro a; %mend; %a %a %put &sysindex %sysmacexist(a) %sysmacexist(zz);"), "2 1 0")
})

test_that("single quotes hide & in %LET; the stored text resolves later in double quotes", {
  code <- "%let t=RESOLVED;
%let q2='AT&t';
data _null_; a = \"&q2\"; b = symget('q2'); put a= / b=; run;
%let t=CHANGED;
data _null_; c = \"&q2\"; put c=; run;"
  expect_equal(mlog(code), c("a='ATRESOLVED'", "b='AT&t'", "c='ATCHANGED'"))
  expect_equal(mlog("%let x=abc; %put %bquote('&x') '&x';"), "'abc' '&x'")
})

test_that("global statements take effect before the macro code that follows them", {
  note <- paste("NOTE: Mathematical operations could not be performed during %SYSFUNC function execution.",
                "The result of the operations have been set to a missing value.")
  # a global statement applies to the macro code after it with
  # no step boundary in between, and the %SYSFUNC NOTE written at
  # LINESIZE=MAX is one line
  expect_identical(msgs(sas_run("options linesize=max;\n%let d = %sysfunc(mdy(2, 30, 2024));")), note)
  expect_identical(msgs(sas_run("%macro m; options linesize=max; %let d = %sysfunc(mdy(2, 30, 2024)); %mend; %m")), note)
  # PROC SQL runs its statements one by one
  expect_true(note %in% msgs(sas_run("proc sql; options linesize=max; %let d = %sysfunc(mdy(2, 30, 2024)); quit;")))
  # the session keeps the option for the next submission
  s <- sas_session()
  sas_run("options linesize=max;", session = s)
  expect_identical(msgs(sas_run("%let d = %sysfunc(mdy(2, 30, 2024));", session = s)), note)
  # an unsupported option fails loud at its own statement, before the macro call after it runs
  expect_error(sas_run("options minoperator; %macro m; %if a in a b %then %put yes; %mend; %m"),
               class = "sasonr_unsupported", regexp = "system option MINOPERATOR")
  expect_error(sas_run("footnote 'f'; %put after;"), class = "sasonr_unsupported", regexp = "FOOTNOTE")
  # the line of an error in an open-code statement after macro code
  e <- tryCatch(sas_run("data a; x = 1;\nrun;\n%put y;\n\noptions\n  linesize=abc;\n"), error = identity)
  expect_s3_class(e, "sasonr_syntax_error")
  expect_equal(e$sas_line, 5)
})

test_that("an OPTIONS statement inside a step applies to the macro code after it in the step", {
  note <- paste("NOTE: Mathematical operations could not be performed during %SYSFUNC function execution.",
                "The result of the operations have been set to a missing value.")
  note132 <- c(paste("NOTE: Mathematical operations could not be performed during %SYSFUNC function execution.",
                     "The result of the operations have been set"), "      to a missing value.")
  step <- function(body) {
    paste0("data a; x = 1; run;\nproc datasets lib=work nolist;\n", body,
           "\nquit;\n%let e = %sysfunc(mdy(2, 30, 2024));\ndata b; x = 1; run;")
  }
  let <- "%let d = %sysfunc(mdy(2, 30, 2024));"
  # the OPTIONS statement applies when it is compiled,
  # before the macro code after it in the step; the session keeps it
  expect_identical(msgs(sas_run(step(paste("options linesize=max;", let)))), c(note, note))
  # macro code before the OPTIONS statement runs under the LINESIZE before it
  expect_identical(msgs(sas_run(step(paste(let, "options linesize=max;")))), c(note132, note))
  # NONOTES suppresses the NOTE, in the step after the statement and after the step
  withr::local_options(sasonr.step_notes = TRUE)
  a_note <- "NOTE: The data set WORK.A has 1 observations and 1 variables."
  expect_identical(msgs(sas_run(step(paste("options nonotes;", let)))), a_note)
  expect_identical(msgs(sas_run(step(paste(let, "options nonotes;")))), c(a_note, note132))
  # ... and in open code (the %SYSFUNC call after the step)
  expect_identical(msgs(sas_run("options nonotes; %let d = %sysfunc(mdy(2, 30, 2024));")), character())
  withr::local_options(sasonr.step_notes = FALSE)
  # a variable, an array element or a sum statement named OPTIONS is not an OPTIONS statement
  r <- sas_run("data a; options = 1; %let d = %sysfunc(mdy(2, 30, 2024)); run;")
  expect_identical(msgs(r), note132)
  expect_equal(r$work$a$options, 1)
  r <- sas_run("data a; array options{2} o1 o2; options{1} = 5; %let d = %sysfunc(mdy(2, 30, 2024)); run;")
  expect_identical(msgs(r), note132)
  expect_equal(r$work$a$o1, 5)
  r <- sas_run("data a; option + 1; %let d = %sysfunc(mdy(2, 30, 2024)); run;")
  expect_identical(msgs(r), note132)
  expect_equal(r$work$a$option, 1)
})

test_that("the %SYSFUNC NOTE fails loud where the options in effect are not known", {
  in_step <- function(opt) {
    paste("data a; x = 1; run; proc datasets lib=work nolist;", opt, "%let d = %sysfunc(mdy(2, 30, 2024)); quit;")
  }
  # an OPTIONS statement of the step that does not parse before the step runs
  expect_error(sas_run(in_step("options minoperator;")), class = "sasonr_unsupported", regexp = "whose effect before the step runs")
  # inside a step only LINESIZE=MAX and NONOTES are supported
  expect_error(sas_run(in_step("options linesize=100;")), class = "sasonr_unsupported", regexp = "whose effect before the step runs")
  expect_error(sas_run(in_step("options nonotes notes;")), class = "sasonr_unsupported", regexp = "whose effect before the step runs")
})

test_that("positive and negative system options apply in the order written", {
  withr::local_options(sasonr.step_notes = TRUE)
  # the last of NOTES / NONOTES wins
  r <- sas_run("options notes nonotes; data a; x = 1; run; options nonotes notes; data b; x = 1; run;")
  expect_identical(msgs(r), "NOTE: The data set WORK.B has 1 observations and 1 variables.")
  # NOSYNTAXCHECK wins, the step after the ERROR runs
  r <- sas_run("options syntaxcheck nosyntaxcheck; data a; x = 1; run; data b; set a y = 2; run;
    data c; x = 1; run;", on_error = "sas")
  expect_identical(r$status, "error")
  expect_identical(nrow(r$work$c), 1L)
  # CENTER wins: the listing is centred
  lst <- sas_run("data a; x = 1; run; options nocenter center; proc print data=a; run;")$listing
  expect_true(any(startsWith(lst, "  ")))
  expect_false(any(startsWith(sas_run("data a; x = 1; run; options center nocenter; proc print data=a; run;")$listing, " Obs")))
  expect_no_error(sas_run("data a; x = 1; run; options center nocenter date nodate number nonumber; proc print data=a; run;"))
})

test_that("%STR(%') in %IF and %DO actions and in macro bodies", {
  code <- "%macro m(x);
  %if &x = 1 %then %let q = %str(%');
  %else %let q = none;
  %put q=&q;   /* q is the quote character; don't change it */
  data a; s = 1; run;
%mend;
%m(2)
%m(1)
"
  r <- sas_run(code)
  expect_true(all(c("q=none", "q='") %in% msgs(r)))
  expect_identical(nrow(r$work$a), 1L)
  r <- sas_run("%macro m;\n  %do;\n    %let q = %str(%');\n  %end;\n  %put q=&q;\n%mend;\n%m\n")
  expect_true("q='" %in% msgs(r))
  r <- sas_run("%macro m;\n  %let q = %str(%');\n  %put q=&q;\n%mend;\n%m\n")
  expect_true("q='" %in% msgs(r))
})

test_that("data lines generated by a macro fail loud", {
  expect_error(sas_run("%macro m;\n  data a; input x; datalines;\n1\n;\n  run;\n%mend;\n%m\n"), class = "sasonr_unsupported")
})

test_that("%' outside %STR, %NRSTR, %QUOTE and %NRQUOTE fails loud", {
  code <- "%macro m(x);\n  %if &x = 1 %then %let q = %bquote(%');\n  %if &x = 2 %then %let q = %bquote(%');\n  %put q=&q;\n%mend;\n%m(2)\n"
  expect_error(sas_run(code), class = "sasonr_unsupported")
  expect_error(sas_run("%let q = %quote(%'); %put q=&q;"), class = "sasonr_unsupported")
  expect_error(sas_run("%let q = %nrquote(%\"); %put q=&q;"), class = "sasonr_unsupported")
})

test_that("options(sasonr.start_time =) pins SYSDATE9, SYSDAY and SYSTIME", {
  withr::local_options(sasonr.start_time = as.POSIXct("2026-11-24 09:05:00", tz = "UTC"))
  r <- sas_run("%put &sysdate9 &sysday &systime;")
  expect_true(any(grepl("24NOV2026 Tuesday 09:05", as.character(sas_log(r)), fixed = TRUE)))
  withr::local_options(sasonr.start_time = "Tuesday")
  expect_error(sas_run("%put &sysday;"), class = "sasonr_error")
})

test_that("OPTIONS MPRINT writes the statements a macro generates, under the innermost macro", {
  r <- sas_run("options mprint;\n%macro inner;\n  p    =    1;\n%mend;\n%macro outer;\n  %inner\n  q = 2;\n%mend;\ndata a;\n  %outer\nrun;\noptions nomprint;\ndata b;\n  %outer\nrun;")
  l <- as.character(sas_log(r))
  expect_identical(l[startsWith(l, "MPRINT")], c("MPRINT(INNER):   p = 1;", "MPRINT(OUTER):   q = 2;"))
  expect_identical(r$work$a, tibble::tibble(p = 1, q = 2))
  # the text as generated (no blanks added), and the text of each call up to
  # its end when no semicolon ends it
  r <- sas_run("options mprint;\n%macro m;\n  x=1;\n%mend;\n%macro two;\n  2\n%mend;\ndata a;\n  %m\n  y = %two + %two;\nrun;")
  l <- as.character(sas_log(r))
  expect_identical(l[startsWith(l, "MPRINT")], c("MPRINT(M):   x=1;", "MPRINT(TWO):   2", "MPRINT(TWO):   2"))
})

test_that("%INCLUDE reads a file as the lines after the statement, not echoed", {
  d <- withr::local_tempdir()
  writeLines(c("data inc;", "  v = 1;", "run;"), file.path(d, "inc.sas"))
  withr::local_dir(d)
  r <- sas_run("data a; x = 1; run;\n%include 'inc.sas';\ndata b; set inc; run;")
  expect_identical(r$work$b, tibble::tibble(v = 1))
  l <- as.character(sas_log(r))
  expect_true(any(startsWith(l, "6          data b; set inc; run;")))
  expect_false(any(grepl("v = 1", l, fixed = TRUE)))
  expect_error(sas_run("%include 'nosuch.sas';"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; x = 1; %include 'inc.sas'; run;"), class = "sasonr_unsupported")
})

test_that("a numbered ERROR in macro-generated text echoes the generated line under the NOTE", {
  r <- sas_run("%macro bad;\n  this is not valid;\n%mend;\ndata a;\n  %bad\nrun;", on_error = "sas")
  l <- as.character(sas_log(r))
  k <- match("NOTE: Line generated by the invoked macro \"BAD\".", l)
  expect_false(is.na(k))
  expect_identical(l[k + 1:3], c("5             this is not valid;", "              ____", "              180"))
  expect_identical(ncol(r$work$a), 0L)
})

test_that("a numbered ERROR on a line of an included file echoes it with a +", {
  d <- withr::local_tempdir()
  writeLines(c("data bad;", "  this is not valid;", "run;"), file.path(d, "bad.sas"))
  withr::local_dir(d)
  l <- as.character(sas_log(sas_run("data a; x = 1; run;\n%include 'bad.sas';", on_error = "sas")))
  k <- match("4         +  this is not valid;", l)
  expect_false(is.na(k))
  expect_identical(l[k + 1:2], c("             ____", "             180"))
})

test_that("%SYSFUNC(GETOPTION(ENCODING)) is the session encoding; other options fail loud", {
  r <- sas_run("%put E=%sysfunc(getoption(encoding));")
  expect_true("E=UTF-8" %in% as.character(sas_log(r, echo = FALSE)))
  expect_error(sas_run("%put %sysfunc(getoption(linesize));"), class = "sasonr_unsupported")
})
