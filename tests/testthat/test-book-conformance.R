# Differences found by running book programs against real SAS 9.4 (our own
# small programs reproduce each one).

test_that("PROC SQL: chained comparisons, INPUT() of a column, COUNT(UNIQUE x)", {
  code <- "data t; x = -2; s = '2003-09-05'; output; x = 3; s = '2003-09-06'; output; run;
    proc sql; create table a as select x, (-5 <= x <= -1) as c,
      case when -5 <= x <= -1 then 'YES' else 'NO' end as w, input(s, yymmdd10.) as d from t;
      create table n as select count(unique x) as k from t; quit;"
  # INPUT() in PROC SQL is verified by the function-pilot SQL cases
  # INPUT_039/040, which read SASHELP: gated where SASHELP is not exported
  if (!"sql" %in% sasonr:::feature_contexts("INPUT")) expect_error(sas_run(code), class = "sasonr_not_verified")
  res <- withr::with_options(list(sasonr.strict = FALSE), withCallingHandlers(
    sas_run(code), sasonr_unverified = function(w) invokeRestart("muffleWarning")))
  expect_equal(res$work$a$c, c(1, 0))
  expect_equal(res$work$a$w, c("YES", "NO"))
  expect_equal(res$work$a$d, c(15953, 15954))
  expect_equal(res$work$n$k, 2)
})

test_that("numeric variables with LENGTH < 8 are stored truncated", {
  res <- sas_run("data c; length x 5; x = 36.6; y = 36.6; run;")
  expect_equal(sprintf("%.9f", res$work$c$x), "36.599999905")
  expect_identical(res$work$c$y, 36.6)
  expect_identical(sasonr:::sas_num_store(c(36.6, NA), 8L), c(36.6, NA))
})

test_that("PROC PRINT: mixed-case headings split, common decimals, FORMAT statement", {
  res <- sas_run("data d; input ToadName $ MeanDaysRain Cost; datalines;
Lucky 15 1253
Spot 18 11350.7
Hop 12 3098.63
;
proc print data=d; format Cost dollar10.2; run;
proc print data=d; var Cost; run;")
  l <- res$listing
  expect_true(any(grepl("^ +Toad +Mean", l)))
  expect_true(any(grepl("Name +DaysRain", l)))
  expect_true(any(grepl("\\$11,350.70", l)))
  expect_true(any(grepl(" 1253.00$", l)))
  expect_error(sas_run("data d; x = 1; run; proc print data=d; format x $5.; run;"), class = "sasonr_syntax_error")
})

test_that("stored names keep their spelling from INPUT groups, ARRAY lists and LABEL", {
  res <- sas_run("data a; input (Score1-Score2) (2.); array s (2) Song1 - Song2; Song1 = 1; datalines;
1 2
;
data b; label Zed = 'z' Alpha = 'a'; input Alpha Zed; datalines;
1 2
;")
  expect_equal(unname(unlist(res$meta$a$disp)), c("Score1", "Score2", "Song1", "Song2"))
  # LABEL fixes the position of the variables it names first
  expect_equal(unname(unlist(res$meta$b$disp)), c("Zed", "Alpha"))
  # a variable named only in LABEL is not created
  res <- sas_run("data c; label q = 'only a label'; x = 1; run;")
  expect_equal(res$meta$c$vars, "x")
})

test_that("%ELSE %IF ... %THEN %DO chains inside a macro", {
  res <- sas_run("%macro a(v); %if &v = 1 %then %do; %put one; %end;
    %else %if &v = 2 %then %do; %put two; %end; %else %do; %put other; %end; %mend a; %a(2) %a(3) %a(1)")
  expect_equal(res$log, c("two", "other", "one"))
})

test_that("quoted physical data set names fail loud; ANYDTDTE reads Julian dates", {
  expect_error(sas_run("data 'lib/x'; a = 1; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; set 'lib/x'; run;"), class = "sasonr_unsupported")
  res <- sas_run("data a; input d anydtdte10.; datalines;
2012024
;")
  expect_equal(res$work$a$d, as.numeric(as.Date("2012-01-24") - as.Date("1960-01-01")))
})

test_that("PROC FREQ crosstab headings keep the leading blanks of a format label", {
  res <- sas_run("proc format; value g 0-49 = 'young' 50-high = '  50+ '; run;
    data d; input t $ a; datalines;
x 10
x 60
;
proc freq data=d; tables t*a / nopercent norow nocol; format a g.; run;")
  expect_true(any(grepl("|  50+   |", res$listing, fixed = TRUE)))
})
