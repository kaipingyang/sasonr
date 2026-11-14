# Procedure and function details: PROC SQL function NOTEs, PROC FREQ cell
# statistics per SAS release, the PROC MEANS N field with BY groups, the
# WEEKDAY / TENDAY / SEMIMONTH intervals, the SUBSTR statement under strict
# mode and PROC DATASETS DELETE _ALL_.

test_that("PROC SQL function NOTEs go through sql_function_notes()", {
  one <- "data one; x = 1; run;"
  # the numbered NOTE of SCAN (argument 2 invalid) has no supported PROC SQL form
  expect_error(sas_run(paste(one, "proc sql; create table t as select scan('a b c', 0) as w from one; quit;")),
               class = "sasonr_unsupported")
  # two function NOTEs in one PROC SQL expression evaluation are unsupported
  expect_error(sas_run(paste(one, "proc sql; create table t as select mdy(2, 30, 2024) + mdy(2, 31, 2024) as d from one; quit;")),
               class = "sasonr_unsupported")
  # the supported form (one MDY NOTE) still runs
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run(paste(one, "proc sql; create table t as select mdy(2, 30, 2024) as d from one; quit;"))
  expect_true("NOTE: Invalid argument to function MDY. Missing values may be generated." %in% r$log)
})

test_that("EXPECTED / DEVIATION / CELLCHI2 go through the statistics registry per release", {
  code <- "data d; input a $ b $; datalines;
x p
x q
y p
y q
y q
;
proc freq data=d; tables a*b / %s; run;"
  #
  withr::local_options(sasonr.sas_version = "SAS 9.4")
  for (o in c("expected", "deviation", "cellchi2")) {
    expect_error(sas_run(sprintf(code, o)), class = "sasonr_unsupported")
  }
  #
  withr::local_options(sasonr.sas_version = "SAS 9.4")
  expect_no_error(sas_run(sprintf(code, "expected deviation cellchi2")))
})

test_that("PROC MEANS with BY groups sizes the N field for the data set", {
  # 13 observations in single-digit BY groups print a 2-column N field
  res <- sas_run("data stacked;
  length grp $8 hex $16;
  input grp $ hex $;
  x = input(hex, hex16.);
  datalines;
P0N0 0000000000000000
P0N0 8000000000000000
N0P0 8000000000000000
N0P0 0000000000000000
N0N0 8000000000000000
N0N0 8000000000000000
P0P0 0000000000000000
P0P0 0000000000000000
N0   8000000000000000
P0   0000000000000000
N0M  8000000000000000
N0M  8000000000000000
N0M  8000000000000000
;
run;
proc sort data=stacked out=sorted; by grp; run;
proc means data=sorted; by grp; var x; output out=agg sum=sum mean=mean min=min max=max; run;")
  l <- sub("\\s+$", "", res$listing)
  expect_true(" N            Mean         Std Dev         Minimum         Maximum" %in% l)
  expect_true(strrep("-", 66) %in% l)
  expect_false(strrep("-", 65) %in% l)
  # the sign of zero: SUM / MEAN +0, MIN / MAX the first value's sign
  hex <- function(v) toupper(paste(rev(as.character(writeBin(v, raw(), size = 8, endian = "little"))), collapse = ""))
  a <- res$work$agg
  i <- match("P0N0", trimws(a$grp))
  expect_identical(c(hex(a$sum[i]), hex(a$mean[i]), hex(a$min[i]), hex(a$max[i])),
                   rep("0000000000000000", 4))
  j <- match("N0M", trimws(a$grp))
  expect_identical(c(hex(a$sum[j]), hex(a$mean[j]), hex(a$min[j]), hex(a$max[j])),
                   c("0000000000000000", "0000000000000000", "8000000000000000", "8000000000000000"))
  # with CLASS variables the wider field is not determined: fail loud
  expect_error(sas_run("data s; input g $ c $ x; datalines;
a u 1
a v 2
b u 3
b v 4
b u 5
b v 6
b u 7
b v 8
b u 9
b v 10
;
proc means data=s n; by g; class c; var x; run;"), class = "sasonr_unsupported")
})

test_that("WEEKDAY / TENDAY / SEMIMONTH run only in their supported form", {
  ok <- sas_run("data d;
  a = intck('weekday', '17JAN2020'd, '20JAN2020'd);
  b = intck('semimonth', '14JAN2020'd, '16JAN2020'd);
  c = intck('tenday', '09JAN2020'd, '11JAN2020'd);
  e = intnx('weekday', '17JAN2020'd, 1);
  f = intnx('tenday', '15FEB2020'd, 1);
run;")
  expect_identical(unlist(ok$work$d), c(a = 1, b = 1, c = 1, e = 21934, f = 21966))
  # an explicit default alignment 'b' (16MAR2020)
  expect_identical(sas_run("data d; r = put(intnx('semimonth', mdy(3, 15, 2020), 1, 'b'), date9.); run;")$work$d$r, "16MAR2020")
  bad <- c("intck('tenday2', '09JAN2020'd, '11FEB2020'd)", "intck('semimonth.2', '09JAN2020'd, '11FEB2020'd)",
           "intck('dtweekday', '09JAN2020:0:0'dt, '11FEB2020:0:0'dt)", "intck('tenday', '09JAN2020'd, '11FEB2020'd, 'c')",
           "intck('weekday', '09JAN2020'd, '11FEB2020'd, 'd')", "intnx('semimonth', '20JAN2020'd, 1, 'e')",
           "intnx('tenday', '20JAN2020'd, 1, 'm')", "intnx('weekday2', '20JAN2020'd, 1)")
  for (b in bad) expect_error(sas_run(paste0("data d; r = ", b, "; run;")), class = "sasonr_unsupported", info = b)
})

test_that("the SUBSTR statement goes through feature_check()", {
  code <- "data a; length t $6; t = 'abcdef'; substr(t, 2, 2) = 'XY'; run;"
  #
  withr::with_options(list(sasonr.sas_version = "SAS 9.4"),
                      expect_error(sas_run(code), class = "sasonr_strict_error"))
  #
  expect_identical(sas_run(code)$work$a$t, "aXYdef")
})

test_that("PROC DATASETS DELETE _ALL_ fails loud (not supported)", {
  # after DELETE _ALL_ every member still exists
  expect_error(sas_run("data a; v = 1; run; proc datasets library=work nolist; delete _all_; run; quit;"),
               class = "sasonr_unsupported")
})
