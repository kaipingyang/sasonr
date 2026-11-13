# Regressions of the independent review of the dev merge dc6d010
# (sasonr-conformance docs/conformance/review_dc6d010.md).  Each test fails on
# dc6d010 and passes after the fix.

test_that("finding 1: PROC SQL function NOTEs go through sql_function_notes() again", {
  one <- "data one; x = 1; run;"
  # the numbered NOTE of SCAN (argument 2 invalid) has no verified PROC SQL form
  expect_error(sas_run(paste(one, "proc sql; create table t as select scan('a b c', 0) as w from one; quit;")),
               class = "sasonr_unsupported")
  # two function NOTEs in one PROC SQL expression evaluation are not verified
  expect_error(sas_run(paste(one, "proc sql; create table t as select mdy(2, 30, 2024) + mdy(2, 31, 2024) as d from one; quit;")),
               class = "sasonr_unsupported")
  # the verified form (one MDY NOTE, TS1M7 gold fn_matrix/MDY) still runs
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run(paste(one, "proc sql; create table t as select mdy(2, 30, 2024) as d from one; quit;"))
  expect_true("NOTE: Invalid argument to function MDY. Missing values may be generated." %in% r$log)
})

test_that("finding 2: EXPECTED / DEVIATION / CELLCHI2 go through the statistics registry per release", {
  code <- "data d; input a $ b $; datalines;
x p
x q
y p
y q
y q
;
proc freq data=d; tables a*b / %s; run;"
  # TS1M2: only the one-way case has gold (cases/ts1m2/freq_stats/oneway_expected)
  withr::local_options(sasonr.sas_version = "TS1M2")
  for (o in c("expected", "deviation", "cellchi2")) {
    expect_error(sas_run(sprintf(code, o)), class = "sasonr_unsupported")
  }
  # TS1M7: verified (freq_stats/c03, c25, c31; one-way tables c58_oneway_expected)
  withr::local_options(sasonr.sas_version = "TS1M7")
  expect_no_error(sas_run(sprintf(code, "expected deviation cellchi2")))
})

test_that("finding 3: PROC MEANS with BY groups sizes the N field for the data set (TS1M7 gold)", {
  # proc_stats/means_zero_sign (SAS 9.4 TS1M7): 13 observations in single-digit
  # BY groups print a 2-column N field (dc6d010: 1 column for TS1M7)
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
  # the sign of zero (TS1M7 gold, put.txt): SUM / MEAN +0, MIN / MAX the first value's sign
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

test_that("finding 4: WEEKDAY / TENDAY / SEMIMONTH run only in their gold-covered form", {
  ok <- sas_run("data d;
  a = intck('weekday', '17JAN2020'd, '20JAN2020'd);
  b = intck('semimonth', '14JAN2020'd, '16JAN2020'd);
  c = intck('tenday', '09JAN2020'd, '11JAN2020'd);
  e = intnx('weekday', '17JAN2020'd, 1);
  f = intnx('tenday', '15FEB2020'd, 1);
run;")
  # SAS 9.4 TS1M7: functions/INTCK/INTCK_014, _016, _017, INTNX_016, _019
  expect_identical(unlist(ok$work$d), c(a = 1, b = 1, c = 1, e = 21934, f = 21966))
  # an explicit default alignment 'b' (opensas/sd_intnx_allintervals: 16MAR2020)
  expect_identical(sas_run("data d; r = put(intnx('semimonth', mdy(3, 15, 2020), 1, 'b'), date9.); run;")$work$d$r, "16MAR2020")
  bad <- c("intck('tenday2', '09JAN2020'd, '11FEB2020'd)", "intck('semimonth.2', '09JAN2020'd, '11FEB2020'd)",
           "intck('dtweekday', '09JAN2020:0:0'dt, '11FEB2020:0:0'dt)", "intck('tenday', '09JAN2020'd, '11FEB2020'd, 'c')",
           "intck('weekday', '09JAN2020'd, '11FEB2020'd, 'd')", "intnx('semimonth', '20JAN2020'd, 1, 'e')",
           "intnx('tenday', '20JAN2020'd, 1, 'm')", "intnx('weekday2', '20JAN2020'd, 1)")
  for (b in bad) expect_error(sas_run(paste0("data d; r = ", b, "; run;")), class = "sasonr_unsupported", info = b)
})

test_that("finding 5: the SUBSTR statement goes through feature_check()", {
  code <- "data a; length t $6; t = 'abcdef'; substr(t, 2, 2) = 'XY'; run;"
  # TS1M2: SUBSTR is unverified there
  withr::with_options(list(sasonr.sas_version = "TS1M2"),
                      expect_error(sas_run(code), class = "sasonr_not_verified"))
  # TS1M7: verified as its own context (functions/SUBSTR/SUBSTR_033)
  expect_identical(sas_run(code)$work$a$t, "aXYdef")
})

test_that("SASHELP: a .sas7bdat copy needs its column attributes, else fails loud", {
  skip_if_not_installed("haven")
  dir <- sasonr:::sashelp_dir()
  src <- file.path(dir, "class.sas7bdat")
  skip_if_not(nzchar(dir) && file.exists(src), "no local SASHELP copy")
  tmp <- withr::local_tempdir()
  file.copy(src, tmp)
  withr::local_options(sasonr.sashelp_dir = tmp)
  rm(list = ls(sasonr:::sashelp_cache), envir = sasonr:::sashelp_cache)
  withr::defer(rm(list = ls(sasonr:::sashelp_cache), envir = sasonr:::sashelp_cache))
  expect_error(sas_run("proc sql; create table w as select name from sashelp.class; quit;"),
               class = "sasonr_unsupported")
  # with the DICTIONARY.COLUMNS rows (gold of semantics/sashelp_class_meta):
  # NAME is $8 and padded to it
  if (file.exists(file.path(dir, "class_meta.csv"))) file.copy(file.path(dir, "class_meta.csv"), tmp)
  if (!file.exists(file.path(tmp, "class_meta.csv"))) skip("no class_meta.csv in the local SASHELP store")
  res <- sas_run("proc sql; create table w as select name, sex from sashelp.class; quit;")
  expect_identical(res$meta$w$len$name, 8L)
  expect_identical(res$meta$w$len$sex, 1L)
  expect_identical(sub(" +$", "", res$work$w$name[1]), "Alfred")
})

test_that("PROC DATASETS DELETE _ALL_ fails loud (SAS 9.4 TS1M7 keeps the members)", {
  # opensas/datasets_delete_all: after DELETE _ALL_ every member still exists
  expect_error(sas_run("data a; v = 1; run; proc datasets library=work nolist; delete _all_; run; quit;"),
               class = "sasonr_unsupported")
})
