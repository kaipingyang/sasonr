# PROC TABULATE: ODS OUTPUT Table= (proc_tabulate_output.R).

to_prog <- "options nocenter nodate nonumber formdlim=' ' pagesize=max linesize=132;
proc format;
  value trtf 1 = 'Low' 2 = 'High';
run;
data have;
  length grp $1 sub $1;
  input grp $ sub $ trt v w;
  label v = 'Value' grp = 'Group';
  format w 6.2;
  datalines;
a x 1 10 1.5
a x 2 20 2.5
a y 1 30 3.5
b x 2 40 4.5
b y 1 50 .
b y 2 60 6.5
c x 1 . 7.5
c y 2 80 8.5
;
run;
"

to_run <- function(code) {
  withr::with_options(list(sasonr.step_notes = TRUE, sasonr.strict = FALSE), {
    withCallingHandlers(sas_run(paste0(to_prog, code)), sasonr_strict_warning = function(w) invokeRestart("muffleWarning"))
  })
}

to_attrs <- function(m) {
  list(names = unname(unlist(m$disp)), type = unname(unlist(m$type)), len = unname(unlist(m$len)))
}

test_that("one CLASS variable and SUM: class, _TYPE_, _PAGE_, _TABLE_, then <var>_Sum", {
  r <- to_run("ods output Table=ta;\nproc tabulate data=have; class grp; var v; table grp, v*sum; run;\n%put last=&syslast;")
  d <- r$work$ta
  expect_identical(to_attrs(r$meta$ta), list(names = c("grp", "_TYPE_", "_PAGE_", "_TABLE_", "v_Sum"),
                                             type = c("char", "char", "num", "num", "num"), len = c(1L, 1L, 8L, 8L, 8L)))
  expect_identical(unname(unlist(r$meta$ta$labels)), c("Group", "Type of Observation", "Page for Observation",
                                                       "Table for Observation"))
  expect_length(r$meta$ta$formats, 0L)
  expect_identical(d$grp, c("a", "b", "c"))
  expect_identical(d$`_type_`, c("1", "1", "1"))
  expect_identical(d$`_page_`, c(1, 1, 1))
  expect_identical(d$`_table_`, c(1, 1, 1))
  expect_identical(d$v_sum, c(60, 150, 80))
  expect_true(any(grepl("^last=WORK.TA\\s*$", r$log)))
  expect_true(any(grepl("NOTE: The data set WORK.TA has 3 observations and 5 variables.", r$log, fixed = TRUE)))
})

test_that("CLASS crossed with ALL: one observation per class-level combination, ordered by type", {
  r <- to_run(paste("ods output Table=tb;\nproc tabulate data=have; class grp trt; format trt trtf.; var v;",
                    "table grp all, (trt all)*v*(n mean); run;"))
  d <- r$work$tb
  m <- r$meta$tb
  expect_identical(to_attrs(m)$names, c("grp", "trt", "_TYPE_", "_PAGE_", "_TABLE_", "v_N", "v_Mean"))
  expect_identical(m$len$`_type_`, 2L)
  expect_identical(m$formats$trt$text, "trtf4.")
  expect_identical(d$`_type_`, c(rep("11", 6), rep("10", 3), "01", "01", "00"))
  expect_identical(trimws(d$grp), c("a", "a", "b", "b", "c", "c", "a", "b", "c", "", "", ""))
  expect_identical(d$trt, c(1, 2, 1, 2, 1, 2, NA, NA, NA, 1, 2, NA))
  expect_identical(d$v_n, c(2, 1, 1, 2, 0, 1, 3, 3, 1, 3, 4, 7))
  expect_identical(d$v_mean[1:6], c(20, 20, 50, 50, NA, 80))
  expect_equal(d$v_mean[12], 290 / 7)
})

test_that("N without an analysis variable is the column N", {
  r <- to_run("ods output Table=tc;\nproc tabulate data=have; class grp sub; table grp, sub*n; run;")
  expect_identical(to_attrs(r$meta$tc)$names, c("grp", "sub", "_TYPE_", "_PAGE_", "_TABLE_", "N"))
  expect_identical(r$work$tc$n, c(2, 1, 1, 2, 1, 1))
})

test_that("a page dimension numbers _PAGE_ and PCTN is named <var>_PctN_<0 per CLASS variable>_<var>", {
  r <- to_run(paste("ods output Table=td;\nproc tabulate data=have; class grp sub; var v w;",
                    "table sub, grp, v*(sum pctn) w*mean; run;"))
  d <- r$work$td
  expect_identical(to_attrs(r$meta$td)$names,
                   c("grp", "sub", "_TYPE_", "_PAGE_", "_TABLE_", "v_Sum", "v_PctN_00_v", "w_Mean"))
  expect_identical(d$sub, c("x", "x", "x", "y", "y", "y"))
  expect_identical(d$`_page_`, c(1, 1, 1, 2, 2, 2))
  expect_identical(d$v_sum, c(30, 40, NA, 30, 110, 80))
  expect_identical(d$v_pctn_00_v, c(2, 1, 0, 1, 2, 1) / 7 * 100)
  expect_identical(d$w_mean, c(2, 4.5, 7.5, 3.5, 6.5, 8.5))
})

test_that("the full path Tabulate.Report.Table is accepted", {
  r <- to_run("ods output Tabulate.Report.Table=t;\nproc tabulate data=have; class grp; var v; table grp, v*sum; run;")
  expect_identical(r$work$t$v_sum, c(60, 150, 80))
})

test_that("layouts outside the measured scope fail loud", {
  bad <- c(
    "class grp; var v; table grp, v*std;",
    "class grp; var v; table grp, v*pctsum;",
    "class grp; var v; table grp, v*sum; table grp, v*n;",
    "class grp sub; var v; table grp, v*sum;",
    "class sub grp; var v; table grp, sub*v*sum;",
    "class grp; var v; table grp, v*sum*f=8.1;",
    "class grp; var v; table grp, v='x'*sum;",
    "class grp; var v; table grp*v, sum;",
    "class grp / missing; var v; table grp, v*sum;",
    "class grp; var v; table all grp, v*sum;",
    "class grp sub; var v; table grp, sub*v*n all*v*mean;"
  )
  for (b in bad) {
    expect_error(to_run(paste0("ods output Table=t;\nproc tabulate data=have; ", b, " run;")),
                 class = "sasonr_unsupported", info = b)
  }
})
