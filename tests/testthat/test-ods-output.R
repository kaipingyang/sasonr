oo_prog <- "data have; input grp $ x; datalines;
a 1
a 2
b 3
b 4
;
run;
"

oo_run <- function(code) withr::with_options(list(sasonr.step_notes = TRUE), sas_run(paste0(oo_prog, code)))

test_that("ODS OUTPUT Summary without CLASS writes one row of <var>_<Statistic> columns", {
  r <- oo_run("ods output Summary=s;\nproc means data=have mean sum; var x; run;")
  expect_identical(names(r$work$s), c("x_mean", "x_sum"))
  expect_identical(r$work$s$x_mean, 2.5)
  expect_identical(r$work$s$x_sum, 10)
  m <- r$meta$s
  expect_identical(unname(unlist(m$disp)), c("x_Mean", "x_Sum"))
  expect_identical(unname(unlist(m$labels)), c("Mean", "Sum"))
  expect_identical(unname(vapply(m$formats, `[[`, "", "name")), c("best", "best"))
  expect_identical(unname(unlist(m$type)), c("num", "num"))
})

test_that("with CLASS the class variable and NObs come before the statistics", {
  r <- oo_run("ods output Summary=s;\nproc means data=have mean sum; class grp; var x; run;")
  expect_identical(names(r$work$s), c("grp", "nobs", "x_mean", "x_sum"))
  expect_identical(r$work$s$grp, c("a", "b"))
  expect_identical(r$work$s$nobs, c(2, 2))
  expect_identical(r$work$s$x_mean, c(1.5, 3.5))
  m <- r$meta$s
  expect_identical(m$disp$nobs, "NObs")
  expect_identical(m$labels$nobs, "N Obs")
  expect_null(m$labels$grp)
  expect_null(m$formats$grp)
  expect_identical(m$type$grp, "char")
  expect_identical(m$len$grp, 8L)
})

test_that("BY groups are concatenated with the BY variable first", {
  r <- oo_run("proc sort data=have; by grp; run;\nods output Summary=s;\nproc means data=have mean; by grp; var x; run;")
  expect_identical(names(r$work$s), c("grp", "x_mean"))
  expect_identical(r$work$s$grp, c("a", "b"))
  expect_identical(r$work$s$x_mean, c(1.5, 3.5))
})

test_that("the path form Means.Summary and a WORK. data set name are accepted", {
  r <- oo_run("ods output Means.Summary=work.s;\nproc means data=have sum; var x; run;")
  expect_identical(r$work$s$x_sum, 10)
})

test_that("the data set NOTE comes before the observations read", {
  r <- oo_run("ods output Summary=s;\nproc means data=have mean; var x; run;")
  i <- grep("WORK.S has 1 observations and 1 variables", r$log, fixed = TRUE)
  j <- grep("There were 4 observations read from the data set WORK.HAVE", r$log, fixed = TRUE)
  expect_length(i, 1L)
  expect_length(j, 1L)
  expect_true(i < j)
})

test_that("the request ends with the step and does not change the listing", {
  two <- "proc means data=have mean sum; var x; run;\nproc means data=have mean sum; var x; run;"
  r0 <- oo_run(two)
  r <- oo_run(paste0("ods output Summary=s;\n", two))
  expect_identical(r$listing, r0$listing)
  expect_identical(names(r$work), c("have", "s"))
})

test_that("a pending request at the end of the program writes nothing", {
  r <- oo_run("ods output Summary=s;")
  expect_identical(names(r$work), "have")
  expect_false(any(grepl("WARNING", r$log)))
})

test_that("forms and layouts without a documented result fail loud", {
  fails <- c(
    "ods output Summary(match_all)=s;\nproc means data=have mean; var x; run;",
    "ods output Summary(persist=proc)=s;\nproc means data=have mean; var x; run;",
    "ods output close;",
    "ods output Summary;",
    "ods output Summary=s(keep=x_mean);",
    "ods output Summary=lib.s;",
    "ods output 'Summary statistics'=s;",
    "ods output Summary=s;\nproc means data=have mean noprint; var x; output out=o; run;",
    "ods output Summary=s;\nproc means data=have mean; class grp; types () grp; var x; run;",
    "ods output Summary=s;\nproc summary data=have print mean; var x; run;",
    "proc sort data=have; by grp; run;\nods output Means.Summary=s;\nproc means data=have mean; by grp; var x; run;",
    "ods output Summary=s;\nproc means data=have mean; class x; var x; run;",
    "ods output Summary=s;\nproc means data=have mean maxdec=2; var x; run;",
    "ods output Summary=s;\nproc means data=have mean; class grp / missing; var x; run;",
    "ods output Summary=s1 Summary=s2;\nproc print data=have; run;",
    "ods output Extremes=e Moments=m;\nproc means data=have mean; var x; run;"
  )
  for (f in fails) expect_error(oo_run(f), class = "sasonr_unsupported", info = f)
})

oo2_prog <- "data have; input grp $ sub $ x y; datalines;
a s1 1 10
a s2 2 20
a s1 3 30
b s1 4 40
b s2 5 50
b s2 . 60
;
run;
"
oo2_run <- function(code) withr::with_options(list(sasonr.step_notes = TRUE), sas_run(paste0(oo2_prog, code)))
oo_attrs <- function(m) {
  paste(unlist(m$disp), unlist(m$type), unlist(m$len),
        vapply(m$vars, function(v) toupper(m$formats[[v]]$text %||% ""), ""),
        vapply(m$vars, function(v) m$labels[[v]] %||% "", ""), sep = "|")
}

test_that("the default statistics give N with BEST<digits>. and the others with BEST12.", {
  r <- oo2_run("ods output Summary=s;\nproc means data=have; var x; run;")
  expect_identical(oo_attrs(r$meta$s), c("x_N|num|8|BEST1.|N", "x_Mean|num|8|BEST12.|Mean", "x_StdDev|num|8|BEST12.|Std Dev",
                                         "x_Min|num|8|BEST12.|Minimum", "x_Max|num|8|BEST12.|Maximum"))
  expect_identical(r$work$s$x_stddev, sd(c(1, 2, 3, 4, 5)))
  expect_true(any(grepl("WORK.S has 1 observations and 5 variables", r$log, fixed = TRUE)))
})

test_that("two analysis variables add a VName_<var> column before the statistics of each variable", {
  r <- oo2_run("ods output Summary=s;\nproc means data=have n mean; var x y; run;")
  expect_identical(unname(unlist(r$meta$s$disp)), c("VName_x", "x_N", "x_Mean", "VName_y", "y_N", "y_Mean"))
  expect_identical(r$work$s$vname_x, "x")
  expect_identical(r$meta$s$len$vname_x, 1L)
  expect_identical(r$meta$s$labels$vname_y, "Variable")
})

test_that("labels add Label_<var> columns as long as the label, and the CLASS variable keeps its format", {
  # (a labelled CLASS variable: its listing heading fails loud, see below)
  r <- oo2_run(paste0("data lab; set have; label x=\"Weight in kg\"; format x 8.2 grp $3.; run;\n",
                      "ods output Summary=s;\nproc means data=lab mean sum; class grp; var x y; run;"))
  expect_identical(oo_attrs(r$meta$s), c(
    "grp|char|8|$3.|", "NObs|num|8|BEST1.|N Obs", "VName_x|char|1||Variable", "Label_x|char|12||Label",
    "x_Mean|num|8|BEST12.|Mean", "x_Sum|num|8|BEST12.|Sum", "VName_y|char|1||Variable", "Label_y|char|1||Label",
    "y_Mean|num|8|BEST12.|Mean", "y_Sum|num|8|BEST12.|Sum"))
  expect_identical(r$work$s$label_x, c("Weight in kg", "Weight in kg"))
  expect_identical(r$work$s$label_y, c("", ""))
})

test_that("a count of two digits gives BEST2. and a long name a VName column of its length", {
  r <- oo2_run(paste0("data big; do i = 1 to 12; weight_kg = i * 2; output; end; run;\n",
                      "ods output Summary=s;\nproc means data=big n mean; var weight_kg i; run;"))
  expect_identical(r$meta$s$len$vname_weight_kg, 9L)
  expect_identical(toupper(r$meta$s$formats$weight_kg_n$text), "BEST2.")
})

test_that("TYPES rows are stacked in type order with blank class values", {
  r <- oo2_run("ods output Summary=s;\nproc means data=have mean sum; class grp sub; types grp sub grp*sub; var x; run;")
  expect_identical(r$work$s$grp, c("", "", "a", "b", "a", "a", "b", "b"))
  expect_identical(r$work$s$sub, c("s1", "s2", "", "", "s1", "s2", "s1", "s2"))
  expect_identical(r$work$s$nobs, c(3, 3, 3, 3, 2, 1, 1, 2))
  expect_identical(r$work$s$x_sum, c(8, 7, 6, 9, 4, 2, 4, 5))
})

test_that("NWAY gives one row per class combination", {
  r <- oo2_run("ods output Summary=s;\nproc means data=have mean nway; class grp sub; var x; run;")
  expect_identical(names(r$work$s), c("grp", "sub", "nobs", "x_mean"))
  expect_identical(r$work$s$x_mean, c(2, 2, 4, 5))
})

test_that("BY and CLASS together put the BY variables first", {
  r <- oo2_run("proc sort data=have out=sorted; by grp sub; run;\nods output Summary=s;\nproc means data=sorted mean sum; by grp; class sub; var x; run;")
  expect_identical(names(r$work$s), c("grp", "sub", "nobs", "x_mean", "x_sum"))
  expect_identical(r$work$s$grp, c("a", "a", "b", "b"))
  r2 <- oo2_run("proc sort data=have out=sorted; by grp sub; run;\nods output Summary=s;\nproc means data=sorted mean; by grp sub; var x; run;")
  expect_identical(names(r2$work$s), c("grp", "sub", "x_mean"))
  expect_identical(r2$work$s$x_mean, c(2, 2, 4, 5))
})

test_that("quartiles and confidence limits are written with their labels", {
  r <- withr::with_options(list(sasonr.strict = FALSE), suppressWarnings(
    oo2_run("ods output Summary=s;\nproc means data=have median q1 q3 clm; class grp; var x; run;")))
  expect_identical(unname(unlist(r$meta$s$labels)), c("N Obs", "Median", "Lower Quartile", "Upper Quartile",
                                                          "Lower 95% CL for Mean", "Upper 95% CL for Mean"))
  expect_identical(r$work$s$x_q3, c(3, 5))
})

test_that("PROC PRINT with a request pending writes the WARNING and ends the request", {
  r <- oo2_run("ods output Summary=s1;\nproc print data=have; run;\nproc means data=have mean; var x; run;")
  i <- grep("WARNING: Output 'Summary' was not created.", r$log, fixed = TRUE)
  expect_length(i, 1L)
  expect_identical(r$log[i + 1L], "         verify that the appropriate procedure options are used to produce the requested output object.  For example, verify that")
  expect_identical(r$log[i + 2L], "         the NOPRINT option is not used.")
  expect_true(i < grep("There were 6 observations read from the data set WORK.HAVE", r$log, fixed = TRUE)[1])
  expect_identical(names(r$work), "have")
})

test_that("a DATA step leaves the request for the next PROC MEANS", {
  r <- oo2_run("ods output Summary=s2;\ndata d; set have; run;\nproc means data=have sum; var x; run;")
  expect_setequal(names(r$work), c("have", "d", "s2"))
  expect_identical(r$work$s2$x_sum, 15)
})

test_that("a table PROC MEANS does not produce gets the WARNING after the data set NOTE", {
  r <- oo2_run("ods output Extremes=e Summary=s;\nproc means data=have mean; var x; run;")
  i <- grep("WORK.S has 1 observations and 1 variables", r$log, fixed = TRUE)
  j <- grep("WARNING: Output 'Extremes' was not created.", r$log, fixed = TRUE)
  k <- grep("There were 6 observations read", r$log, fixed = TRUE)
  expect_true(i < j && j < k)
  expect_false("e" %in% names(r$work))
})

test_that("the ODS OUTPUT data set becomes _LAST_ unless the step also writes an OUT= data set", {
  r <- oo2_run("ods output Summary=s;\nproc means data=have mean; var x; run;\n%put last=&syslast;\nproc print; run;")
  expect_true(any(grepl("^last=WORK.S\\s*$", r$log)))
  expect_true(any(grepl("There were 1 observations read from the data set WORK.S.", r$log, fixed = TRUE)))
  r2 <- oo2_run("ods output Summary=s;\nproc means data=have mean; var x; output out=o mean=m; run;\n%put last=&syslast;")
  expect_true(any(grepl("^last=WORK.O\\s*$", r2$log)))
})

test_that("the listing heading of a labelled CLASS variable fails loud (not implemented)", {
  expect_error(sas_run("data a; g = 'x'; v = 1; label g = 'Treatment group'; run; proc means data=a; class g; var v; run;"),
               class = "sasonr_unsupported")
})
