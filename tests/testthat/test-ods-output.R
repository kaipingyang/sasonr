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
    "ods output Summary=s;\nproc means data=have; var x; run;",
    "ods output Summary=s;\nproc means data=have mean noprint; var x; output out=o; run;",
    "ods output Summary=s;\nproc means data=have mean; class grp; types () grp; var x; run;",
    "ods output Summary=s;\nproc means data=have mean std; var x; run;",
    "ods output Summary=s;\nproc summary data=have print mean; var x; run;",
    "ods output Summary=s;\ndata b; set have; run;",
    "ods output Summary=s;\nproc print data=have; run;",
    "ods output Extremes=s;\nproc means data=have mean; var x; run;",
    "proc sort data=have; by grp; run;\nods output Means.Summary=s;\nproc means data=have mean; by grp; var x; run;",
    "ods output Summary=s;\nproc means data=have mean; var x; run;\nproc means mean; var x_mean; run;"
  )
  for (f in fails) expect_error(oo_run(f), class = "sasonr_unsupported", info = f)
})
