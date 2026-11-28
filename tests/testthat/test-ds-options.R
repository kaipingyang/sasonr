dso_meta <- list(
  a = meta_from_df(data.frame(g = c("A", "A", "B"), v = c(1, 2, 3), w = c(4, 5, 6))),
  b = meta_from_df(data.frame(g = c("A", "B"), z = c(10, 20)))
)
dso_an <- function(code, meta = dso_meta) sas_analyze(sas_parse(code)$steps[[1]], meta)
# the option records of the first reading statement
dso_recs <- function(code, meta = dso_meta) {
  lapply(dso_an(code, meta)$dsir$slots$source[[1]]$datasets, `[[`, "opts")
}

test_that("a data set without options gives an empty record", {
  r <- dso_recs("data c; set a; run;")[[1]]
  expect_s3_class(r, "sas_ds_opts")
  expect_equal(r$name, "a")
  for (f in c("keep", "drop", "rename", "where", "in_var", "firstobs", "obs")) expect_null(r[[f]])
  expect_equal(r$vars, c("g", "v", "w"))
  expect_equal(r$out_names, r$vars)
})

test_that("KEEP= and DROP= keep their items and resolve the variables", {
  r <- dso_recs("data c; set a(keep = g w); run;")[[1]]
  expect_equal(vapply(r$keep, `[[`, "", "name"), c("g", "w"))
  expect_equal(r$vars, c("g", "w"))
  r <- dso_recs("data c; set a(drop = v); run;")[[1]]
  expect_equal(vapply(r$drop, `[[`, "", "name"), "v")
  expect_equal(r$vars, c("g", "w"))
  expect_error(dso_an("data c; set a(keep = nope); run;"), "has never been referenced")
})

test_that("KEEP= with RENAME=: KEEP= names the old variables", {
  r <- dso_recs("data c; set a(keep = g v rename = (v = vv)); run;")[[1]]
  expect_equal(r$rename, c(v = "vv"))
  expect_equal(r$vars, c("g", "v"))
  expect_equal(r$out_names, c("g", "vv"))
  expect_error(dso_an("data c; set a(rename = (v = g)); run;"), "already exists on file")
})

test_that("WHERE= uses the names after RENAME= and is analysed", {
  r <- dso_recs("data c; set a(rename = (v = vv) where = (vv > 1)); run;")[[1]]
  expect_equal(r$where$node, "compare")
  expect_error(dso_an("data c; set a(rename = (v = vv) where = (v > 1)); run;"), "is not on file")
  # a variable dropped by KEEP= is not on file for WHERE=
  expect_error(dso_an("data c; set a(keep = g where = (v > 1)); run;"), "is not on file")
})

test_that("FIRSTOBS= and OBS= are numbers, MIN/MAX and K multiples included", {
  r <- dso_recs("data c; set a(firstobs = 2 obs = 3); run;")[[1]]
  expect_equal(c(r$firstobs, r$obs), c(2, 3))
  r <- dso_recs("data c; set a(firstobs = min obs = 1k); run;")[[1]]
  expect_equal(c(r$firstobs, r$obs), c(1, 1024))
  r <- dso_recs("data c; set a(obs = max); run;")[[1]]
  expect_null(r$firstobs)
  expect_equal(r$obs, 2^63 - 1)
})

test_that("IN= is kept per data set of a MERGE", {
  r <- dso_recs("data c; merge a(in = ina) b(in = inb keep = g); by g; run;")
  expect_equal(vapply(r, `[[`, "", "in_var"), c("ina", "inb"))
  expect_equal(r[[2]]$vars, "g")
})

test_that("the reading code applies the options in order", {
  an <- dso_an("data c; set a(keep = g v rename = (v = vv) where = (vv > 1) firstobs = 2 obs = 3); run;")
  x <- ds_opts_apply(an$dsir$slots$source[[1]]$datasets[[1]]$opts, an)
  code <- cgi_text(x)
  # the text displays the call that runs
  expect_identical(str2lang(code), x$lang)
  pos <- vapply(c("sas_rt_ds(", "rename = ", "dplyr::filter(", "sas_rt_obs_window(2, 3)", "sas_rt_note_read("),
                function(p) regexpr(p, code, fixed = TRUE), 1L)
  expect_true(all(pos > 0))
  expect_false(is.unsorted(pos))
  expect_match(code, "keep = c(\"g\", \"v\")", fixed = TRUE)
})

test_that("the WHERE statement applies only without WHERE=", {
  an <- dso_an("data c; set a(where = (v > 1)) b; where g = 'A'; run;")
  ds <- an$readers[[1]]$datasets
  w <- an$dsir$slots$where$effective
  ca <- cgi_text(ds_code(ds[[1]], an, where = w))
  cb <- cgi_text(ds_code(ds[[2]], an, where = w))
  expect_equal(lengths(regmatches(ca, gregexpr("dplyr::filter(", ca, fixed = TRUE))), 1L)
  expect_match(ca, "sas_rt_gt(v, 1)", fixed = TRUE)
  expect_match(cb, "sas_rt_eq(g, \"A\")", fixed = TRUE)
  # ds_code is the component applied to the record of the data set
  expect_identical(ca, cgi_text(ds_opts_apply(ds_opts_record(ds[[1]]), an, where = w)))
})

test_that("PROC DATA= reads through the same component", {
  step <- sas_parse("proc print data = a(keep = g v firstobs = 2); where v > 1; run;")$steps[[1]]
  inp <- proc_input(step, dso_meta, NULL)
  expect_identical(proc_source_code(inp, obs_col = TRUE),
                   cgi_text(ds_opts_apply(ds_opts_record(inp$ds), list(arrays = list()), obs_col = TRUE, where = inp$where)))
  expect_match(proc_source_code(inp), "sas_rt_obs_window(2, NULL)", fixed = TRUE)
})

# Output data sets of PROC SORT OUT= and PROC MEANS OUTPUT OUT=: a wrong name
# in KEEP=/RENAME= is a WARNING and the step completes.
dso_have <- "data have; input x y grp $; datalines;\n1 10 a\n2 20 b\n3 30 a\n;\nrun;\n"
dso_notes <- function(code) {
  old <- options(sasonr.step_notes = TRUE)
  on.exit(options(old))
  sas_run(paste0(dso_have, code))
}
never_ref <- "WARNING: The variable nosuchvar in the DROP, KEEP, or RENAME list has never been referenced."

test_that("PROC SORT OUT= KEEP= naming an unknown variable: WARNING, no variable kept", {
  r <- dso_notes("proc sort data=have out=sorted(keep=nosuchvar); by x; run;")
  expect_equal(msgs(r)[-1], c(
    never_ref,
    "NOTE: There were 3 observations read from the data set WORK.HAVE.",
    "NOTE: The data set WORK.SORTED has 3 observations and 0 variables."
  ))
  expect_equal(dim(r$work$sorted), c(3L, 0L))
})

test_that("PROC MEANS OUTPUT OUT= KEEP= naming an unknown variable: WARNING, no variable kept", {
  r <- dso_notes("proc means data=have noprint; var y; output out=agg(keep=nosuchvar) mean=m; run;")
  expect_equal(msgs(r)[-1], c(
    never_ref,
    "NOTE: There were 3 observations read from the data set WORK.HAVE.",
    "NOTE: The data set WORK.AGG has 1 observations and 0 variables."
  ))
  expect_equal(dim(r$work$agg), c(1L, 0L))
})

test_that("output data set option forms the measured rules do not cover fail loud", {
  for (p in c("proc sort data=have out=s(keep=x nosuchvar); by x; run;",
              "proc sort data=have out=s(drop=nosuchvar); by x; run;",
              "proc sort data=have out=s(rename=(x=xa nosuchvar=n)); by x; run;",
              "proc sort data=have out=s(rename=(nosuchvar=newname)); by x; run;",
              "proc sort data=have out=s(rename=(x=y)); by x; run;",
              "proc means data=have noprint; var y; output out=a(rename=(m=_freq_)) mean=m; run;",
              "proc sort data=have out=s(where=(x > 1)); by x; run;",
              "proc freq data=have noprint; tables grp / out=f(keep=nosuchvar); run;")) {
    expect_error(sas_run(paste0(dso_have, p)), class = "sasonr_unsupported", info = p)
  }
  # valid options are applied as before
  r <- sas_run(paste0(dso_have, "proc sort data=have out=s(keep=x grp rename=(grp=g)); by descending x; run;"))
  expect_equal(as.data.frame(r$work$s), data.frame(x = c(3, 2, 1), g = c("a", "b", "a")))
})

# DATA step output and input data sets; PROC PRINT DATA=.
dso_sas <- function(code) {
  old <- options(sasonr.step_notes = TRUE, sasonr.on_error = "sas")
  on.exit(options(old))
  sas_run(paste0(dso_have, code))
}
err_warn <- function(r) grep("^(ERROR|WARNING)", msgs(r), value = TRUE)
invalid_have <- "ERROR: Invalid DROP, KEEP, or RENAME option on file WORK.HAVE."
incomplete_out <- "WARNING: The data set WORK.OUT may be incomplete.  When this step was stopped there were 0 observations and 1 variables."

test_that("DATA step output KEEP= naming an unknown variable: WARNING, 0 variables; PROC PRINT has none", {
  r <- dso_sas("data out(keep=nosuchvar); set have; put 'anchor x=' x=; run; proc print data=out; run;")
  expect_equal(msgs(r)[-1], c(
    never_ref, "anchor x=x=1", "anchor x=x=2", "anchor x=x=3",
    "NOTE: There were 3 observations read from the data set WORK.HAVE.",
    "NOTE: The data set WORK.OUT has 3 observations and 0 variables.",
    "NOTE: No variables in data set WORK.OUT."
  ))
  expect_equal(dim(r$work$out), c(3L, 0L))
})

test_that("DATA step output RENAME= of an unknown old name: WARNING, the pair is ignored", {
  r <- dso_sas("data out(rename=(nosuchvar=newname)); set have; run;")
  expect_equal(err_warn(r), never_ref)
  expect_equal(names(r$work$out), c("x", "y", "grp"))
})

test_that("DATA step output RENAME= onto an existing name: WARNING, the renamed variable stays", {
  r <- dso_sas("data out(rename=(x=y)); set have; put 'anchor x=' x=; run;")
  expect_equal(err_warn(r), "WARNING: Variable y already exists on file WORK.OUT.")
  expect_equal(as.data.frame(r$work$out), data.frame(y = c(1, 2, 3), grp = c("a", "b", "a")))
  # the other order is not established: fails loud
  expect_error(sas_run(paste0(dso_have, "data out(rename=(grp=x)); set have; run;")), class = "sasonr_unsupported")
})

test_that("DATA step input KEEP= naming an unknown variable: two ERRORs, the step stops", {
  r <- dso_sas("data out;\n  set have(keep=nosuchvar);\n  put 'anchor x=' x=;\nrun;")
  expect_equal(err_warn(r), c(
    "ERROR: The variable nosuchvar in the DROP, KEEP, or RENAME list has never been referenced.",
    invalid_have, incomplete_out))
  expect_equal(dim(r$work$out), c(0L, 1L))
  expect_equal(names(r$work$out), "x")
})

test_that("PROC PRINT DATA= KEEP= naming an unknown variable: two ERRORs, the next step runs", {
  r <- dso_sas("proc print data=have(keep=nosuchvar); run; data check; x = 1; output; run;")
  expect_equal(err_warn(r), c(
    "ERROR: The variable nosuchvar in the DROP, KEEP, or RENAME list has never been referenced.", invalid_have))
  expect_equal(nrow(r$work$check), 1L)
})

test_that("DATA step input RENAME= of an unknown old name: ERRORs, one pair or two", {
  r <- dso_sas("data out;\n  set have(rename=(nosuchvar=newname));\n  put 'anchor x=' x=;\nrun;")
  expect_equal(err_warn(r), c("ERROR: Variable nosuchvar is not on file WORK.HAVE.", invalid_have, incomplete_out))
  expect_equal(names(r$work$out), "x")
  r <- dso_sas("data out;\n  set have(rename=(x=xa nosuchvar=yb));\n  put 'anchor xa=' xa=;\nrun;")
  expect_equal(err_warn(r), c("ERROR: Variable nosuchvar is not on file WORK.HAVE.", invalid_have, incomplete_out))
  expect_equal(names(r$work$out), "xa")
  expect_equal(nrow(r$work$out), 0L)
})

test_that("DATA step input RENAME= onto an existing name: ERRORs, the step stops", {
  r <- dso_sas("data out;\n  set have(rename=(x=y));\n  put 'anchor y=' y=;\nrun;")
  expect_equal(err_warn(r), c("ERROR: Variable y already exists on file WORK.HAVE.", invalid_have, incomplete_out))
  expect_equal(names(r$work$out), "y")
})

test_that("IN= on PROC PRINT DATA= is ERROR 22-7; on other PROCs it fails loud", {
  r <- dso_sas("proc print data=have(in=flag); run; data check; x = 1; output; run;")
  expect_equal(err_warn(r), "ERROR 22-7: Invalid option name IN.")
  expect_equal(nrow(r$work$check), 1L)
  expect_error(sas_run(paste0(dso_have, "proc sort data=have(in=f) out=s; by x; run;")), class = "sasonr_unsupported")
})

test_that("PROC SQL FROM KEEP= naming an unknown variable: two ERRORs, NOEXEC, the next step runs", {
  r <- dso_sas("proc sql;\n  select * from have(keep=nosuchvar);\nquit;\ndata check;\n  x = 1;\n  output;\nrun;")
  expect_equal(msgs(r)[-1][c(1:3, 5)], c(
    "ERROR: The variable nosuchvar in the DROP, KEEP, or RENAME list has never been referenced.",
    invalid_have,
    "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.",
    "NOTE: The data set WORK.CHECK has 1 observations and 1 variables."))
  expect_false(any(grepl("syntax check mode", msgs(r))))
  expect_equal(sort(names(r$work)), c("check", "have"))
})

test_that("PROC SQL FROM RENAME= of an unknown old name: two ERRORs, NOEXEC, the next step runs", {
  r <- dso_sas("proc sql;\n  select * from have(rename=(nosuchvar=newname));\nquit;\ndata check;\n  x = 1;\n  output;\nrun;")
  expect_equal(msgs(r)[-1][c(1:3, 5)], c(
    "ERROR: Variable nosuchvar is not on file WORK.HAVE.", invalid_have,
    "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.",
    "NOTE: The data set WORK.CHECK has 1 observations and 1 variables."))
  expect_equal(nrow(r$work$check), 1L)
})

test_that("PROC SQL FROM: valid options, FIRSTOBS=/OBS= after WHERE=, a RENAME= clash fails loud", {
  r <- sas_run(paste0(dso_have, "proc sql; create table t as select * from have(keep=x grp rename=(grp=g) where=(x > 1) firstobs=2); quit;"))
  expect_equal(as.data.frame(r$work$t), data.frame(x = 3, g = "a"))
  expect_error(sas_run(paste0(dso_have, "proc sql; select * from have(rename=(x=y)); quit;")), class = "sasonr_unsupported")
})
