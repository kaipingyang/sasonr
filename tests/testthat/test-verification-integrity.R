# Verification integrity (round 11): regression tests for the findings of the
# independent review of commit 53d319b.  Each test fails on the behaviour of
# 53d319b and passes now (the hole is closed).

# ---- HIGH 1-3, 7: verdict rules (helper-conformance.R, shared with tools/update_registry.R)

test_that("HIGH 1: the same value written differently is a mismatch, not a pass", {
  s <- "@@N LOG_e7e50d type=N err=0 hex=40026BB1BBB55516 best=                2.30258509299404"
  o <- "@@N LOG_e7e50d type=N err=0 hex=40026BB1BBB55516 best=                2.30258509299405"
  # 53d319b: a small relative difference, then counted as a pass ("display only")
  expect_true(is.na(conf_tol_diff(s, o)))
  expect_identical(conf_case_class("fail", "files", conf_tol_diff(s, o)), "mismatch")
  # a real last-bit difference of the value stays approximate (not a pass either)
  o2 <- "@@N LOG_e7e50d type=N err=0 hex=40026BB1BBB55517 best=                2.30258509299405"
  expect_identical(conf_case_class("fail", "files", conf_tol_diff(s, o2)), "approximate")
})

test_that("HIGH 2: a log-only difference is a mismatch; partial needs passing cases and fail loud only", {
  expect_identical(conf_case_class("fail", "log", NA_real_), "mismatch")
  expect_identical(conf_case_class("fail", "lst", NA_real_), "mismatch")
  expect_identical(conf_feature_status(c("pass", "mismatch")), "mismatch")
  expect_identical(conf_feature_status(c("pass", "unsupported")), "partial")
  expect_identical(conf_feature_status(c("unsupported", "unsupported")), "unsupported")
  expect_identical(conf_feature_status(c("pass", "approximate")), "approximate")
  expect_identical(conf_feature_status(character()), "unverified")
  # every status other than verified / partial / not_comparable is gated
  for (st in c("mismatch", "approximate", "unsupported", "unverified", NA)) expect_true(sasonr:::feature_gated(st))
  for (st in c("verified", "partial", "not_comparable")) expect_false(sasonr:::feature_gated(st))
  # no feature in the registry is partial without a verified domain
  fn <- sas_feature_registry("functions")
  p <- fn[fn$status == "partial", , drop = FALSE]
  expect_true(all(as.integer(p$pass) > 0))
  expect_true(all(as.integer(p$mismatch) == 0 & as.integer(p$approximate) == 0))
  expect_true(all(nzchar(p$domain)))
})

test_that("HIGH 2: functions whose log differed from SAS (MDY, PRXCHANGE, PRXMATCH) no longer run silently", {
  fn <- sas_feature_registry("functions")
  # MDY: the %SYSFUNC and PROC SQL NOTEs agree with SAS since round 13 -> verified, runs
  expect_identical(fn$status[fn$name == "MDY"], "verified")
  withr::with_options(list(sasonr.strict = TRUE),
                      expect_identical(sas_run("data a; d = mdy(1, 2, 2020); run;")$work$a$d, 21916))
  # MDY: the two matrix cases that differed were the %SYSFUNC and PROC SQL log
  # NOTEs of an out-of-range argument; both texts are reproduced now (a local
  # SAS probe of the same calls), so the function is verified in its contexts.
  expect_true(all(c("data", "sysfunc", "sql") %in% strsplit(fn$contexts_verified[fn$name == "MDY"], " ")[[1]]))
  # SAS 9.4 TS1M7 (fn_matrix/MDY MDY_5ad84f): the NOTE only, no WARNING
  r <- sas_run("%let x = %sysfunc(mdy(2, 30, 2024));")
  expect_false(any(grepl("^WARNING", r$log)))
  expect_true(any(grepl("^NOTE: Mathematical operations could not be performed during %SYSFUNC function execution", r$log)))
  # TS1M2 writes a WARNING as well, which no committed TS1M2 gold shows: fail loud
  withr::with_options(list(sasonr.sas_version = "TS1M2"),
                      expect_error(sas_run("%let x = %sysfunc(mdy(2, 30, 2024));", on_error = "abort"),
                                   class = "sasonr_unsupported"))
  r2 <- withr::with_options(list(sasonr.step_notes = TRUE), sas_run("data one; x = 1; run;
    proc sql; create table t as select mdy(2, 30, 2024) as d from one; quit;"))
  expect_true(any(grepl("^NOTE: Invalid argument to function MDY\\. Missing values may be generated\\.$", r2$log)))
  expect_true(is.na(r2$work$t$d))
  # PRXMATCH / PRXCHANGE: an invalid pattern (SAS writes the Perl diagnostic) now fails loud
  expect_error(sas_f_prxmatch("/[/", "abc"), class = "sasonr_unsupported")
  expect_error(sas_f_prxchange("s/[/x/", 1, "a"), class = "sasonr_unsupported")
  expect_identical(sas_f_prxmatch("/b+/", "abbc"), 2)
})

test_that("HIGH 3: both_error only when sasonr reports a SAS ERROR at the same step", {
  expect_identical(conf_fail_status(sas_err = TRUE, unsupported = TRUE, sas_style = FALSE), "sas_error_unsupported")
  expect_identical(conf_fail_status(TRUE, FALSE, FALSE), "sas_error_rerror")
  expect_identical(conf_fail_status(TRUE, FALSE, TRUE), "both_error")
  expect_identical(conf_fail_status(FALSE, TRUE, FALSE), "unsupported")
  expect_identical(conf_fail_status(FALSE, FALSE, TRUE), "error")
  expect_identical(conf_case_class(c("sas_error_unsupported", "sas_error_rerror"), c("", ""), c(NA, NA)),
                   c("mismatch", "mismatch"))
})

test_that("HIGH 7: a numeric difference in the last bits fails the case under strict", {
  r <- list(status = "fail", layers = c(ds = TRUE, files = FALSE), tol_files = 1e-15)
  expect_identical(conf_case_verdict(r), "fail")
  expect_identical(conf_case_verdict(list(status = "unsupported")), "skip")
  expect_identical(conf_case_verdict(list(status = "sas_error_rerror")), "fail")
  expect_identical(conf_case_verdict(list(status = "fail", layers = c(ds = TRUE, log = TRUE))), "pass")
})

# ---- HIGH 4: context of the call ------------------------------------------------------

test_that("HIGH 4: a function runs only in the contexts in which it is verified", {
  fn <- sas_feature_registry("functions")
  nm <- fn$name[fn$status == "verified" & fn$contexts_verified == "data"][1]
  skip_if(is.na(nm), "no function verified in the DATA step only")
  withr::local_options(sasonr.strict = TRUE)
  expect_silent(sasonr:::feature_check("function", nm, 1, "data"))
  e <- expect_error(sasonr:::feature_check("function", nm, 1, "sql"), class = "sasonr_not_verified")
  expect_identical(e$context, "sql")
  expect_error(sasonr:::feature_check("function", nm, 1, "sysfunc"), class = "sasonr_not_verified")
  expect_error(sasonr:::feature_check("function", nm, 1, "where"), class = "sasonr_not_verified")
  withr::with_options(list(sasonr.strict = FALSE),
                      expect_warning(sasonr:::feature_check("function", nm, 1, "where"), class = "sasonr_unverified"))
})

test_that("HIGH 4: a function in a WHERE statement fails loud (no WHERE verification cases)", {
  fn <- sas_feature_registry("functions")
  skip_if(grepl("where", fn$contexts_verified[fn$name == "ABS"]), "ABS is verified in WHERE")
  withr::local_options(sasonr.strict = TRUE)
  e <- expect_error(sas_run("data a; x = -2; run; data b; set a; where abs(x) > 1; run;"), class = "sasonr_not_verified")
  expect_identical(e$name, "ABS")
  expect_identical(e$context, "where")
  r <- sas_run("data a; x = -2; y = abs(x); run;")
  expect_identical(r$work$a$y, 2)
})

# ---- HIGH 5: formats named at run time; BESTw. above 16 -------------------------------

test_that("HIGH 5: PUTN and INPUTN check the format named at run time", {
  fm <- sas_feature_registry("formats")
  uv <- fm$name[fm$kind == "format" & fm$status == "unverified" & fm$name != "w.d" & !startsWith(fm$name, "$")]
  skip_if(!length(uv), "no unverified numeric format")
  withr::local_options(sasonr.strict = TRUE)
  e <- expect_error(sas_f_putn(1, paste0(uv[1], "12.")), class = "sasonr_not_verified")
  expect_identical(e$name, uv[1])
  expect_identical(e$kind, "format")
  ui <- fm$name[fm$kind == "informat" & fm$status == "unverified" & fm$name != "w.d" & !startsWith(fm$name, "$")]
  if (length(ui)) expect_error(sas_f_inputn("1", paste0(ui[1], "12.")), class = "sasonr_not_verified")
  ok <- fm$name[fm$kind == "format" & fm$status == "verified" & fm$name == "DATE"]
  if (length(ok)) expect_identical(sas_f_putn(0, "date9."), "01JAN1960")
})

test_that("HIGH 5: BESTw. above 16 shows the first 15 digits of the display value", {
  # SAS 9.4 gold (fn_matrix results.txt): the former undetermined window is settled by
  # the round-13 scans: 4.56388888888888 is truncated, LOG(3) rounded up
  v <- readBin(as.raw(c(0x40, 0x12, 0x41, 0x6C, 0x16, 0xC1, 0x6C, 0x17)), "double", endian = "big")
  expect_identical(trimws(sas_fmt_best(v, 32L)), "4.56388888888888")
  expect_identical(trimws(sas_fmt_best(cos(1), 32L)), "0.54030230586813")
  expect_identical(trimws(sas_fmt_best(log(3), 32L)), "1.09861228866811")
  expect_identical(trimws(sas_fmt_best(0.7, 32L)), "0.7")
  expect_identical(trimws(sas_fmt_best(cos(1), 12L)), "0.5403023059")
})

# ---- HIGH 6: what an ERROR leaves in WORK ---------------------------------------------

test_that("HIGH 6: the 0-observation data set after an ERROR has the compiled types and lengths", {
  r <- sas_run("data a; s = 'abc'; n = 1; run; data b; set a; length t $ 5; retain q 0; y = s + ; run;", on_error = "sas")
  expect_identical(r$status, "error")
  expect_identical(names(r$work$b), c("s", "n", "t", "q", "y"))
  expect_identical(nrow(r$work$b), 0L)
  # 53d319b: every variable numeric (a token scan without types), LENGTH and RETAIN missed
  # (testthat's expect_type() is masked by an internal parser helper under load_all)
  expect_true(is.character(r$work$b$s))
  expect_true(is.character(r$work$b$t))
  expect_identical(r$meta$b$len$t, 5L)
  expect_identical(r$meta$b$len$s, 3L)
})

test_that("HIGH 6: when the variables cannot be derived from the parsed step, sasonr fails loud", {
  expect_error(sas_run("data b; x = 1; if x > then y = 1; run;", on_error = "sas"), class = "sasonr_unsupported")
  expect_error(sas_run("proc means data=nosuch; output out=o; run;", on_error = "sas"), class = "sasonr_unsupported")
})

# ---- MEDIUM ---------------------------------------------------------------------------

test_that("MEDIUM 1: PROC FREQ statistics use the common strict gate", {
  d <- "data d; input a $ b $ n; datalines;\nX Y 3\nX Z 4\nW Y 5\nW Z 2\n;\nrun;\n"
  # CMH became verified (freq_stats c51-c57); CMH1 is still unverified and shows
  # that the PROC FREQ statistics go through the common strict gate.
  withr::with_options(list(sasonr.strict = TRUE), {
    e <- tryCatch(sas_run(paste(d, "proc freq data=d; tables a*b / cmh1; weight n; run;")), error = function(z) z)
    expect_s3_class(e, "sasonr_not_verified")
    expect_match(conditionMessage(e), "PROC FREQ statistic CMH1", fixed = TRUE)
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    n <- 0L
    withCallingHandlers(try(sas_run(paste(d, "proc freq data=d; tables a*b / cmh1; weight n; run;")), silent = TRUE),
                        sasonr_unverified = function(w) {
                          n <<- n + 1L
                          invokeRestart("muffleWarning")
                        })
    expect_gt(n, 0L)
  })
})

test_that("MEDIUM 2: one moment kernel; the DATA step functions run inside their verified domain", {
  x <- c(1.5, 2.25, -3.125, 10, 0.001)
  k <- sasonr:::stats_moments(matrix(x, 1), "datastep")
  args <- as.list(x)
  expect_identical(do.call(sas_f_std, args), k$std)
  expect_identical(do.call(sas_f_var, args), k$var)
  expect_identical(do.call(sas_f_css, args), k$css)
  expect_identical(do.call(sas_f_uss, args), k$uss)
  expect_identical(do.call(sas_f_stderr, args), k$stderr)
  expect_identical(do.call(sas_f_cv, args), k$cv)
  expect_identical(sasonr:::stats_sql_moment("css", x), sasonr:::stats_moments(x, "sql")$css)
  # more than 10 non-missing arguments: not verified (fn_probe/STD)
  expect_error(do.call(sas_f_std, as.list(1:11)), class = "sasonr_unsupported")
  expect_identical(do.call(sas_f_uss, as.list(1:12)), sum(as.numeric(1:12)^2))
})

test_that("MEDIUM 4: registry gating is skipped by AST node, not by the name of the function", {
  # 53d319b: a SAS program calling the internal CVT_N2C ran it (name prefix cvt_)
  r <- sas_run("data a; x = cvt_n2c(1); run;", on_error = "sas")
  expect_identical(r$status, "error")
  expect_true(any(grepl("CVT_N2C is unknown", r$log, fixed = TRUE)))
  # the constructs built by the parser still run (SQL LIKE / CASE, type conversion)
  r <- sas_run("data a; s = 'abc'; x = 1; run;
proc sql; create table b as select case when s like 'a%' then 1 else 0 end as k from a; quit;")
  expect_identical(r$work$b$k, 1)
})

# ---- LOW ------------------------------------------------------------------------------

test_that("LOW: the function inventory fails closed", {
  expect_error(sasonr:::sas_function_inventory(""), class = "sasonr_error")
  expect_true(sasonr:::sas_known_function("abs"))
  expect_false(sasonr:::sas_known_function("nosuchfn"))
})
