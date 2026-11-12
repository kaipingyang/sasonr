# Feature registries generated from the offline conformance results
# (tools/update_registry.R) and the run-time verification check.

test_that("the feature registries have no silent mismatch", {
  fn <- sas_feature_registry("functions")
  fm <- sas_feature_registry("formats")
  skip_if(nrow(fn) == 0, "registry not built")
  expect_true(all(fn$status %in% c("verified", "approximate", "partial", "not_comparable", "unverified", "unsupported")))
  expect_true(all(fm$status %in% c("verified", "approximate", "partial", "not_comparable", "unverified", "unsupported")))
  expect_equal(sum(as.integer(fn$mismatch), na.rm = TRUE), 0)
  expect_equal(sum(as.integer(fm$mismatch), na.rm = TRUE), 0)
  v <- fn[fn$status == "verified", ]
  expect_identical(v$cases, v$pass)
  # every exported sas_f_* function except internal helpers has a registry entry
  ns <- getNamespaceExports("sasonr")
  f <- sub("^sas_f_", "", grep("^sas_f_", ns, value = TRUE))
  f <- f[!startsWith(f, "cvt_") & !startsWith(f, "sql") & f != "monotonic"]
  expect_setequal(toupper(f), fn$name)
})

test_that("sasonr.strict: approximate and unverified features fail loud by default", {
  fn <- sas_feature_registry("functions")
  skip_if(nrow(fn) == 0, "registry not built")
  ap <- fn$name[fn$status == "approximate"][1]
  skip_if(is.na(ap), "no approximate function")
  expect_identical(sasonr:::feature_status("function", ap), "approximate")
  withr::with_options(list(sasonr.strict = NULL), {
    e <- expect_error(sasonr:::feature_check("function", ap, 3), class = "sasonr_not_verified")
    expect_s3_class(e, "sasonr_unsupported")
    expect_identical(e$name, toupper(ap))
    expect_identical(e$status, "approximate")
    expect_match(conditionMessage(e), "approximate", fixed = TRUE)
    expect_match(conditionMessage(e), "options(sasonr.strict = FALSE)", fixed = TRUE)
    expect_match(conditionMessage(e), "(SAS line 3)", fixed = TRUE)
  })
  withr::with_options(list(sasonr.strict = TRUE), {
    expect_error(sasonr:::feature_check("function", ap), class = "sasonr_not_verified")
    expect_error(sasonr:::feature_check("function", "no_such_function"), class = "sasonr_not_verified")
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    expect_warning(sasonr:::feature_check("function", ap), class = "sasonr_unverified")
  })
  # verified, partial and clock-dependent features always run
  for (st in c("verified", "partial", "not_comparable")) {
    nm <- fn$name[fn$status == st][1]
    if (is.na(nm)) next
    withr::with_options(list(sasonr.strict = TRUE), expect_silent(sasonr:::feature_check("function", nm)))
  }
  fm <- sas_feature_registry("formats")
  uv <- fm[fm$status == "unverified" & fm$kind == "format", , drop = FALSE]
  if (nrow(uv)) {
    withr::with_options(list(sasonr.strict = NULL), {
      expect_error(sasonr:::feature_check("format", uv$name[1]), class = "sasonr_not_verified")
    })
  }
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("8.2")), "w.d")
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("$char5.")), "$CHAR")
})

test_that("sasonr.strict applies to the DATA step, PROC SQL and %SYSFUNC", {
  fn <- sas_feature_registry("functions")
  skip_if(!"MOD" %in% fn$name[fn$status == "approximate"], "MOD is not approximate")
  withr::with_options(list(sasonr.strict = NULL), {
    expect_error(sas_run("data a; y = mod(7, 3); run;"), class = "sasonr_not_verified")
    expect_error(sas_run("data one; x = 1; run;\nproc sql; create table b as select mod(x, 3) as y from one; quit;"),
                 class = "sasonr_not_verified")
    expect_error(sas_run("%put %sysfunc(mod(7, 3));"), class = "sasonr_not_verified")
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    res <- suppressWarnings(sas_run("data a; y = mod(7, 3); run;"))
    expect_equal(res$work$a$y, 1)
    expect_warning(sas_run("data a; y = mod(7, 3); run;"), class = "sasonr_unverified")
  })
})

test_that("an execution-time ERROR that stops a one-observation step keeps the data set (SAS)", {
  res <- withr::with_options(list(sasonr.step_notes = TRUE), sas_run("data a; r = symget(''); run;"))
  expect_equal(nrow(res$work$a), 1L)
  expect_true(any(startsWith(res$log, "WARNING: The data set WORK.A may be incomplete.")))
  expect_false(any(startsWith(res$log, "NOTE: The data set WORK.A has")))
  expect_error(sas_run("data a; do i = 1 to 2; r = symget(''); output; end; run;"), class = "sasonr_unsupported")
})
