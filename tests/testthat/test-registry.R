# Feature registries (inst/registry/) and strict mode (options(sasonr.strict)).

test_that("the feature registries give every feature a known status", {
  fn <- sas_feature_registry("functions")
  fm <- sas_feature_registry("formats")
  skip_if(nrow(fn) == 0, "registry not built")
  all_st <- c("supported", "partial", "not_comparable", "approximate", "mismatch", "unsupported")
  expect_true(all(fn$status %in% all_st))
  expect_true(all(fm$status %in% all_st))
  # every exported sas_f_* function except the runtime of language constructs has a registry entry
  ns <- getNamespaceExports("sasonr")
  f <- sub("^sas_f_", "", grep("^sas_f_", ns, value = TRUE))
  f <- setdiff(f, c(sasonr:::construct_fns, "monotonic"))
  expect_setequal(toupper(f), fn$name)
})

test_that("sasonr.strict: approximate and unsupported features fail loud by default", {
  fn <- sas_feature_registry("functions")
  skip_if(nrow(fn) == 0, "registry not built")
  ap <- fn$name[fn$status == "approximate"][1]
  skip_if(is.na(ap), "no approximate function")
  expect_identical(sasonr:::feature_status("function", ap), "approximate")
  withr::with_options(list(sasonr.strict = NULL), {
    e <- expect_error(sasonr:::feature_check("function", ap, 3), class = "sasonr_strict_error")
    expect_s3_class(e, "sasonr_unsupported")
    expect_identical(e$name, toupper(ap))
    expect_identical(e$status, "approximate")
    expect_match(conditionMessage(e), "approximate", fixed = TRUE)
    expect_match(conditionMessage(e), "options(sasonr.strict = FALSE)", fixed = TRUE)
    expect_match(conditionMessage(e), "(SAS line 3)", fixed = TRUE)
  })
  withr::with_options(list(sasonr.strict = TRUE), {
    expect_error(sasonr:::feature_check("function", ap), class = "sasonr_strict_error")
    expect_error(sasonr:::feature_check("function", "no_such_function"), class = "sasonr_strict_error")
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    expect_warning(sasonr:::feature_check("function", ap), class = "sasonr_strict_warning")
  })
  # supported, partial and clock-dependent features always run
  for (st in c("supported", "partial", "not_comparable")) {
    nm <- fn$name[fn$status == st][1]
    if (is.na(nm)) next
    withr::with_options(list(sasonr.strict = TRUE), expect_silent(sasonr:::feature_check("function", nm)))
  }
  fm <- sas_feature_registry("formats")
  uv <- fm[fm$status == "unsupported" & fm$kind == "format", , drop = FALSE]
  if (nrow(uv)) {
    withr::with_options(list(sasonr.strict = NULL), {
      expect_error(sasonr:::feature_check("format", uv$name[1]), class = "sasonr_strict_error")
    })
  }
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("8.2")), "w.d")
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("$char5.")), "$CHAR")
})

test_that("sasonr.strict applies to the DATA step, PROC SQL and %SYSFUNC", {
  fn <- sas_feature_registry("functions")
  skip_if(!"EXP" %in% fn$name[fn$status == "approximate"], "EXP is not approximate")
  withr::with_options(list(sasonr.strict = NULL), {
    expect_error(sas_run("data a; y = exp(1); run;"), class = "sasonr_strict_error")
    expect_error(sas_run("data one; x = 1; run;\nproc sql; create table b as select exp(x) as y from one; quit;"),
                 class = "sasonr_strict_error")
    expect_error(sas_run("%put %sysfunc(exp(1));"), class = "sasonr_strict_error")
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    res <- withCallingHandlers(sas_run("data a; y = exp(1); run;"),
                               sasonr_strict_warning = function(w) invokeRestart("muffleWarning"))
    expect_identical(res$work$a$y, exp(1))
    expect_warning(sas_run("data a; y = exp(1); run;"), class = "sasonr_strict_warning")
  })
})

test_that("an execution-time ERROR that stops a one-observation step keeps the data set", {
  res <- withr::with_options(list(sasonr.step_notes = TRUE), sas_run("data a; r = symget(''); run;"))
  expect_equal(nrow(res$work$a), 1L)
  expect_true(any(startsWith(msgs(res), "WARNING: The data set WORK.A may be incomplete.")))
  expect_false(any(startsWith(msgs(res), "NOTE: The data set WORK.A has")))
  expect_error(sas_run("data a; do i = 1 to 2; r = symget(''); output; end; run;"), class = "sasonr_unsupported")
})
