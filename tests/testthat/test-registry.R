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

test_that("sasonr.strict: unverified features fail loud or warn; default unchanged", {
  fn <- sas_feature_registry("functions")
  skip_if(nrow(fn) == 0, "registry not built")
  nv <- fn$name[fn$status != "verified"][1]
  skip_if(is.na(nv), "every function is verified")
  expect_identical(sasonr:::feature_status("function", nv), fn$status[fn$name == nv])
  withr::with_options(list(sasonr.strict = TRUE), {
    expect_error(sasonr:::feature_check("function", nv), class = "sasonr_unsupported")
  })
  withr::with_options(list(sasonr.strict = FALSE), {
    expect_warning(sasonr:::feature_check("function", nv), class = "sasonr_unverified")
  })
  withr::with_options(list(sasonr.strict = NULL), {
    expect_silent(sasonr:::feature_check("function", nv))
  })
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("8.2")), "w.d")
  expect_identical(sasonr:::fmt_registry_name(sasonr:::sas_fmt_text("$char5.")), "$CHAR")
})
