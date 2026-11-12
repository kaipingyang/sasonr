# Reference-page examples (inst/examples/sas_f_*.R, written by
# tools/gen_examples.R): every call that agreed with SAS 9.4 when the examples
# were generated must still agree with the SAS gold standard of its case
# (tests/conformance/fn_examples/<FN>/, local assets; skipped without them).

test_that("reference-page examples agree with SAS 9.4", {
  skip_if(conf_tier() == "none", "conformance tier 'none' (R CMD check)")
  skip_if_not_installed("haven")
  root <- conf_root()
  f <- if (is.null(root)) "" else file.path(root, "fn_examples", "results.csv")
  skip_if_not(file.exists(f), "tests/conformance/fn_examples/ not present (local conformance assets)")
  res <- utils::read.csv(f, colClasses = "character")
  res <- res[res$status == "agree", ]
  expect_gt(nrow(res), 0)
  for (fn in unique(res$fn)) {
    ds <- haven::read_sas(file.path(root, "fn_examples", fn, "expected", "datasets", "ex.sas7bdat"))
    rt$prx <- list()
    env <- new.env(parent = environment())
    for (i in which(res$fn == fn)) {
      v <- suppressWarnings(eval(parse(text = res$r[i])[[1]], envir = env))
      if (is.logical(v)) v <- as.numeric(v)
      s <- ds[[paste0("r", res$k[i])]]
      label <- paste0(fn, ": ", res$sas[i])
      if (is.character(s)) {
        expect_identical(sas_rtrim(v), sub("\\s+$", "", s), label = label)
      } else {
        expect_true(conf_num_eq(conf_sas_num(s), conf_son_num(v)), label = label)
      }
    }
  }
})
