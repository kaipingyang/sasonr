# The exports are explicit (roxygen @export, no export pattern): any change to
# the exported API must also change tests/testthat/_exports.txt.

test_that("the NAMESPACE exports are exactly the intended API", {
  f <- test_path("_exports.txt")
  x <- readLines(f)
  x <- x[nzchar(x) & !startsWith(x, "#")]
  kind <- sub(" .*$", "", x)
  name <- sub("^\\S+ ", "", x)
  expect_true(all(kind %in% c("public", "internal")))
  expect_false(anyDuplicated(name) > 0)
  exp <- getNamespaceExports("sasonr")
  expect_identical(sort(setdiff(exp, name)), character(), label = "exported but not listed")
  expect_identical(sort(setdiff(name, exp)), character(), label = "listed but not exported")
  # the binding of the active session is never exported
  expect_false("sas_rt_active" %in% exp)
  # every SAS function implementation is public; the helpers of translated
  # PROC SQL code (sas_f_cvt_*, sas_f_sql*) are internal
  fns <- grep("^sas_f_", ls(asNamespace("sasonr")), value = TRUE)
  helpers <- paste0("sas_f_", sasonr:::fn_layer_internal)
  expect_setequal(intersect(fns, name[kind == "public"]), setdiff(fns, helpers))
  expect_setequal(intersect(fns, name[kind == "internal"]), helpers)
})
