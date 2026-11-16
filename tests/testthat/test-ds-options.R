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
  code <- ds_opts_apply(an$dsir$slots$source[[1]]$datasets[[1]]$opts, an)
  pos <- vapply(c("sas_ds(", "rename = ", "dplyr::filter(", "sas_obs_window(2, 3)", "sas_note_read("),
                function(p) regexpr(p, code, fixed = TRUE), 1L)
  expect_true(all(pos > 0))
  expect_false(is.unsorted(pos))
  expect_match(code, "keep = c(\"g\", \"v\")", fixed = TRUE)
})

test_that("the WHERE statement applies only without WHERE=", {
  an <- dso_an("data c; set a(where = (v > 1)) b; where g = 'A'; run;")
  ds <- an$readers[[1]]$datasets
  w <- an$dsir$slots$where$effective
  ca <- ds_code(ds[[1]], an, where = w)
  cb <- ds_code(ds[[2]], an, where = w)
  expect_equal(lengths(regmatches(ca, gregexpr("dplyr::filter(", ca, fixed = TRUE))), 1L)
  expect_match(ca, "sas_gt(v, 1)", fixed = TRUE)
  expect_match(cb, "sas_eq(g, \"A\")", fixed = TRUE)
  # ds_code is the component applied to the record of the data set
  expect_identical(ca, ds_opts_apply(ds_opts_record(ds[[1]]), an, where = w))
})

test_that("PROC DATA= reads through the same component", {
  step <- sas_parse("proc print data = a(keep = g v firstobs = 2); where v > 1; run;")$steps[[1]]
  inp <- proc_input(step, dso_meta, NULL)
  expect_identical(proc_source_code(inp, obs_col = TRUE),
                   ds_opts_apply(ds_opts_record(inp$ds), list(arrays = list()), obs_col = TRUE, where = inp$where))
  expect_match(proc_source_code(inp), "sas_obs_window(2, NULL)", fixed = TRUE)
})
