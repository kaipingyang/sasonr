# Data-level PROC functions: the same tables as the PROC step in sas_run().

api_d <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))
# the reference: the PROC step run by sas_run() on a session holding the same data
api_ref <- function(code) sas_run(code, session = sas_session(work = list(data = api_d)))

test_that("sas_means_df() returns the table of the same PROC MEANS step", {
  m <- sas_means_df(api_d, var = "x", class = "grp", stats = c("n", "mean"))
  r <- api_ref("proc means data=data n mean; class grp; var x; run;")
  expect_s3_class(m, "sasonr_ods_table")
  expect_identical(as.data.frame(m), as.data.frame(r$ods[[1]]))
  expect_identical(as.data.frame(m)$x_Mean, c(1.5, 4))
})

test_that("BY groups give a list of tables", {
  b <- sas_means_df(api_d, var = "x", by = "grp")
  expect_s3_class(b, "sasonr_ods_list")
  expect_length(b, 2L)
})

test_that("sas_tabulate_df() and sas_transpose_df()", {
  t <- sas_tabulate_df(api_d, "grp all, x*(n sum)", class = "grp", var = "x")
  r <- api_ref("proc tabulate data=data; class grp; var x; table grp all, x*(n sum); run;")
  expect_identical(as.data.frame(t), as.data.frame(r$ods[[1]]))
  tr <- sas_transpose_df(api_d, var = "x")
  expect_identical(tr$col3, 4)
})

test_that("sas_log() and sas_html() take the result", {
  m <- sas_means_df(api_d, var = "x")
  expect_match(paste(as.character(sas_log(m)), collapse = "\n"), "proc means data=data", fixed = TRUE)
  expect_match(sas_html(m), "<table", fixed = TRUE)
})

test_that("arguments are checked", {
  expect_error(sas_means_df(api_d, var = "x; run; data y"), class = "sasonr_error")
  expect_error(sas_means_df(list(x = 1), var = "x"), class = "sasonr_error")
  expect_error(sas_tabulate_df(api_d, c("a", "b")), class = "sasonr_error")
})
