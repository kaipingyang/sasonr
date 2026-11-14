# Librefs: PROC SQL knows WORK and SASHELP (R/sashelp.R reads SASHELP members
# from the directory set with options(sasonr.sashelp_dir)); others fail loud.

test_that("libraries other than WORK and SASHELP are rejected", {
  expect_error(sas_run("proc sql; create table w as select * from other.t; quit;"),
               class = "sasonr_unsupported")
})
