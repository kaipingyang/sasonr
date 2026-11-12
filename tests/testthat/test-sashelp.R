# SASHELP members in PROC SQL.  The data are exported from the local SAS
# installation into inst/sashelp/ (see tools/build_sashelp.R); that directory
# is in .gitignore, so these tests skip when the export is absent.

has_sashelp <- function(member) {
  d <- system.file("sashelp", package = "sasonr")
  nzchar(d) && file.exists(file.path(d, paste0(member, ".rds")))
}

test_that("PROC SQL reads a SASHELP member", {
  skip_if_not(has_sashelp("class"), "inst/sashelp not exported")
  res <- sas_run("proc sql;
  create table w as select name, age from sashelp.class where age >= 15;
  quit;")
  expect_true(nrow(res$work$w) >= 5)
  expect_identical(names(res$work$w), c("name", "age"))
  # SAS pads character values to the declared length (8 for SASHELP.CLASS.NAME)
  expect_true(all(nchar(res$work$w$name) <= 8))
})

test_that("a missing SASHELP member fails loud", {
  skip_if_not(has_sashelp("class"), "inst/sashelp not exported")
  expect_error(sas_run("proc sql; create table w as select * from sashelp.nosuchmember; quit;"),
               class = "sasonr_unsupported")
})

test_that("libraries other than WORK and SASHELP are rejected", {
  expect_error(sas_run("proc sql; create table w as select * from other.t; quit;"),
               class = "sasonr_unsupported")
})
