# SASHELP members in PROC SQL.  The data are copied from the local SAS
# installation into inst/sashelp/ (R/sashelp.R); that directory is in
# .gitignore and .Rbuildignore, so these tests skip when the copy is absent.

has_sashelp <- function(member) sasonr:::sashelp_has(member)

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
