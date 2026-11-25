# Log lines that SAS wraps at LINESIZE and sasonr does not break: fail loud.

test_that("a PUT line longer than LINESIZE fails loud", {
  expect_error(sas_run(paste0("data _null_; length s $200; s = repeat('a', 199); put s; run;")), class = "sasonr_unsupported")
})

test_that("a WHERE NOTE line longer than LINESIZE fails loud", {
  withr::local_options(sasonr.step_notes = TRUE)
  inlist <- paste(1:45, collapse = ",")
  expect_error(sas_run(paste0("data a; x = 1; run;\ndata b; set a; where x in (", inlist, "); run;")), class = "sasonr_unsupported")
})
