# Log lines that SAS wraps at LINESIZE and sasonr does not break: fail loud.

test_that("a PUT line longer than LINESIZE fails loud", {
  expect_error(sas_run(paste0("data _null_; length s $200; s = repeat('a', 199); put s; run;")), class = "sasonr_unsupported")
})

test_that("a WHERE NOTE line longer than LINESIZE breaks before the word that does not fit, lines but the last at most LINESIZE - 1", {
  withr::local_options(sasonr.step_notes = TRUE)
  inlist <- paste(1:45, collapse = ",")
  r <- sas_run(paste0("data a; do x = 1 to 50; output; end; run;\ndata b; set a; where x in (", inlist, "); run;"))
  lg <- as.character(sas_log(r))
  k <- grep("^      WHERE x in", lg)
  expect_identical(nchar(lg[k]), 128L)
  expect_match(lg[k], "30,$")
  expect_match(lg[k + 1L], "^      31, 32,")
})

test_that("a WHERE token too long for a line starts a new line and is cut at LINESIZE - 6", {
  ln <- sasonr:::log_wrap_where(paste0("      WHERE name='", strrep("A", 140), "';"), 132L)
  expect_identical(ln[1], "      WHERE")
  expect_identical(nchar(ln[2]), 132L)
  expect_identical(ln[3], paste0("      ", strrep("A", 140 - 120), "';"))
})
