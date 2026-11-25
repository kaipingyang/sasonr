# Log lines that SAS wraps at LINESIZE and sasonr does not break: fail loud.

test_that("a PUT of one value wider than LINESIZE is cut every LINESIZE characters", {
  lg <- as.character(sas_log(sas_run("data _null_; length s $200; s = repeat('a', 199); put s; run;")))
  expect_true(strrep("a", 132) %in% lg)
  expect_true(strrep("a", 68) %in% lg)
})

test_that("list PUT starts a value on a new line when it does not fit; a line of exactly LINESIZE stays", {
  lg <- as.character(sas_log(sas_run(paste0("options ls=80;\ndata _null_; length a b c $20 d $17; a = repeat('a', 19); b = a; c = a; ",
                                             "d = repeat('d', 16); put a b c d; put a b c d a; run;"))))
  full <- paste(strrep("a", 20), strrep("a", 20), strrep("a", 20), strrep("d", 17))
  expect_identical(sum(lg == full), 2L)
  expect_true(strrep("a", 20) %in% lg)
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
