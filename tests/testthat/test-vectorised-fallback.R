# Vectorised DATA steps (L1/L2) that stop on an unsupported value, a data
# error or a SAS exception re-run row by row (L3): only the branches that run
# count, and the failed vectorised attempt leaves nothing behind.

test_that("a SELECT branch that does not run cannot fail loud (vectorised code re-runs row by row)", {
  # ROUND(99999.45, 0.1) is 99999.5; the vectorised SELECT also evaluates
  # ROUNDE(99999.45, 0.1), which fails loud (outside its supported domain)
  code <- "data _null_; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; put r; datalines;
ROUND 99999.45 0.1
ROUNDE 1 1
;
"
  res <- sas_run(code)
  expect_identical(res$log[res$log %in% c("99999.5", "1")], c("99999.5", "1"))
  expect_error(sas_run(sub("ROUND 99999", "ROUNDE 99999", code)), class = "sasonr_unsupported")
})

test_that("the vectorised attempt leaves no PRXPARSE ids behind", {
  code <- "data a; input f :$8. x u; id = prxparse('/a/'); select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;
ROUND 99999.45 0.1
ROUND 1 1
;
"
  res <- sas_run(code)
  # the row-by-row run alone: ids 1, 2 (the failed vectorised attempt used 1)
  expect_identical(res$work$a$id, c(1, 2))
  expect_identical(res$work$a$r, c(99999.5, 1))
})

test_that("the vectorised attempt leaves no WORK data, log lines or _ERROR_ behind", {
  code <- "data a; x = 1; run;
data a; input f :$8. x u; y = 1 / x; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; put f= r=; datalines;
ROUND 99999.45 0.1
ROUND 0 1
;
"
  res <- sas_run(code)
  expect_identical(res$work$a$r, c(99999.5, 0))
  # one PUT line per row, one division-by-zero NOTE and PDV dump (row 2 only)
  expect_identical(sum(grepl("^f=ROUND r=", res$log)), 2L)
  expect_identical(sum(grepl("^NOTE: Division by zero detected", res$log)), 1L)
  expect_identical(sum(grepl("_ERROR_=1", res$log)), 1L)
  expect_false(any(grepl("_ERROR_=1", res$log[seq_len(match("f=ROUND r=99999.5", res$log))])))
})

test_that("an unsupported value in a branch that runs still fails loud at L3", {
  code <- "data a; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;
ROUND 1 1
ROUNDE 99999.45 0.1
;
"
  expect_error(sas_run(code), class = "sasonr_unsupported")
})

test_that("a type-level unsupported construct fails loud and is not re-run", {
  # a character IF condition in a branch that does not run is not re-run row
  # by row (that run would miss the conversion NOTE)
  expect_error(sas_run("data a; input x c $; if x = 1 then y = 1; else if c then y = 2; datalines;
1 a
;
"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; input x c $; if c; datalines;
1 a
;
"), class = "sasonr_unsupported")
  # the run-time safety net is not re-run either
  e <- tryCatch(sasonr:::sas_true("a"), error = identity)
  expect_s3_class(e, "sasonr_unsupported_construct")
  e <- tryCatch(sasonr:::sas_cmp("a", 1), error = identity)
  expect_s3_class(e, "sasonr_unsupported_construct")
  # a translation-time unsupported construct in a vectorised step: loud
  expect_error(sas_run("data a; x = 1; if x = 2 then y = _infile_; run;"), class = "sasonr_unsupported")
})

test_that("a SAS exception in a branch that does not run is not reported", {
  # vectorised code evaluated CONSTANT('EXACTINT', 1E300) for x = 1 and stopped
  # the step with the SAS exception block; only the ELSE branch runs
  res <- sas_run("data a; x = 1; if x = 2 then y = constant('EXACTINT', 1E300); else y = 3; run;")
  expect_identical(res$work$a$y, 3)
  expect_false(any(grepl("exception", res$log)))
  # taken: the exception (row-by-row run) as before
  res <- sas_run("data a; x = 2; if x = 2 then y = constant('EXACTINT', 1E300); else y = 3; run;")
  expect_true(any(grepl("An exception has been encountered", res$log)))
  expect_identical(nrow(res$work$a), 0L)
})

test_that("a step that always falls back runs once vectorised and once row by row", {
  n <- 0L
  orig <- sasonr:::engine_eval
  local_mocked_bindings(engine_eval = function(...) {
    n <<- n + 1L
    orig(...)
  })
  rows <- paste(rep("ROUND 99999.45 0.1", 200), collapse = "\n")
  res <- sas_run(paste0("data a; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;\n", rows, "\n;\n"))
  expect_identical(n, 2L)
  expect_identical(unique(res$work$a$r), 99999.5)
})

test_that("the character IF check skips errors.R's variable stand-ins", {
  res <- sas_run("data d; input x k $; datalines;
1 a
;
data w; set d; snd = (k =* \"a\"); run;", on_error = "sas")
  expect_identical(res$status, "error")
  expect_true("w" %in% ls(res$work))
})
