# The generated row-by-row (L3) code shows the step's statements; the
# per-step plumbing lives in run-time helpers (sas_step_state() ...).

l3_code <- function(code) {
  tr <- sas_translate(code, level = "L3")
  paste(tr$steps[[length(tr$steps)]]$code, collapse = "\n")
}

test_that("the step state replaces the plumbing variables", {
  x <- l3_code("data a; do i = 1 to 3; x = i * 2; output; end; run;")
  expect_match(x, ".st <- sas_step_state(", fixed = TRUE)
  expect_match(x, "pdv <- sas_iter_begin(.st, pdv)", fixed = TRUE)
  expect_match(x, ".status <- sas_iter_run({", fixed = TRUE)
  expect_match(x, "sas_write_pdv(.st, pdv)", fixed = TRUE)
  expect_match(x, "work$a <- sas_step_output(.st, 1L)", fixed = TRUE)
  for (old in c(".proto", ".n_iter", ".reset", ".out1", ".vars1", "tryCatch", ".dump")) {
    expect_false(grepl(old, x, fixed = TRUE), label = old)
  }
})

test_that("DO loops with constant bounds and DO UNTIL without CONTINUE are plain loops", {
  x <- l3_code("data a; do i = 1 to 3; x = i; output; end; run;")
  expect_match(x, "pdv$i <- 1\n", fixed = TRUE)
  expect_match(x, "while (sas_do_test(pdv$i, 3, 1)) {", fixed = TRUE)
  expect_false(grepl(".from", x, fixed = TRUE))
  x <- l3_code("data a; do until (x > 2); x = sum(x, 1); output; end; run;")
  expect_false(grepl(".first", x, fixed = TRUE))
  expect_match(x, "if (sas_true(sas_gt(pdv$x, 2))) break", fixed = TRUE)
  # CONTINUE needs the general form (the test after the body)
  x <- l3_code("data a; do i = 1 to 3; if i = 2 then continue; output; end; run;")
  expect_match(x, ".first", fixed = TRUE)
  # the results do not depend on the form
  for (code in c("data a; do i = 1 to 3; x = i; output; end; run;",
                 "data a; do i = 3 to 1 by -1; x = i; output; end; put i=; run;",
                 "data a; do until (x > 2); x = sum(x, 1); output; end; run;",
                 "data a; do i = 1 to 4; if i = 2 then continue; if i = 4 then leave; output; end; run;")) {
    expect_identical(sas_run(code, level = "L3")$work$a, sas_run(code)$work$a)
  }
})

test_that("an empty OTHERWISE writes no empty else; _ERROR_ syncs once without PUT", {
  x <- l3_code("data a; do i = 1 to 2; select (i); when (1) x = 1; otherwise; end; output; end; run;")
  expect_false(grepl("else {\n", x, fixed = TRUE))
  x <- l3_code("data a; do i = 1 to 2; x = 1 / (i - 1); output; end; run;")
  expect_identical(lengths(regmatches(x, gregexpr("sas_err_sync", x, fixed = TRUE))), 1L)
  x <- l3_code("data a; do i = 1 to 2; x = 1 / (i - 1); put x=; output; end; run;")
  expect_gt(lengths(regmatches(x, gregexpr("sas_err_sync", x, fixed = TRUE))), 1L)
})
