test_that("the noncentral F series ends for extreme arguments", {
  setTimeLimit(elapsed = 20, transient = TRUE)
  withr::defer(setTimeLimit(elapsed = Inf))
  f <- sasonr:::pnf_series
  q <- sasonr:::qnf_series
  # every term underflows: the sum stays 0
  expect_identical(f(1e-300, 3, 5, 2), 0)
  # a noncentrality too large to sum term by term fails loud
  expect_error(f(2, 2, 10, 1e300), class = "sasonr_unsupported")
  expect_error(q(0.95, 2, 10, 1e300), class = "sasonr_unsupported")
  expect_true(is.finite(q(0.95, 2, 10, 1)))
})
