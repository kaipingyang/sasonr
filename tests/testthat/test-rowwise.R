rw <- sasonr:::rowwise

test_that("rowwise() keeps the result type and recycles its arguments", {
  expect_identical(rw(function(x, y) paste0(x, y), c("a", "b"), "z"), c("az", "bz"))
  expect_identical(rw(function(x) x * 2, c(1, 2), type = numeric(1)), c(2, 4))
  expect_identical(rw(function(x) nchar(x), c("ab", "c"), type = integer(1)), c(2L, 1L))
  expect_identical(rw(function(x) x > 1, c(1, 2), type = logical(1)), c(FALSE, TRUE))
})

test_that("rowwise() widens integer and logical results to double", {
  expect_identical(rw(function(x) -3L, 1:2, type = numeric(1)), c(-3, -3))
  expect_identical(rw(function(x) NA, 1:2, type = numeric(1)), c(NA_real_, NA_real_))
})

test_that("rowwise() gives a zero-length vector of the result type", {
  expect_identical(rw(function(x) x, character(0)), character(0))
  expect_identical(rw(function(x) x, numeric(0), 1, type = numeric(1)), numeric(0))
  expect_identical(rw(function(x) x, integer(0), type = integer(1)), integer(0))
})

test_that("rowwise() fails on a result of the wrong length or a lossy type", {
  for (n in list(1, 1:2)) expect_error(rw(function(x) c(x, x), n, type = numeric(1)), class = "sasonr_rowwise_contract")
  for (n in list(1, 1:2)) expect_error(rw(function(x) "a", n, type = numeric(1)), class = "sasonr_rowwise_contract")
  for (n in list(1, 1:2)) expect_error(rw(function(x) 1, n), class = "sasonr_rowwise_contract")
  for (n in list(1, 1:2)) expect_error(rw(function(x) 2.7, n, type = integer(1)), class = "sasonr_rowwise_contract")
  for (n in list(1, 1:2)) expect_error(rw(function(x) NULL, n, type = numeric(1)), class = "sasonr_rowwise_contract")
})

test_that("rowwise() on one row coerces like the vector path", {
  vals <- list(2, 2.5, TRUE, 1L, 0L, 2L, "a", NA, NA_character_, c(a = 1), factor("a"), as.Date(0), list(1), NULL, c(1, 2))
  types <- list(integer(1), numeric(1), logical(1), character(1))
  for (v in vals) for (ty in types) {
    one <- tryCatch(rw(function(x) v, 1, type = ty), error = function(e) "error")
    two <- tryCatch(rw(function(x) v, c(1, 1), type = ty), error = function(e) "error")
    expect_identical(one, if (identical(two, "error")) "error" else two[1], info = paste(deparse(v), typeof(ty)))
  }
})

test_that("rowwise() passes a condition raised by the function through unchanged", {
  f <- function(x) rlang::abort("inner", class = "sasonr_error")
  for (n in list(1, 1:3)) expect_error(rw(f, n, type = numeric(1)), class = "sasonr_error")
  for (n in list(1, 1:3)) {
    e <- tryCatch(rw(f, n, type = numeric(1)), error = identity)
    expect_false(inherits(e, "purrr_error_indexed"))
  }
})
