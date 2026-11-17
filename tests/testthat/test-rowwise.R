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
  expect_error(rw(function(x) c(x, x), 1:2, type = numeric(1)))
  expect_error(rw(function(x) "a", 1:2, type = numeric(1)))
  expect_error(rw(function(x) 1, 1:2))
  expect_error(rw(function(x) 2.7, 1:2, type = integer(1)))
  expect_error(rw(function(x) NULL, 1:2, type = numeric(1)))
})
