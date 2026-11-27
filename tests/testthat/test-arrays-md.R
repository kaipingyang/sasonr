# Multi-dimensional arrays, DO OVER and sum statements on array elements.

put_lines <- function(code) as.character(sas_log(sas_run(code), echo = FALSE))

test_that("a multi-dimensional array is row-major; DIM, DIMn, LBOUNDn, HBOUNDn", {
  m <- put_lines("data _null_; array m[2,3] (1 2 3 4 5 6); a = m[1,3]; b = m[2,1]; d1 = dim(m); d2 = dim2(m); d3 = dim(m, 2); h = hbound1(m); put a= b= d1= d2= d3= h=; run;")
  expect_true("a=3 b=4 d1=2 d2=3 d3=3 h=2" %in% m)
  m <- put_lines("data _null_; array q{0:1, 2:3} q1-q4 (1 2 3 4); x = q{1,2}; l = lbound2(q); put x= l=; run;")
  expect_true("x=3 l=2" %in% m)
  expect_error(sas_run("data _null_; array m[2,2]; x = m[3,1]; run;", on_error = "abort"))
})

test_that("DO OVER walks an implicitly subscripted array with _I_", {
  r <- sas_run("data out; array v v1-v3; v1 = 1; v2 = 2; v3 = 3; do over v; v = v * 10; end; run;")
  expect_identical(unlist(r$work$out[c("v1", "v2", "v3")], use.names = FALSE), c(10, 20, 30))
  expect_false("_i_" %in% names(r$work$out))
  expect_error(sas_run("data out; array v[2] (1 2); do over v; v = 1; end; run;"), class = "sasonr_unsupported")
})

test_that("a sum statement on an array element retains it", {
  r <- sas_run("data out; array a[2] a1 a2; do i = 1 to 3; a[1] + i; a[2] + 10; output; end; run;")
  expect_identical(r$work$out$a1, c(1, 3, 6))
  expect_identical(r$work$out$a2, c(10, 20, 30))
  expect_error(sas_run("data out; array a[2]; do i = 1 to 2; a[i] + 1; end; run;"), class = "sasonr_unsupported")
})
