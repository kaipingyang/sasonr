px <- function(code, where = FALSE) expr_to_sexpr(sas_parse_expr(code, where = where))

test_that("arithmetic precedence follows SAS Table 6.6", {
  expect_equal(px("2 + 3 * 4 ** 2 / 8"), "(+ 2 (/ (* 3 (** 4 2)) 8))")
  expect_equal(px("-2 ** 2"), "(neg (** 2 2))")
  expect_equal(px("2 ** 3 ** 2"), "(** 2 (** 3 2))")
  expect_equal(px("a - b - c"), "(- (- a b) c)")
  expect_equal(px("a || b || c"), "(|| (|| a b) c)")
})

test_that("Group I operators bind tighter than everything else", {
  expect_equal(px("not 0 + 1"), "(+ (not 0) 1)")
  expect_equal(px("5 >< 3 ** 2"), "(min 5 (** 3 2))")
  expect_equal(px("a <> b + 1"), "(+ (max a b) 1)")
  expect_equal(px("^x = 4"), "(= (not x) 4)")
})

test_that("logical operators: AND binds tighter than OR", {
  expect_equal(px("1 or 0 and 0"), "(or 1 (and 0 0))")
  expect_equal(px("a | b & c"), "(or a (and b c))")
  expect_equal(px("a < b and c"), "(and (< a b) c)")
})

test_that("comparison chains keep a chain node; parentheses break chains", {
  expect_equal(px("1 < 2 < 3"), "(chain < < 1 2 3)")
  expect_equal(px("3 > 2 > 1"), "(chain > > 3 2 1)")
  expect_equal(px("(3 > 2) > 1"), "(> (> 3 2) 1)")
  expect_equal(px("a eq b"), "(= a b)")
  expect_equal(px("x =: 'AB'"), "(=: x 'AB')")
})

test_that("IN lists accept constants, ranges and NOT IN", {
  expect_equal(px("s in ('ABC', 'X')"), "(in s 'ABC' 'X')")
  expect_equal(px("x in (1:5, 7 -1)"), "(in x 1:5 7 -1)")
  expect_equal(px("x not in (1,2)"), "(not-in x 1 2)")
  expect_equal(px("x in (1,2) and y"), "(and (in x 1 2) y)")
})

test_that("function calls, OF lists and array references", {
  expect_equal(px("sum(of x1-x3, y)"), "(call sum (of x1-x3) y)")
  expect_equal(px("max(of x:)"), "(call max (of x:))")
  expect_equal(px("sum(of a{*})"), "(call sum (of a{*}))")
  expect_equal(px("a{i+1} * b[2]"), "(* (aref a (+ i 1)) (aref b 2))")
  expect_equal(px("first.g and last.h"), "(and first.g last.h)")
  expect_equal(px("f(g(a, (b + c)))"), "(call f (call g a (+ b c)))")
})

test_that("WHERE-only operators require WHERE context", {
  expect_equal(px("v between 2 and 4 and n contains 'b'", where = TRUE),
               "(and (between v 2 4) (contains n 'b'))")
  expect_equal(px("x is missing", where = TRUE), "(is-missing x)")
  expect_error(sas_parse_expr("v between 2 and 4"), class = "sasonr_syntax_error")
})

test_that("literals are typed", {
  e <- sas_parse_expr("'15MAR2026'd")
  expect_equal(e$value, as.numeric(as.Date("2026-03-15") - as.Date("1960-01-01")))
  expect_equal(sas_parse_expr("'10:30't")$value, 37800)
  expect_equal(sas_parse_expr(".A")$code, ".A")
})

test_that("macro references fail loud", {
  expect_error(sas_parse_expr("&x + 1"), class = "sasonr_unsupported")
})
