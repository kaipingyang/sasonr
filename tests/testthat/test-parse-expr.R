px <- function(code, where = FALSE) expr_to_sexpr(sas_parse_expr(code, where = where))

test_that("arithmetic precedence follows the documented Table 6.6", {
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

# The operator table (operators.R): DATA step, WHERE and PROC SQL
pw <- function(code) px(code, where = TRUE)
psql <- function(code) {
  p <- new_parser(sas_lex(code))
  e <- parse_sql_expr(p)
  if (!at_eof(p)) abort_syntax(paste("unexpected", tok_desc(p)), tk_line(p))
  expr_to_sexpr(e)
}

test_that("IN binds tighter than the other comparison operators", {
  expect_equal(px("a <= b in (0, 5)"), "(<= a (in b 0 5))")
  expect_equal(px("a = b in (0, 1)"), "(= a (in b 0 1))")
  expect_equal(px("a in (3, 4) < c"), "(< (in a 3 4) c)")
  expect_equal(px("a in (3, 4) in (0)"), "(in (in a 3 4) 0)")
  expect_equal(px("a + b in (2, 4)"), "(in (+ a b) 2 4)")
  expect_equal(px("a || b in ('ab')"), "(in (|| a b) 'ab')")
  expect_equal(px("a < b in (1) < c"), "(chain < < a (in b 1) c)")
  expect_equal(px("a ne b not in (1)"), "(^= a (not-in b 1))")
})

test_that("an IN list is no left operand of an operator that binds tighter", {
  for (op in c("**", "><", "<>", "*", "/", "+", "-", "||")) {
    expect_error(sas_parse_expr(paste("a in (1, 2)", op, "c")), class = "sasonr_syntax_error")
  }
  expect_equal(px("a in (1, 2) and c"), "(and (in a 1 2) c)")
})

test_that("group 1 groups right to left, the prefix operators included", {
  expect_equal(px("a ** -b ** c"), "(** a (neg (** b c)))")
  expect_equal(px("-3 >< -3"), "(neg (min 3 (neg 3)))")
  expect_equal(px("a >< b <> c"), "(min a (max b c))")
  expect_equal(px("a ** b >< c"), "(** a (min b c))")
  expect_equal(px("not a ** b"), "(not (** a b))")
  expect_equal(px("not a = b"), "(= (not a) b)")
  expect_equal(px("a = not b"), "(= a (not b))")
  expect_equal(px("- not a + b"), "(+ (neg (not a)) b)")
  expect_equal(px("- + a"), "(neg (+ a))")
  expect_equal(px("a min b + c"), "(+ (min a b) c)")
})

test_that("chains of comparison operators", {
  expect_equal(px("a < b < c < d"), "(chain < < < a b c d)")
  expect_equal(px("a = b = c"), "(chain = = a b c)")
  expect_equal(px("a lt b le c"), "(chain < <= a b c)")
  expect_equal(px("(a < b) < c"), "(< (< a b) c)")
  expect_equal(px("a < b + c < d"), "(chain < < a (+ b c) d)")
})

test_that("WHERE: a condition is no operand of a comparison or of another condition", {
  for (e in c("a < b in (0)", "a in (0) < b", "a in (0) in (1)", "a < b like 'x%'", "a like 'x%' = b",
              "a between 1 and 2 < c", "a < b < c in (0)", "a is missing = 1", "a in (0) + b", "a ? 'b' = c")) {
    expect_error(sas_parse_expr(e, where = TRUE), class = "sasonr_syntax_error")
  }
  expect_equal(pw("a < b < c"), "(chain < < a b c)")
  expect_equal(pw("a between b and c - d"), "(between a b (- c d))")
  expect_equal(pw("a between b and c and d"), "(and (between a b c) d)")
  expect_equal(pw("a <> b < c"), "(chain ^= < a b c)")
})

test_that("WHERE: prefix signs bind tighter than **, ** tighter than MIN and MAX", {
  expect_equal(pw("-a ** b"), "(** (neg a) b)")
  expect_equal(pw("a ** - b ** c"), "(** a (** (neg b) c))")
  expect_equal(pw("- a min b"), "(min (neg a) b)")
  expect_equal(pw("a ** b min c"), "(min (** a b) c)")
  expect_equal(pw("a min b max c"), "(max (min a b) c)")
  expect_equal(pw("a * b min c"), "(* a (min b c))")
  expect_equal(pw("not a ** b"), "(not (** a b))")
  expect_equal(pw("not a min b"), "(not (min a b))")
  expect_equal(pw("not a * b"), "(* (not a) b)")
  expect_equal(pw("not a = b"), "(= (not a) b)")
  expect_equal(pw("a = not b"), "(= a (not b))")
  expect_equal(pw("- - a"), "(neg (neg a))")
  expect_equal(pw("not - a"), "(not (neg a))")
  for (e in c("a ** not b", "not not a", "- not a", "- + a", "+ - a")) {
    expect_error(sas_parse_expr(e, where = TRUE), class = "sasonr_syntax_error")
  }
})

test_that("PROC SQL: NOT takes a comparison; the conditions stand alone", {
  expect_equal(psql("not a = b"), "(not (= a b))")
  expect_equal(psql("^ a = b"), "(not (= a b))")
  expect_equal(psql("not a ** b"), "(not (** a b))")
  expect_equal(psql("not a and b"), "(and (not a) b)")
  expect_equal(psql("not a in (1, 2)"), "(not (in a 1 2))")
  expect_equal(psql("a & not b"), "(and a (not b))")
  expect_equal(psql("-a ** b"), "(neg (** a b))")
  expect_equal(psql("a ** b ** c"), "(** a (** b c))")
  expect_equal(psql("a < b < c"), "(chain < < a b c)")
  expect_equal(psql("a = b = c"), "(chain = = a b c)")
  expect_equal(psql("a between b and c - d"), "(between a b (- c d))")
  expect_equal(psql("a like b || c"), "(like a (|| b c))")
  expect_equal(psql("a || b in ('ab')"), "(in (|| a b) 'ab')")
  for (e in c("a < b in (0)", "a in (0) < b", "a in (0) in (1)", "a in (0) + b", "a like b = c", "a < b between 1 and 2",
              "a = not b", "not not a", "- not a", "- + a", "a is missing = 1")) {
    expect_error(psql(e), class = "sasonr_syntax_error")
  }
})

test_that("NOT IN and IN: bind like IN; colon comparisons chain", {
  expect_equal(px("a < b not in (1)"), "(< a (not-in b 1))")
  expect_equal(px("n < s in: ('ab')"), "(< n (=: s 'ab'))")
  expect_equal(px("s =: t < u"), "(chain =: < s t u)")
  expect_equal(px("s < t =: u"), "(chain < =: s t u)")
  expect_equal(pw("s =: t =: u"), "(chain =: =: s t u)")
  expect_error(sas_parse_expr("a < b < c =: d"), class = "sasonr_unsupported")
  expect_equal(psql("s < t eqt u"), "(chain < =: s t u)")
  expect_error(psql("s eqt t < u"), class = "sasonr_unsupported")
  expect_error(psql("s eqt t eqt u"), class = "sasonr_unsupported")
})

test_that("WHERE: NOT as an operand", {
  for (e in c("a * not b", "a - not b", "a + not b", "a / not b")) {
    expect_error(sas_parse_expr(e, where = TRUE), class = "sasonr_unsupported")
  }
  for (e in c("a min not b", "a max not b", "a ** not b", "- not a", "not not a")) {
    expect_error(sas_parse_expr(e, where = TRUE), class = "sasonr_syntax_error")
  }
  expect_equal(pw("a || not b"), "(|| a (not b))")
  expect_equal(pw("a between not b and c"), "(between a (not b) c)")
  expect_equal(pw("s like not t"), "(like s (not t))")
  expect_equal(pw("a < not b"), "(< a (not b))")
  expect_equal(px("a * not b"), "(* a (not b))")
})

test_that("negated conditions are shown in the S-expressions", {
  expect_equal(pw("s not contains 'a'"), "(not-contains s 'a')")
  expect_equal(pw("x is not missing"), "(is-not-missing x)")
})

test_that("a WHERE expression with an error leaves the parser in the DATA step rules", {
  p <- new_parser(sas_lex("where a * ; - + 1"))
  expect_error(parse_where(p), class = "sasonr_syntax_error")
  expect_false(isTRUE(p$where_ctx))
  while (tk_type(p) != "semi") advance(p)
  advance(p)
  expect_equal(expr_to_sexpr(parse_expr(p)), "(neg (+ 1))")
})

test_that("expression nodes carry their source span and an S3 class (spec 09 contract 3)", {
  s <- sas_parse("data out; set a; if x > 1 then y = x + 2*z; run;")$steps[[1]]$stmts[[2]]
  expect_s3_class(s$cond, c("sas_compare", "sas_node"), exact = TRUE)
  expect_identical(s$cond$span[c("line", "col", "end_line", "end_col")], c(line = 1L, col = 21L, end_line = 1L, end_col = 25L))
  v <- s$then$value
  expect_s3_class(v, "sas_binary")
  expect_identical(unname(v$span[c("col", "end_col")]), c(36L, 42L))
  expect_identical(unname(v$rhs$span[c("col", "end_col")]), c(40L, 42L))
  expect_true(v$lhs$span[["tok"]] <= v$lhs$span[["tok_end"]])
})
