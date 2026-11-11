tok <- function(code) {
  t <- sas_lex(code)
  paste(t$type, t$value, sep = ":")
}

test_that("names, numbers and operators are tokenised", {
  expect_equal(
    tok("x=1e3+.5**2;"),
    c("name:x", "op:=", "num:1e3", "op:+", "num:.5", "op:**", "num:2", "semi:;")
  )
  expect_equal(tok("a<>b><c ^= d ~= e"), c("name:a", "op:<>", "name:b", "op:><", "name:c", "op:^=", "name:d", "op:^=", "name:e"))
  expect_equal(tok("a || b !! c"), c("name:a", "op:||", "name:b", "op:||", "name:c"))
  expect_equal(tok("s =: 'AB'"), c("name:s", "op:=:", "str:AB"))
})

test_that("strings collapse doubled quotes and keep semicolons", {
  expect_equal(tok("t = \"c\"\"d\"; u = 'it''s;';"),
               c("name:t", "op:=", "str:c\"d", "semi:;", "name:u", "op:=", "str:it's;", "semi:;"))
})

test_that("typed literals carry their suffix", {
  t <- sas_lex("d='15MAR2026'd; t='10:30't; dt='01JAN2020:00:00:00'dt; n='a b'n;")
  expect_equal(t$type[c(3, 7, 11, 15)], c("date", "time", "datetime", "namelit"))
  expect_equal(t$value[3], "15MAR2026")
})

test_that("special missing values are recognised only where a value is expected", {
  expect_equal(tok("x = .A;"), c("name:x", "op:=", "missing:.A", "semi:;"))
  expect_equal(tok("if ._ then"), c("name:if", "missing:._", "name:then"))
  expect_equal(tok("first.g"), c("name:first", "dot:.", "name:g"))
  expect_equal(tok("put x date9.;"), c("name:put", "name:x", "name:date9", "dot:.", "semi:;"))
  expect_equal(tok("y = .;"), c("name:y", "op:=", "missing:.", "semi:;"))
})

test_that("comments are skipped, statement comments only at statement start", {
  expect_equal(tok("x = 1; /* ; */ * a comment; y = 2 * 3;"),
               c("name:x", "op:=", "num:1", "semi:;", "name:y", "op:=", "num:2", "op:*", "num:3", "semi:;"))
  expect_equal(tok("%* macro ; x=1;"), c("name:x", "op:=", "num:1", "semi:;"))
})

test_that("an unmatched quote in a comment swallows the rest, like SAS", {
  expect_warning(t <- sas_lex("%* it's ;\nx = 1;"), class = "sasonr_unmatched_quote_in_comment")
  expect_equal(nrow(t), 0)
})

test_that("datalines blocks are captured raw", {
  expect_error(sas_lex("data a; input s $; datalines;\na b\nc;d\n;\nrun;"), class = "sasonr_unsupported")
  t <- sas_lex("data a; input s $; datalines4;\na;b\n;;;;\nrun;")
  expect_equal(t$value[t$type == "datalines"], "a;b")
  expect_true("datalines_end" %in% t$type)
  t2 <- sas_lex("data a; input s $; cards;\nx\ny\n;\nrun;")
  expect_equal(t2$value[t2$type == "datalines"], c("x", "y"))
})

test_that("macro tokens are cut out but not expanded", {
  expect_equal(tok("%let a=1; x=&a; y=&&v&i;"),
               c("macro:%let", "name:a", "op:=", "num:1", "semi:;", "name:x", "op:=", "macro_var:&a",
                 "semi:;", "name:y", "op:=", "macro_var:&&v", "macro_var:&i", "semi:;"))
  expect_equal(sas_lex("x = \"&a\";")$type[3], "str_macro")
  expect_equal(sas_lex("x = '&a';")$type[3], "str")
})

test_that("line and column are tracked", {
  t <- sas_lex("data a;\n  x = 1;")
  expect_equal(t$line, c(1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_equal(t$col[4], 3L)
})
