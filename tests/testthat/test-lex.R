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

test_that("an unmatched quote in a comment swallows the rest of the program", {
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

# ---- lossless token table --------------------------------------------------

# round trip and equality with the sas_lex() stream
expect_lossless <- function(code) {
  tok <- sas_lex(code, lossless = TRUE)
  expect_identical(paste0(tok$text, collapse = ""), enc2utf8(paste(code, collapse = "\n")))
  expect_identical(tokens_semantic(tok), sas_lex(code))
  invisible(tok)
}
kinds_of <- function(tok) paste(tok$kind, tok$text, sep = ":")

test_that("the lossless table keeps whitespace, line feeds and comments", {
  tok <- expect_lossless("data out; x = 1; run;")
  expect_named(tok, c("id", "kind", "text", "value", "line", "col", "end_line", "end_col"))
  expect_equal(sum(tok$kind == "ws"), 5L)
  tok <- expect_lossless("x = 1; /* a ; b */\n* stmt comment;\ny = 2 * 3;")
  expect_equal(tok$text[tok$kind == "comment"], c("/* a ; b */", "* stmt comment;"))
  expect_equal(tok$kind[tok$text == "*"], "op")
  expect_equal(tok$end_col[tok$kind == "comment"][1], 18L)
})

test_that("nested-looking comments end at the first */", {
  tok <- expect_lossless("/* a /* b */ x = 1; */")
  expect_equal(tok$text[1], "/* a /* b */")
  # the `*` after `;` starts a statement comment that runs to the end
  expect_equal(tail(kinds_of(tok), 1), "comment:*/")
})

test_that("comments inside macro code", {
  tok <- expect_lossless("%macro m;\n  %* note it''s;\n  * stmt;\n  data a; /* c */ run;\n%mend m;")
  expect_equal(tok$text[tok$kind == "comment"], c("%* note it''s;", "* stmt;", "/* c */"))
})

test_that("DATALINES and CARDS4 data round-trip", {
  tok <- expect_lossless("data a; input s $; datalines4;  \na;b\n;;;;\nrun;")
  expect_equal(tok$text[tok$kind == "datalines"], "a;b")
  expect_equal(tok$text[tok$kind == "datalines_end"], ";;;;")
  tok <- expect_lossless("data a; input s $; cards4;\nx;y\n\n;;;;\nrun;")
  expect_equal(tok$text[tok$kind == "datalines"], c("x;y", ""))
  tok <- expect_lossless("data a; input s $; cards; /* note */\nx\n  ;\nrun;")
  expect_equal(tok$text[tok$kind == "comment"], " /* note */")
  expect_equal(tok$text[tok$kind == "datalines_end"], "  ;")
  expect_lossless("data a; input s $; datalines;\nx\ny")
})

test_that("in-stream data starts on the line after DATALINES", {
  # text after the statement on its line is statements, not data: fail loud
  for (code in c("data a; input x; datalines; 3 1 2 ; run; proc rank data=a out=b; run;",
                 "data a; input x; cards; 3 1 2 ;\nrun;",
                 "data a; input x; datalines4; 3 1 2 ;\n;;;;\nrun;",
                 "data a; input x; lines; run;\n3\n;\n",
                 "data a; input x;\ndatalines; /* open\n3\n*/\n;\nrun;")) {
    e <- tryCatch(sas_lex(code), error = function(e) e)
    expect_s3_class(e, "sasonr_unsupported")
    expect_match(conditionMessage(e), "text after DATALINES on the same line", fixed = TRUE)
    tok <- sas_lex(code, lossless = TRUE)
    expect_identical(paste0(tok$text, collapse = ""), code)
    e2 <- tryCatch(tokens_semantic(tok), error = function(e) e)
    expect_identical(conditionMessage(e2), conditionMessage(e))
  }
  tok <- sas_lex("data a; input x; datalines; 3 1 2 ;", lossless = TRUE)
  expect_false(any(tok$kind == "datalines"))
  expect_equal(tok$text[tok$kind == "num"], c("3", "1", "2"))
  # blanks and closed comments on the DATALINES line; the data follow
  t <- sas_lex("data a; input x;\ndatalines; /* a */ /* b */  \n3\n1\n;\nrun;")
  expect_equal(t$value[t$type == "datalines"], c("3", "1"))
  t <- sas_lex("data a; input x;\ndatalines;\n3\n1\n;\nrun;")
  expect_equal(t$value[t$type == "datalines"], c("3", "1"))
})

test_that("quoted strings, hex and date literals keep their source text", {
  tok <- expect_lossless("t = \"c\"\"d\"; u = 'it''s;'; h = 0FFx; d = '15mar2026'd; x = 'AB'x;")
  expect_equal(tok$text[tok$kind %in% c("str", "date", "hexstr")], c("\"c\"\"d\"", "'it''s;'", "'15mar2026'd", "'AB'x"))
  expect_equal(tok$text[tok$kind == "num"], "0FFx")
  expect_equal(tok$value[tok$kind == "num"], "255")
  expect_lossless("a ~= b !! c")
})

test_that("CRLF, tabs, trailing blanks and latin1 round-trip", {
  tok <- expect_lossless("data a;\r\n\tx = 1;   \r\nrun;\r\n")
  expect_equal(tok$text[tok$kind == "newline"], rep("\r\n", 3))
  expect_equal(tok$text[tok$kind == "ws"][2], "\t")
  expect_lossless("x = 1;\r")
  expect_lossless("\r")
  expect_lossless(c("x = 1;", "y = 2;"))
  expect_lossless("")
  lat <- "x = 'caf\xe9';"
  Encoding(lat) <- "latin1"
  expect_lossless(lat)
})

test_that("inputs sas_lex() rejects still round-trip and fail in tokens_semantic()", {
  for (code in c("x = 1; /* open", "x = 'open", "x = 1 ` 2;", "x = 12345678901234x;",
                 "data a; input s $; datalines;\nc;d\n;\nrun;")) {
    tok <- sas_lex(code, lossless = TRUE)
    expect_identical(paste0(tok$text, collapse = ""), code)
    e1 <- tryCatch(sas_lex(code), error = function(e) e)
    e2 <- tryCatch(tokens_semantic(tok), error = function(e) e)
    expect_s3_class(e1, "sasonr_error")
    expect_identical(class(e2), class(e1))
    expect_identical(conditionMessage(e2), conditionMessage(e1))
  }
  tok <- sas_lex("x = 1; /* open", lossless = TRUE)
  expect_equal(tail(tok$kind, 1), "comment")
  expect_warning(tok <- sas_lex("%* it's ;\nx = 1;", lossless = TRUE), class = "sasonr_unmatched_quote_in_comment")
  expect_equal(tok$kind, "comment")
})
