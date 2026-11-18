# Character storage and the LENGTH functions in a UTF-8 session: lengths count
# bytes. Non-ASCII text is written with \u escapes.

skip_utf8 <- function() skip_if_not(isTRUE(l10n_info()[["UTF-8"]]))
hex <- function(x) paste(toupper(as.character(charToRaw(x))), collapse = "")
e1 <- "\u00e9"                                    # one character, two bytes
hotel <- "\u00e9t\u00e9 \u00e0 l''h\u00f4tel"     # SAS literal text (doubled quote)

run_a <- function(code) sas_run(paste0("data a; ", code, " run;"))$work$a
# the stored bytes of s (the WORK data sets returned by sas_run() are trimmed)
stored <- function(len, value) {
  run_a(paste0("length s $", len, " h $", 2 * len, "; s = ", value, "; h = put(s, $hex", 2 * len, ".);"))$h
}

test_that("a $n variable stores the first n bytes, padded with blanks", {
  skip_utf8()
  expect_identical(stored(5, paste0("'", e1, e1, "'")), "C3A9C3A920")
  expect_identical(stored(6, paste0("'", e1, "'")), "C3A920202020")
  expect_identical(hex(sas_char(e1, 6L)), "C3A920202020")
  expect_identical(hex(sas_char(paste0(e1, e1, e1), 4L)), "C3A9C3A9")
})

test_that("a cut inside a multi-byte character fails loud", {
  skip_utf8()
  expect_error(run_a(paste0("length s $3; s = '", e1, e1, "';")), class = "sasonr_unsupported")
  ete <- paste0(e1, "t", e1)
  expect_error(run_a(paste0("length s $1; s = '", ete, "';")), class = "sasonr_unsupported")
  expect_error(run_a(paste0("length s $4; s = '", ete, "';")), class = "sasonr_unsupported")
  # from a longer variable: the cut happens at the assignment
  expect_error(run_a(paste0("length s $5 t $3; s = '", e1, e1, "'; t = s;")), class = "sasonr_unsupported")
  expect_error(sas_char(paste0(e1, e1), 3L), class = "sasonr_unsupported")
})

test_that("a literal takes its length in bytes", {
  skip_utf8()
  d <- run_a(paste0("s = '", e1, "'; n = lengthc(s);"))
  expect_identical(d$n, 2)
  expect_identical(hex(d$s), "C3A9")
})

test_that("LENGTH, LENGTHN and LENGTHC count bytes; KLENGTH counts characters", {
  skip_utf8()
  d <- run_a(paste0("length s $20; s = '", hotel, "'; l = length(s); n = lengthn(s); c = lengthc(s); k = klength(s);"))
  expect_identical(c(d$l, d$n, d$c, d$k), c(17, 17, 20, 13))
  expect_identical(sas_f_length(paste0(e1, "  ")), 2)
  expect_identical(sas_f_lengthn(e1), 2)
  expect_identical(sas_f_lengthc(paste0(e1, " ")), 3)
  expect_identical(sas_f_klength(paste0(e1, "t", e1, "  ")), 3)
  # blank values: LENGTH 1, LENGTHN 0
  expect_identical(c(sas_f_length(" "), sas_f_lengthn(" ")), c(1, 0))
})

test_that("KLENGTH of a blank value fails loud", {
  expect_error(sas_f_klength("   "), class = "sasonr_unsupported")
  expect_error(run_a("s = ' '; k = klength(s);"), class = "sasonr_unsupported")
})

test_that("KLENGTHN and KLENGTHC are unknown functions", {
  expect_error(sas_translate("data _null_; x = klengthn('a'); run;"), class = "sasonr_syntax_error")
  expect_error(sas_translate("data _null_; x = klengthc('a'); run;"), class = "sasonr_syntax_error")
  r <- sas_run("data _null_; x = klengthn('a'); run;", on_error = "sas")
  expect_true(any(grepl("ERROR 68-185: The function KLENGTHN is unknown", sas_log(r), fixed = TRUE)))
})

test_that("comparisons ignore trailing blanks", {
  skip_utf8()
  d <- run_a(paste0("length s $6; s = '", e1, "'; eq = (s = '", e1, "  ');"))
  expect_identical(d$eq, 1)
})

test_that("other character functions fail loud on an argument outside ASCII", {
  skip_utf8()
  x <- paste0("'", e1, "t", e1, "'")
  for (call in c(paste0("r = substr(", x, ", 2, 1)"), paste0("n = index(", x, ", 't')"), paste0("r = scan(", x, ", 1)"),
                 paste0("r = reverse(", x, ")"), paste0("r = put(", x, ", $8.)"), paste0("r = input(", x, ", $8.)"),
                 paste0("n = lengthm(", x, ")"))) {
    expect_error(run_a(paste0("length r $20; ", call, ";")), class = "sasonr_strict_error", info = call)
  }
  # a variable argument (row code and vectorised code)
  expect_error(run_a(paste0("s = ", x, "; t = substr(s, 1, 2);")), class = "sasonr_strict_error")
  expect_error(sas_run(paste0("data a; s = ", x, "; output; run; data b; set a; if s ne '' then t = left(s); run;")),
               class = "sasonr_error")
  # direct calls and %SYSFUNC
  expect_error(sas_f_reverse(paste0(e1, "t")), class = "sasonr_strict_error")
  expect_error(sas_f_index(paste0(e1, "t"), "t"), class = "sasonr_strict_error")
  expect_error(sas_run(paste0("%put %sysfunc(reverse(", e1, "t));")), class = "sasonr_strict_error")
  expect_error(run_a(paste0("call symputx('m', ", x, ");")), class = "sasonr_strict_error")
  # the case functions and the lengths stay available
  expect_identical(sas_f_upcase(e1), "\u00c9")
})

test_that("INPUT statements from records with bytes outside ASCII fail loud", {
  skip_utf8()
  expect_error(sas_run(paste0("data a; input s $; datalines;\n", e1, "\n;\nrun;")), class = "sasonr_unsupported")
})

test_that("ASCII values are unchanged", {
  expect_identical(sas_char("abc", 5L), "abc  ")
  expect_identical(sas_char("abcdef", 3L), "abc")
  expect_identical(sas_char(c("a", NA), 2L), c("a ", "  "))
  d <- run_a("length s $3; s = 'abcdef'; l = length(s); c = lengthc(s); r = reverse(s);")
  expect_identical(c(d$s, d$r), c("abc", "cba"))
  expect_identical(c(d$l, d$c), c(3, 3))
  expect_identical(sas_f_klength("ab "), 2)
})

test_that("SET into a shorter declared length cuts by bytes", {
  skip_utf8()
  ee <- paste0("'", e1, e1, "'")
  src <- paste0("data a; length s $4; s = ", ee, "; run; ")
  r <- sas_run(paste0(src, "data b; length s $2; set a; h = put(s, $hex4.); run;"))
  expect_identical(r$work$b$h, "C3A9")
  expect_error(sas_run(paste0(src, "data b; length s $3; set a; run;")), class = "sasonr_unsupported")
  # a later data set of the SET statement
  src2 <- paste0("data a1; length s $3; s = 'abc'; run; ", src)
  expect_error(sas_run(paste0(src2, "data b; set a1 a; run;")), class = "sasonr_unsupported")
})

test_that("PROC SQL INSERT, UPDATE and ALTER cut by bytes", {
  skip_utf8()
  ee <- paste0("'", e1, e1, "'")
  r <- sas_run(paste0("proc sql; create table t (s char(2)); insert into t values(", ee, "); quit; ",
                      "data u; set t; h = put(s, $hex4.); run;"))
  expect_identical(r$work$u$h, "C3A9")
  expect_error(sas_run(paste0("proc sql; create table t (s char(3)); insert into t values(", ee, "); quit;")),
               class = "sasonr_unsupported")
  expect_error(sas_run(paste0("proc sql; create table t (s char(3)); insert into t values('a'); ",
                              "update t set s = ", ee, "; quit;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste0("proc sql; create table t (s char(4)); insert into t values(", ee, "); ",
                              "alter table t modify s char(3); quit;")), class = "sasonr_unsupported")
})

test_that("a user format label outside ASCII fails loud", {
  skip_utf8()
  expect_error(sas_run(paste0("proc format; value $f 'a' = '", e1, "t", e1, "'; run; ",
                              "data a; length r $10; r = put('a', $f3.); run;")), class = "sasonr_strict_error")
})
