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

test_that("KLENGTH of a blank value is 0", {
  expect_identical(sas_f_klength(c("", " ", "   ")), c(0, 0, 0))
  d <- run_a("length s $5; s = ' '; k = klength(s); e = klength(''); b = klength(' ');")
  expect_identical(c(d$k, d$e, d$b), c(0, 0, 0))
})

test_that("KLENGTH of a numeric value converts it with BEST12.", {
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run("data a;\n  k1 = klength(123); k2 = klength(3.14159); k3 = klength(-42.5); k4 = klength(.);\nrun;")
  d <- r$work$a
  expect_identical(c(d$k1, d$k2, d$k3, d$k4), c(12, 12, 12, 12))
  m <- msgs(r)
  k <- which(startsWith(m, "NOTE: Numeric values have been converted to character values"))
  expect_length(k, 1L)
  expect_match(m[k + 1L], "^ +2:")
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
  for (call in c(paste0("r = ksubstr(", x, ", 2, 1)"), paste0("n = index(", x, ", 't')"), paste0("r = scan(", x, ", 1)"),
                 paste0("r = reverse(", x, ")"), paste0("r = put(", x, ", $8.)"), paste0("r = input(", x, ", $8.)"),
                 paste0("n = lengthm(", x, ")"))) {
    expect_error(run_a(paste0("length r $20; ", call, ";")), class = "sasonr_strict_error", info = call)
  }
  # a variable argument (row code and vectorised code)
  expect_error(run_a(paste0("s = ", x, "; t = ksubstr(s, 1, 2);")), class = "sasonr_strict_error")
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

test_that("a colon comparison of values outside ASCII fails loud", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]))
  expect_error(sasonr:::sas_cmp_trunc("=", "été", "é"), class = "sasonr_unsupported")
  expect_equal(sasonr:::sas_cmp_trunc("=", "abc", "ab"), 1)
})

test_that("SUBSTR counts positions and lengths in bytes", {
  skip_utf8()
  lit <- paste0("'", hotel, "'")
  d <- run_a(paste0("length s $20; s = ", lit, "; t = substr(s, 1, 3); h = put(t, $hex6.); v = put(s, $hex34.);"))
  expect_identical(d$v, "C3A974C3A920C3A0206C2768C3B474656C")
  expect_identical(d$h, "C3A974")
  expect_identical(d$t, paste0(e1, "t"))
  # the rest from byte 4; a length running past the end: the rest, NOTE and _ERROR_
  expect_identical(sas_f_substr(paste0(e1, "t", e1), 4), e1)
  r <- sas_run(paste0("data a; length s $5; s = '", e1, "t", e1, "'; t = substr(s, 4, 9); run;"))
  expect_identical(r$work$a$t, e1)
  expect_true(any(grepl("NOTE: Invalid third argument to function SUBSTR", sas_log(r), fixed = TRUE)))
  # a position beyond the last byte: blank, NOTE and _ERROR_
  r <- sas_run(paste0("data a; s = '", e1, "t'; t = substr(s, 4); run;"))
  expect_identical(r$work$a$t, "")
  expect_true(any(grepl("NOTE: Invalid second argument to function SUBSTR", sas_log(r), fixed = TRUE)))
  # the result takes the length of the source (bytes)
  d <- run_a(paste0("s = '", e1, "t'; t = substr(s, 3, 1); n = lengthc(t);"))
  expect_identical(d$n, 3)
})

test_that("SUBSTR with a start or end inside a multi-byte character fails loud", {
  skip_utf8()
  lit <- paste0("'", e1, "t", e1, "'")
  expect_error(run_a(paste0("s = ", lit, "; t = substr(s, 2);")), class = "sasonr_unsupported")
  expect_error(run_a(paste0("s = ", lit, "; t = substr(s, 1, 1);")), class = "sasonr_unsupported")
  expect_error(sas_f_substr(paste0(e1, "t", e1), 3, 2), class = "sasonr_unsupported")
})

test_that("SUBSTR outside a DATA step and on the left of = fail loud outside ASCII", {
  skip_utf8()
  lit <- paste0("'", e1, "t'")
  expect_error(run_a(paste0("length s $4; s = 'abcd'; substr(s, 1, 2) = ", lit, ";")), class = "sasonr_unsupported")
  expect_error(run_a(paste0("length s $4; s = ", lit, "; substr(s, 4, 1) = 'x';")), class = "sasonr_unsupported")
  expect_error(sas_run(paste0("%put %sysfunc(substr(", e1, "t, 1, 2));")), class = "sasonr_strict_error")
  expect_error(sas_run(paste0("data a; s = ", lit, "; run; proc sql; create table b as select substr(s, 1, 2) as t from a; quit;")),
               class = "sasonr_error")
})

test_that("KSUBSTR counts characters", {
  skip_utf8()
  # the computation in characters (a value outside ASCII is not supported yet)
  ks <- sasonr:::fn_layer_impl[["ksubstr"]]
  expect_identical(hex(ks(hotel, 1, 3)), "C3A974C3A9")
  expect_error(run_a(paste0("s = '", hotel, "'; t = ksubstr(s, 1, 3);")), class = "sasonr_strict_error")
  expect_error(sas_f_ksubstr(hotel, 1, 3), class = "sasonr_strict_error")
  expect_error(ks(hotel, 20, 1), class = "sasonr_unsupported")
  expect_error(ks(hotel, 1, 20), class = "sasonr_unsupported")
})

test_that("KSUBSTR invalid positions and lengths", {
  ks_log <- function(expr) {
    r <- sas_run(paste0("data a; length r $6; r = ", expr, "; e = _error_; run;"))
    list(r = r$work$a$r, e = r$work$a$e, log = sas_log(r))
  }
  expect_identical(sas_f_ksubstr("abcdef", 2, 3), "bcd")
  # a length past the end: the rest, no NOTE
  x <- ks_log("ksubstr('abc', 2, 5)")
  expect_identical(x$r, "bc")
  expect_identical(x$e, 0)
  expect_false(any(grepl("KSUBSTR", x$log, fixed = TRUE)))
  # an invalid position
  x <- ks_log("ksubstr('abc', 4)")
  expect_identical(x$r, "")
  expect_identical(x$e, 1)
  expect_true(any(grepl("NOTE: Argument 2 to function KSUBSTR('abc',4) at line 1 column", x$log, fixed = TRUE)))
  # a missing length: blank (SUBSTR returns the rest)
  x <- ks_log("ksubstr('abcdef', 2, .)")
  expect_identical(x$r, "")
  expect_true(any(grepl("NOTE: Argument 3 to function KSUBSTR('abcdef',2,.) at line 1 column", x$log, fixed = TRUE)))
  # a nonpositive length: ERROR and NOTE
  x <- ks_log("ksubstr('abcdef', 2, 0)")
  expect_identical(x$r, "")
  expect_identical(x$e, 1)
  expect_true(any(grepl("ERROR: Argument three to KSUBSTR is invalid. The value must be greater than zero.", x$log, fixed = TRUE)))
  # a huge argument
  x <- ks_log("ksubstr('abcdef', 1e300, 3)")
  expect_identical(x$r, "")
  expect_true(any(grepl("NOTE: Invalid argument to function KSUBSTR('abcdef',1E300,3) at line 1 column", x$log, fixed = TRUE)))
  # undetermined: fails loud
  expect_error(sas_run("data a; r = ksubstr('abcdef', 1e6, 3); run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; r = ksubstr('abcdef', 2, 0.5); run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; r = ksubstr('abcdef', 0, 0); run;"), class = "sasonr_unsupported")
  # ASCII SUBSTR is unchanged
  r <- sas_run("data a; length s $6; s = 'abcdef'; u = substr(s, 2, 9); run;")
  expect_identical(r$work$a$u, "bcdef")
  expect_true(any(grepl("NOTE: Invalid third argument to function SUBSTR", sas_log(r), fixed = TRUE)))
})
