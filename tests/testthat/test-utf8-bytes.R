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
  for (call in c(paste0("r = ksubstr(", x, ", 2, 1)"), paste0("r = quote(", x, ")"), paste0("r = dequote(", x, ")"),
                 paste0("r = put(", x, ", $upcase8.)"), paste0("r = input(", x, ", $8.)"),
                 paste0("n = lengthm(", x, ")"))) {
    expect_error(run_a(paste0("length r $20; ", call, ";")), class = "sasonr_strict_error", info = call)
  }
  # a variable argument (row code and vectorised code)
  expect_error(run_a(paste0("s = ", x, "; t = ksubstr(s, 1, 2);")), class = "sasonr_strict_error")
  expect_error(sas_run(paste0("data a; s = ", x, "; output; run; data b; set a; if s ne '' then t = quote(s); run;")),
               class = "sasonr_error")
  # direct calls and %SYSFUNC
  expect_error(sas_f_quote(paste0(e1, "t")), class = "sasonr_strict_error")
  expect_error(sas_f_rank(paste0(e1, "t")), class = "sasonr_strict_error")
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

test_that("a colon comparison truncates to the shorter value in bytes", {
  skip_utf8()
  expect_equal(sasonr:::sas_cmp_trunc("=", "\u00e9t\u00e9", "\u00e9"), 1)
  expect_equal(sasonr:::sas_cmp_trunc("=", "\u00e9a", "\u00e0"), 0)
  # the prefix may end inside a character: 'a' C3 against 'ab'
  expect_equal(sasonr:::sas_cmp_trunc(">", "a\u00e9", "ab"), 1)
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

# Byte semantics of the character functions --------------------------------
# s is 'été à l''hôtel': é 1-2, t 3, é 4-5, à 7-8, l 10, ' 11, h 12, ô 13-14, tel 15-17.
s17 <- "été à l'hôtel"
s20 <- paste0(s17, "   ")
uni <- "Ünïcödé"          # 11 bytes
euro <- "€"                                 # 3 bytes
clef <- "\U0001d11e"                             # 4 bytes
a_ <- "à"
o_ <- "ô"
ae <- "ä"
E1 <- "É"

test_that("INDEX, INDEXW, FIND and FINDW return byte positions", {
  skip_utf8()
  expect_identical(sas_f_index(s17, a_), 7)
  expect_identical(sas_f_index(s17, paste0("h", o_, "tel")), 12)
  expect_identical(sas_f_index(uni, "ö"), 7)
  expect_identical(sas_f_index(paste0("a", euro, "b"), "b"), 5)
  expect_identical(sas_f_index(paste0("a", clef, "b"), "b"), 6)
  expect_identical(sas_f_indexw(s17, a_), 7)
  expect_identical(sas_f_indexw(s17, paste0("l'h", o_, "tel")), 10)
  expect_identical(sas_f_find(s17, paste0("h", o_, "tel")), 12)
  # the I modifier folds ASCII letters only; the start counts bytes
  expect_identical(sas_f_find(s17, "HÔTEL", "i"), 0)
  expect_identical(sas_f_find(s17, e1, 2), 4)
  expect_identical(sas_f_findw(s17, paste0("h", o_, "tel"), " '"), 12)
  expect_identical(sas_f_findw(s17, a_), 7)
  expect_identical(sas_f_findw(s17, a_, " ", "e"), 2)
})

test_that("INDEXC, FINDC and VERIFY take the character list as a set of bytes", {
  skip_utf8()
  expect_identical(sas_f_indexc(s17, o_), 1)
  expect_identical(sas_f_indexc(paste0("ab", ae), e1), 3)
  expect_identical(sas_f_findc(s17, o_), 1)
  expect_identical(sas_f_findc(s17, o_, "k"), 2)
  expect_identical(sas_f_findc(paste0("ab", ae), e1), 3)
  expect_identical(sas_f_findc(s17, e1, 2), 2)
  expect_identical(sas_f_verify(s17, paste0(e1, "t", e1)), 6)
  expect_identical(sas_f_verify(paste0(e1, "a"), e1), 3)
  expect_identical(sas_f_verify(ae, e1), 2)
  expect_identical(sas_f_verify(paste0(euro, "x"), euro), 4)
})

test_that("class modifiers recognise ASCII characters only", {
  skip_utf8()
  expect_identical(sas_f_findc(paste0("1", e1), "", "a"), 0)
  expect_identical(sas_f_findc(paste0("1", e1, "a"), "", "a"), 4)
  expect_identical(sas_f_findc(paste0("1", E1), "x", "u"), 0)
  expect_identical(sas_f_findc(paste0(e1, "1"), "x", "d"), 3)
})

test_that("ANY* and NOT* classify characters and return byte positions", {
  skip_utf8()
  expect_identical(sas_f_anyalpha(paste0("1", e1)), 2)
  expect_identical(sas_f_notalpha(paste0(e1, "a")), 0)
  expect_identical(sas_f_notalpha(paste0("a", e1, "1")), 4)
  expect_identical(sas_f_anyupper(paste0("a", E1)), 2)
  expect_identical(sas_f_anylower(paste0("1", e1)), 2)
  expect_identical(sas_f_notupper(paste0(E1, "Ü", "a")), 5)
  expect_identical(sas_f_anydigit(paste0(e1, "1")), 3)
  expect_identical(sas_f_anyalnum(paste0("-", e1)), 2)
  expect_identical(sas_f_notalnum(paste0(e1, "-")), 3)
  expect_identical(sas_f_anypunct(paste0("a", euro)), 0)
  expect_identical(sas_f_anyspace(paste0(e1, " ", e1)), 3)
  expect_identical(sas_f_anypunct(paste0("a", clef)), 0)
  expect_identical(sas_f_notalpha(paste0("a", clef)), 2)
})

test_that("the start of ANY* and NOT* counts bytes", {
  skip_utf8()
  expect_identical(sas_f_anyalpha(paste0(e1, e1, "1a"), 3), 3)
  # a start on a later byte of a character starts at the next character
  expect_identical(sas_f_anyalpha(paste0(e1, "1"), 2), 0)
  expect_identical(sas_f_anyalpha(paste0("1", e1, "2", e1), 3), 5)
  expect_identical(sas_f_notalpha(paste0(e1, e1, e1, "1"), 2), 7)
  expect_identical(sas_f_notalpha(paste0(e1, e1, "1a"), 3), 5)
  expect_identical(sas_f_anyupper(paste0(E1, E1, "aA"), 3), 3)
  # backward: the character that holds the start byte
  expect_identical(sas_f_anyalpha(paste0("a1", e1, e1), -4), 3)
  expect_identical(sas_f_anyalpha(paste0("12345", e1), -6), 6)
  # characters whose class is not known fail loud
  expect_error(sas_f_anyalpha("中"), class = "sasonr_unsupported")
})

test_that("COUNT, COUNTC and COUNTW count bytes", {
  skip_utf8()
  expect_identical(sas_f_count(s17, e1), 2)
  expect_identical(sas_f_count(strrep(e1, 3), paste0(e1, e1)), 1)
  expect_identical(sas_f_count(s17, E1, "i"), 0)
  expect_identical(sas_f_countc(s17, e1), 6)
  expect_identical(sas_f_countc(paste0("a", ae), e1), 1)
  expect_identical(sas_f_countc(s17, "", "a"), 6)
  expect_identical(sas_f_countc(s17, NULL, "a"), 6)
  expect_identical(sas_f_countc("ab1", NULL, "a"), 2)
  expect_identical(sas_f_countc(paste0("a", euro, "b"), euro), 3)
  expect_identical(sas_f_countc(s20, e1, "v"), 14)
  expect_identical(sas_f_countw(s17), 3)
  expect_identical(sas_f_countw(paste0("a", euro, "b", euro, "c"), euro), 3)
  expect_identical(sas_f_countw(s17, e1), 4)
})

test_that("SCAN takes the delimiters as a set of bytes", {
  skip_utf8()
  expect_identical(sas_f_scan(s17, 2, "'"), paste0("h", o_, "tel"))
  expect_identical(sas_f_scan(s17, 2), a_)
  expect_identical(sas_f_scan(s17, -1), paste0("l'h", o_, "tel"))
  expect_identical(sas_f_scan(s17, 1, e1), "t")
  expect_identical(sas_f_scan(paste0("a", euro, "b", euro, "c"), 2, euro), "b")
  # a word that starts with a later byte of a character (AF 63)
  expect_error(sas_f_scan(uni, 2, "ö"), class = "sasonr_unsupported")
})

test_that("SUBSTRN counts bytes; a cut inside a character fails loud", {
  skip_utf8()
  expect_identical(hex(sas_f_substrn(s17, 4, 3)), "C3A920")
  expect_identical(sas_f_substrn(s17, 0, 3), e1)
  expect_identical(sas_f_substrn(s17, 15, 10), "tel")
  expect_error(sas_f_substrn(s17, 2, 1), class = "sasonr_unsupported")
  expect_error(sas_f_substrn(paste0("a", euro, "b"), 2, 2), class = "sasonr_unsupported")
})

test_that("LEFT, RIGHT, TRIM, STRIP and COMPBL pad and trim by bytes", {
  skip_utf8()
  x <- paste0("  ", e1, e1, "  ")
  expect_identical(hex(sas_f_left(x)), "C3A9C3A920202020")
  expect_identical(hex(sas_f_right(x)), "20202020C3A9C3A9")
  expect_identical(hex(sas_f_trim(x)), "2020C3A9C3A9")
  expect_identical(sas_f_strip(x), paste0(e1, e1))
  expect_identical(sas_f_compbl(paste0(e1, "  ", a_, "   l")), paste0(e1, " ", a_, " l"))
})

test_that("COMPRESS and TRANSLATE work on bytes; a broken character fails loud", {
  skip_utf8()
  expect_identical(sas_f_compress(s17), paste0(e1, "t", e1, a_, "l'h", o_, "tel"))
  expect_identical(hex(sas_f_compress(s17, NULL, "a")), "C3A9C3A920C3A02027C3B4")
  expect_identical(sas_f_compress(s17, NULL, "ka"), "tlhtel")
  expect_identical(sas_f_compress(paste0("a", euro, "b"), euro), "ab")
  expect_identical(sas_f_compress(s17, NULL, "p"), paste0(e1, "t", e1, " ", a_, " lh", o_, "tel"))
  expect_error(sas_f_compress(s17, e1), class = "sasonr_unsupported")
  expect_error(sas_f_compress(paste0("a", ae), e1), class = "sasonr_unsupported")
  expect_error(sas_f_translate(s17, "x", e1), class = "sasonr_unsupported")
  expect_error(sas_f_translate(paste0("a", ae), "b", e1), class = "sasonr_unsupported")
})

test_that("TRANWRD and TRANSTRN replace byte strings", {
  skip_utf8()
  expect_identical(sas_f_tranwrd(s17, e1, "e"), paste0("ete ", a_, " l'h", o_, "tel"))
  expect_identical(sas_f_tranwrd(s17, paste0("h", o_, "tel"), "inn"), paste0(e1, "t", e1, " ", a_, " l'inn"))
  expect_identical(sas_f_transtrn(s17, a_, euro), paste0(e1, "t", e1, " ", euro, " l'h", o_, "tel"))
})

test_that("REVERSE fails loud and REPEAT repeats bytes", {
  skip_utf8()
  expect_error(sas_f_reverse(paste0("a", e1)), class = "sasonr_unsupported")
  expect_identical(sas_f_repeat(e1, 2), strrep(e1, 3))
  expect_identical(sas_f_repeat(euro, 1), strrep(euro, 2))
})

test_that("CAT, CATS, CATT and CATX count bytes", {
  skip_utf8()
  d <- run_a(paste0("r1 = cat('", e1, "', 'a'); r2 = cats(' ", e1, " ', '", a_, " '); r3 = catt('", e1, " ', ' ", a_,
                    "'); r4 = catx('-', '", e1, "', '", euro, "', '", clef, "'); n4 = lengthn(r4);"))
  expect_identical(c(d$r1, d$r2, d$r3, d$r4), c(paste0(e1, "a"), paste0(e1, a_), paste0(e1, " ", a_),
                                               paste0(e1, "-", euro, "-", clef)))
  expect_identical(d$n4, 11)
})

test_that("a CAT result cut inside a character is blank, with the buffer WARNING", {
  skip_utf8()
  r <- sas_run(paste0("data a; length c4 x4 $4; c4 = cats('a', '", e1, "', '", e1, "'); x4 = catx('-', '", e1, "', '", e1,
                      "'); e = _error_; run;"))
  d <- r$work$a
  expect_identical(c(d$c4, d$x4), c("", ""))
  expect_identical(d$e, 1)
  log <- msgs(r)
  expect_true(any(grepl("WARNING: In a call to the CATS function, the buffer allocated", log, fixed = TRUE)))
  expect_true(any(grepl("The correct result would contain 5 characters", log, fixed = TRUE)))
  expect_true(any(grepl(paste0("NOTE: Argument 3 to function CATS('a','", e1, "','", e1, "')"), log, fixed = TRUE)))
  expect_true(any(grepl(paste0("NOTE: Argument 3 to function CATX('-','", e1, "','", e1, "')"), log, fixed = TRUE)))
  # a result longer than the target that ends between characters: not supported
  expect_error(sas_run(paste0("data a; length c4 $4; c4 = cats('", e1, "', '", e1, "', 'a'); run;")),
               class = "sasonr_unsupported")
})

test_that("$w., $CHARw. and $QUOTEw. count bytes; a cut inside a character fails loud", {
  skip_utf8()
  d <- run_a(paste0("length x $11 y $8; x = '", uni, "'; y = 'l''h", o_, "tel';",
                    " f1 = put(x, $3.); f6 = put(x, $char11.); f7 = put(x, $quote8.); f8 = put(x, $quote13.);",
                    " h9 = put(put(y, $quote12.), $hex24.); f15 = put('", e1, e1, "', $5.);"))
  expect_identical(d$f1, "Ün")
  expect_identical(d$f6, uni)
  expect_identical(d$f7, "\"Ünïc\"")
  expect_identical(d$f8, paste0("\"", uni, "\""))
  expect_identical(d$h9, "226C2768C3B474656C222020")
  expect_identical(d$f15, paste0(e1, e1))
  for (f in c("$4.", "$char4.", "$1.")) {
    expect_error(run_a(paste0("length r $11; r = put('", uni, "', ", f, ");")), class = "sasonr_unsupported", info = f)
  }
  expect_error(run_a(paste0("length r $5; r = put('", e1, e1, "', $quote5.);")), class = "sasonr_unsupported")
  # $UPCASEw. stays outside the exact domain
  expect_error(run_a(paste0("length r $11; r = put('", uni, "', $upcase11.);")), class = "sasonr_strict_error")
})

test_that("the byte functions stay outside the exact domain in %SYSFUNC and PROC SQL", {
  skip_utf8()
  expect_error(sas_run(paste0("%put %sysfunc(index(", e1, "t, t));")), class = "sasonr_strict_error")
  expect_error(sas_run(paste0("data a; s = '", e1, "t'; run; proc sql; create table b as select index(s, 't') as n from a; quit;")),
               class = "sasonr_error")
})
