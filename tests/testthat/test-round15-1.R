# Round 15.1: the independent review of round 15 (review_081e9ac.md; one
# regression test per finding, each fails on 081e9ac).

test_that("w.d integer zeros: only the verified domain and the gold points (finding 1)", {
  f <- function(x, fmt) trimws(sas_f_putn(x, fmt))
  # 081e9ac wrote 1234567890123456.0000 etc. silently: 16 digits with 4 decimals is not verified
  expect_error(sas_f_putn(1234567890123456, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "DOLLAR32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1234567890123456, "Z32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(939586291501115008, "32.4"), class = "sasonr_unsupported")
  expect_error(sas_f_putn(-1e15, "COMMA32.4"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; x = 1234567890123456; put x 32.4; run;"), class = "sasonr_unsupported")
  # the gold points
  expect_identical(f(1e15, "COMMA32.4"), "1,000,000,000,000,000.0000")   # SAS 9.4 (fmt_matrix COMMA_ec3f12)
  expect_identical(f(1e15, "DOLLAR32.4"), "$1,000,000,000,000,000.0000") # SAS 9.4 (fmt_matrix DOLLAR_ef9d86)
  expect_identical(f(939586291501115008, "32.15"), "939586291501115008.0000000000000")  # SAS 9.4 (wd_digits D0172)
  expect_identical(f(26745296926266265600, "32.20"), "26745296926266265600.00000000000") # SAS 9.4 (wd_digits D0186)
  # verified domain: below 1E15 any decimals, 3 decimals below 2^53
  expect_identical(f(123456789012345, "32.10"), "123456789012345.0000000000")
  expect_identical(f(1234567890123456, "32.3"), "1234567890123456.000")
})

test_that("ROUNDE and ROUNDZ fail loud from |x/unit| = 2^49 as ROUND does (finding 2)", {
  # SAS 9.4: ROUND(507561523905906.125, 0.25) = 507561523905906 (not the nearest multiple)
  expect_error(sas_f_round(507561523905906.125, 0.25), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(507561523905906.125, 0.25), class = "sasonr_unsupported") # 081e9ac: silent
  expect_error(sas_f_roundz(507561523905906.125, 0.25), class = "sasonr_unsupported") # 081e9ac: silent
  expect_error(sas_run("data _null_; y = rounde(507561523905906.125, 0.25); run;"), class = "sasonr_unsupported")
})

test_that("integers from 1E18 to 1E21 without decimals: gold point only (finding 3)", {
  drt <- sasonr:::display_round_text
  expect_error(drt(1e18, 0L), class = "sasonr_unsupported")               # 081e9ac: "1000000000000000000"
  expect_error(sas_f_putn(1e18, "32."), class = "sasonr_unsupported")
  expect_identical(trimws(sas_f_putn(26745296926266265600, "32.")), "26745296926266265600") # SAS 9.4 (D0186)
  expect_identical(drt(939586291501115008, 0L), "939586291501115008")     # SAS 9.4 (D0172)
  expect_identical(trimws(sas_f_putn(1e18, "8.")), "1E18")                # no fixed form fits: E notation
})

test_that("ROUNDE: the refuted window fails loud from 1.1E-12 |q| > 1E-6 (finding 4)", {
  # 1.5E-6 below 1000000.5: outside the old 0.8-1.25 W band, 081e9ac rounded it down silently
  expect_error(sas_f_rounde(1000000.5 - 1.5e-6, 1), class = "sasonr_unsupported")
  expect_error(sas_f_rounde(1000000.5 - 1e-9, 1), class = "sasonr_unsupported")
  expect_equal(sas_f_rounde(1000.5 - 1.5e-6, 1), 1000) # small |q|: unchanged rule
  expect_equal(sas_f_rounde(1000000.5, 1), 1000000)    # exact tie: even
})

test_that("wide BEST E form: BEST16.-BEST19. and k = 13 fail loud outside the gold (owner decision)", {
  # 081e9ac wrote 1.2345678901234E-6 (18 columns) in BEST16.; the width-derived
  # k (e94d5a0) has no gold: fail loud until gold exists
  expect_error(sas_f_putn(1.2345678901234567e-6, "BEST16."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(-1.2345678901234567e-6, "BEST16."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1.23456789e-20, "BEST19."), class = "sasonr_unsupported")
  expect_error(sas_f_putn(1.2345e-7, "BEST17."), class = "sasonr_unsupported")
  # k = 13 at BEST20. (-1.5E-12: 20 - 1 - 3 - 1 - 2 = 13 mantissa decimals)
  expect_error(sas_f_putn(-1.2345678901234567e-12, "BEST20."), class = "sasonr_unsupported")
  # TS1M7 gold fmt_b07 (BEST16.): mantissas of at most 4 significant digits
  expect_identical(sas_f_putn(c(-1e15, 1e16, -1e16, 1e20, -1e20, 1.234e-7, -1.234e-7, 1e-10, -1e-10), "BEST16."),
                   formatC(c("-1E15", "1E16", "-1E16", "1E20", "-1E20", "1.234E-7", "-1.234E-7", "1E-10", "-1E-10"), width = 16))
  # BEST32. (k >= 14) unchanged
  expect_identical(trimws(sas_f_putn(1.2345678901234567e-6, "BEST32.")), "1.2345678901234E-6")
})

test_that("an exact tie at the 15th digit fails loud when it decides the E digits (finding 6)", {
  # 123456789012345: 1.2345678901234|5 exactly; E19. (12 decimals) depends on the tie
  expect_error(sas_f_putn(123456789012345, "E19."), class = "sasonr_unsupported") # 081e9ac: 1.234567890123E+14
  expect_error(sasonr:::sci_dec_text(123456789012345, 12L), class = "sasonr_unsupported")
  # 2^-21 = 4.76837158203125E-7: a tie whose direction does not change E10.
  expect_identical(trimws(sas_f_putn(2^-21, "E10.")), "4.768E-07")
})

test_that("ROUND above 2^49: only exact multiples are exempt (finding 7)", {
  # x integer, unit 0.3, x/unit not an integer: 081e9ac exempted it (no gold)
  expect_error(sas_f_round(600000000000001, 0.3), class = "sasonr_unsupported")
  expect_equal(sas_f_round(123456789012.5, 1e-4), 123456789012.5) # x/unit an integer: SAS 9.4 (round_b01)
})

test_that("a SELECT branch SAS does not run cannot fail loud (vectorised code re-runs row by row)", {
  # rounding/values/round_b01 R00269: ROUND(99999.45, 0.1) is 99999.5 (SAS 9.4); the
  # vectorised SELECT also evaluated ROUNDE(99999.45, 0.1), which fails loud since finding 4
  code <- "data _null_; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; put r; datalines;
ROUND 99999.45 0.1
ROUNDE 1 1
;
"
  res <- sas_run(code)
  expect_identical(res$log[res$log %in% c("99999.5", "1")], c("99999.5", "1"))
  expect_error(sas_run(sub("ROUND 99999", "ROUNDE 99999", code)), class = "sasonr_unsupported")
})

# Review of 1da6f1f (vectorised step re-run row by row) ------------------------

test_that("the vectorised attempt leaves no PRXPARSE ids behind (review of 1da6f1f, a)", {
  code <- "data a; input f :$8. x u; id = prxparse('/a/'); select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;
ROUND 99999.45 0.1
ROUND 1 1
;
"
  res <- sas_run(code)
  # the row-by-row run alone: ids 1, 2 (the failed vectorised attempt used 1)
  expect_identical(res$work$a$id, c(1, 2))
  expect_identical(res$work$a$r, c(99999.5, 1))
})

test_that("the vectorised attempt leaves no WORK data, log lines or _ERROR_ behind (review of 1da6f1f, a)", {
  code <- "data a; x = 1; run;
data a; input f :$8. x u; y = 1 / x; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; put f= r=; datalines;
ROUND 99999.45 0.1
ROUND 0 1
;
"
  res <- sas_run(code)
  expect_identical(res$work$a$r, c(99999.5, 0))
  # one PUT line per row, one division-by-zero NOTE and PDV dump (row 2 only)
  expect_identical(sum(grepl("^f=ROUND r=", res$log)), 2L)
  expect_identical(sum(grepl("^NOTE: Division by zero detected", res$log)), 1L)
  expect_identical(sum(grepl("_ERROR_=1", res$log)), 1L)
  expect_false(any(grepl("_ERROR_=1", res$log[seq_len(match("f=ROUND r=99999.5", res$log))])))
})

test_that("an unsupported value in a branch SAS runs still fails loud at L3 (review of 1da6f1f, b)", {
  code <- "data a; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;
ROUND 1 1
ROUNDE 99999.45 0.1
;
"
  expect_error(sas_run(code), class = "sasonr_unsupported")
})

test_that("a type-level unsupported construct fails loud and is not re-run (review of 1da6f1f, c)", {
  # 1da6f1f: a character IF condition in a branch that does not run was
  # re-run row by row and ran without the conversion NOTE SAS writes
  expect_error(sas_run("data a; input x c $; if x = 1 then y = 1; else if c then y = 2; datalines;
1 a
;
"), class = "sasonr_unsupported")
  expect_error(sas_run("data a; input x c $; if c; datalines;
1 a
;
"), class = "sasonr_unsupported")
  # the run-time safety net is not re-run either
  e <- tryCatch(sasonr:::sas_true("a"), error = identity)
  expect_s3_class(e, "sasonr_unsupported_construct")
  e <- tryCatch(sasonr:::sas_cmp("a", 1), error = identity)
  expect_s3_class(e, "sasonr_unsupported_construct")
  # a translation-time unsupported construct in a vectorised step: loud
  expect_error(sas_run("data a; x = 1; if x = 2 then y = _infile_; run;"), class = "sasonr_unsupported")
})

test_that("a SAS exception in a branch that does not run is not reported (review of 1da6f1f)", {
  # vectorised code evaluated CONSTANT('EXACTINT', 1E300) for x = 1 and stopped
  # the step with the SAS exception block; SAS runs the ELSE branch
  res <- sas_run("data a; x = 1; if x = 2 then y = constant('EXACTINT', 1E300); else y = 3; run;")
  expect_identical(res$work$a$y, 3)
  expect_false(any(grepl("exception", res$log)))
  # taken: the exception (row-by-row run) as before
  res <- sas_run("data a; x = 2; if x = 2 then y = constant('EXACTINT', 1E300); else y = 3; run;")
  expect_true(any(grepl("An exception has been encountered", res$log)))
  expect_identical(nrow(res$work$a), 0L)
})

test_that("a step that always falls back runs once vectorised and once row by row (review of 1da6f1f, d)", {
  n <- 0L
  orig <- sasonr:::engine_eval
  local_mocked_bindings(engine_eval = function(...) {
    n <<- n + 1L
    orig(...)
  })
  rows <- paste(rep("ROUND 99999.45 0.1", 200), collapse = "\n")
  res <- sas_run(paste0("data a; input f :$8. x u; select (f); when ('ROUND') r = round(x, u);
    when ('ROUNDE') r = rounde(x, u); otherwise r = -1; end; datalines;\n", rows, "\n;\n"))
  expect_identical(n, 2L)
  expect_identical(unique(res$work$a$r), 99999.5)
})
