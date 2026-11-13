# Round 13.1: the independent review of round 13 (one regression test per
# finding; each fails on 81624eb).
hexd <- function(h) readBin(as.raw(strtoi(substring(h, seq(1, 15, 2), seq(2, 16, 2)), 16L)), "double", size = 8L, endian = "big")

test_that("PERCENT: rounding down just below a half point fails loud again (finding 1)", {
  f <- function(x, fmt) sas_f_putn(x, fmt)
  expect_equal(trimws(f(1.0049999999999997, "PERCENT8.")), "101%") # SAS 9.4 gold
  # x * 100 lies 3 to 5 ulps below 100.5: w.d rounds it down, PERCENT is not verified there
  expect_error(f(1.005 - 2 * 2^-52, "PERCENT8."), class = "sasonr_unsupported")
  expect_false("down_at" %in% names(formals(sasonr:::fixed_body)))
  expect_false("down_at" %in% names(formals(sasonr:::fit_number)))
  expect_false("down_at" %in% names(formals(sasonr:::sci_dec_text)))
  expect_false("down_at" %in% names(formals(sasonr:::display_round_text)))
})

test_that("exact integer digits only where verified; 1E17 and beyond keep the E form (finding 2)", {
  drt <- sasonr:::display_round_text
  expect_identical(drt(939586291501115008, 0L), "939586291501115008") # SAS 9.4 gold (32.)
  expect_identical(drt(1e18, 0L), NA_character_)                     # E form, as before round 13
  expect_identical(drt(2^70, 0L), NA_character_)
  expect_error(drt(1234567890123456, 2L), class = "sasonr_unsupported")
  expect_identical(drt(123456789012345, 2L), "123456789012345.00")
  expect_identical(trimws(sas_f_putn(1e18, "32.")), "1E18")
})

test_that("BESTw. narrow E form and HOURw.d go through the display engine (finding 3)", {
  # BEST8. of a value just below the half point of its 3-digit mantissa:
  # sprintf("%.2E") rounded it silently; the engine fails loud
  expect_error(sas_f_putn(1.2345e-6, "BEST8."), class = "sasonr_unsupported") # 81624eb: 1.234E-6
  expect_identical(trimws(sas_f_putn(1.25e-6, "BEST8.")), trimws(sasonr:::sci_dec_text(1.25e-6, 2L)$text))
  # HOUR4.1 of 900 seconds (0.25 hours, an exact half) rounds as 4.1 does
  expect_identical(trimws(sas_f_putn(900, "HOUR4.1")), trimws(sas_f_putn(0.25, "4.1")))
})

test_that("SAS exception: SELECT whens and otherwise are traversed (finding 5)", {
  expect_error(sas_run("data a; select; when (1) output; otherwise; end; x = constant('EXACTINT', 1e300); run;"),
               class = "sasonr_unsupported")
  expect_error(sas_run("data a; if 0 then y = 1; else output; x = constant('EXACTINT', 1e300); run;"),
               class = "sasonr_unsupported")
  st <- sas_parse("data a; select; when (1) y = 1; otherwise y = 2; end; run;")$steps[[1]]
  expect_true(sasonr:::step_runs_once(st))
})

test_that("PROC SQL: math-error NOTEs fail loud instead of being dropped, the runtime state is restored (finding 6)", {
  expect_error(sas_run("data one; x = 0; run; proc sql; create table t as select 1 / x as y from one; quit;"),
               class = "sasonr_unsupported")
  # SAS 9.4 writes no missing-value NOTE in PROC SQL (gold semantics/sql_null)
  expect_silent(sas_run("data one; x = .; run; proc sql; create table t as select x + 1 as y from one; quit;"))
  before <- list(sasonr:::rt$mv, sasonr:::rt$math, sasonr:::rt$notes)
  try(sas_run("data one; x = 0; run; proc sql; create table t as select 1 / x as y from one; quit;", on_error = "abort"),
      silent = TRUE)
  expect_identical(list(sasonr:::rt$mv, sasonr:::rt$math, sasonr:::rt$notes), before)
})

test_that("FIRSTOBS=MIN is 1, OBS=MIN is 0 (finding 6; SAS 9.4 gold datastep/r13/firstobs_min)", {
  st <- sas_parse("data b; set a(firstobs=min); run;")
  expect_identical(st$steps[[1]]$stmts[[1]]$datasets[[1]]$options$firstobs, 1)
  r <- sas_run("data a; do i = 1 to 4; output; end; run; data b; set a(firstobs=min); run; data d; set a(obs=min); run;")
  expect_equal(nrow(r$work$b), 4L)
  expect_equal(nrow(r$work$d), 0L)
})

test_that("WHERE note: a variable compared with any non-variable is written first (finding 7)", {
  wt <- function(src) sasonr:::where_text(sas_parse_expr(src))
  expect_identical(wt("x + 1 = y"), "y=x+1")         # SAS 9.4 gold datastep/r13/where_echo_shapes
  expect_identical(wt("2 = y"), "y=2")
  expect_identical(wt("-x < y"), "y>-x")
  expect_identical(wt("abs(x) < y"), "y>ABS(x)")
  expect_identical(wt("x + 1 = abs(y)"), "x+1=ABS(y)")
  expect_identical(wt("y > x"), "y>x")
  expect_error(wt("2 = x + 1"), class = "sasonr_unsupported")
  expect_error(wt("x + 1 = 2"), class = "sasonr_unsupported")
})

test_that("WHERE: a concatenation beyond 32767 characters fails loud (finding 4, length probe gold)", {
  withr::local_options(list(sasonr.strict = FALSE))
  src <- "data a; length s $32767; s = repeat('x', 32766); r = s || '|'; run; data b; set a; where s || '|' = r; run;"
  expect_error(sas_run(src), class = "sasonr_unsupported")
})
