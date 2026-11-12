level_of <- function(code, work = list()) {
  vapply(sas_translate(code, work)$steps, `[[`, "", "level")
}
dat <- list(a = data.frame(g = c("A", "A", "B"), v = c(1, 2, 3)))

test_that("pure row-wise steps are L1", {
  expect_equal(level_of("data b; set a; if v > 1 then w = 1; else w = 2; run;", dat), "L1")
  expect_equal(level_of("data b; set a; where v > 1; keep g; run;", dat), "L1")
})

test_that("recognised cross-row patterns are L2", {
  expect_equal(level_of("data b; set a; by g; if first.g then n = 0; n + v; if last.g then output; run;", dat), "L2")
  expect_equal(level_of("data b; set a; p = lag(v); run;", dat), "L2")
  expect_equal(level_of("data b; set a end = e; if e then x = 1; run;", dat), "L2")
})

test_that("anything else falls back to L3", {
  expect_equal(level_of("data b; set a; if v > 1 then p = lag(v); run;", dat), "L3")
  expect_equal(level_of("data b; set a; retain m; if v > m then m = v; run;", dat), "L3")
  expect_equal(level_of("data b; do i = 1 to 3; output; end; run;"), "L3")
  expect_equal(level_of("data b c; set a; output b; run;", dat), "L3")
  expect_equal(level_of("data b; set a; put v=; v = 1; run;", dat), "L3")
})

test_that("PDV types and lengths follow first use", {
  an <- sas_analyze(sas_parse("data _null_; x='ab'; x='abcd'; if 0 then y='long text'; y='hi'; z = x || '|'; run;")$steps[[1]])
  expect_equal(an$info$x$len, 2L)
  expect_equal(an$info$y$len, 9L)
  expect_equal(an$info$z$len, 3L)
})

test_that("unknown functions and formats fail loud", {
  # not a SAS 9.4 function: SAS's compile-time ERROR 68-185
  expect_error(sas_translate("data _null_; x = notafunction(1); run;"), class = "sasonr_syntax_error")
  expect_error(sas_translate("data _null_; x = 1; put x= mystery8.; run;"), class = "sasonr_unsupported")
  expect_error(sas_translate("data _null_; x = '1' + 1; run;"), class = "sasonr_unsupported")
})

adlb <- list(adlb = data.frame(
  usubjid = c("01", "01", "01", "02", "02", "03", "03"),
  aval = c(10, 12, NA, 5, 7, 9, 4),
  ablfl = c("Y", "", "", "", "Y", "", "Y")
))

test_that("baseline carry-forward (retain + first./condition) is L2 and equals L3", {
  codes <- c(
    "data b; set adlb; by usubjid; retain base; if first.usubjid then base = aval; chg = aval - base; run;",
    "data b; set adlb; by usubjid; retain base; if first.usubjid then base = .;
       if ablfl = 'Y' then base = aval; chg = aval - base; run;",
    "data b; set adlb; by usubjid; length bvis $4; retain bvis 'none' n 0;
       if first.usubjid then bvis = 'x'; if ablfl = 'Y' then bvis = 'B' || usubjid; run;",
    "data b; set adlb; by usubjid; retain base; if aval > 0; if first.usubjid then base = .;
       if ablfl = 'Y' then base = aval; chg = aval - base; if last.usubjid then output; run;",
    "data b; set adlb; retain last_y; if ablfl = 'Y' then last_y = aval; run;"
  )
  for (code in codes) {
    auto <- sas_run(code, adlb)
    rows <- sas_run(code, adlb, level = "L3")
    expect_equal(auto$translation$steps[[1]]$level, "L2", info = code)
    expect_equal(auto$work, rows$work, info = code)
  }
  res <- sas_run(codes[2], adlb)$work$b
  expect_equal(res$base, c(10, 10, 10, NA, 7, NA, 4))
  expect_equal(sas_run(codes[3], adlb)$work$b$bvis, c("B01", "B01", "B01", "x", "B02", "x", "B03"))
})

test_that("carry-forward shapes that depend on state stay L3", {
  l3 <- c(
    "data b; set adlb; by usubjid; retain base; if first.usubjid then base = .;
       if base = . and aval > 0 then base = aval; run;",
    "data b; set adlb; by usubjid; retain base; x = base; if first.usubjid then base = aval; run;",
    "data b; set adlb; retain base; if ablfl = 'Y' then base = lag(aval); run;",
    "data b; set adlb; by usubjid; retain base; if first.usubjid then base = .; if aval > 6;
       if ablfl = 'Y' then base = aval; run;",
    "data b; set adlb; retain base; if ablfl = 'Y' then do; base = aval; end; run;"
  )
  for (code in l3) {
    auto <- sas_run(code, adlb)
    expect_equal(auto$translation$steps[[1]]$level, "L3", info = code)
    expect_equal(auto$work, sas_run(code, adlb, level = "L3")$work, info = code)
  }
})
