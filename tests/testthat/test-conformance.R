# Conformance against real SAS 9.4 output (PUT lines joined by " ;; ").
case_dir <- test_path("fixtures", "parse_stress")
expected <- local({
  x <- readLines(file.path(case_dir, "expected_sas.txt"))
  bar <- regexpr("|", x, fixed = TRUE)
  stats::setNames(substring(x, bar + 1L), substr(x, 1L, bar - 1L))
})
cases <- sub("\\.sas$", "", sort(list.files(case_dir, pattern = "\\.sas$")))

# Every case is supported now (t23 PROC SQL since the fourth version).
unsupported_cases <- character()

run_case <- function(name, level = "auto") {
  code <- readLines(file.path(case_dir, paste0(name, ".sas")))
  # these programs test the language; approximate functions (MOD) run with a warning
  res <- withr::with_options(list(sasonr.strict = FALSE), suppressWarnings(sas_run(code, level = level)))
  paste(res$log, collapse = " ;; ")
}

for (nm in setdiff(cases, unsupported_cases)) {
  test_that(paste("conformance:", nm), {
    expect_equal(run_case(nm), expected[[nm]])
    expect_equal(run_case(nm, level = "L3"), expected[[nm]])
  })
}

for (nm in unsupported_cases) {
  test_that(paste("fail loud:", nm), {
    code <- readLines(file.path(case_dir, paste0(nm, ".sas")))
    expect_error(sas_run(code), class = "sasonr_unsupported")
  })
}

test_that("vectorised (L1/L2) and row-wise (L3) code agree on data sets", {
  code <- "
data a; input g $ h $ v; datalines;
A x 1
A x .
A y 3
B y 4
B z 5
;
data s(rename=(tot=total)); set a; by g;
  retain cnt 10;
  if first.g then tot = 0;
  tot + v;
  cnt + 1;
  prev = lag(v);
  if last.g then output;
  keep g tot cnt prev;
run;
data t; set a; if v > 1; length cls $4;
  select (g); when ('A') cls = 'aa'; otherwise cls = 'other'; end;
  if v >= 4 then do; big = 1; note = 'big'; end; else note = 'small';
run;
"
  auto <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(vapply(auto$translation$steps, `[[`, "", "level"), c("L1", "L2", "L1"))
  expect_equal(auto$work, rows$work)
  expect_equal(auto$work$s$total, c(4, 9))
  expect_equal(auto$work$s$cnt, c(13, 15))
  expect_equal(auto$work$t$note, c("sma", "big", "big"))
})
