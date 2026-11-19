# Readability passes (R/codegen_passes.R): each pass on its own, the matched
# and a not-matched pattern, and runs with the passes on and off.

step_exprs <- function(code, i = 2L) sas_translate(code)$steps[[i]]$exprs
# The names a step's pipeline mentions, also as argument names (columns it creates).
step_names <- function(code, i = 2L) {
  walk <- function(e) if (is.call(e)) c(names(e), unlist(lapply(as.list(e), walk))) else if (is.symbol(e)) as.character(e)
  setdiff(walk(step_exprs(code, i)[[1]]), "")
}

# The WORK data sets (with attributes) and the log of a run with the passes
# given (NULL: the default).
run_with_passes <- function(code, passes = NULL, work = list()) {
  withr::local_options(sasonr.step_notes = TRUE, sasonr.passes = passes)
  r <- sas_run(code, work = work)
  list(work = r$work, log = msgs(r))
}

expect_same_run <- function(code, work = list()) {
  on <- run_with_passes(code, work = work)
  off <- run_with_passes(code, character(), work = work)
  expect_identical(on$work, off$work)
  expect_identical(on$log, off$log)
}

lab_data <- "
data lab;
  input usubjid $ visit aval;
  datalines;
S01 1 10
S01 2 12
S02 1 .
S02 2 18
S03 1 7
;
run;
"

baseline <- paste0(lab_data, "
data chg;
  set lab;
  by usubjid;
  retain base;
  if first.usubjid then base = aval;
  chg = aval - base;
run;
")

test_that("the pass table lists the passes and the option selects them", {
  tab <- codegen_pass_table()
  expect_named(tab, c("id", "description"))
  expect_setequal(tab$id, c("by_first_value", "unused_first_last"))
  withr::local_options(sasonr.passes = "unused_first_last")
  expect_identical(codegen_passes_on(), "unused_first_last")
  withr::local_options(sasonr.passes = "nope")
  expect_error(codegen_passes_on(), class = "sasonr_error")
})

test_that("unused FIRST./LAST. columns are dropped", {
  code <- paste0(lab_data, "
data x;
  set lab;
  by usubjid;
  if last.usubjid then flag = 1;
run;
")
  withr::local_options(sasonr.passes = "unused_first_last")
  nm <- step_names(code)
  expect_false("first.usubjid" %in% nm)
  expect_true("last.usubjid" %in% nm)
  withr::local_options(sasonr.passes = character())
  expect_true("first.usubjid" %in% step_names(code))
  expect_same_run(code)
})

test_that("a FIRST./LAST. column read by a later statement or by PUT stays", {
  code <- paste0(lab_data, "
data _null_;
  set lab;
  by usubjid;
  if first.usubjid then put usubjid=;
run;
")
  withr::local_options(sasonr.passes = "unused_first_last")
  nm <- step_names(code)
  expect_true("first.usubjid" %in% nm)
  expect_false("last.usubjid" %in% nm)
  expect_same_run(code)
})

test_that("a baseline set at FIRST. becomes one grouped value per BY group", {
  withr::local_options(sasonr.passes = "by_first_value")
  e <- step_exprs(baseline)[[1]]
  nm <- all.names(e)
  expect_false(any(c("sas_carry", ".cv_base_1", ".cs_base_1") %in% nm))
  expect_true(all(c("group_by", "ungroup", "first", "cumsum") %in% nm))
  txt <- sas_translate(baseline)$steps[[2]]$code
  expect_match(txt, "dplyr::group_by(.grp = cumsum(first.usubjid))", fixed = TRUE)
  expect_match(txt, "base = as.numeric(dplyr::first(aval))", fixed = TRUE)
  expect_same_run(baseline)
})

test_that("both passes together give the same run, also on an empty input", {
  expect_same_run(baseline)
  empty <- "
data chg;
  set lab;
  by usubjid;
  retain base;
  if first.usubjid then base = aval;
  chg = aval - base;
run;
"
  lab0 <- list(lab = data.frame(usubjid = character(), aval = numeric()))
  expect_same_run(empty, work = lab0)
  # input columns that are integer, labelled or dates
  lab1 <- list(lab = data.frame(usubjid = c("a", "a", "b"), aval = c(3L, 4L, NA)))
  attr(lab1$lab$aval, "label") <- "Analysis value"
  expect_same_run(empty, work = lab1)
  lab2 <- list(lab = data.frame(usubjid = c("a", "a", "b"), aval = as.Date(c("2020-01-01", NA, "2020-02-01"))))
  expect_same_run(empty, work = lab2)
})

test_that("a baseline with special missing BY values keeps the runs of FIRST.", {
  code2 <- "
data lab;
  input g aval;
  if g = 9 then k = .a;
  else if g = 8 then k = .b;
  else k = g;
  datalines;
9 1
9 2
8 3
1 4
;
run;
data chg;
  set lab;
  by k;
  retain base;
  if first.k then base = aval;
run;
"
  withr::local_options(sasonr.passes = "by_first_value")
  expect_false("sas_carry" %in% step_names(code2))
  expect_identical(sas_run(code2)$work$chg$base, c(1, 1, 3, 4))
  expect_same_run(code2)
})

test_that("patterns the baseline pass cannot prove equal are left alone", {
  withr::local_options(sasonr.passes = "by_first_value")
  not_rewritten <- function(step) {
    code <- paste0(lab_data, step)
    expect_true("sas_carry" %in% step_names(code))
    expect_same_run(code)
  }
  # an expression, not a column: evaluated per row it can write a NOTE
  not_rewritten("data x; set lab; by usubjid; retain base; if first.usubjid then base = aval * 2; run;")
  # a second assignment
  not_rewritten("data x; set lab; by usubjid; retain base; if first.usubjid then base = aval; if aval > 11 then base = aval; run;")
  # a subsetting IF before the assignment removes the first row of a group
  not_rewritten("data x; set lab; by usubjid; retain base; if visit > 1; if first.usubjid then base = aval; run;")
  # a condition that is not FIRST.
  not_rewritten("data x; set lab; by usubjid; retain base; if last.usubjid then base = aval; run;")
  # a character variable
  not_rewritten("data x; set lab; by usubjid; length b $8; retain b; if first.usubjid then b = usubjid; run;")
})

test_that("with all passes off the code is the code built without passes", {
  withr::local_options(sasonr.passes = character())
  code2 <- "
data lab;
  input usubjid $ visit aval;
  datalines;
S01 1 10
S01 2 12
S02 1 20
S02 2 18
;
run;

data chg;
  set lab;
  by usubjid;
  retain base;
  if first.usubjid then base = aval;
  chg = aval - base;
run;
"
  tr <- sas_translate(code2)
  got <- unlist(lapply(tr$steps, function(s) {
    c("## code", s$code, "## exprs", unlist(lapply(s$exprs, function(e) deparse(e, width.cutoff = 500L))))
  }))
  want <- readLines(test_path("fixtures", "getting_started_passes_off.txt"))
  expect_identical(strsplit(paste(got, collapse = "\n"), "\n", fixed = TRUE)[[1]], want)
})
