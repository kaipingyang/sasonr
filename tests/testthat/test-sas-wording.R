# sasonr's own output never presents itself as SAS: every string literal in
# R/ that contains the word "SAS" must be one of the uses listed here.

sas_wording_allowed <- data.frame(
  text = c(
    "(SAS line ",
    "(read-only in SAS)",
    "as a SAS-supplied format",
    "Option value for SAS option LINESIZE",
    "the SAS 9.4 inventory",
    "the edge of the SAS fuzz window",
    "not a SAS name",
    "of the SAS program",
    "# SAS character values",
    "The SAS text must be",
    "the SAS position of the statement",
    "no SAS statements were generated"
  ),
  reason = c(
    "the line of the user's SAS program (messages, generated-code headers)",
    "describes the documented SAS behaviour of the variable (a fail-loud message)",
    "the built-in formats of the SAS language, named in ERROR 48-59 of the program",
    "a system option of the SAS language, named in ERROR 18-12 of the program",
    "SAS 9.4 as the language version",
    "describes the documented rounding fuzz of SAS (a fail-loud message)",
    "the naming rules of the SAS language (a fail-loud message)",
    "the user's SAS program (header of a script written by sas_script())",
    "comment in a script: the data set keeps the attributes of character values in the program",
    "the SAS program text given to sas_script() (an argument error)",
    "the line and column of the statement in the user's SAS program",
    "statements of the SAS language, which CALL EXECUTE generates (a NOTE of the program)"
  )
)

# The wordings with which a catalogue message avoids naming SAS as the one
# that acted (log_catalog.R, `neutral`).
sas_neutral_lines <- c("sasonr", "The task name is [")

# Every string constant of the R files (comments and roxygen are not
# constants) that contains the word SAS, with file and line.
sas_literals <- function(dir) {
  out <- list()
  for (f in list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    s <- pd[pd$token == "STR_CONST" & grepl("\\bSAS\\b", pd$text, perl = TRUE), ]
    if (nrow(s)) out[[f]] <- data.frame(where = paste0(basename(f), ":", s$line1), text = s$text)
  }
  do.call(rbind, c(list(data.frame(where = character(), text = character())), out))
}

test_that("every SAS in a string literal is an allowed use", {
  d <- testthat::test_path("..", "..", "R")
  if (!dir.exists(d)) testthat::skip("package sources (R/) not available")
  lits <- sas_literals(d)
  expect_gt(nrow(lits), 0)
  # remove the allowed uses, then the word may not be left anywhere
  rest <- lits$text
  for (a in sas_wording_allowed$text) rest <- gsub(a, "", rest, fixed = TRUE)
  ok <- !grepl("\\bSAS\\b", rest, perl = TRUE)
  expect_identical(paste(lits$where[!ok], lits$text[!ok]), character())
  # every entry is still used (the list stays short)
  used <- vapply(sas_wording_allowed$text, function(a) any(grepl(a, lits$text, fixed = TRUE)), logical(1))
  expect_identical(sas_wording_allowed$text[!used], character())
})

test_that("the catalogue templates name sasonr where the original wording names itself", {
  tpl <- unlist(lapply(sasonr:::log_catalog, function(m) c(m$template, m$wrapped)))
  rest <- tpl
  for (a in sas_wording_allowed$text) rest <- gsub(a, "", rest, fixed = TRUE)
  hits <- tpl[grepl("\\bSAS\\b", rest, perl = TRUE)]
  expect_identical(unname(hits), character())
  # a neutral entry has a neutral line somewhere in its template; no other
  # entry has one (except the messages only sasonr writes, ids "sasonr.*")
  cat <- sasonr:::log_catalog
  has_neutral <- vapply(cat, function(m) {
    any(vapply(sas_neutral_lines, function(n) any(grepl(n, c(m$template, m$wrapped), fixed = TRUE)), logical(1)))
  }, logical(1))
  marked <- vapply(cat, function(m) isTRUE(m$neutral), logical(1))
  expect_identical(names(cat)[marked & !has_neutral], character())
  own <- startsWith(names(cat), "sasonr.")
  expect_identical(names(cat)[!marked & has_neutral & !own], character())
  # a new neutral entry needs a reviewed change of this list
  expect_identical(sort(names(cat)[marked]),
                   c("input.new_line", "step.exception", "step.stopped", "step.syntax_check"))
})

test_that("the allowlist check catches SAS next to an allowed use", {
  rest <- paste("SAS", "(SAS line ")
  for (a in sas_wording_allowed$text) rest <- gsub(a, "", rest, fixed = TRUE)
  expect_true(grepl("\\bSAS\\b", rest, perl = TRUE))
})
