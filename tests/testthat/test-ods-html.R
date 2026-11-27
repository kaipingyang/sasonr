html_prog <- "data a; input grp $ x y; datalines;
a 1 10
a 2 20
b 4 40
;
run;
"

html_body <- function(doc) sub("(?s)<style>.*</style>", "", doc, perl = TRUE)

test_that("a CLASS table renders the spanning heading, row headers and right-aligned numbers", {
  r <- sas_run(paste0(html_prog, "proc means data=a; class grp; var x; run;"))
  tb <- sasonr:::render_html(r$ods[[1]])
  expect_identical(tb[1], '<table class="sas-table" summary="Procedure Means: Summary statistics">')
  expect_true(any(grepl('colspan="7">Analysis Variable : x </th>', tb, fixed = TRUE)))
  expect_true(any(grepl('<th class="sas-rowheader" scope="row">a</th><td class="sas-data sas-num">2</td>', tb, fixed = TRUE)))
  # the missing standard deviation of a one-observation group is the listing text "."
  expect_true(any(grepl('<td class="sas-data sas-num">.</td>', tb, fixed = TRUE)))
})

test_that("cell text is the listing text", {
  r <- sas_run(paste0(html_prog, "proc means data=a maxdec=2; var x; run;"))
  tb <- sasonr:::render_html(r$ods[[1]])
  expect_true(any(grepl(">2.33<", tb, fixed = TRUE)))
  expect_true(any(grepl("2.33", r$listing, fixed = TRUE)))
})

test_that("several analysis variables stack one value per variable in a cell", {
  r <- sas_run(paste0(html_prog, "proc means data=a; var x y; label x='LX'; run;"))
  tb <- paste(sasonr:::render_html(r$ods[[1]]), collapse = "\n")
  expect_match(tb, '<div class="sas-stack">x</div><div class="sas-stack">y</div>', fixed = TRUE)
  expect_match(tb, '<div class="sas-stack">2.3333333</div><div class="sas-stack">23.3333333</div>', fixed = TRUE)
  expect_match(tb, ">Label</th>", fixed = TRUE)
})

test_that("repeated CLASS values are left blank", {
  r <- sas_run("data b; input g $ h $ x; datalines;
a p 1
a q 2
;
run;
proc means data=b; class g h; var x; run;")
  tb <- sasonr:::render_html(r$ods[[1]])
  expect_true(any(grepl('<tr><th class="sas-rowheader" scope="row">&nbsp;</th><th class="sas-rowheader" scope="row">q</th>', tb, fixed = TRUE)))
})

test_that("CLASS with several analysis variables keeps one row per class level and stacks the variables", {
  r <- sas_run(paste0(html_prog, "proc means data=a; class grp; var x y; label y='LY'; run;"))
  tb <- sasonr:::render_html(r$ods[[1]])
  head <- tb[which(tb == "<thead>") + 1L]
  expect_identical(lengths(regmatches(head, gregexpr("<th ", head))), 9L)
  expect_match(head, '<th class="sas-header" scope="col">grp</th><th class="sas-header sas-num" scope="col">N Obs</th><th class="sas-header" scope="col">Variable</th><th class="sas-header" scope="col">Label</th>', fixed = TRUE)
  expect_no_match(head, "colspan", fixed = TRUE)
  body <- tb[seq(which(tb == "<tbody>") + 1L, which(tb == "</tbody>") - 1L)]
  expect_length(body, 2L)
  expect_match(body[1], '<th class="sas-rowheader" scope="row">a</th><td class="sas-data sas-num">2</td>', fixed = TRUE)
  expect_match(body[1], '<div class="sas-stack">x</div><div class="sas-stack">y</div>', fixed = TRUE)
  expect_match(body[1], '<div class="sas-stack">&nbsp;</div><div class="sas-stack">LY</div>', fixed = TRUE)
  expect_match(body[1], '<div class="sas-stack">1.5000000</div><div class="sas-stack">15.0000000</div>', fixed = TRUE)
  # a one-observation group: the standard deviations are missing in both blocks
  expect_match(body[2], '<div class="sas-stack">.</div><div class="sas-stack">.</div>', fixed = TRUE)
})

test_that("two CLASS variables with several analysis variables blank the repeated outer level", {
  r <- sas_run("data b; input g $ h $ x y; datalines;
a p 1 10
a q 2 20
b p 3 30
;
run;
proc means data=b; class g h; var x y; run;")
  tb <- sasonr:::render_html(r$ods[[1]])
  body <- tb[seq(which(tb == "<tbody>") + 1L, which(tb == "</tbody>") - 1L)]
  expect_length(body, 3L)
  expect_match(body[2], '^<tr><th class="sas-rowheader" scope="row">&nbsp;</th><th class="sas-rowheader" scope="row">q</th>')
  expect_identical(lengths(regmatches(body[2], gregexpr('class="sas-data sas-num sas-stacked"', body[2]))), 5L)
})

test_that("BY groups under a TITLE: the title once, one byline and one two-row table per group", {
  r <- sas_run(paste0(html_prog, "title 'BY groups';\nproc means data=a; by grp; var x; run;\ntitle;"))
  body <- html_body(sas_html(r))
  count <- function(pat) lengths(regmatches(body, gregexpr(pat, body, fixed = TRUE)))
  expect_identical(count('<div class="sas-title">BY groups</div>'), 1L)
  expect_identical(count("The MEANS Procedure"), 1L)
  expect_identical(count('<table class="sas-table"'), 2L)
  expect_identical(count('colspan="5">Analysis Variable : x </th>'), 2L)
  expect_true(regexpr("sas-title", body) < regexpr("sas-proctitle", body))
  expect_true(regexpr("sas-proctitle", body) < regexpr("grp=a", body))
  expect_true(regexpr("grp=a", body) < regexpr("sas-table", body))
  expect_true(regexpr("</table>", body) < regexpr("grp=b", body))
})

test_that("sas_html() writes titles, the procedure title, bylines and the CSS", {
  r <- sas_run(paste0(html_prog, "title 'Report';\nproc means data=a; var x; run;\ntitle;\nproc means data=a; by grp; var x; run;"))
  doc <- sas_html(r)
  expect_match(doc, "^<!DOCTYPE html>")
  expect_match(doc, ".sas-table", fixed = TRUE)
  body <- html_body(doc)
  expect_match(body, '<div class="sas-title">Report</div>', fixed = TRUE)
  expect_identical(lengths(regmatches(body, gregexpr("The MEANS Procedure", body))), 2L)
  expect_match(body, '<div class="sas-byline">grp=a</div>', fixed = TRUE)
  expect_match(body, '<div class="sas-byline">grp=b</div>', fixed = TRUE)
  expect_identical(lengths(regmatches(body, gregexpr('<table class="sas-table"', body))), 3L)
})

test_that("ODS HTML CLOSE and ODS HTML control which outputs reach the document", {
  r <- sas_run(paste0(html_prog, "ods html close;\nproc means data=a; var x; run;\nods html;\nproc means data=a; var y; run;"))
  expect_identical(r$status, "ok")
  body <- html_body(sas_html(r))
  expect_match(body, "Analysis Variable : y", fixed = TRUE)
  expect_no_match(body, "Analysis Variable : x", fixed = TRUE)
  # the listing gets both tables
  expect_identical(sum(grepl("Analysis Variable", r$listing)), 2L)
  expect_identical(html_body(sas_html(sas_run(paste0(html_prog, "ods html close;\nproc means data=a; run;")))) |>
                     grepl(pattern = "sas-table", fixed = TRUE), FALSE)
})

test_that("ODS HTML FILE= writes the document at ODS HTML CLOSE", {
  d <- withr::local_tempdir()
  f <- file.path(d, "out.html")
  r <- sas_run(paste0("data a; x = 1; run; ods html file='", f, "' style=htmlblue; proc means data=a; run; ods html close;"))
  expect_true(file.exists(f))
  expect_match(paste(readLines(f), collapse = "\n"), "The MEANS Procedure", fixed = TRUE)
  expect_true(any(grepl("^NOTE: Writing HTML Body file: ", as.character(sas_log(r, echo = FALSE)))))
})

test_that("ODS HTML options and other ODS statements fail loud", {
  # an HTML file the program does not close; options other than FILE= and STYLE=HTMLBlue
  expect_error(sas_run("ods html file='out.html';"), class = "sasonr_unsupported")
  expect_error(sas_run("ods html path='.' file='out.html'; ods html close;"), class = "sasonr_unsupported")
  expect_error(sas_run("ods html file='out.html' style=journal; ods html close;"), class = "sasonr_unsupported")
  expect_error(sas_run("ods rtf;"), class = "sasonr_unsupported")
})

test_that("sas_html() writes a file and adds the log on request", {
  r <- sas_run(paste0(html_prog, "proc means data=a; var x; run;"))
  f <- withr::local_tempfile(fileext = ".html")
  doc <- sas_html(r, file = f, log = TRUE)
  expect_identical(paste(readLines(f), collapse = "\n"), doc)
  expect_match(doc, '<div class="sas-log">', fixed = TRUE)
  expect_error(sas_html(list()), "result")
  expect_error(sas_html(r, file = 1), "file")
})

test_that("values are escaped", {
  r <- sas_run("data a; input g $ x; datalines;
<b> 1
;
run;
proc means data=a; class g; var x; run;")
  expect_match(paste(sasonr:::render_html(r$ods[[1]]), collapse = ""), "&lt;b&gt;", fixed = TRUE)
})
