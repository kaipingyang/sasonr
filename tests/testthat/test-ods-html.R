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
  r <- sas_run(paste0(html_prog, "proc means data=a; class grp; var x y; run;"))
  expect_error(sasonr:::render_html(r$ods[[1]]), class = "sasonr_unsupported")
  r <- sas_run("data b; input g $ h $ x; datalines;
a p 1
a q 2
;
run;
proc means data=b; class g h; var x; run;")
  tb <- sasonr:::render_html(r$ods[[1]])
  expect_true(any(grepl('<tr><th class="sas-rowheader" scope="row">&nbsp;</th><th class="sas-rowheader" scope="row">q</th>', tb, fixed = TRUE)))
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

test_that("ODS HTML options and other ODS statements fail loud", {
  expect_error(sas_run("ods html file='out.html';"), class = "sasonr_unsupported")
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
