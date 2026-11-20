# PROC TABULATE HTML output (template base.tabulate): the table on the
# logical grid of the listing, with rowspan/colspan.

# The statistics of PROC TABULATE have no rows in the statistics registry yet,
# so the runs use sasonr.strict = FALSE (the registry warnings are muffled).
tab_html_run <- function(prog) {
  withr::local_options(sasonr.strict = FALSE)
  withCallingHandlers(sas_run(prog), sasonr_strict_warning = function(w) invokeRestart("muffleWarning"))
}

tab_html_data <- "options nocenter nodate nonumber formdlim=' ' pagesize=max linesize=132;
data have;
  length grp $1 sub $1;
  input grp $ sub $ trt v w;
  datalines;
a x 1 10 1.5
a x 2 20 2.5
a y 1 30 3.5
b x 2 40 4.5
b y 1 50 .
b y 2 60 6.5
c x 1 . 7.5
c y 2 80 8.5
. y 1 90 9.5
;
run;
"

tab_html_table <- function(r, k = 1L) sasonr:::render_html(r$ods[[k]])
tab_html_part <- function(tb, open, close) tb[seq(which(tb == open) + 1L, length.out = which(tb == close) - which(tb == open) - 1L)]

test_that("a two-dimensional table: corner cell, class heading row and data cells spanning it", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp; var v; table grp, v*mean; run;"))
  tb <- tab_html_table(r)
  expect_identical(tb[1], '<table class="sas-table" summary="Procedure Tabulate: Table 1">')
  expect_identical(tab_html_part(tb, "<thead>", "</thead>"), c(
    '<tr><th class="sas-header" scope="col" rowspan="2">&nbsp;</th><th class="sas-header" scope="col">v</th></tr>',
    '<tr><th class="sas-header" scope="col">Mean</th></tr>'))
  expect_identical(tab_html_part(tb, "<tbody>", "</tbody>"), c(
    '<tr><th class="sas-rowheader" scope="row">grp</th><td class="sas-data sas-num" rowspan="2">20.00</td></tr>',
    '<tr><th class="sas-rowheader" scope="row">a</th></tr>',
    '<tr><th class="sas-rowheader" scope="row">b</th><td class="sas-data sas-num">50.00</td></tr>',
    '<tr><th class="sas-rowheader" scope="row">c</th><td class="sas-data sas-num">80.00</td></tr>'))
})

test_that("nested row levels span their sub-rows and the corner spans the row-title columns", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp sub; table grp*sub all, n; run;"))
  tb <- tab_html_table(r)
  expect_identical(tab_html_part(tb, "<thead>", "</thead>"),
    '<tr><th class="sas-header" scope="colgroup" colspan="2">&nbsp;</th><th class="sas-header" scope="col">N</th></tr>')
  body <- tab_html_part(tb, "<tbody>", "</tbody>")
  expect_identical(body[1], paste0('<tr><th class="sas-rowheader" scope="row">grp</th><th class="sas-rowheader" scope="row">sub</th>',
                                   '<td class="sas-data sas-num" rowspan="2">2</td></tr>'))
  expect_identical(body[2], '<tr><th class="sas-rowheader" scope="rowgroup" rowspan="2">a</th><th class="sas-rowheader" scope="row">x</th></tr>')
  expect_identical(body[3], '<tr><th class="sas-rowheader" scope="row">y</th><td class="sas-data sas-num">1</td></tr>')
  expect_match(body[length(body)], '<th class="sas-rowheader" scope="row" colspan="2">All</th>', fixed = TRUE)
})

test_that("N and NMISS print without decimals; the listing keeps them", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp; var v; table grp, v*(n nmiss sum pctn); run;
proc tabulate data=have format=dollar9.2; class grp; var v; table grp, v*(n mean); run;"))
  b1 <- tab_html_part(tab_html_table(r, 1L), "<tbody>", "</tbody>")
  expect_match(b1[1], '>3</td><td class="sas-data sas-num" rowspan="2">0</td><td class="sas-data sas-num" rowspan="2">60.00</td>',
               fixed = TRUE)
  b2 <- tab_html_part(tab_html_table(r, 2L), "<tbody>", "</tbody>")
  expect_match(b2[1], '>$3</td><td class="sas-data sas-num" rowspan="2">$20.00</td>', fixed = TRUE)
  expect_true(any(grepl("|    $3.00|   $20.00|", r$listing, fixed = TRUE)))
})

test_that("a page dimension writes one table per page under its page text, without a procedure title", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp sub; var v; table sub, grp, v*(n pctn); run;"))
  body <- sub("(?s)<style>.*</style>", "", sas_html(r), perl = TRUE)
  count <- function(pat) lengths(regmatches(body, gregexpr(pat, body, fixed = TRUE)))
  expect_identical(count('<table class="sas-table"'), 2L)
  expect_identical(count('<div class="sas-pagetext">sub x</div>'), 1L)
  expect_identical(count('<div class="sas-pagetext">sub y</div>'), 1L)
  expect_identical(count("TABULATE Procedure"), 0L)
  expect_true(regexpr("sub x", body) < regexpr("sas-table", body))
})

test_that("a table without column headings has an empty thead and no BOX text", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp; var v;
table grp='' all='Total', v=''*mean='' / box='Corner'; run;"))
  tb <- tab_html_table(r)
  expect_length(tab_html_part(tb, "<thead>", "</thead>"), 0L)
  expect_no_match(paste(tb, collapse = ""), "Corner", fixed = TRUE)
  expect_identical(tab_html_part(tb, "<tbody>", "</tbody>")[4],
                   '<tr><th class="sas-rowheader" scope="row">Total</th><td class="sas-data sas-num">41.43</td></tr>')
})

test_that("a short MISSTEXT is the text of an empty cell", {
  r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp sub; var v; table grp, sub*v*sum / misstext='none'; run;"))
  expect_match(paste(tab_html_table(r), collapse = ""), '<td class="sas-data sas-num">none</td>', fixed = TRUE)
})

test_that("HTML shapes that are not supported fail loud while the listing is written", {
  progs <- c(
    "label v='A very long label for the analysis variable'; table grp, v*mean*f=6.2;",
    "table grp, sub*v*sum / misstext='a much longer missing text than fits';",
    "table grp, v*mean / box='Corner';",
    "table grp, v*n*f=8.1;"
  )
  for (p in progs) {
    r <- tab_html_run(paste0(tab_html_data, "proc tabulate data=have; class grp sub; var v; ", p, " run;"))
    expect_identical(r$status, "ok")
    expect_true(any(grepl("^-+$", r$listing)))
    expect_error(sas_html(r), class = "sasonr_unsupported")
  }
})

test_that("ODS HTML CLOSE keeps a PROC TABULATE table out of the document", {
  r <- tab_html_run(paste0(tab_html_data, "ods html close; proc tabulate data=have; class grp; var v; table grp, v*mean; run;"))
  expect_no_match(sas_html(r), "sas-table\"", fixed = TRUE)
  expect_true(any(grepl("Mean", r$listing, fixed = TRUE)))
})
