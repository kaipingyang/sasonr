# PROC TABULATE: cell computation and the table object (no listing yet).

have <- "
proc format;
  value trtf 1 = 'Low' 2 = 'High';
run;
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

# Run the data steps of `data_code`, then compute every TABLE of the PROC
# TABULATE step `proc_code` (no output: the step itself stops before the
# listing).  `rows` selects input rows (a WHERE stand-in).
tab_compute <- function(data_code, proc_code, rows = NULL) {
  s <- sasonr:::SasSession$new()
  on.exit(s$close())
  sas_run(data_code, session = s)
  old <- sasonr:::sas_rt_active(s)
  on.exit(sasonr:::sas_rt_active(old), add = TRUE)
  withr::local_options(sasonr.strict = FALSE)
  step <- sas_parse(proc_code)$steps[[1]]
  decl <- sasonr:::tab_declared(step)
  tables <- Filter(function(x) identical(x$stmt, "table"), step$stmts)
  irs <- lapply(tables, sasonr:::tab_table_ir, decl = decl)
  inp <- sasonr:::proc_input(step, s$meta, s$last)
  sp <- sasonr:::tab_spec(step, decl, irs, inp)
  d <- tibble::as_tibble(get(inp$ds$name, envir = s$work))
  if (!is.null(rows)) d <- d[rows, , drop = FALSE]
  res <- withCallingHandlers(sasonr:::tabulate_compute(d, sp),
                             sasonr_strict_warning = function(w) invokeRestart("muffleWarning"))
  res$sp <- sp
  res$d <- d
  res
}

# The stat columns of a table as a numeric matrix (rows x columns).
cells <- function(t) {
  cols <- attr(t, "cols")
  unname(as.matrix(as.data.frame(t)[cols$name[cols$role == "stat"]]))
}

hdr_text <- function(t, lvl) {
  h <- attr(t, "header")
  vapply(h$text[h$level == lvl], paste, "", collapse = " ")
}

test_that("a two-dimensional table has one row per class level", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v; table grp, v*mean; run;")
  expect_length(r$tables, 1L)
  t <- r$tables[[1]]
  expect_s3_class(t, "sas_ods_table")
  expect_identical(attr(t, "name"), "Table")
  expect_identical(attr(t, "label"), "Table 1")
  expect_identical(attr(t, "path"), "Tabulate.Report.Table")
  expect_identical(attr(t, "template"), "base.tabulate")
  expect_identical(t$grp, c("a", "b", "c"))
  expect_equal(unname(cells(t)[, 1]), c(20, 50, 80))
  expect_identical(hdr_text(t, 1L), "v")
  expect_identical(hdr_text(t, 2L), "Mean")
  rh <- attr(t, "row_header")
  expect_identical(unlist(rh$text), c("grp", "a", "b", "c"))
  expect_identical(rh$heading, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(attr(t, "misstext"), ".")
  expect_identical(attr(t, "formats")[[1]][[1]]$text, "12.2")
})

test_that("a column crossing gives one column per level and statistic", {
  r <- tab_compute(have, "proc tabulate data=have; class grp sub; var v; table grp, sub*v*(n mean); run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)), matrix(c(2, 1, 0, 15, 40, NA, 1, 2, 1, 30, 55, 80), 3))
  h <- attr(t, "header")
  expect_identical(hdr_text(t, 1L), "sub")
  expect_identical(h$span, c(4L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L))
  expect_identical(hdr_text(t, 2L), c("x", "y"))
  expect_identical(hdr_text(t, 3L), c("v", "v"))
  expect_identical(hdr_text(t, 4L), c("N", "Mean", "N", "Mean"))
  expect_identical(attr(t, "cols")$stat[attr(t, "cols")$role == "stat"], c("n", "mean", "n", "mean"))
})

test_that("ALL summarises the kept rows in rows and columns", {
  r <- tab_compute(have, "proc tabulate data=have; class grp sub; table grp*sub all, n; run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)[, 1]), c(2, 1, 1, 2, 1, 1, 8))
  expect_identical(t$sub, c("x", "y", "x", "y", "x", "y", NA))
  rh <- attr(t, "row_header")
  all_cell <- rh[vapply(rh$text, identical, NA, "All"), ]
  expect_identical(c(all_cell$col, all_cell$ncol, all_cell$row), c(1L, 2L, 7L))
  lvl_a <- rh[vapply(rh$text, identical, NA, "a") & !rh$heading, ]
  expect_identical(lvl_a$nrow, 2L)
  r <- tab_compute(have, "proc tabulate data=have; class grp sub; table grp all, sub all; run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)), matrix(c(2, 1, 1, 4, 1, 2, 1, 4, 3, 3, 2, 8), 4))
  h <- attr(t, "header")
  all_h <- h[vapply(h$text, identical, NA, "All"), ]
  expect_identical(c(all_h$level, all_h$rowspan), c(1L, 2L))
  expect_identical(hdr_text(t, 3L), c("N", "N", "N"))
})

test_that("an empty label removes the heading; the total keeps the rows with all CLASS values", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v;
    table grp='' all='Total', v=''*mean='' / box='B'; run;")
  t <- r$tables[[1]]
  expect_equal(round(unname(cells(t)[, 1]), 2), c(20, 50, 80, 41.43))
  expect_identical(cells(t)[4, 1], 290 / 7)
  expect_identical(nrow(attr(t, "header")), 0L)
  expect_identical(unlist(attr(t, "row_header")$text), c("a", "b", "c", "Total"))
  expect_identical(attr(t, "box"), list(kind = "text", value = "B"))
})

test_that("the page dimension keeps the whole-table denominators", {
  r <- tab_compute(have, "proc tabulate data=have; class grp sub; var v; table sub, grp, v*(n pctn pctsum); run;")
  expect_length(r$tables, 2L)
  x <- r$tables[[1]]
  y <- r$tables[[2]]
  expect_identical(attr(x, "page"), list(text = c("sub", "x"), role = c("title", "class")))
  expect_identical(attr(y, "page")$text, c("sub", "y"))
  expect_equal(round(unname(cells(x)), 2), matrix(c(2, 1, 0, 28.57, 14.29, 0, 10.34, 13.79, NA), 3))
  expect_equal(round(unname(cells(y)), 2), matrix(c(1, 2, 1, 14.29, 28.57, 14.29, 10.34, 37.93, 27.59), 3))
  # n / N * 100 and sum / S * 100 exactly
  expect_identical(cells(x)[2, 2], 1 / 7 * 100)
  expect_identical(cells(y)[3, 3], 80 / 290 * 100)
  expect_identical(cells(x)[1, 3], 30 / 290 * 100)
})

test_that("absent combinations: no row in the row dimension, an empty cell across dimensions", {
  gap <- "data gap; length grp $1 sub $1; input grp $ sub $ v; datalines;
a x 10
a y 20
b x 30
b y 40
c x 50
;
run;"
  r <- tab_compute(gap, "proc tabulate data=gap; class grp sub; table grp*sub, n; run;")
  t <- r$tables[[1]]
  expect_identical(paste0(t$grp, t$sub), c("ax", "ay", "bx", "by", "cx"))
  expect_equal(unname(cells(t)[, 1]), rep(1, 5))
  r <- tab_compute(gap, "proc tabulate data=gap; class grp sub; table grp, sub*n; run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)), matrix(c(1, 1, 1, 1, 1, NA), 3))
})

test_that("cell formats: the column F= wins, PROC FORMAT= applies to every statistic", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v; table grp*f=5.1, v*mean*f=8.3; run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)[, 1]), c(20, 50, 80))
  expect_identical(attr(t, "cols")$format[[2]]$text, "8.3")
  r <- tab_compute(have, "proc tabulate data=have format=dollar9.2; class grp; var v; table grp, v*(n mean); run;")
  t <- r$tables[[1]]
  expect_equal(unname(cells(t)), matrix(c(3, 3, 1, 20, 50, 80), 3))
  expect_identical(vapply(attr(t, "cols")$format[2:3], `[[`, "", "text"), c("dollar9.2", "dollar9.2"))
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v; table grp*f=5.1, v*mean; run;")
  expect_identical(attr(r$tables[[1]], "cols")$format[[2]]$text, "5.1")
})

test_that("every statistic matches the PROC MEANS kernel exactly", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v;
    table grp, v*(n nmiss sum mean std min max range median p50 pctn pctsum); run;")
  t <- r$tables[[1]]
  m <- cells(t)
  expect_equal(unname(m[, 1:10]), matrix(c(3, 3, 1, 0, 0, 1, 60, 150, 80, 20, 50, 80, 10, 10, NA, 10, 40, 80,
                                           30, 60, 80, 20, 20, 0, 20, 50, 80, 20, 50, 80), 3))
  expect_equal(round(unname(m[, 11:12]), 2), matrix(c(42.86, 42.86, 14.29, 20.69, 51.72, 27.59), 3))
  expect_identical(hdr_text(t, 2L), c("N", "NMiss", "Sum", "Mean", "Std", "Min", "Max", "Range", "Median", "P50",
                                      "PctN", "PctSum"))
  sp <- list(freq = NULL, vardef = "df", exclnpwgt = FALSE, alpha = 0.05, qntldef = 5L)
  d <- r$d[!is.na(r$d$grp) & r$d$grp != "", ]
  stats <- c("n", "nmiss", "sum", "mean", "std", "min", "max", "range", "median", "p50")
  for (i in 1:3) {
    x <- d$v[d$grp == c("a", "b", "c")[i]]
    ref <- sasonr:::means_cell_stats(x, rep(1, length(x)), NULL, stats, sp, FALSE)
    expect_identical(unname(m[i, 1:10]), unname(ref))
  }
  # a statistic needing a value of an empty cell
  r <- tab_compute(have, "proc tabulate data=have; class grp sub; var v; table grp, sub*v*(n sum pctsum); run;")
  expect_equal(unname(cells(r$tables[[1]])[3, 1:3]), c(0, NA, NA))
})

test_that("a class variable crossed with itself keeps only the diagonal", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v; table grp*grp; run;")
  t <- r$tables[[1]]
  expect_identical(nrow(t), 1L)
  expect_equal(unname(cells(t)[1, ]), c(3, 3, 2))
  expect_identical(hdr_text(t, 1L), "grp")
  expect_identical(hdr_text(t, 2L), c("a", "b", "c"))
  expect_identical(hdr_text(t, 5L), c("N", "N", "N"))
})

test_that("CLASS / MISSING keeps the missing level first with an empty heading", {
  r <- tab_compute(have, "proc tabulate data=have; class grp sub / missing; var v; table grp, v*sum; run;")
  t <- r$tables[[1]]
  expect_identical(trimws(t$grp), c("", "a", "b", "c"))
  expect_equal(unname(cells(t)[, 1]), c(90, 60, 150, 80))
  expect_identical(unlist(attr(t, "row_header")$text)[2], "")
})

test_that("ORDER=FORMATTED sorts by the formatted text", {
  r <- tab_compute(paste(have, "proc format; value revf 1 = 'Zeta' 2 = 'Alpha'; run;"),
                   "proc tabulate data=have order=formatted; class trt / order=formatted; var v; format trt revf.;
                    table trt, v*sum; run;")
  t <- r$tables[[1]]
  expect_identical(t$trt, c(2, 1))
  expect_identical(unlist(attr(t, "row_header")$text), c("trt", "Alpha", "Zeta"))
  expect_equal(unname(cells(t)[, 1]), c(200, 180))
})

test_that("ORDER=FREQ ties are taken only when every candidate order agrees", {
  r <- tab_compute(have, "proc tabulate data=have order=freq; class grp; var v; table grp, v*sum; run;")
  expect_identical(r$tables[[1]]$grp, c("a", "b", "c"))
  tie <- "data t; length g $1; input g $ v; datalines;
b 1
a 2
;
run;"
  expect_error(tab_compute(tie, "proc tabulate data=t order=freq; class g; var v; table g, v*sum; run;"),
               class = "sasonr_unsupported")
})

test_that("an empty input prints the headings and no body row", {
  r <- tab_compute(have, "proc tabulate data=have; class grp; var v; table grp, v*sum; run;", rows = integer())
  t <- r$tables[[1]]
  expect_identical(nrow(t), 0L)
  expect_identical(hdr_text(t, 2L), "Sum")
  expect_identical(r$events, character())
  allmiss <- "data allmiss; length grp $1; input grp $ v; datalines;
. 1
. 2
;
run;"
  r <- tab_compute(allmiss, "proc tabulate data=allmiss; class grp; var v; table grp, v*sum; run;")
  expect_identical(nrow(r$tables[[1]]), 0L)
  expect_identical(r$events, "WARNING: A class, frequency, or weight variable is missing on every observation.")
  expect_error(tab_compute(have, "proc tabulate data=have; class grp; var v; table grp all, v*sum; run;",
                           rows = integer()), class = "sasonr_unsupported")
})

test_that("shapes outside the measured rules fail loud", {
  expect_error(tab_compute(have, "proc tabulate data=have; class grp; var v; table grp, v*stddev; run;"),
               "STDDEV", class = "sasonr_unsupported")
  expect_error(tab_compute(have, "proc tabulate data=have; class grp; class sub / missing; var v; table grp, v*sum; run;"),
               "MISSING", class = "sasonr_unsupported")
  expect_error(tab_compute(have, "proc tabulate data=have; class grp; var v; table grp, v*n v=''*sum=''; run;"),
               "heading rows", class = "sasonr_unsupported")
})

test_that("a step that passes the checks still stops before the output", {
  chk <- sas_check(paste(have, "proc tabulate data=have; class grp sub; var v; table sub, grp all, v*(n pctn); run;"))
  expect_identical(chk$feature, "PROC TABULATE output is not implemented yet")
})
