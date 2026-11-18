# Match-merge (MERGE + BY): the vectorised input table and the row-wise
# (L3) PDV loop give the same data sets and the same log (step NOTEs on).

withr::local_options(sasonr.step_notes = TRUE)

merge_levels <- function(res) vapply(res$translation$steps, `[[`, "", "level")

expect_merge_same <- function(code, levels) {
  auto <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(merge_levels(auto), levels)
  expect_identical(auto$work, rows$work)
  expect_identical(auto$log, rows$log)
  invisible(auto)
}

merge_inputs <- "
data a; length name $3; input id name $ v; label v = 'Value A'; format v 8.2; datalines;
. na 14
1 aa 10
1 ab 11
2 ba 12
4 da 13
;
data b; length name $5; input id name $ w v; datalines;
1 alpha 100 20
2 beta 200 21
3 gamma 300 24
;
data c; input id z; datalines;
2 7
3 9
5 10
;
"

test_that("one data set: MERGE with BY is the BY-group read of that data set", {
  code <- paste0(merge_inputs, "
data m; merge a; by id; f = first.id; l = last.id; run;
")
  expect_merge_same(code, c("L1", "L1", "L1", "L2"))
})

test_that("two data sets: overlapping variables, one-to-many, groups in one data set, IN=", {
  code <- paste0(merge_inputs, "
data m; merge a(in = ina) b(in = inb); by id;
  both = ina and inb; fid = first.id; lid = last.id;
run;
data inner; merge a(in = ina) b(in = inb); by id; if ina and inb; run;
")
  res <- expect_merge_same(code, c("L1", "L1", "L1", "L2", "L2"))
  m <- res$work$m
  # a data set that has run out of rows in the group keeps its values
  expect_equal(m$w[m$id %in% 1], c(100, 100))
  # NAME has the length of its first definition (A, $3)
  expect_equal(trimws(m$name[m$id %in% 1]), c("alp", "ab"))
  expect_equal(m$v[m$id %in% 1], c(20, 11))
  # a group present in one data set only: the other data set's variables are missing
  expect_true(is.na(m$w[m$id %in% 4]))
  expect_equal(trimws(m$name[m$id %in% 3]), "gam")
})

test_that("three data sets: non-overlapping variables, one-to-many groups", {
  code <- paste0(merge_inputs, "
data m; merge a(in = ina) b c(in = inc); by id; k = _n_; run;
data mm; merge b(keep = id w) c; by id; run;
")
  expect_merge_same(code, c("L1", "L1", "L1", "L2", "L2"))
})

merge_note <- "NOTE: MERGE statement has more than one data set with repeats of BY values."

test_that("many-to-many: the NOTE about repeats of BY values comes before the reads", {
  code <- paste0(merge_inputs, "
data b2; input id w; datalines;
2 200
2 201
2 202
;
data c2; input id z; datalines;
2 7
2 8
;
data mm; merge b2 c2(in = inc); by id; put id= w= z=; run;
")
  res <- expect_merge_same(code, c("L1", "L1", "L1", "L1", "L1", "L2"))
  # B has three rows for id 2, C2 two: the third row keeps C2's last value
  expect_equal(res$work$mm$z[res$work$mm$id %in% 2], c(7, 8, 8))
  lg <- as.character(res$log)
  k <- which(lg == merge_note)
  expect_length(k, 1L)
  expect_identical(lg[k + 1:2], c("NOTE: There were 3 observations read from the data set WORK.B2.",
                                  "NOTE: There were 2 observations read from the data set WORK.C2."))
  expect_true(k > max(grep("^id=", lg)))
})

test_that("two many-to-many MERGE steps write one NOTE each; NONOTES drops it", {
  code <- paste0(merge_inputs, "
data b1; set b; output; if id = 1 then output; run;
data m3; merge a b1 c; by id; run;
data m4; merge a b1; by id; run;
")
  res <- expect_merge_same(code, c("L1", "L1", "L1", "L3", "L2", "L2"))
  expect_equal(sum(as.character(res$log) == merge_note), 2L)
  quiet <- sas_run(paste0("options nonotes;", code))
  expect_equal(sum(as.character(quiet$log) == merge_note), 0L)
})

test_that("repeats of BY values in several data sets but different BY groups write no NOTE", {
  code <- paste0(merge_inputs, "
data d; input id u; datalines;
2 1
2 2
;
data m; merge a d; by id; run;
")
  res <- expect_merge_same(code, c("L1", "L1", "L1", "L1", "L2"))
  expect_equal(sum(as.character(res$log) == merge_note), 0L)
  expect_true("NOTE: There were 5 observations read from the data set WORK.A." %in% as.character(res$log))
})

test_that("missing BY values and DESCENDING", {
  code <- "
data a; input k $ n x; datalines;
. . 1
. 2 2
b 1 3
a . 4
a 3 5
;
data b; input k $ n y; datalines;
. 2 10
b 1 20
b 1 21
a 3 30
;
data sa; set a; run;
proc sort data = sa; by descending k n; run;
proc sort data = b out = sb; by descending k n; run;
data m; merge sa(in = ia) sb(in = ib); by descending k n;
  fk = first.k; ln = last.n; src = ia * 10 + ib;
run;
proc sort data = a out = ua; by k n; run;
proc sort data = b out = ub; by k n; run;
data u; merge ua ub; by k n; run;
"
  res <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  lv <- merge_levels(res)
  expect_true(all(lv[c(6, 9)] %in% c("L1", "L2")))
  expect_identical(res$work, rows$work)
  expect_identical(res$log, rows$log)
})

test_that("BY-group accumulators, subsetting IF, END=, KEEP/RENAME and WHERE run vectorised", {
  code <- paste0(merge_inputs, "
data s; merge a(rename = (v = va)) b(drop = name) end = eof; by id;
  where id ne 3;
  if first.id then tot = 0;
  tot + w;
  last_row = eof;
  if va > 10;
  keep id va w tot last_row;
run;
")
  expect_merge_same(code, c("L1", "L1", "L1", "L2"))
})

test_that("empty data sets and character lengths that differ between data sets", {
  code <- "
data a; length s $2; input id s $; datalines;
1 xx
2 yy
;
data b; length s $6; input id s $ t; datalines;
2 longer 1
3 abcdef 2
;
data e; set a; if id > 99; run;
data m; merge a b; by id; run;
data n; merge e b; by id; run;
"
  expect_merge_same(code, c("L1", "L1", "L1", "L2", "L2"))
})

test_that("a step that assigns or retains a variable read by MERGE stays row-wise", {
  code <- paste0(merge_inputs, "
data m; merge a b; by id; if w = . then w = 0; run;
data r; merge a b; by id; retain v; run;
")
  res <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(merge_levels(res), c("L1", "L1", "L1", "L3", "L3"))
  expect_identical(res$work, rows$work)
  reasons <- lapply(res$translation$steps, `[[`, "reasons")
  expect_true("MERGE with an assignment to or RETAIN of a variable read by MERGE (w)" %in% reasons[[4]])
  expect_true("MERGE with an assignment to or RETAIN of a variable read by MERGE (v)" %in% reasons[[5]])
  # id 4 is only in A: W is missing and the step sets it to 0
  expect_equal(res$work$m$w[res$work$m$id %in% 4], 0)
})

test_that("an assigned variable read by MERGE carries its changed value within the BY group", {
  code <- paste0(merge_inputs, "
data p; merge a b; by id; w = w + 1; run;
")
  res <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(merge_levels(res)[4], "L3")
  expect_identical(res$work, rows$work)
  # B has one row for id 1: the second row keeps the incremented W
  expect_equal(res$work$p$w[res$work$p$id %in% 1], c(101, 102))
})

test_that("a MERGE with repeats of BY values in a step that runs STOP fails loud", {
  withr::local_options(sasonr.step_notes = TRUE)
  code <- "
data a; input g x; datalines;
1 1
2 2
2 3
;
data b; input g y; datalines;
1 10
2 20
2 30
;
data m; merge a b; by g; if g = 1 then stop; run;"
  expect_error(sas_run(code), class = "sasonr_unsupported")
  # STOP without repeats: unchanged
  ok <- sub("2 3\n;", ";", code, fixed = TRUE)
  expect_no_error(sas_run(ok))
})
