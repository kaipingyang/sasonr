tr_data <- "
data long; length param $4 pname $12; input usubjid $ param $ pname $ aval; label aval = 'Analysis Value';
datalines;
S01 ALT Alanine 30
S01 AST Aspartate 25
S02 ALT Alanine 40
;
run;
"

test_that("BY + ID + VAR: wide layout, _NAME_ / _LABEL_ and their attributes", {
  res <- sas_run(paste(tr_data, "proc transpose data=long out=wide; by usubjid; id param; var aval; run;"))
  w <- res$work$wide
  expect_equal(names(w), c("usubjid", "_name_", "_label_", "alt", "ast"))
  expect_equal(w$alt, c(30, 40))
  expect_equal(w$ast, c(25, NA))
  expect_equal(w$`_label_`, c("Analysis Value", "Analysis Value"))
  m <- res$meta$wide
  expect_equal(m$disp[["alt"]], "ALT")
  expect_equal(m$labels[["_name_"]], "NAME OF FORMER VARIABLE")
  expect_equal(m$len[["_label_"]], 40L)
})

test_that("no ID: COLn, PREFIX/SUFFIX, NAME=, IDLABEL", {
  res <- sas_run(paste(tr_data, "proc transpose data=long out=a prefix=v suffix=_x name=src; by usubjid; var aval; run;
    proc transpose data=long(where=(usubjid='S01')) out=b(drop=_name_ _label_); id param; idlabel pname; var aval; run;"))
  expect_equal(names(res$work$a), c("usubjid", "src", "_label_", "v1_x", "v2_x"))
  expect_equal(res$meta$b$labels[["alt"]], "Alanine")
})

test_that("ID values become valid names; duplicates need LET; multiple IDs concatenate", {
  res <- sas_run("data d; infile datalines dsd; input id $ g v; datalines;
A B,1,10
x-1,1,20
1a,2,30
;
run;
proc transpose data=d out=o; id id; var v; run;
proc transpose data=d out=m delimiter=_; id id g; var v; run;")
  expect_equal(names(res$work$o), c("_name_", "a_b", "x_1", "_1a"))
  expect_equal(res$meta$o$disp[["a_b"]], "A_B")
  expect_equal(res$meta$m$disp[["a_b_1"]], "A_B_1")
  dup <- "data d; input g id v; datalines;
1 1 10
1 1 20
;
run;"
  expect_error(sas_run(paste(dup, "proc transpose data=d out=o; by g; id id; var v; run;")), class = "sasonr_runtime_error")
  res2 <- sas_run(paste(dup, "proc transpose data=d out=o let; by g; id id; var v; run;"))
  expect_equal(res2$work$o$`_1`, 20)
})

test_that("mixed character / numeric variables become character; COPY rows", {
  res <- sas_run("data d; input g $ x c $ k; datalines;
a 1 p 7
a 2 q 8
;
run;
proc transpose data=d out=t; by g; var x c; run;
proc transpose data=d out=cp; by g; var x; copy k; run;")
  t <- res$work$t
  expect_true(is.character(t$col1))
  expect_equal(trimws(t$col1), c("1", "p"))
  expect_equal(res$meta$t$len[["col1"]], 12L)
  expect_equal(res$work$cp$k, c(7, 8))
  expect_equal(res$work$cp$`_name_`, c("x", ""))
  expect_equal(res$work$cp$col2, c(2, NA))
})

test_that("unsupported TRANSPOSE surfaces fail loud", {
  d <- "data a; x = 1; run;"
  expect_error(sas_run(paste(d, "proc transpose data=a; var x; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc transpose data=a out=b indb=no; var x; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc transpose data=a out=b; id x; var x; run;")), NA)
  expect_error(sas_run("data a; x = -1; y = 2; run; proc transpose data=a out=b; id x; var y; run;"), class = "sasonr_unsupported")
})
