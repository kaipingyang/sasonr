squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x[nzchar(x)]
}

means_data <- "
data d; input g $ h $ x w f; datalines;
A P 10 1 2
A Q 20 2 1
B P 30 3 1
B . 40 4 3
. P 50 1 1
;
run;
"

test_that("default statistics and the listing of one analysis variable", {
  res <- sas_run(paste(means_data, "proc means data=d; var x; run;"))
  expect_equal(squish(res$listing), c(
    "The MEANS Procedure", "Analysis Variable : x",
    "N Mean Std Dev Minimum Maximum", strrep("-", 65),
    "5 30.0000000 15.8113883 10.0000000 50.0000000", strrep("-", 65)
  ))
})

test_that("OUTPUT OUT= with CLASS: _TYPE_ numbering, _FREQ_, missing class values excluded", {
  res <- sas_run(paste(means_data, "proc means data=d noprint; class g h; var x;
    output out=o mean=m n=cnt; run;"))
  o <- res$work$o
  expect_equal(names(o), c("g", "h", "_type_", "_freq_", "m", "cnt"))
  expect_equal(o$`_type_`, c(0, 1, 1, 2, 2, 3, 3, 3))
  expect_equal(o$`_freq_`, c(3, 2, 1, 2, 1, 1, 1, 1))
  expect_equal(o$m, c(20, 20, 20, 15, 30, 10, 20, 30))
  expect_equal(o$g, c("", "", "", "A", "B", "A", "A", "B"))
  expect_equal(res$meta$o$disp[["_type_"]], "_TYPE_")
})

test_that("CLASS / MISSING, NWAY, TYPES and WAYS select the combinations", {
  res <- sas_run(paste(means_data, "proc means data=d noprint nway; class g / missing; var x;
    output out=o sum=s; run;
    proc means data=d noprint; class g h; types g h*g; var x; output out=t n=n; run;
    proc means data=d noprint; class g h; ways 1; var x; output out=w n=n; run;"))
  expect_equal(res$work$o$g, c("", "A", "B"))
  expect_equal(res$work$o$s, c(50, 30, 70))
  expect_equal(res$work$t$`_type_`, c(2, 2, 3, 3, 3))
  expect_equal(res$work$w$`_type_`, c(1, 1, 2, 2))
})

test_that("FREQ and WEIGHT: weighted SUM / MEAN / VAR, frequencies count in N", {
  res <- sas_run(paste(means_data, "proc means data=d noprint; var x; freq f;
    output out=a n=n sum=s mean=m; run;
    proc means data=d noprint; var x; weight w; output out=b n=n sum=s mean=m var=v sumwgt=sw stderr=se; run;"))
  expect_equal(unlist(res$work$a[c("_freq_", "n", "s", "m")]), c(`_freq_` = 8, n = 8, s = 240, m = 30))
  b <- res$work$b
  expect_equal(b$s, 10 + 40 + 90 + 160 + 50)
  expect_equal(b$sw, 11)
  expect_equal(b$m, 350 / 11)
  css <- sum(c(1, 2, 3, 4, 1) * (c(10, 20, 30, 40, 50) - 350 / 11)^2)
  expect_equal(b$v, css / 4)
  expect_equal(b$se, sqrt(css / 4 / 11))
})

test_that("quantiles follow QNTLDEF (default 5) and weighted percentiles", {
  res <- sas_run("data q; do x = 1 to 10; output; end; run;
    proc means data=q noprint; var x; output out=a median=md q1=q1 q3=q3 p20=p20 p90=p90; run;
    proc means data=q noprint qntldef=1; var x; output out=b q1=q1; run;
    proc means data=q noprint qntldef=4; var x; output out=c q1=q1; run;")
  expect_equal(unlist(res$work$a[c("md", "q1", "q3", "p20", "p90")]), c(md = 5.5, q1 = 3, q3 = 8, p20 = 2.5, p90 = 9.5))
  expect_equal(res$work$b$q1, 2.5)
  expect_equal(res$work$c$q1, 2.75)
})

test_that("default OUT= is the _STAT_ long form; AUTONAME and empty names", {
  res <- sas_run(paste(means_data, "proc means data=d noprint; var x w; output out=o; run;
    proc means data=d noprint; var x; output out=a mean= max= / autoname; run;
    proc means data=d noprint; var x w; output out=b mean=; run;"))
  expect_equal(res$work$o$`_stat_`, c("N", "MIN", "MAX", "MEAN", "STD"))
  expect_equal(names(res$work$o), c("_type_", "_freq_", "_stat_", "x", "w"))
  expect_equal(res$meta$a$disp[["x_mean"]], "x_Mean")
  expect_equal(names(res$work$b), c("_type_", "_freq_", "x", "w"))
})

test_that("BY groups must be sorted; ID keeps the group maximum", {
  expect_error(sas_run(paste(means_data, "proc means data=d noprint; by g; var x; output out=o; run;")),
               class = "sasonr_runtime_error")
  res <- sas_run("data d; input id g $ x; datalines;
1 A 10
2 A 20
4 B 40
;
run;
proc means data=d noprint; class g; id id; var x; output out=o mean=m; run;")
  expect_equal(res$work$o$id, c(4, 2, 4))
  expect_equal(names(res$work$o), c("g", "id", "_type_", "_freq_", "m"))
})

test_that("confidence limits, t and p values; MAXDEC in the listing", {
  res <- sas_run("data d; do x = 2 to 10; output; end; run;
    proc means data=d n mean stderr clm t probt maxdec=3; var x; run;")
  expect_equal(squish(res$listing)[6], "9 6.000 0.913 3.895 8.105 6.57 0.0002")
  expect_equal(squish(res$listing)[3], "Lower 95% Upper 95%")
})

test_that("PROC SUMMARY needs PRINT or OUTPUT; unsupported options fail loud", {
  d <- "data a; x = 1; run;"
  expect_error(sas_run(paste(d, "proc summary data=a; var x; run;")), class = "sasonr_runtime_error")
  expect_equal(squish(sas_run(paste(d, "proc summary data=a print; var x; run;"))$listing)[1], "The SUMMARY Procedure")
  expect_error(sas_run(paste(d, "proc means data=a completetypes; var x; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc means data=a; var x; output out=o mean=m / levels; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(d, "proc means data=a; zzz x; run;")), class = "sasonr_syntax_error")
})

# Printed-table geometry: every value field is FW= columns wide whatever the
# value looks like, panels hold as many statistics as fit and the dashed rule is
# exactly the table width.  All expected values below are SAS 9.4 TS1M2 output
# for these exact programs (24 observations, so N prints in two columns).
panel_data <- "
data p;
  length g $8;
  do i = 1 to 24;
    x = i / 7 + 0.125;
    g = 'A'; if i > 12 then g = 'B';
    output;
  end;
  drop i;
run;
"
twelve <- "n mean std stderr min max median q1 q3 sum var cv"
rules_of <- function(res) {
  l <- squish(res$listing)
  l[grepl("^-+$", l)]
}
# heading lines: everything except the dashed rules and the data rows (a value
# row starts with a digit, '.' or '-' -- or with one of the two CLASS levels)
heads_of <- function(res) {
  l <- squish(res$listing)
  l[!grepl("^[0-9.<-]", l) & !grepl("^[AB] ", l)]
}

test_that("twelve statistics print in two panels of 10 + 2 at LINESIZE=132", {
  res <- sas_run(paste(panel_data, "proc means data=p", twelve, "; var x; run;"))
  expect_equal(heads_of(res), c(
    "The MEANS Procedure", "Analysis Variable : x", "Lower Upper",
    "N Mean Std Dev Std Error Minimum Maximum Median Quartile Quartile Sum",
    "Analysis Variable : x", "Coeff of", "Variance Variation"
  ))
  expect_equal(rules_of(res), c(rep(strrep("-", 128), 2L), rep(strrep("-", 28), 2L)))
})

test_that("MAXDEC= does not change the table geometry", {
  res <- sas_run(paste(panel_data, "proc means data=p maxdec=0", twelve, "; var x; run;"))
  expect_equal(rules_of(res), c(rep(strrep("-", 128), 2L), rep(strrep("-", 28), 2L)))
  res4 <- sas_run(paste(panel_data, "proc means data=p maxdec=4", twelve, "; var x; run;"))
  expect_equal(rules_of(res4), rules_of(res))
})

test_that("fewer statistics use wider gaps (16 columns apart, one panel)", {
  res <- sas_run(paste(panel_data, "proc means data=p n mean std min max; var x; run;"))
  expect_equal(rules_of(res), rep(strrep("-", 66), 2L))
  res9 <- sas_run(paste(panel_data, "options linesize=200; proc means data=p", twelve, "; var x; run;"))
  expect_equal(rules_of(res9), rep(strrep("-", 178), 2L))
})

test_that("LINESIZE= sets the panel width", {
  res <- sas_run(paste(panel_data, "options linesize=100; proc means data=p", twelve, "; var x; run;"))
  expect_equal(rules_of(res), c(rep(strrep("-", 100), 2L), rep(strrep("-", 60), 2L)))
  expect_equal(heads_of(res), c(
    "The MEANS Procedure", "Analysis Variable : x", "Lower",
    "N Mean Std Dev Std Error Minimum Maximum Median Quartile",
    "Analysis Variable : x", "Upper Coeff of", "Quartile Sum Variance Variation"
  ))
})

test_that("CLASS tables repeat the fixed columns in every panel", {
  res <- sas_run(paste(panel_data, "proc means data=p", twelve, "; class g; var x; run;"))
  expect_equal(heads_of(res), c(
    "The MEANS Procedure", "Analysis Variable : x", "N Lower Upper",
    "g Obs N Mean Std Dev Std Error Minimum Maximum Median Quartile Quartile",
    "Analysis Variable : x", "N Coeff of", "g Obs Sum Variance Variation"
  ))
  expect_equal(rules_of(res), c(rep(strrep("-", 129), 2L), rep(strrep("-", 59), 2L)))
})

test_that("undetermined panel layouts fail loud instead of printing a wrong rule", {
  expect_error(sas_run(paste(panel_data, "proc means data=p fw=8 n mean std min max; var x; run;")),
               class = "sasonr_unsupported")
  # a CLASS column keeps its declared length; SAS narrows one that does not fit
  expect_error(sas_run("data q; length c $200; do i = 1 to 10; c = 'A'; x = i; output; end; drop i; run;
    proc means data=q n mean std stderr min max median q1 q3 sum var cv; class c; var x; run;"),
               class = "sasonr_unsupported")
})

