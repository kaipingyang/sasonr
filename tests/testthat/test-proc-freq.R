squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x[nzchar(x)]
}

freq_data <- "
data d; input trt $ resp $ s n; datalines;
A Y 1 12
A N 1 5
B Y 1 6
B N 1 9
A Y 2 7
A N 2 8
B Y 2 3
B N 2 11
;
run;
"

test_that("one-way table listing, missing values and OUT=", {
  res <- sas_run("data d; input g; datalines;
1
1
2
.
;
run;
proc freq data=d; tables g / out=o; run;")
  expect_equal(squish(res$listing), c(
    "The FREQ Procedure", "Cumulative Cumulative", "g Frequency Percent Frequency Percent", strrep("-", 54),
    "1 2 66.67 2 66.67", "2 1 33.33 3 100.00", "Frequency Missing = 1"
  ))
  expect_equal(res$work$o$count, c(1, 2, 1))
  expect_equal(res$work$o$percent, c(NA, 200 / 3, 100 / 3))
  expect_equal(res$meta$o$labels$count, "Frequency Count")
})

test_that("crosstabulation listing and OUTPCT", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / outpct out=o; weight n; run;"))
  l <- squish(res$listing)
  expect_equal(l[2:8], c("Table of trt by resp", "trt resp", "Frequency|", "Percent |", "Row Pct |",
                         "Col Pct |N |Y | Total", "---------+--------+--------+"))
  expect_equal(l[9:12], c("A | 13 | 19 | 32", "| 21.31 | 31.15 | 52.46", "| 40.63 | 59.38 |", "| 39.39 | 67.86 |"))
  expect_equal(utils::tail(l, 2), c("Total 33 28 61", "54.10 45.90 100.00"))
  o <- res$work$o
  expect_equal(names(o), c("trt", "resp", "count", "percent", "pct_row", "pct_col"))
  expect_equal(o$pct_row[1], 13 / 32 * 100)
})

test_that("TABLES grouping syntax and LIST / n-way tables", {
  res <- sas_run(paste(freq_data, "proc freq data=d noprint; tables (trt resp)*s / out=o; run;
    proc freq data=d; tables s*trt*resp / list; run;"))
  expect_equal(names(res$work$o), c("resp", "s", "count", "percent"))
  expect_true("s trt resp Frequency Percent Frequency Percent" %in% squish(res$listing))
})

test_that("statistics not yet verified against SAS fail loud by default", {
  for (src in c("proc freq data=d; tables trt / binomial; weight n; run;",
                "proc freq data=d; by s; tables trt*resp / chisq; weight n; run;",
                "proc freq data=d; tables s*trt*resp / cmh; weight n; run;",
                "proc freq data=d; tables trt*resp / alpha=0.1; weight n; run;",
                "proc freq data=d; tables trt*resp; exact binomial; weight n; run;",
                "proc freq data=d; tables trt*resp / noprint; output out=o n; weight n; run;")) {
    expect_error(sas_run(paste(freq_data, src)), class = "sasonr_unsupported")
  }
  # plain frequency tables are unaffected; CHISQ / FISHER are verified
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / out=o outpct; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq fisher; exact fisher; weight n; run;")))
})

test_that("CHISQ / FISHER listing layout and the no-statistics cases (SAS gold)", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq; weight n; run;"))
  l <- res$listing
  expect_true("Statistic                     DF       Value      Prob" %in% l)
  expect_true(any(grepl("^Phi Coefficient {22}-?0\\.", l)))
  expect_true("       Fisher's Exact Test" %in% l)
  expect_true(any(grepl("^Two-sided Pr <= P {11}", l)))
  # FISHER alone also prints the chi-square table
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / fisher; weight n; run;"))
  expect_true(any(grepl("^Chi-Square ", res$listing)))
  # a level kept by WEIGHT / ZEROS with a zero row: no statistics
  res <- sas_run("data z; input a $ b $ w; datalines;
x p 3
x q 2
y p 0
y q 0
z p 4
z q 1
;
proc freq data=z; weight w / zeros; tables a*b / chisq; run;")
  expect_true("Row or column sum zero. No statistics computed for this table." %in% res$listing)
  expect_false(any(grepl("^Chi-Square ", res$listing)))
})

test_that("CHISQ, FISHER, CMH and BINOMIAL agree with reference computations", {
  # CMH and BINOMIAL are unverified against SAS: strict off, their warnings expected
  withr::local_options(sasonr.strict = FALSE)
  res <- withCallingHandlers(sas_run(paste(freq_data, "proc freq data=d noprint; tables trt*resp / chisq; weight n;
    output out=c chisq; run;
    proc freq data=d noprint; tables s*trt*resp / cmh; weight n; output out=m cmh; run;")),
    sasonr_unverified = function(w) invokeRestart("muffleWarning"))
  tb <- matrix(c(13, 20, 19, 9), 2, dimnames = list(c("A", "B"), c("N", "Y")))
  ref <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
  ref_aj <- stats::chisq.test(tb, correct = TRUE)
  c1 <- res$work$c
  expect_equal(c1$`_pchi_`, unname(ref$statistic))
  expect_equal(c1$p_pchi, ref$p.value)
  expect_equal(c1$`_ajchi_`, unname(ref_aj$statistic))
  e <- outer(rowSums(tb), colSums(tb)) / sum(tb)
  expect_equal(c1$`_lrchi_`, 2 * sum(tb * log(tb / e)))
  expect_equal(c1$`_mhchi_`, (sum(tb) - 1) / sum(tb) * unname(ref$statistic))
  expect_equal(abs(c1$`_phi_`), sqrt(unname(ref$statistic) / sum(tb)))
  arr <- array(c(12, 6, 5, 9, 7, 3, 8, 11), c(2, 2, 2))
  mh <- stats::mantelhaen.test(arr, correct = FALSE)
  expect_equal(res$work$m$`_cmhga_`, unname(mh$statistic))
  expect_equal(res$work$m$`_cmhcor_`, unname(mh$statistic))
  st <- sasonr:::freq_fisher(tb)
  expect_equal(st$xp2, stats::fisher.test(tb)$p.value)
  expect_equal(st$xpl, stats::fisher.test(tb, alternative = "less")$p.value)
  b <- sasonr:::freq_binomial(c(7, 13))
  ci <- stats::binom.test(7, 20)$conf.int
  expect_equal(c(b$xl_bin, b$xu_bin), as.numeric(ci))
  expect_equal(b$bin, 0.35)
})

test_that("listing of statistics and fail-loud surfaces", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq; weight n; run;"))
  l <- squish(res$listing)
  expect_true("Statistics for Table of trt by resp" %in% l)
  expect_true(any(startsWith(l, "Two-sided Pr <= P")))
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / relrisk; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt / expected; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt; output out=o binomial; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp; exact jt; run;")), class = "sasonr_unsupported")
})

test_that("an all-zero stratum kept by WEIGHT / ZEROS shows '.' for the total percent", {
  res <- sas_run("data z; input s $ a $ b $ w; datalines;
1 x p 3
1 y q 2
2 x p 0
2 y q 0
;
proc freq data=z; weight w / zeros; tables s*a*b; run;")
  l <- trimws(res$listing)
  expect_true(any(grepl("^\\. +\\. +\\.$", l)))
})

test_that("crosstab headings show variable labels; a long row heading wraps the column heading", {
  res <- sas_run("data d; input a b $; label a = 'Analysis Value' b = 'Planned Treatment'; datalines;
0 x
1 y
;
proc freq data=d; tables a*b; run;")
  expect_true("a(Analysis Value)" %in% res$listing)
  expect_true("          b(Planned Treatment)" %in% res$listing)
})
