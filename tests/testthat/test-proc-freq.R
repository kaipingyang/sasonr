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

test_that("CHISQ, FISHER, CMH and BINOMIAL agree with reference computations", {
  res <- sas_run(paste(freq_data, "proc freq data=d noprint; tables trt*resp / chisq; weight n;
    output out=c chisq; run;
    proc freq data=d noprint; tables s*trt*resp / cmh; weight n; output out=m cmh; run;"))
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
