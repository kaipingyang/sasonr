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
  # EXPECTED on a one-way table is accepted and ignored by SAS (case
  # ts1m2/freq_stats/oneway_expected), so it is not in this list
  for (src in c("proc freq data=d; by s; tables trt*resp / relrisk; weight n; run;",
                "proc freq data=d; tables trt*resp; exact relrisk; weight n; run;",
                "proc freq data=d; tables trt*resp / noprint; output out=o n; weight n; run;",
                "proc freq data=d; tables s*trt*resp / cmh1; weight n; run;")) {
    expect_error(sas_run(paste(freq_data, src)), class = "sasonr_unsupported")
  }
  # plain frequency tables are unaffected; CHISQ / FISHER / BINOMIAL, the
  # RELRISK / RISKDIFF tables, the multiway CMH summary, EXACT CHISQ for a 2x2
  # table and CHISQ / FISHER with BY groups are verified
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / out=o outpct; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq fisher; exact fisher; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt / binomial; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / relrisk riskdiff; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / riskdiff(cl=(wald newcombe ac ha)); exact riskdiff; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables s*trt*resp / cmh; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq; exact chisq; weight n; run;")))
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; by s; tables trt*resp / chisq fisher; weight n; run;")))
  # CMH is verified only for the multiway summary: a two-way table request,
  # SCORES= on a two-way table and exact statistics on a multiway table fail loud
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / cmh; weight n; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq scores=rank; weight n; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables s*trt*resp / chisq; exact fisher; weight n; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables s*trt*resp / chisq scores=ridit; weight n; run;")), class = "sasonr_unsupported")
  # EXACT CHISQ is verified only for the 2x2 exact enumeration
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables s*trt*resp / chisq; exact chisq; weight n; run;")), class = "sasonr_unsupported")
})

test_that("PROC FREQ option combinations outside the verified domain fail loud", {
  # one guard per audited silent-value risk (see the PROC FREQ registry note)
  err <- function(code, pattern) {
    e <- tryCatch(sas_run(code), error = function(e) e)
    expect_s3_class(e, "sasonr_unsupported")
    expect_match(conditionMessage(e), pattern, fixed = TRUE)
  }
  frac <- "data d; input trt $ resp $ cnt; datalines;
A Yes 10.5
A No 4.25
B Yes 6.75
B No 12.5
;
run;
proc freq data=d; weight cnt; tables trt*resp / %s; run;"
  # EXACT CHISQ enumerates integer tables only (like EXACT FISHER)
  err(sprintf(frac, "chisq; exact chisq"), "EXACT CHISQ with non-integer weights")
  # SCORES=RANK is verified only for the multiway CMH summary without CHISQ
  err(paste(freq_data, "proc freq data=d; weight n; tables s*trt*resp / chisq scores=rank; run;"), "SCORES=RANK")
  err(paste(freq_data, "proc freq data=d; weight n; tables s*trt*resp / cmh chisq scores=rank; run;"), "SCORES=RANK")
  # table scores of a numeric variable with the MISSING option are not verified
  miss_num <- "data d; input site $ trt $ sev w; datalines;
1 A 1 5
1 A 2 4
1 B 1 3
1 B 2 2
2 A 1 6
2 A 2 3
2 B 1 2
2 B 2 4
2 A . 3
;
run;"
  err(paste(miss_num, "proc freq data=d; weight w; tables trt*sev / chisq missing; run;"), "numeric table-score")
  err(paste(miss_num, "proc freq data=d; weight w; tables site*trt*sev / cmh missing; run;"), "numeric table-score")
  # EXACT CHISQ needs the asymptotic CHISQ option (the exact blocks hang off it)
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp; exact chisq; run;"), "EXACT CHISQ requires the CHISQ option")
  # MISSPRINT combinations whose missing-value semantics are not verified
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp / list missprint; run;"), "MISSPRINT with the LIST option")
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp / missprint relrisk; run;"), "MISSPRINT with RELRISK")
  miss_2x3 <- "data d; input a $ b $ w; datalines;
X P 3
X Q 4
X R 2
Y P 5
Y Q 2
Y R 4
. P 1
;
run;
proc freq data=d; weight w; tables a*b / chisq missprint; run;"
  err(miss_2x3, "MISSPRINT with statistics for a table that is not 2x2")
  # EXACT BINOMIAL enumerates integer tables only
  err("data d; input trt $ w; datalines;
A 10.5
B 12.25
B 3.5
;
run;
proc freq data=d; weight w; tables trt / binomial; exact binomial; run;",
      "EXACT BINOMIAL with non-integer weights")
  # CMH / RELRISK / RISKDIFF are verified with integer weights only
  frac_cmh <- "data d; input site $ trt $ resp $ cnt; datalines;
1 A Yes 5.5
1 A No 2
1 B Yes 3
1 B No 4.5
2 A Yes 3.5
2 A No 3.5
2 B Yes 2
2 B No 5.5
;
run;
proc freq data=d; weight cnt; tables site*trt*resp / %s; run;"
  err(sprintf(frac_cmh, "cmh"), "CMH statistics with non-integer weights")
  err(sprintf(frac, "relrisk"), "RELRISK / RISKDIFF with non-integer weights")
  err(sprintf(frac, "riskdiff"), "RELRISK / RISKDIFF with non-integer weights")
  # ALPHA= is verified only for the binomial confidence limits
  err(paste(freq_data, "proc freq data=d; weight n; tables s*trt*resp / cmh alpha=0.1; run;"), "ALPHA= with CMH")
  # Fisher with fractional frequencies is verified for 2x2 tables only
  frac_2x3 <- "data d; input trt $ resp $ cnt; datalines;
A L 3.5
A M 4.5
A H 2.5
B L 5.5
B M 4
B H 6.5
;
run;
proc freq data=d; weight cnt; tables trt*resp / fisher; run;"
  err(frac_2x3, "FISHER with non-integer weights for a table that is not 2x2")
  # BY groups: only the 2x2 CHISQ / FISHER listing is verified
  err(paste(freq_data, "proc freq data=d; by s; weight n; tables trt / chisq; run;"), "with BY groups")
  by_2x3 <- "data d; input site $ a $ b $ w; datalines;
1 X P 3
1 X Q 4
1 X R 2
1 Y P 5
1 Y Q 2
1 Y R 4
;
run;
proc sort data=d; by site; run;
proc freq data=d; by site; weight w; tables a*b / chisq; run;"
  err(by_2x3, "only CHISQ / FISHER for a 2x2 table")
  # negative WEIGHT values (plain frequency table; statistics already aborted)
  err("data d; input a $ w; datalines;
A 3
B -1
A 2
;
run;
proc freq data=d; weight w; tables a; run;", "negative weights")
  # two confidence-limit tables in one listing are not verified
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp / relrisk(cl=(wald)) riskdiff(cl=(wald)); run;"),
      "two confidence-limit tables")
  # EXACT RISKDIFF layout is verified only together with RISKDIFF(CL=)
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp / riskdiff; exact riskdiff; run;"),
      "EXACT RISKDIFF without the RISKDIFF(CL=)")
  # a zero cell with the RISKDIFF(CL=) table is not verified
  err("data d; input trt $ resp $ cnt; datalines;
A Yes 0
A No 6
B Yes 5
B No 4
;
run;
proc freq data=d; weight cnt; tables trt*resp / riskdiff(cl=(wald)); run;",
      "RISKDIFF CL= with a zero cell")
  # statistics after a LIST table are not verified
  err(paste(freq_data, "proc freq data=d; weight n; tables trt*resp / list chisq; run;"), "LIST option")
  err(paste(freq_data, "proc freq data=d; weight n; tables s*trt*resp / list cmh; run;"), "LIST option")
})

test_that("PROC FREQ fractional weights: CHISQ runs, FISHER is not computed", {
  code <- "data d; input trt $ resp $ cnt; datalines;
A Yes 10.5
A No 4.25
B Yes 6.75
B No 12.5
;
run;
proc freq data=d; weight cnt; tables trt*resp / chisq; run;"
  l <- squish(sas_run(code)$listing)
  expect_true(any(grepl("^A \\| 4\\.25 \\| 10\\.5 \\| 14\\.75$", l)))
  expect_true("Chi-Square 1 4.3594 0.0368" %in% l)
  expect_false("Fisher's Exact Test" %in% l)
  l <- squish(sas_run(sub("chisq", "fisher", code))$listing)
  expect_true("Chi-Square 1 4.3594 0.0368" %in% l)
  expect_false("Fisher's Exact Test" %in% l)
  expect_identical(utils::tail(l, 1), "Sample Size = 34")
})

test_that("PROC FREQ degenerate table, BY groups, EXACT CHISQ and TESTP/TESTF", {
  # a single-row table prints no statistics block at all (c32)
  res <- sas_run("data d; input trt $ cat $ cnt; datalines;
A X 4
A Y 7
A Z 2
;
run;
proc freq data=d; weight cnt; tables trt*cat / chisq; run;")
  l <- squish(res$listing)
  expect_identical(utils::tail(l, 1), "30.77 53.85 15.38 100.00")
  expect_false(any(startsWith(l, "Statistics for Table")))
  # BY groups repeat the procedure title and print the statistics per group (c34)
  res <- sas_run("data d; input site $ trt $ resp $ cnt; datalines;
S1 A Yes 8
S1 A No 3
S1 B Yes 4
S1 B No 9
S2 A Yes 2
S2 A No 6
S2 B Yes 1
S2 B No 7
;
run;
proc sort data=d; by site; run;
proc freq data=d; weight cnt; by site; tables trt*resp / chisq fisher; run;")
  l <- squish(res$listing)
  expect_true(sum(l == "The FREQ Procedure") >= 2L)
  expect_true("site=S1" %in% l)
  expect_true("site=S2" %in% l)
  expect_true("Chi-Square 1 4.1958 0.0405" %in% l)
  expect_true("Chi-Square 1 0.4103 0.5218" %in% l)
  # EXACT CHISQ adds the three exact-test blocks (c08)
  res <- sas_run("data d; input trt $ resp $ cnt; datalines;
A Yes 12
A No 5
B Yes 7
B No 15
;
run;
proc freq data=d; weight cnt; tables trt*resp / chisq; exact chisq; run;")
  l <- squish(res$listing)
  expect_true("Pearson Chi-Square Test" %in% l)
  expect_true("Likelihood Ratio Chi-Square Test" %in% l)
  expect_true("Mantel-Haenszel Chi-Square Test" %in% l)
  expect_true("Exact Pr >= ChiSq 0.0248" %in% l)
  expect_true("Fisher's Exact Test" %in% l)
  # one-way CHISQ(TESTP=) / CHISQ(TESTF=) (c48, c49)
  res <- sas_run("data d; input cat $ cnt; datalines;
A 18
B 27
C 35
;
run;
proc freq data=d; weight cnt; tables cat / chisq testp=(0.2 0.3 0.5); run;")
  l <- squish(res$listing)
  expect_true("Test Cumulative Cumulative" %in% l)
  expect_true(any(grepl("^A 18 22\\.50 20\\.00 18 22\\.50$", l)))
  expect_true("for Specified Proportions" %in% l)
  expect_true("Chi-Square 1.2500" %in% l)
  l <- squish(sas_run("data d; input cat $ cnt; datalines;
A 18
B 27
C 35
;
run;
proc freq data=d; weight cnt; tables cat / chisq testf=(10 30 40); run;")$listing)
  expect_true(any(grepl("^A 18 10 22\\.50 18 22\\.50$", l)))
  expect_true("for Specified Frequencies" %in% l)
  expect_true("Chi-Square 7.3250" %in% l)
})

test_that("PROC FREQ MISSPRINT lists the missing level but excludes it (c18)", {
  res <- sas_run("data d; input trt $ resp $; datalines;
A Yes
A Yes
A No
A Yes
A .
A No
A Yes
B No
B No
B Yes
. Yes
B No
B .
B No
A Yes
. No
B Yes
B No
A Yes
B No
A No
B No
. .
A Yes
B Yes
;
run;
proc freq data=d; tables trt*resp / chisq missprint; run;")
  l <- squish(res$listing)
  expect_true("Col Pct | |No |Yes | Total" %in% l)
  expect_true("| 1 | 1 | 1 | ." %in% l)
  expect_true(any(grepl("^A \\| 1 \\| 3 \\| 7 \\| 10$", l)))
  expect_true(any(grepl("^Total \\. 10 10 20$", l)))
  expect_true("Chi-Square 1 3.2000 0.0736" %in% l)
  expect_true("Sample Size = 20" %in% l)
  expect_true("WARNING: 20% of the data are missing." %in% l)
})

test_that("PROC FREQ CMH summary: statistics, common OR/RR, Breslow-Day, scores", {
  # the c51 table (3 strata of a 2x2 table): TS1M7 gold values
  code <- "data d; input site $ trt $ resp $ cnt; datalines;
S1 A Yes 10
S1 A No 5
S1 B Yes 6
S1 B No 9
S2 A Yes 7
S2 A No 7
S2 B Yes 4
S2 B No 11
S3 A Yes 12
S3 A No 3
S3 B Yes 9
S3 B No 6
;
run;
proc freq data=d; weight cnt; tables site*trt*resp / cmh; run;"
  res <- sas_run(code)
  l <- squish(res$listing)
  expect_true(sum(l == "The FREQ Procedure") >= 2L)
  expect_true("Summary Statistics for trt by resp" %in% l)
  expect_true("Controlling for site" %in% l)
  expect_true("Cochran-Mantel-Haenszel Statistics (Based on Table Scores)" %in% l)
  expect_true("Statistic Alternative Hypothesis DF Value Prob" %in% l)
  expect_true("1 Nonzero Correlation 1 5.0567 0.0245" %in% l)
  expect_true("2 Row Mean Scores Differ 1 5.0567 0.0245" %in% l)
  expect_true("3 General Association 1 5.0567 0.0245" %in% l)
  expect_true("Common Odds Ratio and Relative Risks" %in% l)
  expect_true("Odds Ratio Mantel-Haenszel 0.3557 0.1450 0.8730" %in% l)
  expect_true("Logit 0.3556 0.1449 0.8727" %in% l)
  expect_true(any(grepl("^Relative Risk \\(Column 1\\) Mantel-Haenszel 0\\.5949 ", l)))
  expect_true("Breslow-Day Test for" %in% l)
  expect_true("Chi-Square 0.0121" %in% l)
  expect_true("DF 2" %in% l)
  expect_true("Pr > ChiSq 0.9940" %in% l)
  expect_identical(utils::tail(l, 1), "Total Sample Size = 89")
  # per-stratum CHISQ blocks (c56 gold layout)
  res <- sas_run(sub("cmh", "cmh chisq", code))
  l <- squish(res$listing)
  expect_true("Statistics for Table 1 of trt by resp" %in% l)
  expect_true("Controlling for site=S1" %in% l)
  expect_true("Chi-Square 1 2.1429 0.1432" %in% l)
  expect_true("Sample Size = 30" %in% l)
  expect_identical(utils::tail(l, 1), "Total Sample Size = 89")
  # SCORES=RANK: rank scores are computed within each stratum (c55 gold values)
  code2 <- "data d; input site $ dose sev cnt; datalines;
S1 1 1 7
S1 1 2 4
S1 1 3 2
S1 2 1 5
S1 2 2 6
S1 2 3 4
S1 3 1 2
S1 3 2 5
S1 3 3 8
S2 1 1 9
S2 1 2 3
S2 1 3 1
S2 2 1 6
S2 2 2 5
S2 2 3 3
S2 3 1 3
S2 3 2 4
S2 3 3 7
;
run;
proc freq data=d; weight cnt; tables site*dose*sev / cmh scores=rank; run;"
  res <- sas_run(code2)
  l <- squish(res$listing)
  expect_true("Cochran-Mantel-Haenszel Statistics (Based on Rank Scores)" %in% l)
  expect_true("1 Nonzero Correlation 1 14.1941 0.0002" %in% l)
  expect_true("2 Row Mean Scores Differ 2 14.3188 0.0008" %in% l)
  expect_true("3 General Association 4 15.1896 0.0043" %in% l)
  expect_identical(utils::tail(l, 1), "Total Sample Size = 84")
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

test_that("CHISQ, FISHER, CMH and BINOMIAL print the SAS 9.4 (TS1M7) values", {
  # the printed lines below are copied from the SAS gold listings of
  # freq_stats/c01_w22_chisq, c51_cmh_2x2x3 and c41_bin_exact (strict mode:
  # the statistics are verified in the TS1M7 registry)
  # (compared with runs of blanks squished, as the conformance listing layer does)
  has <- function(res, lines) for (l in lines) expect_true(squish(l) %in% squish(res$listing), info = l)
  r1 <- sas_run("data d; input trt $ resp $ cnt; datalines;
A Yes 12
A No 5
B Yes 7
B No 15
;
proc freq data=d; weight cnt; tables trt*resp / chisq; run;")
  has(r1, c("Chi-Square                     1      5.7696    0.0163",
            "Likelihood Ratio Chi-Square    1      5.9211    0.0150",
            "Continuity Adj. Chi-Square     1      4.3221    0.0376",
            "Mantel-Haenszel Chi-Square     1      5.6217    0.0177",
            "Phi Coefficient                      -0.3846",
            "Left-sided Pr <= F          0.0182",
            "Two-sided Pr <= P           0.0248"))
  r2 <- sas_run("data d; input site $ trt $ resp $ cnt; datalines;
S1 A Yes 10
S1 A No 5
S1 B Yes 6
S1 B No 9
S2 A Yes 7
S2 A No 7
S2 B Yes 4
S2 B No 11
S3 A Yes 12
S3 A No 3
S3 B Yes 9
S3 B No 6
;
proc freq data=d; weight cnt; tables site*trt*resp / cmh; run;")
  has(r2, c("1        Nonzero Correlation        1      5.0567    0.0245",
            "3        General Association        1      5.0567    0.0245",
            "Odds Ratio                  Mantel-Haenszel        0.3557        0.1450        0.8730",
            "Chi-Square          0.0121"))
  r3 <- sas_run("data d; input resp $ cnt; datalines;
Yes 13
No 27
;
proc freq data=d; weight cnt; tables resp / binomial(p=0.3 level='Yes'); exact binomial; run;")
  has(r3, c("Proportion (P)               0.3250",
            "95% Lower Conf Limit         0.1857",
            "Z                            0.3450",
            "Two-sided = 2 * One-sided    0.8456"))
})

test_that("listing of statistics and fail-loud surfaces", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq; weight n; run;"))
  l <- squish(res$listing)
  expect_true("Statistics for Table of trt by resp" %in% l)
  expect_true(any(startsWith(l, "Two-sided Pr <= P")))
  # one-way tables accept and ignore the cell statistics options
  expect_silent(sas_run(paste(freq_data, "proc freq data=d; tables trt / expected; run;")))
  # EXACT RELRISK is not verified: the TS1M7 gold value is not reproducible
  # from the documented tail-method score statistic, so it fails loud
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / relrisk(cl=(wald score)); exact relrisk; weight n; run;")),
               class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt; output out=o binomial; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp; exact jt; run;")), class = "sasonr_unsupported")
  # only the default COLUMN=1 confidence-limit tables are implemented
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / relrisk(cl=(wald) column=2); weight n; run;")),
               class = "sasonr_unsupported")
  expect_error(sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / riskdiff(cl=(wald) column=2); weight n; run;")),
               class = "sasonr_unsupported")
})

test_that("PROC FREQ RISKDIFF / RELRISK print the risk estimates and the OR/RR table", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / relrisk riskdiff; weight n; run;"))
  l <- squish(res$listing)
  # table fixture: A 32 (13 N / 19 Y), B 29 (20 N / 9 Y), total 61
  expect_true("Column 1 Risk Estimates" %in% l)
  expect_true("Column 2 Risk Estimates" %in% l)
  expect_true("Risk ASE Confidence Limits Confidence Limits" %in% l)
  expect_true(any(grepl("^Row 1 0\\.406", l)))
  expect_true(any(grepl("^Difference -0\\.283", l)))
  expect_true("Difference is (Row 1 - Row 2)" %in% l)
  expect_true("Odds Ratio and Relative Risks" %in% l)
  expect_true(any(grepl("^Odds Ratio 0\\.3079 ", l)))
  expect_true(any(grepl("^Relative Risk \\(Column 1\\) 0\\.5891 ", l)))
  expect_equal(utils::tail(l, 1), "Sample Size = 61")
  # RISKDIFF(CL=...) adds the confidence-limits table for the first column
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / riskdiff(cl=(wald ac ha newcombe)); weight n; run;"))
  l <- squish(res$listing)
  expect_true("Confidence Limits for the Risk Difference" %in% l)
  expect_true(any(grepl("^Risk Difference = -0\\.283", l)))
  expect_true(any(grepl("^Agresti-Caffo ", l)))
  expect_true(any(grepl("^Newcombe ", l)))
  expect_true(any(grepl("^Column 1 \\(resp = N\\)$", l)))
  # EXACT RISKDIFF adds the exact limits to the difference row and the CL table
  res <- sas_run("data e; input a $ b $ w; datalines;
A N 5
A Y 12
B N 15
B Y 7
;
run;
proc freq data=e; weight w; tables a*b / riskdiff(cl=(wald exact)); exact riskdiff; run;")
  l <- squish(res$listing)
  expect_true(any(grepl("^Exact confidence limits for the risk difference are based on a score statistic\\.$", l)))
  expect_true(any(grepl("^Exact \\(Score\\) -0\\.6502 -0\\.0587$", l)))
  expect_true(any(grepl("^Difference -0\\.3877 0\\.1486 -0\\.6789 -0\\.0965 -0\\.6502 -0\\.0587$", l)))
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

test_that("PROC FREQ BINOMIAL prints the proportion, limits and the test", {
  code <- "
data d; input resp $ cnt; datalines;
No 27
Yes 13
;
run;
proc freq data=d; weight cnt; tables resp / binomial(level='Yes'); run;"
  lst <- squish(sas_run(code)$listing)
  expect_true(any(grepl("Binomial Proportion", lst)))
  expect_true(any(grepl("resp = Yes", lst)))
  expect_true(any(grepl("^Proportion 0\\.3250$", lst)))
  expect_true(any(grepl("^ASE 0\\.0741$", lst)))
  expect_true(any(grepl("95% Lower Conf Limit 0\\.1799$", lst)))
  expect_true(any(grepl("Exact Conf Limits", lst)))
  expect_true(any(grepl("95% Upper Conf Limit 0\\.4913$", lst)))
  expect_true(any(grepl("^Z -2\\.2136$", lst)))
  expect_true(any(grepl("Two-sided Pr > \\|Z\\| 0\\.0269$", lst)))
  expect_true(any(grepl("Sample Size = 40", lst)))
})

test_that("PROC FREQ BINOMIAL confidence limit types and P= option", {
  code <- "
data d; input resp $ cnt; datalines;
No 27
Yes 13
;
run;
proc freq data=d; weight cnt; tables resp / binomial(level='Yes' p=0.3 cl=(wald wilson)); run;"
  lst <- squish(sas_run(code)$listing)
  expect_true(any(grepl("Confidence Limits for the Binomial Proportion", lst)))
  expect_true(any(grepl("^Test of H0: Proportion = 0.3$", lst)))
  expect_true(any(grepl("^Wald 0\\.1799 0\\.4701$", lst)))
  expect_true(any(grepl("^Wilson ", lst)))
})
test_that("PROC FREQ EXPECTED / DEVIATION / CELLCHI2 add the crosstab cell rows", {
  res <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq expected deviation cellchi2; weight n; run;"))
  l <- squish(res$listing)
  expect_true(any(grepl("^Expected", l)))
  expect_true(any(grepl("^Deviation", l)))
  expect_true(any(grepl("^Cell Chi-Square", l)))
  # fixture totals: A row 32, N column 33, 61 observations -> A/N expected
  # 17.311, deviation -4.311 (BEST6 in the crosstab)
  expect_true(any(grepl("17\\.311", l)))
  expect_true(any(grepl("-4\\.311", l)))
  # the plain crosstab still has no Expected row
  res2 <- sas_run(paste(freq_data, "proc freq data=d; tables trt*resp / chisq; weight n; run;"))
  expect_false(any(grepl("^Expected", squish(res2$listing))))
})
