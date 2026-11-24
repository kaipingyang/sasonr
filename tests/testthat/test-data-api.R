# Data-level PROC functions: the same tables as the PROC step in sas_run().

api_d <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))
# the reference: the PROC step run by sas_run() on a session holding the same data
api_ref <- function(code) sas_run(code, session = sas_session(work = list(data = api_d)))

test_that("sas_means_df() returns the table of the same PROC MEANS step", {
  m <- sas_means_df(api_d, var = "x", class = "grp", stats = c("n", "mean"))
  r <- api_ref("proc means data=data n mean; class grp; var x; run;")
  expect_s3_class(m, "sasonr_ods_table")
  expect_identical(as.data.frame(m), as.data.frame(r$ods[[1]]))
  expect_identical(as.data.frame(m)$x_Mean, c(1.5, 4))
})

test_that("BY groups give a list of tables", {
  b <- sas_means_df(api_d, var = "x", by = "grp")
  expect_s3_class(b, "sasonr_ods_list")
  expect_length(b, 2L)
})

test_that("sas_tabulate_df() and sas_transpose_df()", {
  t <- sas_tabulate_df(api_d, "grp all, x*(n sum)", class = "grp", var = "x")
  r <- api_ref("proc tabulate data=data; class grp; var x; table grp all, x*(n sum); run;")
  expect_identical(as.data.frame(t), as.data.frame(r$ods[[1]]))
  tr <- sas_transpose_df(api_d, var = "x")
  expect_identical(tr$col3, 4)
})

test_that("sas_log() and sas_html() take the result", {
  m <- sas_means_df(api_d, var = "x")
  expect_s3_class(sas_log(m), "sasonr_log")
  expect_length(sas_log(m), 0L)
  expect_match(sas_html(m), "<table", fixed = TRUE)
  # the WARNINGs of the computation are its log
  t <- sas_tabulate_df(api_d, "grp*grp", class = "grp")
  expect_match(as.character(sas_log(t)), "crossed with itself", fixed = TRUE)
})

test_that("arguments are checked", {
  expect_error(sas_means_df(api_d, var = "x; run; data y"), class = "sasonr_error")
  expect_error(sas_means_df(list(x = 1), var = "x"), class = "sasonr_error")
  expect_error(sas_tabulate_df(api_d, c("a", "b")), class = "sasonr_error")
})

test_that("sas_freq_df() gives the OneWayFreqs table of the program", {
  d <- data.frame(g = c("a", "a", "b", NA), w = c(1, 2, 3, 4))
  tb <- sas_freq_df(d, "g")
  expect_s3_class(tb, "sasonr_ods_table")
  expect_identical(attr(tb, "name"), "OneWayFreqs")
  expect_identical(attr(tb, "path"), "Freq.Table1.OneWayFreqs")
  expect_equal(tb$Frequency, c(2, 1))
  expect_equal(tb$CumPercent, c(200 / 3, 100))
  r <- sas_run("data d; input g $ w; datalines;\na 1\na 2\nb 3\n. 4\n;\nrun;\nproc freq data=d; tables g; run;")
  expect_equal(as.data.frame(r$ods[[1]])[-1], as.data.frame(tb)[-1])
  tw <- sas_freq_df(d, "g", weight = "w")
  expect_equal(tw$Frequency, c(3, 3))
  two <- sas_freq_df(d, "g w")
  expect_s3_class(two, "sasonr_ods_list")
  expect_identical(vapply(two, attr, "", "path"), c("Freq.Table1.OneWayFreqs", "Freq.Table2.OneWayFreqs"))
})

test_that("sas_freq_df() fails loud on output without an ODS table yet", {
  d <- data.frame(g = c("a", "a", "b"), h = c("x", "y", "y"), k = c("u", "u", "v"))
  ct <- sas_freq_df(d, "g*h")
  expect_identical(attr(ct, "name"), "CrossTabFreqs")
  expect_identical(ct[["_TYPE_"]], c("11", "11", "10", "11", "11", "10", "01", "01", "00"))
  expect_equal(ct$Frequency, c(1, 1, 2, 0, 1, 1, 1, 2, 3))
  st <- sas_freq_df(d, "g*h / chisq fisher")
  expect_identical(vapply(st, attr, "", "name"), c("CrossTabFreqs", "ChiSq", "FishersExact"))
  expect_identical(st[[2]]$Statistic[1], "Chi-Square")
  expect_identical(st[[3]]$Name, c("FISHER_F", "XPL_FISH", "XPR_FISH", "XPT_FISH", "XP2_FISH"))
  expect_identical(vapply(sas_freq_df(d, "g*h*k"), attr, "", "name"), c("CrossTabFreqs", "CrossTabFreqs"))
  expect_identical(attr(sas_freq_df(d, "g*h / list"), "name"), "List")
  expect_identical(attr(sas_freq_df(d, "g*h / relrisk")[[2]], "name"), "RelativeRisks")
  expect_identical(vapply(sas_freq_df(d, "g / chisq"), attr, "", "name"), c("OneWayFreqs", "OneWayChiSq"))
  expect_identical(vapply(sas_freq_df(d, "g / binomial"), attr, "", "name"),
                   c("OneWayFreqs", "Binomial", "BinomialCLs", "BinomialTest"))
  expect_identical(vapply(sas_freq_df(d, "g / binomial chisq"), attr, "", "name"),
                   c("OneWayFreqs", "OneWayChiSq", "Binomial", "BinomialCLs", "BinomialTest"))
  expect_identical(attr(sas_freq_df(d, "g", options = "nlevels")[[1]], "name"), "NLevels")
  expect_match(sas_html(sas_freq_df(d, "g*h")), "Table\u00a0of\u00a0g\u00a0by\u00a0h", fixed = TRUE)
  expect_match(sas_html(sas_freq_df(d, "g / binomial")), "Test of H0: Proportion = 0.5", fixed = TRUE)
  d2 <- data.frame(g = c("a", "a", "b", "b", "a", "b"), h = c("x", "y", "x", "y", "x", "y"))
  expect_match(sas_html(sas_freq_df(d2, "g*h / relrisk(cl=wald)")), "Relative Risk =", fixed = TRUE)
})

test_that("sas_html() fails loud on PROC PRINT and PROC FREQ output instead of leaving it out", {
  r <- sas_run("data a; input g $ x; datalines;\na 1\nb 2\n;\nrun;\nproc means data=a; var x; run;\nproc print data=a; run;")
  expect_error(sas_html(r), "the HTML output of PROC PRINT \\(SAS line 7\\)", class = "sasonr_unsupported")
  # FREQ tables without a measured HTML view
  r3 <- sas_run("data a; input s $ g $ h $; datalines;\n1 a x\n1 b y\n2 a y\n;\nrun;\nproc freq data=a; tables s*g*h / chisq; run;")
  expect_match(sas_html(r3), "Controlling for s=1", fixed = TRUE)
  r4 <- sas_run("data a; input g $ h $; datalines;\na x\nb y\na y\nb x\n;\nrun;\nproc freq data=a; tables g / binomial; exact binomial; run;")
  expect_error(sas_html(r4), "binomial test", class = "sasonr_unsupported")
  r2 <- sas_run("data a; x = 1; run;\nods html close;\nproc print data=a; run;\nods html;\nproc means data=a; var x; run;")
  expect_match(sas_html(r2), "sas-table|<table", all = FALSE)
})

test_that("the multiway CMH summary is CMH, CommonRelRisks and BreslowDayTest tables", {
  r <- sas_run(paste0("data a; input s $ g $ h $ n; datalines;\n",
                      "1 a x 3\n1 a y 5\n1 b x 7\n1 b y 2\n2 a x 4\n2 a y 4\n2 b x 6\n2 b y 1\n;\nrun;\n",
                      "proc freq data=a; weight n; tables s*g*h / cmh; run;"))
  expect_true(any(grepl("Breslow-Day", r$listing, fixed = TRUE)))
  nm <- vapply(r$ods, attr, "", "name")
  expect_identical(nm, c("CrossTabFreqs", "CrossTabFreqs", "CMH", "CommonRelRisks", "BreslowDayTest"))
  expect_identical(r$ods[[4]]$Method, rep(c("Mantel-Haenszel", "Logit"), 3L))
})

test_that("TABLES / NOPRINT keeps the statistics and drops the frequency table", {
  prog <- "data a; input g $ h $; datalines;\na x\na y\nb y\nb y\n;\nrun;\nproc freq data=a; tables g*h / chisq noprint; run;"
  r <- sas_run(prog)
  expect_identical(vapply(r$ods, attr, "", "name"), c("ChiSq", "FishersExact"))
  expect_false(any(grepl("Table of g by h", r$listing, fixed = TRUE) & !grepl("Statistics", r$listing, fixed = TRUE)))
  withr::local_options(sasonr.step_notes = TRUE)
  r2 <- sas_run(sub("chisq noprint", "noprint", prog, fixed = TRUE))
  expect_length(r2$listing, 0L)
  expect_true(any(grepl("no valid requests for displayed output", as.character(sas_log(r2)), fixed = TRUE)))
  expect_false(any(grepl("observations read", as.character(sas_log(r2)), fixed = TRUE)))
})
