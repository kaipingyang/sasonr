ods_example <- function() {
  x <- tibble::tibble(grp = c("a", "b"), NObs = c(2, 2), x_Mean = c(1.5, 3.5))
  cols <- sasonr:::ods_cols(
    name = c("grp", "NObs", "x_Mean"), label = c(NA, "N Obs", "Mean"),
    role = c("class", "stat", "stat"), var = c(NA, NA, "x"), stat = c(NA, "nobs", "mean")
  )
  header <- sasonr:::ods_header(
    level = c(1, 1, 1), text = list("grp", c("N", "Obs"), "Mean"), span = c(1, 1, 1),
    columns = list("grp", "NObs", "x_Mean"), role = c("class", "stat", "stat")
  )
  sasonr:::new_sasonr_ods_table(x, name = "Summary", label = "Summary statistics", path = "Means.Summary",
                             proc = "Means", template = "base.summary", cols = cols, header = header)
}

test_that("a sasonr_ods_table is a tibble with its metadata", {
  t <- ods_example()
  expect_s3_class(t, c("sasonr_ods_table", "tbl_df"))
  expect_identical(attr(t, "path"), "Means.Summary")
  expect_identical(t$x_Mean, c(1.5, 3.5))
  expect_invisible(sasonr:::validate_sasonr_ods_table(t))
})

test_that("the validator rejects inconsistent metadata", {
  t <- ods_example()
  attr(t, "path") <- character()
  expect_error(sasonr:::validate_sasonr_ods_table(t), "path")
  t <- ods_example()
  attr(t, "cols")$name[2] <- "nobs"
  expect_error(sasonr:::validate_sasonr_ods_table(t), "cols")
  t <- ods_example()
  attr(t, "header")$columns[[1]] <- "zz"
  expect_error(sasonr:::validate_sasonr_ods_table(t), "header")
})

test_that("print shows name, path, columns with roles and rows", {
  out <- capture.output(print(ods_example()))
  expect_identical(out[1:3], c("<sasonr_ods_table> Summary (Summary statistics)", "  path:     Means.Summary",
                               "  proc:     Means, template base.summary"))
  expect_true("    grp [class]" %in% out)
  expect_true("    x_Mean [stat] \"Mean\"" %in% out)
  expect_identical(out[length(out)], "  rows:     2")
})

test_that("r$ods holds the MEANS Summary tables in output order", {
  r <- sas_run("data a; input grp $ x; datalines;
a 1
a 2
b 3
;
run;
proc means data=a; class grp; var x; run;
proc means data=a noprint; var x; output out=o mean=m; run;
proc means data=a mean; by grp; var x; run;")
  expect_identical(r$status, "ok")
  expect_length(r$ods, 3L)
  expect_identical(vapply(r$ods, attr, "", "path"),
                   c("Means.Summary", "Means.ByGroup1.Summary", "Means.ByGroup2.Summary"))
  t1 <- r$ods[[1]]
  expect_identical(names(t1), c("grp", "NObs", "x_N", "x_Mean", "x_StdDev", "x_Min", "x_Max"))
  expect_identical(t1$x_Mean, c(1.5, 3))
  expect_identical(attr(r$ods[[3]], "by")$text, "grp=b")
  expect_identical(sasonr:::render_listing(t1, 132L)[1], "Analysis Variable : x ")
})
