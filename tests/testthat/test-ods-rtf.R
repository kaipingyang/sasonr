# ODS RTF (default style): the document of PROC PRINT tables.

rtf_prog <- function(body) paste0("data have; input grp $ x; datalines;\na 1\na 2\nb 3\n;\nrun;\n", body)

test_that("ODS RTF FILE= writes the document at ODS RTF CLOSE with its NOTE", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("rtf")
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run(rtf_prog("ods rtf file=\"rtf/out.rtf\";\nproc print data=have; run;\nods rtf close;"))
  expect_true(any(grepl("NOTE: Writing RTF Body file: rtf/out.rtf", as.character(sas_log(r)), fixed = TRUE)))
  x <- readLines("rtf/out.rtf")
  expect_identical(x[1], "{\\rtf1\\ansi\\ansicpg1252\\uc1\\deff0\\deflang1033\\deflangfe1033")
  expect_identical(sum(grepl("\\\\trowd", x)), 4L)
  expect_identical(sum(grepl("\\\\trhdr", x)), 1L)
  # the column boundaries of the measured rule: Obs 522, grp, x
  expect_identical(regmatches(paste(x, collapse = ""), gregexpr("cellx[0-9]+", paste(x, collapse = "")))[[1]][1:3],
                   c("cellx522", "cellx984", "cellx1222"))
})

test_that("ODS RTF outside the measured rules fails loud", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("rtf")
  fails <- function(body) expect_error(sas_run(rtf_prog(body)), class = "sasonr_unsupported")
  fails("ods rtf file=\"rtf/out.rtf\";\nproc print data=have; run;")
  fails("ods rtf file=\"rtf/out.rtf\" style=journal;\nproc print data=have; run;\nods rtf close;")
  fails("ods rtf file=\"rtf/out.rtf\";\nproc means data=have; var x; run;\nods rtf close;")
})

test_that("the N line widens the columns it spans (N with BY)", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("rtf")
  sas_run(rtf_prog("ods rtf file=\"rtf/out.rtf\";\nproc print data=have n; by grp; var x; run;\nods rtf close;"))
  x <- paste(readLines("rtf/out.rtf"), collapse = "")
  # x: 125 + floor(width of "N = 2" / 2 columns) = 358, so 522 + 358
  expect_true(all(c("cellx522", "cellx880") %in% regmatches(x, gregexpr("cellx[0-9]+", x))[[1]]))
  expect_identical(lengths(regmatches(x, gregexpr("N = [12]\\\\cell", x))), 2L)
})

test_that("a class heading row inside the row dimension merges the data cells below it", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("rtf")
  sas_run(rtf_prog("ods rtf file=\"rtf/out.rtf\";\nproc tabulate data=have; class grp; var x; table all grp, x*sum; run;\nods rtf close;"))
  x <- readLines("rtf/out.rtf")
  # the data cells of the first level start their merge on the grp heading row
  g <- grep("{grp\\cell}", x, fixed = TRUE)
  row_start <- max(grep("\\trowd", x[seq_len(g)], fixed = TRUE))
  expect_true(any(grepl("\\clvmgf", x[row_start:g], fixed = TRUE)))
  expect_true(any(grepl("{grp\\cell}", x, fixed = TRUE)))
})

test_that("a PROC PRINT table wider than the page splits into balanced column groups", {
  groups <- function(w) lengths(sasonr:::rtf_print_groups(w))
  expect_identical(groups(rep(462, 24)), 24L)
  expect_identical(groups(rep(462, 25)), c(18L, 7L))
  expect_identical(groups(rep(462, 52)), c(21L, 21L, 10L))
  expect_identical(groups(rep(987, 23)), c(9L, 9L, 5L))
  # balanced groups that would need one more group: filled up to the page
  expect_identical(groups(rep(810, 40)), c(14L, 14L, 12L))
  expect_identical(groups(rep(5748, 4)), c(2L, 2L))
})

test_that("each column group is a table of its own after an empty paragraph", {
  withr::local_dir(withr::local_tempdir())
  dir.create("rtf")
  sas_run(paste0("options ls=256; data a; length ", paste0("c", 1:28, collapse = " "), " $8; ",
                 paste0("c", 1:28, "='i';", collapse = " "), " run;\n",
                 "ods rtf file='rtf/out.rtf'; proc print data=a noobs; run; ods rtf close;"))
  x <- readLines("rtf/out.rtf")
  expect_length(grep("\\trhdr", x, fixed = TRUE), 2L)
  expect_true("\\pard\\par" %in% x)
  sas_run(paste0("options ls=256; data a; length ", paste0("c", 1:28, collapse = " "), " $8; ",
                 paste0("c", 1:28, "='i';", collapse = " "), " run;\n",
                 "ods rtf file='rtf/out.rtf'; proc print data=a; run; ods rtf close;"))
  x <- readLines("rtf/out.rtf")
  expect_length(grep("{Obs\\cell}", x, fixed = TRUE), 2L)
})

test_that("the Obs or ID column starts every group and counts once per group", {
  groups <- function(w, h) lengths(sasonr:::rtf_print_groups(w, h))
  expect_identical(groups(c(512, rep(462, 24)), 1L), c(18L, 8L))
  expect_identical(groups(c(247, rep(462, 28)), 1L), c(20L, 10L))
})

test_that("each column group ends with its own N line", {
  withr::local_dir(withr::local_tempdir())
  dir.create("rtf")
  sas_run(paste0("options ls=256; data a; length ", paste0("c", 1:28, collapse = " "), " $8; ",
                 paste0("c", 1:28, "='i';", collapse = " "), " run;\n",
                 "ods rtf file='rtf/out.rtf'; proc print data=a noobs n; run; ods rtf close;"))
  x <- readLines("rtf/out.rtf")
  expect_length(grep("{N = 1\\cell}", x, fixed = TRUE), 2L)
})
