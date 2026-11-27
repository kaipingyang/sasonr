# Read-only libraries (LIBNAME): XPORT transport files.

# A transport file written by haven (version 5): member MEMBER, x = 1:3,
# s = "a", "b", "c" (a character variable of length 4).
xpt_file <- function() {
  if (!requireNamespace("haven", quietly = TRUE)) testthat::skip("haven is not installed")
  f <- withr::local_tempfile(fileext = ".xpt", .local_envir = parent.frame())
  d <- data.frame(x = c(1, 2, 3), s = c("a", "b", "c"))
  attr(d$s, "width") <- 4L
  haven::write_xpt(d, f, version = 5, name = "MEMBER")
  f
}

test_that("LIBNAME XPORT reads a member with its attributes; CLEAR deassigns", {
  f <- xpt_file()
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run(paste0("libname xi xport '", f, "';\ndata back; set xi.member; run;\nlibname xi clear;"))
  expect_identical(nrow(r$work$back), 3L)
  m <- as.character(sas_log(r, echo = FALSE))
  expect_true("NOTE: Libref XI was successfully assigned as follows:" %in% m)
  expect_true("NOTE: There were 3 observations read from the data set XI.MEMBER." %in% m)
  expect_true("NOTE: Libref XI has been deassigned." %in% m)
})

test_that("writing to a library, unknown engines and missing paths fail loud", {
  expect_error(sas_run("libname x 'nosuchdir_zz'; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("libname x json 'a.json';"), class = "sasonr_unsupported")
  d <- withr::local_tempdir()
  expect_error(sas_run(paste0("libname ro '", d, "'; data ro.new; x = 1; run;")), class = "sasonr_unsupported")
})
