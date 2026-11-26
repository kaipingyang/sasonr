# OPTIONS CENTER: each centring unit of a listing moves right by
# floor((LINESIZE - longest line) / 2); TITLE lines, procedure titles and
# headings are units of one line; BY lines are written between dashes.

center_lst <- function(code) {
  withr::local_options(sasonr.center = TRUE)
  lst <- sas_run(code)$listing
  lst[nzchar(lst)]
}

test_that("a session starts at CENTER", {
  withr::local_options(sasonr.center = NULL)
  withr::local_envvar(SASONR_CENTER = NA)
  lst <- sas_run("options ls=80; data a; x = 1; y = 'abc'; run; proc print data=a; run;")$listing
  expect_identical(lst[nzchar(lst)], c(paste0(strrep(" ", 32), "Obs    x     y"), paste0(strrep(" ", 33), "1     1    abc")))
})

test_that("titles are centred line by line, the table as one unit", {
  lst <- center_lst(paste("data a; input name $ age; datalines;\nAlfred 14\n;\nrun;",
                          "title 'SQL title'; proc sql; select name, age from a; quit; title;"))
  expect_identical(lst, c(paste0(strrep(" ", 61), "SQL title"), paste0(strrep(" ", 57), "name           age"),
                          paste0(strrep(" ", 57), strrep("-", 18)), paste0(strrep(" ", 57), "Alfred          14")))
})

test_that("a BY line is written between dashes over the whole line", {
  lst <- center_lst("data a; length grp $ 2; grp = 'ab'; x = 1; run; proc print data=a noobs; by grp; run;")
  expect_identical(lst[1], paste0(strrep("-", 62), " grp=ab ", strrep("-", 62)))
})

test_that("the N line of PROC PRINT is centred on its own", {
  lst <- center_lst("data a; x = 1; run; proc print data=a n; run;")
  expect_identical(lst[length(lst)], paste0(strrep(" ", 63), "N = 1"))
})

test_that("output whose centring is not implemented fails loud under CENTER", {
  expect_error(center_lst("data a; x = 1; run; proc datasets lib=work nolist; contents data=a; quit;"),
               class = "sasonr_unsupported")
})
