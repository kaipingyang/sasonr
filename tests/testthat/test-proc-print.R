# PROC PRINT on the PROC registry: layout rule and the statements it supports.

pr_have <- "
data have;
  input grp $ sub $ x y;
  label x='X value' y='Y value';
  datalines;
a s1 1 10
a s2 2 20
b s1 3 30
b s2 4 40
;
run;
"
pr_lst <- function(code) {
  l <- sas_run(paste0(pr_have, code))$listing
  l[nzchar(l)]
}

test_that("fields are centred with floor, numbers right- and characters left-aligned", {
  expect_identical(pr_lst("proc print data=have; run;"), c(
    "Obs    grp    sub    x     y",
    " 1      a     s1     1    10",
    " 2      a     s2     2    20",
    " 3      b     s1     3    30",
    " 4      b     s2     4    40"))
})

test_that("a two-line heading centres its field with ceiling", {
  expect_identical(pr_lst("proc print data=have label; var x y; run;")[1:3], c(
    "         X        Y",
    "Obs    value    value",
    " 1       1        10"))
  expect_identical(pr_lst("data b; set have; label x='A*b'; run; proc print data=b split='*'; var x; run;")[1:2], c(
    "       A", "Obs    b"))
})

test_that("OBS=, NOOBS, ID and N", {
  expect_identical(pr_lst("proc print data=have obs='Row'; var x; run;")[1], "Row    x")
  expect_identical(pr_lst("proc print data=have noobs; var x; run;")[1:2], c("x", "1"))
  expect_identical(pr_lst("proc print data=have; id sub; var x; run;")[1:2], c("sub    x", "s1     1"))
  expect_identical(utils::tail(pr_lst("proc print data=have n; run;"), 1), "N = 4")
})

test_that("BY pages keep the observation numbers; SUM adds subtotals and the grand total", {
  l <- pr_lst("proc sort data=have; by grp; run; proc print data=have; by grp; sum x; sumby grp; var sub x; run;")
  expect_identical(l, c(
    "grp=a", "Obs    sub    x", "  1    s1     1", "  2    s2     2", "---           -", "grp           3",
    "grp=b", "Obs    sub     x", "  3    s1      3", "  4    s2      4", "---           --", "grp            7",
    "              ==", "              10"))
  expect_identical(utils::tail(pr_lst("proc print data=have; sum x y; run;"), 2), c(
    "                     ==    ===", "                     10    100"))
})

test_that("an empty data set prints nothing", {
  expect_identical(pr_lst("proc print data=have(where=(x > 9)); run;"), character())
})

test_that("PROC PRINT outside the measured rules fails loud", {
  fails <- function(code) expect_error(sas_run(paste0(pr_have, code)), class = "sasonr_unsupported")
  fails("proc print data=have; sum grp; run;")
  fails("proc print data=have; sum x; var y; run;")
  fails("proc print data=have n; sum x; run;")
  fails("proc print data=have; by grp sub; sum x; run;")
  fails("proc print data=have; id sub grp; run;")
  fails("data b; set have; label x='a b c d e f'; run; proc print data=b label; var x; run;")
  fails("proc print data=have double; run;")
})

test_that("a heading piece still longer than the column splits again at an underscore", {
  r <- sas_run("options nocenter; data h; abcd_efgh_ijkl = 1; Var_Mean_Std_Dev = 1; run; proc print data=h; run;")
  lst <- r$listing
  expect_identical(lst[2:4], c("       abcd_", "       efgh_    Var_Mean_", "Obs     ijkl     Std_Dev"))
})

test_that("a name with one lowercase-uppercase change splits only when longer than 4 and its values", {
  lst <- sas_run("options nocenter; data h; AbCd = 1; run; proc print data=h; run;")$listing
  expect_true("Obs    AbCd" %in% lst)
  expect_error(sas_run("data h; AbCdEf = 1; run; proc print data=h; run;"), class = "sasonr_unsupported")
})
