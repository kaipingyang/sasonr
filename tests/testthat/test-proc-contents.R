squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x[nzchar(x)]
}

cdata <- "
data d;
  length name $ 20 city $ 15;
  label name = 'Person Name';
  format age 3.;
  name = 'Alice'; city = 'Rome'; age = 30;
run;
"

test_that("PROC CONTENTS variable table (alphabetic and VARNUM) and OUT=", {
  res <- sas_run(paste(cdata, "proc contents data=d; run; proc contents data=d varnum; run;
    proc contents data=d out=meta noprint; run;"))
  l <- squish(res$listing)
  i <- match("Alphabetic List of Variables and Attributes", l)
  expect_equal(l[i + 1:4], c("# Variable Type Len Format Label", "3 age Num 8 3.", "2 city Char 15", "1 name Char 20 Person Name"))
  j <- match("Variables in Creation Order", l)
  expect_equal(l[j + 2], "1 name Char 20 Person Name")
  m <- res$work$meta
  expect_equal(m$name, c("age", "city", "name"))
  expect_equal(m$type, c(1, 2, 2))
  expect_equal(m$length, c(8, 15, 20))
  expect_equal(m$varnum, c(3, 2, 1))
  expect_equal(m$npos, c(0, 28, 8))
  expect_equal(m$formatl, c(3, 0, 0))
  expect_equal(res$meta$meta$disp[["memname"]], "MEMNAME")
  expect_equal(length(res$meta$meta$vars), 41L)
})

test_that("PROC DATASETS: MODIFY attributes, RENAME, CHANGE, DELETE, APPEND, KILL", {
  res <- sas_run(paste(cdata, "data e; x = 1; run; data f; x = 2; run;
proc datasets lib=work nolist;
  modify d (label='People');
    format age 6.2;
    label city='Town' name=;
    rename age=years;
  change e=e2;
  delete nosuch;
  append base=e2 data=f;
quit;"))
  m <- res$meta$d
  expect_equal(m$formats$years$text, "F6.2")
  expect_equal(m$labels$city, "Town")
  expect_null(m$labels$name)
  expect_equal(names(res$work$d), c("name", "city", "years"))
  expect_equal(res$work$e2$x, c(1, 2))
  expect_false("e" %in% names(res$work))
  res2 <- sas_run("data a; x = 1; run; proc datasets lib=work kill nolist; quit; data b; y = 1; run;")
  expect_equal(names(res2$work), "b")
})

test_that("first appearance in FORMAT / ATTRIB fixes character length and stored name", {
  res <- sas_run("data d; format nm $char10.; nm = 'hi'; attrib SITEID length=$4; SITEID = 'S1'; length XA1-XA2 $3; run;")
  expect_equal(res$meta$d$len$nm, 10L)
  expect_equal(res$meta$d$disp$siteid, "SITEID")
  expect_equal(res$meta$d$disp$xa2, "XA2")
  expect_error(sas_run("data b; a = 'x'; length a 8; run;"), class = "sasonr_syntax_error")
})

test_that("unsupported CONTENTS / DATASETS surfaces fail loud", {
  expect_error(sas_run(paste(cdata, "proc contents data=_all_; run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(cdata, "proc datasets lib=work; quit;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(cdata, "proc datasets lib=work nolist; copy out=x; quit;")), class = "sasonr_unsupported")
})

test_that("Observation Length: padded to 8 only when there is an 8-byte numeric variable", {
  res <- sas_run("data a; length c $ 4; c = 'x'; run; proc contents data=a; run;
    data b; length c $ 3; x = 1; c = 'y'; run; proc contents data=b; run;
    data e; length x 5; x = 1; run; proc contents data=e; run;")
  l <- squish(res$listing)
  ol <- grep("Observation Length", l, value = TRUE)
  expect_equal(sub(".*Observation Length ", "", ol), c("4", "16", "5"))
})
