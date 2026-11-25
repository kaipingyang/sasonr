# CALL EXECUTE: the queue runs after the DATA step, each step echoed as `N + text`.

ce_log <- function(code) {
  withr::local_options(sasonr.step_notes = TRUE)
  as.character(sas_log(sas_run(code)))
}

test_that("the queued entries run after the step, one echo line per step", {
  lg <- ce_log(paste0("data base; input g $ v; datalines;\na 1\nb 2\n;\nrun;\n",
                      "data _null_; set base; call execute('data o' || trim(g) || '; set base; where g=\"' || trim(g) || '\"; run;'); run;"))
  k <- which(lg == "NOTE: CALL EXECUTE generated line.")
  expect_length(k, 1L)
  expect_identical(lg[k - 1L], "NOTE: There were 2 observations read from the data set WORK.BASE.")
  expect_identical(lg[k + 1L], "1         + data oa; set base; where g=\"a\"; run;")
  expect_identical(lg[k + 2L], "")
  expect_true("2         + data ob; set base; where g=\"b\"; run;" %in% lg)
})

test_that("a second step of the same entry blanks what came before it, a newline counting as one", {
  r <- sas_run("data _null_; call execute('data a; x=1; run;' || '0A'x || 'data b; y=2; run;'); run;")
  lg <- as.character(sas_log(r))
  expect_true("1         + data a; x=1; run;" %in% lg)
  expect_true(paste0("1         + ", strrep(" ", 18), "data b; y=2; run;") %in% lg)
  expect_identical(r$work$b$y, 2)
})

test_that("CALL EXECUTE outside the supported text fails loud", {
  fails <- function(arg) {
    expect_error(sas_run(paste0("data _null_; call execute(", arg, "); run;")), class = "sasonr_unsupported")
  }
  fails("'data a; x=&y; run;'")
  fails("'%nrstr(%put x;)'")
  fails("1")
  fails("'title \"t\";'")
  fails("'data a; x=1;'")
  fails("' data a; x=1; run;'")
  fails("'data a; ' || '0A'x || 'x=1; run;'")
  fails("'proc print data=nosuch; run;'")
})

test_that("macro code in the argument runs at the call, the text it generates after the step", {
  lg <- ce_log(paste0("%macro m(v); %let acc=&acc.&v; %put now &acc; data _null_; put \"was &acc\"; run; %mend m;\n%let acc=;\n",
                      "data _null_; put 'a'; call execute('%m(A)'); x = symget('acc'); put x=; call execute('%m(B)'); run;"))
  i <- match(c("a", "now A", "x=A", "now AB", "NOTE: CALL EXECUTE generated line.",
               "1         + data _null_; put \"was A\"; run;", "was A",
               "2         + data _null_; put \"was AB\"; run;", "was AB"), lg)
  expect_false(anyNA(i))
  expect_false(is.unsorted(i))
})

test_that("macro code that generates no text writes its NOTE", {
  r <- sas_run("data _null_; call execute('%let q=1;'); run;\n%put q=&q;")
  lg <- as.character(sas_log(r))
  expect_true("NOTE: CALL EXECUTE routine executed successfully, but no SAS statements were generated." %in% lg)
  expect_true("q=1" %in% lg)
})

test_that("an entry without text keeps its number", {
  lg <- as.character(sas_log(sas_run("data _null_; call execute('%let w=5;'); call execute('data b; y=&w; run;'); run;")))
  expect_true("2         + data b; y=5; run;" %in% lg)
  expect_false(any(grepl("no SAS statements", lg)))
})

test_that("%NRSTR keeps a macro call for the time the entry runs: one echo line", {
  lg <- ce_log("%macro m(n);\ndata m&n;\n  z=&n;\nrun;\n%mend m;\ndata _null_; call execute('%nrstr(%m(1))'); run;")
  k <- which(lg == "1         + %m(1)")
  expect_length(k, 1L)
  expect_identical(lg[k + 1L], "")
  expect_identical(lg[k + 2L], "NOTE: The data set WORK.M1 has 1 observations and 1 variables.")
})

test_that("an ERROR in a generated step stops that step and the queue goes on", {
  withr::local_options(sasonr.on_error = "sas")
  lg <- ce_log("data _null_; call execute('proc print data=work.nosuch; run;'); call execute('data good; y=2; run;'); run;")
  k <- which(lg == "1         + proc print data=work.nosuch; run;")
  expect_length(k, 1L)
  expect_match(lg[k + 1L], "^ERROR: ")
  expect_true("2         + data good; y=2; run;" %in% lg)
  expect_true("NOTE: The data set WORK.GOOD has 1 observations and 1 variables." %in% lg)
})

test_that("a long echo breaks at the blank before the word that does not fit; continuation lines start in column 1", {
  withr::local_options(sasonr.step_notes = TRUE)
  code <- "options ls=80;\ndata _null_; call execute('data w; a=1; b=2; c=3; d=4; e=5; f=6; g=7; h=8; i=9; j=10; k=11; l=12; m=13; n=14; o=15; p=16; q=17; r=18; run;'); run;"
  lg <- as.character(sas_log(sas_run(code)))
  k <- grep("^1         \\+ data w;", lg)
  expect_identical(lg[k], "1         + data w; a=1; b=2; c=3; d=4; e=5; f=6; g=7; h=8; i=9; j=10; k=11;")
  expect_identical(lg[k + 1L], "l=12; m=13; n=14; o=15; p=16; q=17; r=18; run;")
})

test_that("a word longer than LINESIZE in the echo starts a new line and is cut", {
  f <- sasonr:::ce_echo_wrap("1         + ", paste0("put \"", strrep("x", 140), "\"; run;"), 132L, stop)
  expect_identical(f[1], "1         + put")
  expect_identical(nchar(f[2]), 132L)
  expect_identical(f[3], paste0(strrep("x", 9), "\"; run;"))
})
