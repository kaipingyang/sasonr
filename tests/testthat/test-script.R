script_prog <- "
options nonotes;
title 'Ages';
data adsl;
  input usubjid $ age sex $;
  length grp $4;
  if age >= 65 then grp = 'old'; else grp = 'yng';
  datalines;
a 70 F
b 40 M
c 66 M
;
run;
options notes;
proc sort data=adsl out=srt nodupkey; by descending age; run;
proc means data=srt noprint nway; class sex; var age; output out=m mean=mage; run;
proc sql; create table c as select * from srt where age > 50; quit;
data _null_; set srt; put usubjid= age=; run;
"

run_script <- function(sc) {
  env <- new.env(parent = globalenv())
  suppressMessages(eval(parse(text = as.character(sc)), env))
  env
}

test_that("sas_script() returns a list of step nodes with the IR fields", {
  sc <- sas_script(sas_translate(script_prog))
  expect_s3_class(sc, "sasonr_script")
  nd <- sc[[3]]
  expect_true(all(c("lang", "text", "sas", "sas_lines", "macro_stack", "include", "kind", "comments") %in% names(nd)))
  expect_equal(nd$kind, "static")
  expect_equal(nd$sas_lines[1], 4L)
  expect_null(nd$include)
  expect_equal(nd$macro_stack, list())
  expect_true(is.call(nd$lang))
  expect_equal(vapply(sc, `[[`, "", "verb"),
               c("global", "global", "data", "global", "sas_proc_sort", "sas_proc_means", "sas_proc_sql", "data"))
})

test_that("the script of a translation names it and runs to the same WORK and log", {
  ref <- sas_run(script_prog)
  sc <- sas_script(sas_translate(script_prog))
  txt <- as.character(sc)
  expect_match(txt[1], "Translation of the SAS program", fixed = TRUE)
  expect_true("library(sasonr)" %in% txt)
  env <- run_script(sc)
  expect_identical(sas_work(env$s), ref$work)
  expect_identical(sas_meta(env$s)[names(ref$meta)], ref$meta)
  expect_identical(as.character(sas_log(env$s, echo = FALSE)), as.character(sas_log(ref, echo = FALSE)))
})

test_that("the trace of a run runs to the same WORK and log", {
  ref <- sas_run(script_prog)
  sc <- sas_script(ref)
  expect_match(as.character(sc)[1], "Trace of this run", fixed = TRUE)
  expect_true(all(vapply(sc, `[[`, "", "kind") == "traced"))
  env <- run_script(sc)
  expect_identical(sas_work(env$s), ref$work)
  expect_identical(as.character(sas_log(env$s, echo = FALSE)), as.character(sas_log(ref, echo = FALSE)))
})

test_that("PROC SORT and PROC MEANS show only the options the program gave", {
  txt <- as.character(sas_script(sas_translate(script_prog)))
  expect_true(any(grepl("sas_proc_sort(\"data=adsl out=srt nodupkey\", by = \"descending age\"", txt, fixed = TRUE)))
  expect_true(any(grepl("sas_proc_means(\"data=srt noprint nway\", class = \"sex\", var = \"age\", output = \"out=m mean=mage\"",
                        txt, fixed = TRUE)))
  expect_true(any(grepl("sas_proc_sql(r\"(create table c as select * from srt where age > 50;)\"", txt, fixed = TRUE)))
})

test_that("a DATA step verb runs the code of its function", {
  s <- sas_session()
  s |> sas_data_step("a", \(work) {
    work$a <- tibble::tibble(x = 5)
  }, sas = "data a; x = 1; run;")
  expect_identical(sas_work(s)$a$x, 5)
  expect_error(sas_data_step(s, "a", 1, sas = "data a; x = 1; run;"), class = "sasonr_error")
})

test_that("the step verbs chain and return the session", {
  s <- sas_session(work = list(a = data.frame(x = c(2, 1, 3))))
  out <- s |> sas_proc_sort("data=a out=b", by = "descending x")
  expect_identical(out, s)
  s |> sas_proc_means("data=b noprint", var = "x", output = "out=m mean=xm")
  expect_identical(sas_work(s)$b$x, c(3, 2, 1))
  expect_identical(sas_work(s)$m$xm, 2)
  s |> sas_options("nonotes") |> sas_title("'T'", n = 2L)
  expect_identical(s$titles[2], "T")
  expect_error(sas_proc_sort(s, "data=a", "x"), class = "sasonr_error")
})

test_that("macro code that depends on run-time values is deferred", {
  prog <- "
data _null_; call symputx('n', 2); run;
%macro m; %if &n = 2 %then %do; data b; y = 2; run; %end; %mend;
%m
"
  ref <- sas_run(prog)
  # the macro processor warns about &n before the run sets it
  sc <- sas_script(suppressWarnings(sas_translate(prog)))
  kinds <- vapply(sc, `[[`, "", "kind")
  expect_true("deferred" %in% kinds)
  expect_true(any(grepl("sas_deferred(", as.character(sc), fixed = TRUE)))
  env <- run_script(sc)
  expect_identical(sas_work(env$s), ref$work)
})

test_that("open-code macro statements stay in the script", {
  prog <- "%let k = 3;\n%put k is &k;\ndata a; x = &k; run;"
  ref <- sas_run(prog)
  env <- run_script(sas_script(sas_translate(prog)))
  expect_identical(sas_work(env$s), ref$work)
  expect_identical(as.character(sas_log(env$s, echo = FALSE)), as.character(sas_log(ref, echo = FALSE)))
})

test_that("the tidy form writes WORK data sets as R variables with the same values", {
  skip_if_not_installed("tidylog")
  ref <- sas_run(script_prog)
  for (x in list(sas_translate(script_prog), ref)) {
    sc <- sas_script(x, tidy = TRUE)
    txt <- as.character(sc)
    expect_true(all(c("library(dplyr)", "library(tidylog)") %in% txt))
    expect_true(any(grepl("^adsl <- ", txt)))
    expect_true(any(grepl("^# In a sasonr session: ", txt)))
    env <- sasonr_tidy_log(run_script(sc))
    for (nm in names(ref$work)) {
      got <- dplyr::mutate(tibble::as_tibble(env[[nm]]), dplyr::across(dplyr::where(is.character), sas_rtrim))
      expect_identical(got, ref$work[[nm]])
    }
  }
})

test_that("sasonr_tidy_log() sets the tidylog display only for the expression", {
  old <- options(tidylog.display = NULL)
  on.exit(options(old))
  inside <- sasonr_tidy_log(getOption("tidylog.display"))
  expect_identical(inside, list(message))
  expect_null(getOption("tidylog.display"))
})

test_that("sas_script() rejects other objects", {
  expect_error(sas_script(list()), class = "sasonr_error")
})

test_that("sas_work(trim = FALSE) keeps the padded values and sas_session_add() adds data sets", {
  s <- sas_session()
  sas_run("data a; length c $4; c = 'x'; run;", session = s)
  expect_identical(sas_work(s, trim = FALSE)$a$c, "x   ")
  sas_session_add(s, list(b = data.frame(y = 1)))
  expect_true("b" %in% names(sas_work(s)))
})
