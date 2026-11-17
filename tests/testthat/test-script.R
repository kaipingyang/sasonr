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

# ---- SAS comments ---------------------------------------------------------------

cm_prog <- "/* derive
   the groups */
data b; /* one row per x */
  set a;
  * double it;
  y = x * 2;
  z = y + 1; /* plus one */
run;
* between the steps;
data c;
  set b;
  do i = 1 to 2;
    /* scaled */
    k = i * x;
    output;
  end;
run;
%* a macro comment;
"

cm_places <- function(nd) vapply(nd$comments, function(c) c$place, "")

test_that("script_comment_attach() gives leading, trailing and step comments", {
  tok <- sas_tokens("/* a */ data b; * s;\n  x = 1; /* t */\n  y = 2 * x;\nrun; /* r */\n")
  recs <- script_comment_attach(tok, data.frame(line = 1L, col = 9L))
  expect_equal(vapply(recs, `[[`, "", "text"), c("/* a */", "* s;", "/* t */", "/* r */"))
  expect_equal(vapply(recs, `[[`, "", "position"), c("leading", "trailing", "trailing", "trailing"))
  expect_equal(vapply(recs, `[[`, TRUE, "first"), c(TRUE, TRUE, FALSE, FALSE))
  # `* x` after `2` is an operator, not a comment
  expect_length(recs, 4L)
})

test_that("a `*` operator and a macro comment are not comments of the script", {
  sc <- sas_script(sas_translate(cm_prog, work = list(a = data.frame(x = c(1, 2)))))
  all_lines <- unlist(lapply(sc, function(nd) unlist(lapply(nd$comments, `[[`, "lines"))))
  expect_false(any(grepl("macro comment", all_lines)))
  expect_false(any(grepl("^# 2", all_lines)))
})

test_that("SAS comments become R comment lines above the step or the statement", {
  a <- data.frame(x = c(1, 2))
  sc <- sas_script(sas_translate(cm_prog, work = list(a = a)))
  txt <- as.character(sc)
  # a multi-line block comment and a trailing comment of the DATA statement
  i <- match("# derive", txt)
  expect_equal(txt[i + 1:2], c("# the groups", "# one row per x"))
  expect_match(txt[i + 3], "^s [|]> sas_data_step[(]\"b\"")
  # a statement comment next to an assignment, a trailing comment
  j <- grep("^ +# double it$", txt)
  expect_match(txt[j + 1L], "^ +y = sas_arith[(]x, 2")
  expect_match(txt[grep("^ +# plus one$", txt) + 1L], "^ +z = sas_arith[(]y, 1")
  # a comment between steps goes above the next step
  expect_match(txt[match("# between the steps", txt) + 1L], "^s [|]> sas_data_step[(]\"c\"")
  # inside a row-by-row step
  expect_match(txt[grep("^ +# scaled$", txt) + 1L], "pdv\\$k <- ")
  expect_equal(cm_places(sc[[1]]), c("step", "step", "body", "body"))
  # the script still runs to the same WORK
  env <- new.env(parent = globalenv())
  env$a <- a
  suppressMessages(eval(parse(text = txt), env))
  expect_identical(sas_work(env$s), sas_run(cm_prog, work = list(a = a))$work)
  # the executed objects do not change
  # the code parses to the same objects as without the comment lines
  for (nd in sc) {
    tl <- strsplit(nd$text, "\n", fixed = TRUE)[[1]]
    expect_identical(as.list(parse(text = tl, keep.source = FALSE)),
                     as.list(parse(text = tl[!grepl("^ *#", tl)], keep.source = FALSE)))
  }
})

test_that("comments are kept in the tidy form and in a trace", {
  a <- data.frame(x = c(1, 2))
  txt <- as.character(sas_script(sas_translate(cm_prog, work = list(a = a)), tidy = TRUE))
  expect_true(all(c("# derive", "# between the steps") %in% txt))
  expect_true(any(grepl("^ +# double it$", txt)))
  expect_true(any(grepl("^ +# scaled$", txt)))
  expect_no_error(parse(text = txt))
  tr <- as.character(sas_script(sas_run(cm_prog, work = list(a = a))))
  expect_true(any(grepl("^ +# double it$", tr)))
})

test_that("a comment after the last step follows it; a statement without code puts it above the step", {
  sc <- sas_script(sas_translate("data b; x = 1;\n  /* no code */\n  length c $ 2;\nrun;\n/* the end */\n"))
  cms <- sc[[1]]$comments
  expect_equal(vapply(cms, `[[`, "", "place"), c("step", "after"))
  expect_false(is.null(cms[[1]]$why))
  txt <- as.character(sc)
  expect_equal(txt[length(txt)], "# the end")
})

test_that("a tidy step reads earlier data sets as R variables in one pipeline", {
  skip_if_not_installed("tidylog")
  prog <- paste("data in; input g x; datalines;\n1 2\n2 3\n;\nrun;",
                "data out; set in; y = x*2; run;",
                "data out2; set out(keep = g y); if y > 4; run;", sep = "\n")
  ref <- sas_run(prog)
  sc <- sas_script(ref, tidy = TRUE)
  txt <- as.character(sc)
  expect_false(any(grepl("sas_session|sas_data_step|^\\.d <- |sas_note_read|\\.at = ", txt)))
  expect_true("out <- `in` |>" %in% txt)
  expect_true("out2 <- out |> sas_ds(keep = c(\"g\", \"y\")) |>" %in% txt)
  expect_true(any(grepl("^  select\\(g, x, y\\)$", txt)))
  env <- sasonr_tidy_log(run_script(sc))
  for (nm in names(ref$work)) {
    got <- dplyr::mutate(tibble::as_tibble(env[[nm]]), dplyr::across(dplyr::where(is.character), sas_rtrim))
    expect_identical(got, ref$work[[nm]])
  }
})

test_that("a tidy step after a session block reads the data sets the block returned", {
  skip_if_not_installed("tidylog")
  prog <- "data a; x = 1; put x=; run;\ndata b; set a; y = x + 1; run;"
  ref <- sas_run(prog)
  txt <- as.character(sas_script(ref, tidy = TRUE))
  expect_true(any(grepl("^# In a sasonr session: the step writes to the log", txt)))
  expect_true("b <- a |>" %in% txt)
  env <- run_script(sas_script(ref, tidy = TRUE))
  expect_identical(tibble::as_tibble(env$b), ref$work$b)
})

test_that("a variable assigned before it is read starts without its missing value", {
  txt <- as.character(sas_script(sas_translate("data a; x = 1; y = y + x; if x then z = 2; w = 3; w = w + 1; run;")))
  expect_false("      x = NA_real_," %in% txt)
  expect_true(all(c("      y = NA_real_,", "      z = NA_real_,") %in% txt))
  expect_false("      w = NA_real_," %in% txt)
  ref <- sas_run("data a; x = 1; y = y + x; if x then z = 2; w = 3; w = w + 1; run;")
  expect_identical(sas_work(run_script(sas_script(ref))$s), ref$work)
})

test_that("the tidy form keeps body comments where a pipeline was merged", {
  a <- data.frame(x = c(1, 2))
  prog <- "data b;\n  set a;\n  y = x;\n  /* double it */\n  z = y * 2;\nrun;"
  txt <- as.character(sas_script(sas_translate(prog, work = list(a = a)), tidy = TRUE))
  k <- match("    # double it", txt)
  expect_false(is.na(k))
  expect_match(txt[k + 1L], "^    z = ")
  expect_true("b <- a |>" %in% txt)
})

test_that("a MERGE step runs in a session block that says why", {
  prog <- "data a; k = 1; x = 1; run;\ndata b; k = 1; y = 2; run;\ndata c; merge a b; by k; run;"
  txt <- as.character(sas_script(sas_translate(prog), tidy = TRUE))
  expect_true("# In a sasonr session: a MERGE step (MERGE runs row by row, L3)" %in% txt)
  expect_true(all(c("a <- tibble::tibble(.rows = 1L) |>", "b <- tibble::tibble(.rows = 1L) |>") %in% txt))
})
