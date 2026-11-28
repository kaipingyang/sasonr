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

test_that("PROC SORT is an R call; PROC MEANS shows only the options the program gave", {
  txt <- as.character(sas_script(sas_translate(script_prog)))
  expect_true(any(grepl("sas_proc_sort(data = \"adsl\", out = \"srt\", nodupkey = TRUE, by = desc(age),", txt, fixed = TRUE)))
  expect_true(any(grepl("sas_proc_means(data = \"srt\", noprint = TRUE, nway = TRUE, class = sex, var = age, output = sas_stats_output(out = \"m\", mean = mage)",
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
  out <- s |> sas_proc_sort(data = "a", out = "b", by = desc(x))
  expect_identical(out, s)
  s |> sas_proc_means(data = "b", noprint = TRUE, var = x, output = sas_stats_output(out = "m", mean = xm))
  expect_identical(sas_work(s)$b$x, c(3, 2, 1))
  expect_identical(sas_work(s)$m$xm, 2)
  s |> sas_options("nonotes") |> sas_title("'T'", n = 2L)
  expect_identical(s$titles[2], "T")
  expect_error(sas_proc_sort(s, data = "a", by = x, frobnicate = TRUE), class = "sasonr_error")
  expect_error(sas_proc_sort(s, data = "a"), class = "sasonr_error")
})

test_that("PROC SORT runs as its R call; the call gives back the parsed step", {
  prog <- "data a; input id x; datalines;\n2 5\n1 .\n3 7\n;\nrun;\nproc sort data=a(where=(id > 1)) out=b nodupkeys; by descending id x; run;"
  st <- sas_parse(prog)$steps[[2]]
  call <- proc_call_of_ir(st)
  expect_identical(rlang::call_args(call)$by, quote(c(desc(id), x)))
  expect_identical(rlang::call_args(call)$data, quote(sas_dataset("a", where = sas_expr("id > 1"))))
  expect_true(rlang::call_args(call)$nodupkey)
  w <- st
  w$titles <- list()
  expect_identical(proc_ir_core(proc_ir_of_call(call, st$line)), proc_ir_core(w))
  # the trace runs the call and gives the same WORK and log
  r <- sas_run(prog)
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  # BY items as strings, a vector injected with !!, the SAS-text form (deprecated)
  s <- sas_session(work = list(a = data.frame(id = c(2, 1, 3))))
  v <- "descending id"
  s |> sas_proc_sort(data = "a", out = "b", by = !!v)
  expect_identical(sas_work(s)$b$id, c(3, 2, 1))
  s |> sas_proc_sort(data = "a", out = "c", by = c("id"))
  expect_identical(sas_work(s)$c$id, c(1, 2, 3))
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(s |> sas_proc_sort("data=a out=d", by = "descending id"), "deprecated")
  expect_identical(sas_work(s)$d$id, c(3, 2, 1))
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
      got <- dplyr::mutate(tibble::as_tibble(env[[nm]]), dplyr::across(dplyr::where(is.character), sas_rt_rtrim))
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
  expect_match(txt[j + 1L], "^ +y = sas_rt_arith[(]x, 2")
  expect_match(txt[grep("^ +# plus one$", txt) + 1L], "^ +z = sas_rt_arith[(]y, 1")
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
  expect_false(any(grepl("sas_session|sas_data_step|^\\.df <- |sas_rt_note_read|\\.at = ", txt)))
  # (laid out as air does: a pipeline that fits stays on one line)
  expect_true("out <- `in` |> mutate(y = sas_rt_arith(x, 2, \"*\")) |> select(g, x, y)" %in% txt)
  expect_true(all(c("out2 <- out |>", "  sas_rt_ds(keep = c(\"g\", \"y\")) |>", "  select(g, y)") %in% txt))
  env <- sasonr_tidy_log(run_script(sc))
  for (nm in names(ref$work)) {
    got <- dplyr::mutate(tibble::as_tibble(env[[nm]]), dplyr::across(dplyr::where(is.character), sas_rt_rtrim))
    expect_identical(got, ref$work[[nm]])
  }
})

test_that("a tidy step after a session block reads the data sets the block returned", {
  skip_if_not_installed("tidylog")
  prog <- "data a; x = 1; put x=; run;\ndata b; set a; y = x + 1; run;"
  ref <- sas_run(prog)
  txt <- as.character(sas_script(ref, tidy = TRUE))
  expect_true(any(grepl("^# In a sasonr session: the step writes to the log", txt)))
  expect_true(any(startsWith(txt, "b <- a |> ")))
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

test_that("a row-wise MERGE step runs in a session block that says why", {
  prog <- "data a; k = 1; x = 1; run;\ndata b; k = 1; y = 2; run;\ndata c; merge a b; by k; x = 5; run;"
  txt <- as.character(sas_script(sas_translate(prog), tidy = TRUE))
  expect_true("# In a sasonr session: a MERGE step (MERGE runs row by row, L3)" %in% txt)
  expect_true(all(c("a <- tibble::tibble(.rows = 1L) |>", "b <- tibble::tibble(.rows = 1L) |>") %in% txt))
})

test_that("a vectorised MERGE step is a pipeline of the tidy form", {
  prog <- "data a; k = 1; x = 1; run;\ndata b; k = 1; y = 2; run;\ndata c; merge a b; by k; run;"
  ref <- sas_run(prog)
  sc <- sas_script(sas_translate(prog), tidy = TRUE)
  txt <- as.character(sc)
  expect_false(any(grepl("In a sasonr session", txt, fixed = TRUE)))
  expect_true("c <- sas_rt_merge_input(" %in% txt)
  env <- run_script(sc)
  expect_identical(tibble::as_tibble(env$c), ref$work$c)
})

test_that("PROC FREQ and PROC TRANSPOSE become session verbs in the script", {
  code <- "data a; input g $ x; datalines;\na 1\nb 2\na 3\n;\nrun;
proc freq data=a; tables g; run;
proc transpose data=a out=t; var x; run;"
  sc <- sas_script(sas_translate(code))
  verbs <- vapply(sc, `[[`, "", "verb")
  expect_true(all(c("sas_proc_freq", "sas_proc_transpose") %in% verbs))
  s <- sas_session()
  for (nd in sc) s <- eval(nd$lang, list(s = s), asNamespace("sasonr"))
  r <- sas_run(code)
  expect_identical(sas_listing(s), r$listing)
  expect_identical(sas_work(s)$t, r$work$t)
})

test_that("PROC PRINT and PROC TABULATE become session verbs in the script", {
  code <- "data a; input g $ h $ x; datalines;\na u 1\nb u 2\na v 3\n;\nrun;
proc print data=a noobs; var g x; sum x; run;
proc tabulate data=a; class g; class h; var x; table g all, x*(sum mean); table h, x*n; run;"
  sc <- sas_script(sas_translate(code))
  verbs <- vapply(sc, `[[`, "", "verb")
  expect_true(all(c("sas_proc_print", "sas_proc_tabulate") %in% verbs))
  s <- sas_session()
  for (nd in sc) s <- eval(nd$lang, list(s = s), asNamespace("sasonr"))
  r <- sas_run(code)
  expect_identical(sas_listing(s), r$listing)
})

test_that("a script node's lang runs a step whose vectorised code falls back row by row", {
  # CATS into a short variable with bytes outside ASCII is decided row by row
  code <- "data _null_; length c4 $4; c4 = cats('a', 'é', 'é'); put c4= $hex8.; run;"
  sc <- sas_script(sas_translate(code))
  s <- sas_session()
  for (nd in sc) s <- eval(nd$lang, list(s = s), asNamespace("sasonr"))
  expect_identical(as.character(sas_log(s, echo = FALSE)), as.character(sas_log(sas_run(code), echo = FALSE)))
})

test_that("a translation without a run applies KEEP= of a PROC MEANS OUTPUT data set", {
  code <- "data ex; input id $ d; datalines;\na 1\na 2\nb 3\n;\nrun;
proc means data=ex noprint nway; class id; var d; output out=tot(keep=id total) sum=total; run;
data pct; merge ex tot; by id; p = d / total; run;"
  sc <- sas_script(sas_translate(code))
  s <- sas_session()
  for (nd in sc) s <- eval(nd$lang, list(s = s), asNamespace("sasonr"))
  expect_identical(sas_work(s)$pct, sas_run(code)$work$pct)
})

test_that("a step with a macro-time WARNING is kept as SAS text in the trace, so the WARNING comes again", {
  withr::local_options(sasonr.step_notes = TRUE)
  r <- suppressWarnings(sas_run("data a; x=\"&nosuch\"; run;"))
  s <- sas_script(r)
  expect_true(any(grepl("a macro WARNING inside the step", as.character(s), fixed = TRUE)))
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(as.character(s), f)
  e <- new.env()
  suppressWarnings(sys.source(f, envir = e))
  expect_true("WARNING: Apparent symbolic reference NOSUCH not resolved." %in% as.character(sas_log(e$s)))
})

test_that("the script of a program with %INCLUDE runs the included steps once, without the statement", {
  d <- withr::local_tempdir()
  inc <- file.path(d, "inc.sas")
  writeLines("data b; set a; y = x + 1; run;", inc)
  r <- sas_run(paste0("data a; x = 1; run;\n%include '", inc, "';\ndata c; set b; run;"))
  s <- sas_script(r)
  expect_false(any(grepl("sas_deferred", format(s), fixed = TRUE)))
  e <- new.env()
  eval(parse(text = format(s)), e)
  expect_identical(e$s$work$c, r$work$c)
})

test_that("PROC PRINT runs as its R call, statements as names or SAS text", {
  prog <- paste0("options nocenter;\ndata a;\n  input id x $ y;\n  label y = 'Why';\n  datalines;\n2 b 5\n1 a .\n3 c 7\n;\nrun;\n",
                 "proc sort data=a out=b; where y ne .; by descending id; run;\n",
                 "proc print data=b noobs label split='*';\n  var id x y;\n  sum y;\n  format y 5.1;\nrun;\n")
  r <- sas_run(prog)
  txt <- as.character(sas_script(r))
  expect_true(any(grepl("sas_proc_sort(data = \"a\", out = \"b\", where = sas_expr(\"y ne .\"), by = desc(id),", txt, fixed = TRUE)))
  expect_true(any(grepl("sas_proc_print(data = \"b\", noobs = TRUE, label = TRUE, split = \"*\", var = c(id, x, y), sum = y, format = c(y = \"5.1\"),",
                        txt, fixed = TRUE)))
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  expect_identical(sas_listing(env$s), r$listing)
  # a source that is not the step of the arguments fails
  s <- sas_session(work = list(a = data.frame(id = c(2, 1))))
  expect_error(s |> sas_proc_sort(data = "a", out = "b", by = id, .sas_src = sas_src(1:3, "proc sort data=a out=c;\n  by id;\nrun;")),
               class = "sasonr_error")
  # a BY string with more than one statement fails; an unknown statement fails
  expect_error(s |> sas_proc_sort(data = "a", by = "id; x"), class = "sasonr_syntax_error")
  expect_error(s |> sas_proc_print(data = "a", table = "id"), class = "sasonr_error")
  # the old form with options = and with several words warns and runs
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(s |> sas_proc_sort("nodupkey data=a out=d", by = "id"), "deprecated")
  expect_identical(sas_work(s)$d$id, c(1, 2))
})

test_that("PROC MEANS and SUMMARY run as their R calls; options as arguments, statistics as stats", {
  prog <- paste0("options nocenter;\ndata trial;\n  input arm $ week aval;\n  datalines;\nA 0 10.2\nA 4 11.8\nB 0 10.9\nB 4 9.8\n;\nrun;\n",
                 "proc means data=trial n mean std maxdec=2 nway;\n  class arm;\n  var aval;\n  output out=m mean=mval;\nrun;\n",
                 "proc summary data=trial(where=(week > 0)) print;\n  var aval;\nrun;\n")
  r <- sas_run(prog)
  txt <- as.character(sas_script(r))
  expect_true(any(grepl("sas_proc_means(data = \"trial\", stats = c(n, mean, std), maxdec = 2, nway = TRUE, class = arm, var = aval,",
                        txt, fixed = TRUE)))
  expect_true(any(grepl("sas_proc_summary(data = sas_dataset(\"trial\", where = sas_expr(\"week > 0\")), print = TRUE, var = aval,", txt, fixed = TRUE)))
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  expect_identical(sas_listing(env$s), r$listing)
  call <- proc_call_of_ir(sas_parse(prog)$steps[[3]])
  expect_identical(proc_ir_core(proc_ir_of_call(call, 1L)), proc_ir_core(sas_parse(prog)$steps[[3]]))
  s <- sas_session()
  invisible(sas_run(prog, session = s))
  s |> sas_proc_means(data = "trial", stats = c(n, mean), maxdec = 1, class = arm, var = aval)
  expect_true(any(grepl("^A +2 +2 +11\\.0$", sas_listing(s))))
  expect_error(s |> sas_proc_means(data = "trial", frobnicate = TRUE, var = aval), class = "sasonr_error")
})

test_that("sas_stats_output() builds the OUTPUT statement; the trace writes it so", {
  expect_identical(unclass(sas_stats_output(out = "st", mean = c(m_x, m_y), std = c(sd_x = x), n = TRUE, autoname = TRUE)),
                   "out=st mean=m_x m_y std(x)=sd_x n= / autoname")
  expect_error(sas_stats_output(mean = c(a, b = x)), class = "sasonr_error")
  prog <- "data t;\n  input arm $ x y;\n  datalines;\nA 1 2\nA 3 5\nB 2 2\n;\nrun;\nproc means data=t noprint nway;\n  class arm;\n  var x y;\n  output out=m(drop=_type_) mean=mx my std(x)=sx n= / autoname;\nrun;\n"
  r <- sas_run(prog)
  txt <- paste(as.character(sas_script(r)), collapse = "\n")
  expect_match(txt, "output = sas_stats_output(out = sas_dataset(\"m\", drop = `_type_`), mean = c(mx, my), std = c(sx = x), n = TRUE, autoname = TRUE)", fixed = TRUE)
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  # a statement value held in a variable is injected
  s <- sas_session(work = list(t = r$work$t))
  o <- sas_stats_output(out = "st", mean = c(m_x, m_y))
  s |> sas_proc_means(data = "t", noprint = TRUE, var = c(x, y), output = !!o)
  expect_identical(sas_work(s)$st$m_x, 2)
})

test_that("sas_expr(), sas_vars() and sas_dataset() mark SAS syntax; the trace uses them", {
  expect_s3_class(sas_expr("x > 1 and y = 'a'"), "sas_expr")
  expect_error(sas_expr("x > "), class = "sasonr_syntax_error")
  expect_error(sas_vars("x1- ;"), class = "sasonr_error")
  expect_identical(unclass(sas_dataset("t", where = sas_expr("w > 0"), keep = c(a, b), rename = c(bb = b), obs = 5)),
                   "t(keep=a b rename=(b=bb) where=(w > 0) obs=5)")
  expect_error(sas_dataset("t", where = "w > 0"), class = "sasonr_error")
  prog <- paste0("options nocenter;\ndata t;\n  input arm $ x1 x2 y;\n  datalines;\nA 1 2 3\nA 3 5 1\nB 2 2 .\n;\nrun;\n",
                 "proc sort data=t(where=(yy ne .) keep=arm x1 x2 y rename=(y=yy)) out=s2(drop=x2); by arm; run;\n",
                 "proc print data=t; var x1-x2 y; where x1 > 1 and arm = 'A'; run;\n")
  r <- sas_run(prog)
  txt <- paste(as.character(sas_script(r)), collapse = "\n")
  expect_match(txt, "data = sas_dataset(\"t\", where = sas_expr(\"yy ne .\"), keep = c(arm, x1, x2, y), rename = c(yy = y))", fixed = TRUE)
  expect_match(txt, "var = sas_vars(\"x1-x2 y\"), where = sas_expr(\"x1 > 1 and arm = 'A'\")", fixed = TRUE)
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  expect_identical(sas_listing(env$s), r$listing)
})

test_that("PROC TRANSPOSE runs as its R call; sas_transpose_df() takes the verb's arguments", {
  prog <- paste0("data l;\n  input subj $ visit param $ aval;\n  datalines;\nS01 1 ALT 25\nS01 1 AST 22\nS02 1 ALT 31\n;\nrun;\n",
                 "proc transpose data=l out=w prefix=v_ name=src;\n  by subj visit;\n  id param;\n  var aval;\nrun;\n")
  r <- sas_run(prog)
  txt <- paste(as.character(sas_script(r)), collapse = "\n")
  expect_match(txt, "sas_proc_transpose(data = \"l\", out = \"w\", prefix = \"v_\", name = \"src\", by = c(subj, visit), id = param, var = aval",
               fixed = TRUE)
  env <- run_script(sas_script(r))
  expect_identical(sas_work(env$s), r$work)
  df <- data.frame(subj = c("S01", "S01", "S02"), param = c("ALT", "AST", "ALT"), aval = c(25, 22, 31))
  t1 <- sas_transpose_df(df, var = aval, by = subj, id = param)
  expect_identical(names(t1), c("subj", "_name_", "alt", "ast"))
  expect_identical(t1$alt, c(25, 31))
})

test_that("PROC TABULATE runs as its R call; the TABLE statement through sas_tabulate_table()", {
  prog <- paste0("options nocenter;\ndata t;\n  input arm $ sex $ age;\n  datalines;\nA F 34\nA M 41\nB M 52\n;\nrun;\n",
                 "proc tabulate data=t missing;\n  class arm sex;\n  var age;\n  table sex all, arm*age*(n mean);\nrun;\n")
  r <- sas_run(prog)
  txt <- paste(as.character(sas_script(r)), collapse = "\n")
  expect_match(txt, "sas_proc_tabulate(data = \"t\", missing = TRUE, class = c(arm, sex), var = age, table = sas_tabulate_table(\"sex all, arm*age*(n mean)\")",
               fixed = TRUE)
  env <- run_script(sas_script(r))
  expect_identical(sas_listing(env$s), r$listing)
  expect_error(sas_tabulate_table("sex all, ("), class = "sasonr_error")
  # a step with a feature outside the scope still fails loud as before
  expect_error(sas_run(paste0(prog, "proc tabulate data=t; class arm; table arm; frobnicate x; run;")), class = "sasonr_unsupported")
})

test_that("an option named like a statement is <name>_opt in the call (TABULATE FORMAT=, TRANSPOSE LABEL=)", {
  prog <- "options nocenter;\ndata t;\n  input g $ v;\n  datalines;\na 1\nb 2\n;\nrun;\nproc tabulate data=t format=8.2;\n  class g;\n  var v;\n  format v 5.1;\n  table g, v*sum;\nrun;\n"
  r <- sas_run(prog)
  txt <- paste(as.character(sas_script(r)), collapse = "\n")
  expect_match(txt, "sas_proc_tabulate(data = \"t\", format_opt = 8.2, class = g, var = v, format = c(v = \"5.1\")", fixed = TRUE)
  env <- run_script(sas_script(r))
  expect_identical(sas_listing(env$s), r$listing)
})
