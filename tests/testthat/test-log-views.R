# The log views: plain text with the source echo, ANSI colours, HTML.

log_prog <- paste(
  "data work.a;",
  "  x = 1;",
  "  put x=;",
  "run;",
  "data work.b;",
  "  x = nosuchfn(2);",
  "run;",
  "data c;",
  "  input v;",
  "  datalines;",
  "1",
  "2",
  ";",
  "run;",
  "%put done;",
  sep = "\n"
)

log_res <- function(code = log_prog) {
  withr::with_options(list(sasonr.step_notes = TRUE), sas_run(code, on_error = "sas"))
}

test_that("the plain log echoes the program lines", {
  r <- log_res()
  expect_identical(r$log, as.character(sas_log(r)))
  expect_snapshot(cat(r$log, sep = "\n"))
  # without the echo: the messages and PUT output
  expect_false(any(grepl("^[0-9]+ ", sas_log(r, echo = FALSE))))
  expect_true("done" %in% sas_log(r, echo = FALSE))
})

test_that("long program lines continue on ! lines at LINESIZE", {
  long <- paste0("data _null_; x = 1; ", strrep("y = 2; ", 20), "put x=; run;")
  r <- sas_run(paste0("options ls=64;\n", long))
  src <- r$log[grepl("^2 {10}", r$log)]
  expect_length(src, 1)
  cont <- r$log[startsWith(r$log, "2        ! ")]
  expect_gt(length(cont), 1)
  expect_true(all(nchar(c(src, cont)) <= 64))
  expect_identical(sasonr:::log_echo_line(4L, "abc def", 132L), "4          abc def")
  # a blank after the last full column starts the next line
  expect_identical(sasonr:::log_echo_line(1L, paste0(strrep("a", 53), " b"), 64L),
                   c(paste0("1          ", strrep("a", 53)), "1        !  b"))
  expect_identical(sasonr:::log_echo_line(1L, paste0(strrep("a", 50), " bbbbbb"), 64L),
                   c(paste0("1          ", strrep("a", 50)), "1        ! bbbbbb"))
})

test_that("OPTIONS NOSOURCE and SOURCE turn the echo off and on", {
  r <- sas_run("options nosource;\ndata _null_; put 'a'; run;\noptions source;\ndata _null_; put 'b'; run;")
  expect_true(any(startsWith(r$log, "1          options nosource;")))
  expect_false(any(startsWith(r$log, "2 ")))
  expect_false(any(startsWith(r$log, "3 ")))
  expect_true(any(startsWith(r$log, "4          data _null_;")))
})

test_that("the ANSI view colours the messages under a summary line", {
  r <- log_res()
  withr::local_options(cli.num_colors = 256)
  expect_snapshot(cat(sas_log(r, "ansi"), sep = "\n"))
  expect_identical(cli::ansi_strip(sas_log(r, "ansi"))[-1], r$log)
  expect_match(cli::ansi_strip(sas_log(r, "ansi")[1]), "1 ERROR, 1 WARNING")
})

test_that("the HTML view uses the sasonr CSS classes", {
  r <- log_res()
  h <- sas_log(r, "html")
  expect_snapshot(cat(h))
  expect_false(grepl("style=", h, fixed = TRUE))
  for (cl in c("sas-note", "sas-warning", "sas-error", "sas-put", "sas-src")) expect_match(h, cl, fixed = TRUE)
  expect_true(file.exists(system.file("css", "sasonr.css", package = "sasonr")))
})

test_that("print() uses the ANSI view unless sasonr.log_style is plain", {
  r <- log_res("data _null_; put 'hi'; run;")
  withr::local_options(cli.num_colors = 256)
  out <- utils::capture.output(suppressMessages(print(r)))
  expect_true(any(grepl("\033[", out, fixed = TRUE)))
  withr::local_options(sasonr.log_style = "plain")
  out <- utils::capture.output(suppressMessages(print(r)))
  expect_false(any(grepl("\033[3", out, fixed = TRUE)))
})

test_that("the events of a result and of a session", {
  r <- log_res()
  ev <- r$log_events
  expect_s3_class(ev, "tbl_df")
  expect_named(ev, c("seq", "submission", "step", "phase", "kind", "id", "args", "at", "places", "text", "source_line"))
  expect_identical(unlist(ev$text[ev$kind != "echo"]), as.character(sas_log(r, echo = FALSE)))
  expect_true("ds.created" %in% ev$id)
  err <- ev[ev$kind == "error", ]
  expect_identical(err$at[[1]], c(6L, 7L))
  s <- sas_session()
  withr::defer(sas_session_close(s))
  sas_run("%put one;", session = s)
  sas_run("%put two;", session = s)
  expect_identical(as.character(sas_log(s)), c("1          %put one;", "one", "1          %put two;", "two"))
  expect_identical(as.character(sas_log(s, echo = FALSE)), c("one", "two"))
  expect_error(sas_log(1), class = "sasonr_error")
  expect_error(sas_log(r, "pdf"))
})

test_that("sasonr.strict = FALSE: one NOTE and one warning list the approximate features", {
  withr::local_options(sasonr.strict = FALSE)
  expect_warning(r <- sas_run("data a; y = probt(1.5, 10); z = probt(1, 2); run;"), class = "sasonr_strict_warning")
  expect_identical(utils::tail(r$log, 1), "NOTE: sasonr used approximate implementations: function PROBT.")
  expect_identical(r$log_events$id[nrow(r$log_events)], "sasonr.approximate")
  # one warning per feature and submission, at the gate
  n <- 0L
  withCallingHandlers(sas_run("data a; y = probt(1.5, 10); z = probt(1, 2); w = probt(2, 3); run;"),
                      sasonr_strict_warning = function(w) {
                        n <<- n + 1L
                        invokeRestart("muffleWarning")
                      })
  expect_identical(n, 1L)
  # a submission that stops with an error still records the event
  s <- sas_session()
  withr::defer(sas_session_close(s))
  expect_error(suppressWarnings(sas_run("data a; y = probt(1.5, 10); run; %put %sysfunc(rand(uniform));",
                                        session = s, on_error = "abort")))
  ev <- sasonr:::log_session_events(s)
  expect_true("sasonr.approximate" %in% ev$id)
  expect_identical(as.character(utils::tail(sas_log(s, echo = FALSE), 1)), "NOTE: sasonr used approximate implementations: function PROBT.")
  withr::local_options(sasonr.strict = TRUE)
  expect_error(sas_run("data a; y = probt(1.5, 10); run;"), class = "sasonr_strict_error")
})

test_that("blank PUT lines stay in the log", {
  expect_identical(msgs(sas_run("data _null_; x = 1; put x= / ' ' / 'd'; run;")), c("x=1", "", "d"))
  # a line of blank values
  expect_identical(msgs(sas_run("data _null_; length x $4; x = ' '; put 'a'; put x $char4.; put 'b'; run;")),
                   c("a", "", "b"))
  # in the echo view too, between the step's lines and its NOTEs
  r <- sas_run("data _null_;\n  put ' ';\nrun;")
  expect_identical(r$log, c("1          data _null_;", "2            put ' ';", "3          run;", "", ""))
})

test_that("the empty lines a PUT statement writes by its form fail loud", {
  for (code in c("data _null_; put; run;", "data _null_; put 'c' /; run;", "data _null_; put 'c' / / 'd'; run;",
                 "data _null_; put / 'd'; run;", "data _null_; put +3; run;", "data _null_; set a; put; run;")) {
    expect_error(sas_run(code), class = "sasonr_unsupported", label = code)
  }
  expect_error(sas_run("data _null_; put; run;"), "PUT with no items")
  expect_error(sas_run("data _null_;\n  put 'c' /;\nrun;"), "writes an empty line.*SAS line 2")
})

test_that("a PUT pointer beyond the line size fails loud", {
  expect_error(sas_run("data _null_; put 'a' @40000 'x'; run;"), "PUT pointer beyond the line size")
  expect_error(sas_run("data _null_; put @40000 'x' / 'b'; run;"), "PUT pointer beyond the line size")
  expect_error(sas_run("data _null_; put @133 'x'; run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; put @130 'xyzw'; run;"), "writing past the line size")
  # also before a `/`, also in a vectorised step
  expect_error(sas_run("data _null_; put @130 'xyzw' / 'b'; run;"), "writing past the line size")
  expect_identical(sasonr:::sas_put_layout(list(list(k = "col", v = 132), list(k = "text", s = "x", blank = FALSE))),
                   paste0(strrep(" ", 131), "x"))
  expect_error(sas_run("data a; do i = 1 to 2; output; end; run; data _null_; set a; p = 120 + 20 * i; put @p 'x'; run;"),
               "PUT pointer beyond the line size")
})

test_that("a PUT to the log that starts with a pointer beyond 32767 writes no line", {
  expect_identical(msgs(sas_run("data _null_; put 'a'; put @2147483648 'x'; put 'b'; run;")), c("a", "b"))
  expect_identical(msgs(sas_run("data a; do i = 1 to 3; output; end; run; data _null_; set a; put @40000 'x'; put 'b'; run;")),
                   rep("b", 3))
  # a FILE: a hard limit, whatever its LRECL
  withr::local_dir(withr::local_tempdir())
  expect_error(sas_run("data _null_; file 'o.txt' lrecl=1000000000; put @500000000 'x'; run;"),
               "PUT pointer beyond the line size")
})

test_that("a conditional PUT lays out only the rows where it executes", {
  pre <- "data a; do i = 1 to 3; p = 10 * i; if i = 2 then p = 500; if i = 3 then p = .; output; end; run; "
  expect_identical(msgs(sas_run(paste0(pre, "data _null_; set a; if i = 1 then put @p 'x'; run;"))),
                   paste0(strrep(" ", 9), "x"))
  r <- sas_run(paste0(pre, "data _null_; set a; if i < 3 then put 'a'; if i = 1 then put @p 'x' / 'y'; run;"))
  expect_identical(msgs(r), c("a", paste0(strrep(" ", 9), "x"), "y", "a"))
  # the rows that execute are still checked
  expect_error(sas_run(paste0(pre, "data _null_; set a; if i = 2 then put @p 'x'; run;")), "beyond the line size")
})

test_that("a FILE destination lays PUT out by its LRECL, the log by LINESIZE", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  expect_error(sas_run("options ls=200; data _null_; file 'o.txt' lrecl=132; put @150 'x'; run;"),
               "PUT pointer beyond the line size")
  sas_run("options ls=80; data _null_; file 'o.txt' lrecl=132; put @100 'x'; run;")
  expect_identical(readLines(file.path(d, "o.txt")), paste0(strrep(" ", 99), "x"))
  r <- sas_run("options ls=200; data _null_; put @150 'x'; run;")
  expect_identical(msgs(r), paste0(strrep(" ", 149), "x"))
  expect_error(sas_run("options ls=80; data _null_; put @100 'x'; run;"), "PUT pointer beyond the line size")
})

test_that("PUT without variables writes one line per iteration of a vectorised step", {
  for (n in c(0L, 1L, 5L)) {
    src <- sprintf("data a; do i = 1 to %d; output; end; run; data _null_; set a; put 'x'; run;", n)
    expect_identical(msgs(sas_run(src)), rep("x", n), label = n)
    # pointer controls; a literal-only PUT under a condition
    src <- sprintf("data a; do i = 1 to %d; output; end; run; data _null_; set a; if i > 2 then put @3 'z'; run;", n)
    expect_identical(msgs(sas_run(src)), rep("  z", max(0L, n - 2L)), label = n)
    src <- sprintf("data a; do i = 1 to %d; output; end; run; data _null_; set a; if i > 2 then put 'z'; run;", n)
    expect_identical(msgs(sas_run(src)), rep("z", max(0L, n - 2L)), label = n)
  }
})

test_that("only the step buffer marks lines as PUT output", {
  eng <- new.env()
  sasonr:::log_engine_init(eng)
  # lines without names (compile-time and procedure NOTEs) are classified by their text
  sasonr:::log_emit_lines(eng, c("NOTE: a note.", "x"), phase = "run")
  expect_identical(vapply(eng$events, `[[`, "", "kind"), c("note", "put"))
  eng$events <- list()
  sasonr:::log_emit_buffer(eng, c("NOTE: put text", stats::setNames("NOTE: a note.", "msg")))
  expect_identical(vapply(eng$events, `[[`, "", "kind"), c("put", "note"))
})

test_that("PUT output that starts with ERROR: is PUT output", {
  r <- sas_run("data _null_; put 'ERROR: custom'; run;")
  expect_identical(msgs(r), "ERROR: custom")
  expect_identical(r$log_events$kind[r$log_events$kind != "echo"], "put")
  expect_identical(r$log[length(r$log)], "ERROR: custom")
  withr::local_options(cli.num_colors = 256)
  expect_match(cli::ansi_strip(sas_log(r, "ansi")[1]), "No ERRORs or WARNINGs", fixed = TRUE)
})

test_that("arguments outside the exact domain that give a missing value fail loud", {
  withr::local_options(sasonr.strict = FALSE)
  expect_error(suppressWarnings(sas_run("data a; x = mdy(1152921504606846976, 1, 1); run;")), class = "sasonr_unsupported")
  expect_error(suppressWarnings(sas_run("data a; x = gcd(1e300, 2); run;")), class = "sasonr_unsupported")
})

test_that("no ERROR marker under a line that does not hold the token", {
  # a numbered ERROR in a step that a macro generates: the echo shows the
  # generated line, which is not implemented
  r <- sas_run("%macro m;\ndata a; x=nosuchfn(1);\nrun;\n%mend;\n%m", on_error = "sas")
  expect_false(any(grepl("^ +_+$", r$log)))
  expect_match(r$log[1], "no source echo for this submission: a numbered ERROR on a line with macro references")
  r <- sas_run("data a;\n  x = NoSuchFn(1);\nrun;", on_error = "sas")
  expect_true(any(grepl("^ {17}_{8}$", r$log)))
})

test_that("OPTIONS NOSOURCE lasts across the submissions of a session", {
  s <- sas_session()
  withr::defer(sas_session_close(s))
  sas_run("options nosource;", session = s)
  r <- sas_run("%put two;", session = s)
  expect_identical(r$log, "two")
  r <- sas_run("options source;\n%put three;", session = s)
  expect_identical(r$log, c("2          %put three;", "three"))
})

test_that("a multi-line OPTIONS statement takes effect after its last line", {
  r <- sas_run("options\nnosource;\n%put a;")
  expect_identical(r$log, c("1          options", "2          nosource;", "a"))
})

test_that("the approximate NOTE after an error wraps at the LINESIZE of the submission", {
  withr::local_options(sasonr.strict = FALSE)
  s <- sas_session()
  withr::defer(sas_session_close(s))
  code <- paste("options ls=64; data a; y = probt(1.5, 10); z = probf(1, 2, 3); w = probnorm(1); run;",
                "%put %sysfunc(rand(uniform));")
  expect_error(suppressWarnings(sas_run(code, session = s, on_error = "abort")))
  note <- sas_log(s, echo = FALSE)
  note <- note[which(startsWith(note, "NOTE: sasonr used")):length(note)]
  expect_gt(length(note), 1)
  expect_true(all(nchar(note) <= 64))
})

test_that("a numbered ERROR is followed by a blank line, other ERRORs are not", {
  r <- sas_run("data a;\n  x = nosuchfn(2);\nrun;", on_error = "sas")
  i <- which(startsWith(r$log, "ERROR 68-185"))
  expect_identical(r$log[i + 1L], "")
  r <- sas_run("%put ERROR: from put;\n%put next;")
  expect_identical(r$log, c("1          %put ERROR: from put;", "ERROR: from put", "2          %put next;", "next"))
  # an ERROR without a number: the input data set does not exist (the next
  # program line follows, as documented)
  r <- sas_run("proc sort data=work.nosuch out=work.s;\n  by x;\nrun;", on_error = "sas")
  i <- which(startsWith(r$log, "ERROR:"))
  expect_length(i, 1L)
  expect_identical(r$log[i + 1L], "2            by x;")
})

test_that("the blank line after an ERROR follows its number and the procedure", {
  # one ERROR event right after program line 2, in a step labelled `label`
  view <- function(text, label) {
    ev <- sasonr:::log_events_table(list(
      list(seq = 1L, step = 1L, phase = "compile", kind = "error", id = "raw", text = text, src = 2L)
    ), list(`1` = list(lines = c("a", "b", "c"), steps = data.frame(step = 1L, label = label, line = 1L),
                       source = data.frame(line = 0L, on = TRUE), ls = data.frame(line = 0L, ls = 132L),
                       skip = integer())))
    sasonr:::log_view_plain(ev, TRUE)
  }
  num <- "ERROR 22-322: Syntax error."
  expect_identical(view(num, "DATA step")[4], "")
  expect_identical(view(num, "PROC SQL")[4], "")
  expect_identical(view(num, "PROC MEANS")[4], "3          c")
  expect_identical(view(num, "PROC TRANSPOSE")[4], "3          c")
  expect_identical(view("ERROR: WHERE clause operator requires character variables.", "DATA step")[4], "3          c")
})

test_that("the blank line that ends a step's lines follows its last line, not its ERROR", {
  # a step of `lines` labelled `label`: ERROR events after program line `at`,
  # the end of the step's lines, then the step's stop NOTE
  view <- function(lines, label, errors, at, echo = TRUE) {
    n <- length(lines)
    ev <- c(lapply(seq_along(errors), function(k)
      list(seq = k, step = 1L, phase = "compile", kind = "error", id = "raw", text = errors[k], src = at)),
      list(list(seq = length(errors) + 1L, step = 1L, phase = "step_end", kind = "echo", id = "raw", text = "", src = n),
           list(seq = length(errors) + 2L, step = 1L, phase = "run", kind = "note", id = "raw", text = stop_note, src = n)))
    sasonr:::log_view_plain(sasonr:::log_events_table(ev, list(`1` = list(
      lines = lines, steps = data.frame(step = 1L, label = label, line = 1L),
      source = data.frame(line = 0L, on = TRUE), ls = data.frame(line = 0L, ls = 132L), skip = integer()))), echo)
  }
  stop_note <- "NOTE: The step was stopped because of errors."
  e180 <- "ERROR 180-322: Statement is not valid or it is used out of proper order."
  msg22 <- "ERROR 22-322: Syntax error."
  msg202 <- "ERROR 202-322: The option or parameter is not recognized and will be ignored."
  src <- function(...) {
    x <- c(...)
    sprintf("%-10d %s", seq_along(x), x)
  }
  # a statement that is not valid in PROC MEANS / SUMMARY: the rest of the step
  # is echoed, the blank line comes after RUN;
  means <- c("proc means data=a;", "  frobnicate;", "  var x;", "run;")
  for (label in c("PROC MEANS", "PROC SUMMARY")) {
    expect_identical(view(sub("means", tolower(sub("PROC ", "", label)), means), label, e180, 2L),
                     c(src(sub("means", tolower(sub("PROC ", "", label)), means))[1:2], e180,
                       src(sub("means", tolower(sub("PROC ", "", label)), means))[3:4], "", stop_note))
  }
  # an unknown option of PROC SORT / FREQ: two numbered ERRORs, no blank lines
  for (p in list(c("PROC SORT", "proc sort data=a frobnicate;", "  by x;"),
                 c("PROC FREQ", "proc freq data=a frobnicate;", "  tables g;"))) {
    l <- c(p[2:3], "run;")
    expect_identical(view(l, p[1], c(msg22, msg202), 1L), c(src(l)[1], msg22, msg202, src(l)[2:3], "", stop_note))
  }
  # an ERROR without a number in a DATA step
  l <- c("data _null_;", "  set nosuch;", "  put x;", "run;")
  e <- "ERROR: File WORK.NOSUCH.DATA does not exist."
  expect_identical(view(l, "DATA step", e, 2L), c(src(l)[1:2], e, src(l)[3:4], "", stop_note))
  # a DATA step that ends on the line of its numbered ERRORs: the blank line
  # comes before them, the ERRORs without a number stay above it
  l <- "data a; set b; attrib x format=; run;"
  msg76 <- "ERROR 76-322: Syntax error, statement will be ignored."
  expect_identical(view(l, "DATA step", c(msg22, msg76), 1L), c(src(l), "", msg22, "", msg76, "", stop_note))
  w <- "ERROR: Syntax error while parsing WHERE clause."
  l <- "data a; set b; where x ^ = 3; run;"
  expect_identical(view(l, "DATA step", c(w, msg22, msg76), 1L), c(src(l), w, "", msg22, "", msg76, "", stop_note))
  # PROC PRINT on one line
  l <- "proc print data=d nobyline; run;"
  expect_identical(view(l, "PROC PRINT", c(msg22, msg202), 1L), c(src(l), "", msg22, "", msg202, "", stop_note))
  # PROC SORT and MEANS on one line: a blank line after each numbered ERROR
  # too (mid-step these procedures write none)
  l <- "proc sort data=a frobnicate; run;"
  expect_identical(view(l, "PROC SORT", c(msg22, msg202), 1L), c(src(l), "", msg22, "", msg202, "", stop_note))
  l <- "proc means data=a; frobnicate; run;"
  expect_identical(view(l, "PROC MEANS", e180, 1L), c(src(l), "", e180, "", stop_note))
  # the messages-only view does not change
  expect_identical(view(l, "PROC PRINT", c(msg22, msg202), 1L, echo = FALSE), c(msg22, msg202, stop_note))
  expect_identical(view(means, "PROC MEANS", e180, 2L, echo = FALSE), c(e180, stop_note))
  # what is not implemented leaves the echo of the submission out (a NOTE says
  # why, the messages stay): numbered ERRORs of a step after no echoed line
  # of it (a NOTE in between, here), an unnumbered ERROR after them, a
  # numbered WARNING
  omitted <- function(x, why) {
    expect_match(x[1], paste0("^NOTE: sasonr wrote no source echo for this submission: ", why))
    expect_false(any(grepl("^[0-9]+ {2,}", x)))
  }
  ev <- sasonr:::log_events_table(list(
    list(seq = 1L, step = 1L, phase = "compile", kind = "error", id = "raw", text = msg22, src = 1L),
    list(seq = 2L, step = 1L, phase = "compile", kind = "note", id = "raw", text = "NOTE: x.", src = 1L),
    list(seq = 3L, step = 1L, phase = "step_end", kind = "echo", id = "raw", text = "", src = 1L)),
    list(`1` = list(lines = "proc print data=a frob; run;", steps = data.frame(step = 1L, label = "PROC PRINT", line = 1L),
                    source = data.frame(line = 0L, on = TRUE), ls = data.frame(line = 0L, ls = 132L), skip = integer())))
  x <- sasonr:::log_view_plain(ev, TRUE)
  omitted(x, "the blank lines around numbered ERRORs")
  expect_identical(x[-1], c(msg22, "NOTE: x."))
  expect_identical(sasonr:::log_view_plain(ev, FALSE), c(msg22, "NOTE: x."))
  omitted(view("data a; set b; x=1; run;", "DATA step", c(msg22, "ERROR: other."), 1L), "the blank lines")
  ev$kind[1] <- "warning"
  ev$text[[1]] <- "WARNING 1-322: Assuming the symbol X was misspelled as y."
  omitted(sasonr:::log_view_plain(ev, TRUE), "a numbered WARNING")
  # a numbered ERROR in a step that a macro generates or a compile-time ERROR
  # on a line that NOSOURCE hides: the echo is left out, the run goes on
  r <- sas_run("%macro m;\ndata b; x = nosuchfn(1); run;\n%mend;\n%m\ndata c; x = 1; run;", on_error = "sas")
  omitted(r$log, "a numbered ERROR on a line with macro references")
  expect_true("ERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed." %in% r$log)
  expect_identical(nrow(r$work$c), 0L)
  expect_identical(as.character(sas_log(r, echo = FALSE)), r$log[-1])
  r <- sas_run("options nosource;\ndata b; x = nosuchfn(1); run;\noptions source;\n%put done;", on_error = "sas")
  omitted(r$log, "an ERROR on a line that OPTIONS NOSOURCE hides")
  expect_identical(utils::tail(r$log, 1), "done")
  # a numbered ERROR on a line with a macro reference, also after it, also
  # when the reference resolves to nothing: the echo is left out
  for (code in c("%let v = 1234;\ndata _null_; y = &v; x = nosuchfn(1); run;",
                 "%let sfx = ;\ndata _null_; y = 1&sfx; x = nosuchfn(1); run;",
                 "data _null_; y = 1 %str(   ); x = nosuchfn(1); run;",
                 "%macro nothing; %mend;\ndata _null_; %nothing x = nosuchfn(1); run;",
                 "data _null_; %* a macro comment; x = nosuchfn(1); run;")) {
    omitted(sas_run(code, on_error = "sas")$log, "a numbered ERROR on a line with macro references")
  }
  # a macro reference on another line of the step, or an ERROR that is not
  # a compile-time ERROR at a known column under NOSOURCE: the echo stays
  r <- sas_run("%let v = 1;\ndata b;\n  y = &v;\n  x = nosuchfn(1);\nrun;", on_error = "sas")
  expect_identical(r$log[1:4], c("1          %let v = 1;", "2          data b;", "3            y = &v;", "4            x = nosuchfn(1);"))
  r <- sas_run("options nosource;\ndata b;\n  set nosuch;\nrun;\noptions source;", on_error = "sas")
  expect_identical(r$log[1], "1          options nosource;")
  # OPTIONS SOURCE / NOSOURCE with more text on its line (a comment is no text)
  omitted(sas_run("options nosource; data a; x = 1; run;")$log, "OPTIONS SOURCE or NOSOURCE followed by more text")
  omitted(sas_run("%macro m;\noptions nosource;\n%mend;\n%m data a; x = 1; run;")$log, "OPTIONS SOURCE or NOSOURCE")
  r <- sas_run("options nosource; /* c */\ndata a; x = 1; run;\noptions source;\ndata b; x = 1; run;")
  expect_identical(r$log[1], "1          options nosource; /* c */")
  # text from a macro variable on line 3 is at line 3
  r <- sas_run("%let s = %str(x = nosuchfn(1););\ndata a;\n&s\nrun;", on_error = "sas")
  expect_identical(r$error_line, 3L)
  # a macro call over two lines: the program lines after it keep their numbers
  r <- sas_run("%macro m(a, b); %mend;\ndata a; x = .;\n%m(1,\n 2)\nz = nosuchfn(1);\nrun;", on_error = "sas")
  expect_identical(r$error_line, 5L)
  expect_identical(r$log[5:7], c("5          z = nosuchfn(1);", "               ________", "               68"))
  el <- function(code) sas_run(code, on_error = "sas")$error_line
  # a comment over two lines in an open-code %IF block, and outside one
  expect_identical(el("%if 1 %then %do;\ndata t;\n  x = 1;\n  /* a\n b */\n%end;\n  y = nosuchfn(1);\nrun;"), 7L)
  expect_identical(el("data t;\n  x = 1; /* a\n b */\n  y = nosuchfn(1);\nrun;"), 4L)
  # a comment right after a trigger that spans lines starts on its own line
  expect_identical(el("%macro m;\n%mend;/* c */\ndata t;\n  y = nosuchfn(1);\nrun;"), 4L)
  expect_identical(el("%let a = 1\n 2;/* c */\ndata t;\n  y = nosuchfn(1);\nrun;"), 4L)
  expect_no_error(sas_run("%macro m;\n%mend;/* c */\ntitle \"&sysdate\";"))
  # a macro comment over two lines
  expect_identical(el("%* a\n b;\ndata t;\n  y = nosuchfn(1);\nrun;"), 4L)
  # a call over two lines in an open-code %IF block before the first step
  expect_identical(el("%macro m(a=, b=); %mend;\n%if 1 %then %do;\n%m(a=1,\n b=2)\ndata t;\n  x = 1;\n  y = nosuchfn(1);\nrun;\n%end;"),
                   7L)
  # nested calls over several lines
  expect_identical(el("%macro i(a, b); %mend;\n%macro o(a, b); %mend;\ndata t;\n  x = 1;\n%o(%i(1,\n2),\n 3)\n  y = nosuchfn(1);\nrun;"),
                   8L)
  # the line in a sasonr error after a call over two lines
  expect_error(sas_run("%macro m(a, b); %mend;\ndata t;\n%m(1,\n 2)\n  set nosuch;\nrun;"), "SAS line 5")
  # a macro call over two lines inside a quoted string: the lines after it are not known
  expect_error(sas_run("%macro m(a, b); %mend;\ndata t;\n  x = \"%m(1,\n 2)\";\n  y = 1;\nrun;"),
               "spanning lines inside a quoted string", class = "sasonr_unsupported")
  # the last step without RUN;
  r <- sas_run("data a;\n  x = nosuchfn(1);", on_error = "sas")
  expect_identical(r$log[1:6], c("1          data a;", "2            x = nosuchfn(1);", "                 ________",
                                 "                 68", "", "ERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed."))
  expect_identical(r$log[7:8], c("", "NOTE: sasonr stopped processing this step because of errors."))
  # an ERROR in PROC SQL: the NOEXEC NOTE follows it, QUIT; and the stop NOTE without a blank line
  r <- sas_run("data a; x = 1; run;\nproc sql;\n  select nosuch from a;\nquit;", on_error = "sas")
  expect_identical(r$log[-(1:2)], c("2          proc sql;", "3            select nosuch from a;",
    "ERROR: The following columns were not found in the contributing tables: nosuch.",
    "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.",
    "4          quit;", "NOTE: sasonr stopped processing this step because of errors."))
  # the syntax-check NOTE after an ERROR in a global statement is what the
  # next step writes: after the blank line that ends its lines
  r <- sas_run("options ls=abc;\ndata _null_;\n  put 'x';\nrun;", on_error = "sas")
  i <- which(r$log == "4          run;")
  expect_identical(r$log[i + 1:2], c("", "NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode."))
  # PUT pointers past the line size still fail loud
  expect_error(sas_run("data _null_; put +40000 'x'; run;"), class = "sasonr_unsupported")
  # end to end, with the marker under the line
  r <- sas_run("data a; x = nosuchfn(2); run;", on_error = "sas")
  i <- which(startsWith(r$log, "ERROR 68-185"))
  expect_identical(r$log[(i - 3L):(i + 2L)],
                   c("                       ________", "                       68", "",
                     "ERROR 68-185: The function NOSUCHFN is unknown, or cannot be accessed.", "",
                     "NOTE: sasonr stopped processing this step because of errors."))
})

test_that("%PUT after a step is open code in the HTML view", {
  r <- log_res("data a; x = 1; run;\n%put done;")
  h <- sas_log(r, "html")
  expect_match(h, "<summary>Open code</summary>", fixed = TRUE)
  expect_identical(r$log_events$step[r$log_events$kind == "put"], 0L)
})

test_that("sas_log() still takes session =", {
  s <- sas_session()
  withr::defer(sas_session_close(s))
  sas_run("%put one;", session = s)
  expect_identical(sas_log(session = s), sas_log(s))
  expect_identical(sas_log(session = s, "html"), sas_log(s, "html"))
  r <- sas_run("%put two;")
  expect_error(sas_log(r, session = s), class = "sasonr_error")
  expect_error(sas_log("html", "plain", session = s), class = "sasonr_error")
})

test_that("the summary line counts ERROR lines", {
  ev <- sasonr:::log_events_table(list(
    list(seq = 1L, step = 1L, phase = "run", kind = "error", id = "raw", text = c("ERROR: one", "Other", "ERROR: two")),
    list(seq = 2L, step = 1L, phase = "run", kind = "warning", id = "raw", text = c("WARNING: w", "      more")),
    list(seq = 3L, step = 1L, phase = "run", kind = "put", id = "raw", text = "ERROR: put")
  ), list())
  expect_identical(sasonr:::log_summary_counts(ev), c(error = 2L, warning = 1L))
})

test_that("the lines after the data lines stay in the DATALINES step in the HTML view", {
  r <- log_res("data c;\n  input v;\n  datalines;\n1\n;\nrun;\n%put done;")
  h <- sas_log(r, "html")
  blocks <- strsplit(h, "<details", fixed = TRUE)[[1]][-1]
  expect_length(blocks, 2L)
  expect_match(blocks[1], "5          ;", fixed = TRUE)
  expect_match(blocks[1], "6          run;", fixed = TRUE)
  expect_match(blocks[2], "<summary>Open code</summary>", fixed = TRUE)
  expect_match(blocks[2], "7          %put done;", fixed = TRUE)
})

test_that("only a RUN; chunk joins the DATALINES step in the HTML view", {
  r <- log_res("data c;\n  input v;\n  datalines;\n1\n;\noptions ls=100;\n/* c */\nrun;\n%put done;")
  blocks <- strsplit(sas_log(r, "html"), "<details", fixed = TRUE)[[1]][-1]
  expect_match(blocks[1], "5          ;", fixed = TRUE)
  expect_no_match(blocks[1], "6          options", fixed = TRUE)
  expect_no_match(blocks[1], "8          run;", fixed = TRUE)
  # a RUN; with a comment after it joins the step
  r <- log_res("data c;\n  input v;\n  datalines;\n1\n;\n\nrun; /* c */\n%put done;")
  blocks <- strsplit(sas_log(r, "html"), "<details", fixed = TRUE)[[1]][-1]
  expect_match(blocks[1], "7          run; /* c */", fixed = TRUE)
  # a macro statement between the data lines and RUN; keeps RUN; out of the step
  r <- log_res("data c;\n  input v;\n  datalines;\n1\n;\n%put hi;\nrun;")
  blocks <- strsplit(sas_log(r, "html"), "<details", fixed = TRUE)[[1]][-1]
  expect_no_match(blocks[1], "7          run;", fixed = TRUE)
})

test_that("%PUT with no text fails loud", {
  expect_error(sas_run("%put;"), "%PUT with no text")
  expect_error(sas_run("%let e = ;\n%put &e;"), "%PUT with no text.*SAS line 2")
  expect_error(sas_run("%put   ;"), class = "sasonr_unsupported")
})

test_that("OPTIONS that a macro generates take effect at the line of the invocation", {
  echoed <- function(r) {
    l <- sas_log(r)
    as.integer(sub(" .*", "", l[grepl("^[0-9]+ ", l)]))
  }
  # in a macro body: NOSOURCE from the %m line on, SOURCE again after line 8
  src <- paste0("data a; x = 1; run;\n%macro m;\n proc print data=a;\n run;\n options nosource;\n%mend;\n",
                "%m\noptions source;\ndata b; x = 1; run;")
  expect_identical(echoed(sas_run(src)), c(1:7, 9L))
  src <- paste0("data a; x = 1; run;\n%macro m;\n proc print data=a;\n run;\n options nosource;\n%mend;\n",
                "%m\ndata b; x = 1; run;\ndata c; x = 1; run;")
  expect_identical(echoed(sas_run(src)), 1:7)
  # in a macro variable that holds several lines
  src <- "%let t = %str(data a;\n x = 1;\n\n\n run;\n options ls=100;);\n&t\noptions ls=120;\ndata b; x = 1; run;"
  expect_identical(echoed(sas_run(src)), 1:9)
  src <- "%let o = %str(options\n nosource;);\n&o\ndata _null_; put 'h'; run;"
  expect_identical(echoed(sas_run(src)), 1:3)
  expect_identical(sasonr:::log_ls_at_all(data.frame(line = c(0L, 4L, 8L), ls = c(132L, 100L, 120L)), c(3L, 5L, 9L)),
                   c(132L, 100L, 120L))
  # the settings are recorded in line order (internal check)
  expect_error(sasonr:::log_setting_order_check(c(0L, 9L), 7L), class = "rlang_error")
})

test_that("the log stops on NA lines", {
  expect_error(sasonr:::log_split_lines(c("a", NA)), class = "rlang_error")
})

test_that("the echo of a long line always ends", {
  setTimeLimit(elapsed = 10, transient = TRUE)
  withr::defer(setTimeLimit(elapsed = Inf))
  echo <- sasonr:::log_echo_line
  # no blank, only blanks between words, a blank right after the prefix
  expect_true(all(nchar(echo(1L, strrep("x", 1000), 64L)) <= 64L))
  expect_true(all(nchar(echo(2L, paste(rep("a", 600), collapse = " "), 64L)) <= 64L))
  expect_true(all(nchar(echo(3L, paste0(strrep(" ", 70), strrep("y", 70)), 64L)) <= 64L))
  # a line size that cannot hold the text: an internal error, not a loop
  expect_error(echo(4L, strrep("z", 40), 10L))
})

test_that("a FILE record longer than LRECL fails loud; items that fit are wrapped", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  expect_error(sas_run("data _null_; file 'o.txt' lrecl=20; put 'abcdefghijklmnopqrstuvwxyz'; run;"),
               "PUT item longer than the FILE LRECL", class = "sasonr_unsupported")
  sas_run("data _null_; file 'o.txt' lrecl=20; put 'abcdefghij' 'klmnopqrst'; run;")
  expect_identical(readLines(file.path(d, "o.txt")), "abcdefghijklmnopqrst")
  sas_run("data _null_; file 'o.txt' lrecl=20; x = 'abcdefghijklmno'; y = 'pqrstuvwxyz'; put x y; run;")
  expect_identical(readLines(file.path(d, "o.txt")), c("abcdefghijklmno", "pqrstuvwxyz"))
})

test_that("a log view prints as its lines, without quotes or indices", {
  r <- sas_run("data _null_;\n  put 'hi';\nrun;")
  out <- utils::capture.output(print(sas_log(r)))
  expect_identical(out, r$log)
  expect_identical(utils::capture.output(sas_log(r, echo = FALSE)), "hi")
  expect_s3_class(sas_log(r), "sasonr_log")
  # the result keeps the log as a plain character vector
  expect_identical(class(r$log), "character")
})

test_that("a log view keeps its character content when subset or combined", {
  r <- sas_run("data _null_;\n  put 'hi';\nrun;")
  x <- sas_log(r)
  expect_identical(as.character(x), r$log)
  expect_s3_class(x[2:3], "sasonr_log")
  expect_identical(as.character(x[2:3]), r$log[2:3])
  expect_identical(c(x, "more"), c(r$log, "more"))
  expect_identical(grepl("^hi$", x), grepl("^hi$", r$log))
  expect_identical(as.character(utils::tail(x, 1)), utils::tail(r$log, 1))
})

test_that("the compact layout numbers the lines in the columns the largest number needs", {
  code <- paste(c("data a;", "  y = . + 3;", rep("  w = 1;", 8), "run;"), collapse = "\n")
  r <- log_res(code)
  x <- sas_log(r, compact = TRUE)
  expect_identical(as.character(x[1:2]), c("1  data a;", "2    y = . + 3;"))
  expect_true("10   w = 1;" %in% x)
  # the places in a NOTE are program lines and columns: the message is unchanged
  msg <- sas_log(r, echo = FALSE)
  expect_true(all(msg %in% x))
  expect_true("      2:9  " %in% x)
  # the plain and ANSI views and the HTML view use the same layout
  expect_identical(cli::ansi_strip(sas_log(r, "ansi", compact = TRUE))[-1], as.character(x))
  expect_match(sas_log(r, "html", compact = TRUE), ">10   w = 1;<", fixed = TRUE)
})

test_that("the compact layout keeps the ERROR marker under its token", {
  r <- sas_run("data a;\n  x = 1;\n  z = nosuchfn(1);\nrun;", on_error = "sas")
  x <- as.character(sas_log(r, compact = TRUE))
  i <- which(x == "3   z = nosuchfn(1);")
  expect_length(i, 1)
  expect_identical(x[i + 1:2], c("        ________", "        68"))
  expect_identical(as.integer(regexpr("_", x[i + 1L])), as.integer(regexpr("nosuchfn", x[i])))
})

test_that("continued lines start with ! in the compact layout", {
  long <- paste0("data _null_; x = 1; ", strrep("y = 2; ", 20), "put x=; run;")
  r <- sas_run(paste0("options ls=64;\n", long))
  x <- as.character(sas_log(r, compact = TRUE))
  expect_true(startsWith(x[2], "2 data _null_;"))
  expect_gt(sum(startsWith(x, "! ")), 1)
})

test_that("the default layout keeps the line numbers in 11 columns", {
  r <- sas_run("data a;\n  x = 1;\n  z = nosuchfn(1);\nrun;", on_error = "sas")
  expect_identical(as.character(sas_log(r)), r$log)
  expect_identical(sas_log(r, compact = FALSE), sas_log(r))
  expect_true("3            z = nosuchfn(1);" %in% r$log)
  expect_error(sas_log(r, compact = NA), class = "sasonr_error")
  expect_error(sas_log(r, "plain", TRUE, TRUE))
})
