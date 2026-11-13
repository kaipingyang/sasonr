# SasSession (round 16, P0-0): run state lives in a session object; successive
# sas_run(session = s) calls behave like successive submissions to one SAS
# session; nothing a run changes is package-global.

test_that("WORK, macro variables, formats, options and titles persist across submissions", {
  s <- sas_session(work = list(inp = data.frame(x = c(1, 2))))
  on.exit(sas_session_close(s))
  r1 <- sas_run("
data a; set inp; run;
%let k = 10;
options nonotes ls = 80;
title 'T1';
proc format; value yn 1 = 'one' 2 = 'two'; run;
", session = s)
  expect_identical(r1$status, "ok")
  r2 <- sas_run("data b; set a; y = x * &k; z = put(x, yn.); run;", session = s)
  expect_identical(r2$work$b$y, c(10, 20))
  expect_identical(r2$work$b$z, c("one", "two"))
  expect_identical(sort(names(sas_work(s))), c("a", "b", "inp"))
  expect_identical(sas_macro_vars(s)$k, "10")
  expect_true("syscc" %in% names(sas_macro_vars(s, "automatic")))
  expect_false("syscc" %in% names(sas_macro_vars(s)))
  expect_identical(s$options$linesize, 80L)
  expect_false(s$options$notes)
  expect_identical(s$titles, "T1")
  expect_identical(s$last, "b")
  expect_named(sas_meta(s), c("a", "b", "inp"))
})

test_that("the session log and listing accumulate the submissions", {
  s <- sas_session()
  on.exit(sas_session_close(s))
  r1 <- sas_run("data _null_; put 'first'; run;", session = s)
  r2 <- sas_run("%put second;", session = s)
  expect_true("first" %in% r1$log)
  expect_false("first" %in% r2$log)
  expect_identical(sas_log(s), c(r1$log, r2$log))
  r3 <- sas_run("data a; x = 1; run; proc print data = a; run;", session = s)
  expect_identical(sas_listing(s), r3$listing)
})

test_that("an ERROR does not put the next submission in syntax-check mode; SYSCC carries over", {
  s <- sas_session()
  on.exit(sas_session_close(s))
  r1 <- sas_run("data a; set nosuch; run; data b; x = 1; run;", session = s, on_error = "sas")
  expect_identical(r1$status, "error")
  expect_identical(nrow(r1$work$b), 0L)
  r2 <- sas_run("data c; x = 1; run;", session = s, on_error = "sas")
  expect_identical(r2$status, "ok")
  expect_identical(nrow(r2$work$c), 1L)
  expect_identical(sas_macro_vars(s, "automatic")$syscc, "1012")
})

test_that("a run without a session is independent of every other run", {
  r1 <- sas_run("data a; x = 1; run; %let m = 1; options ls = 64; proc format; value yn 1 = 'y'; run;")
  r2 <- sas_run("data b; x = 1; run;")
  expect_identical(names(r2$work), "b")
  expect_null(r2$macro_vars$m)
  expect_error(sas_run("data c; z = put(1, yn.); run;", on_error = "abort"), class = "sasonr_syntax_error")
  # nothing is left bound after a run
  expect_null(sasonr:::.active$session)
})

test_that("two interleaved sessions give independent results", {
  s1 <- sas_session()
  s2 <- sas_session()
  on.exit({
    sas_session_close(s1)
    sas_session_close(s2)
  })
  sas_run("%let v = one; data a; x = 1; run; proc format; value fs 1 = 'S1'; run; options ls = 70;", session = s1)
  sas_run("%let v = two; data a; x = 2; run; proc format; value fs 2 = 'S2'; run; options ls = 90;", session = s2)
  r1 <- sas_run("data b; set a; y = put(x, fs.); length w $ 8; w = \"&v\"; run;", session = s1)
  r2 <- sas_run("data b; set a; y = put(x, fs.); length w $ 8; w = \"&v\"; run;", session = s2)
  expect_identical(r1$work$b$y, "S1")
  expect_identical(r2$work$b$y, "S2")
  expect_identical(r1$work$b$w, "one")
  expect_identical(r2$work$b$w, "two")
  expect_identical(c(s1$options$linesize, s2$options$linesize), c(70L, 90L))
})

test_that("a nested sas_run() inside a step does not corrupt the outer run", {
  inner <- NULL
  orig <- sasonr:::sas_f_upcase
  local_mocked_bindings(sas_f_upcase = function(...) {
    if (is.null(inner)) {
      inner <<- sas_run("data z; q = 1; y = 1 / 0; run; %let mv = inner; options ls = 64;")
    }
    orig(...)
  })
  s <- sas_session()
  on.exit(sas_session_close(s))
  res <- sas_run("options ls = 100; %let mv = outer;
data a; length c $ 3; x = 0; c = upcase('ab'); y = 1 / x; put c= y=; run;", session = s, level = "L3")
  # the inner run had its own session
  expect_identical(names(inner$work), "z")
  expect_identical(inner$macro_vars$mv, "inner")
  # the outer step kept its data error, its log and its options
  expect_identical(res$work$a$c, "AB")
  expect_true(any(grepl("Division by zero", res$log)))
  expect_false("z" %in% names(res$work))
  expect_identical(res$macro_vars$mv, "outer")
  expect_identical(s$options$linesize, 100L)
  expect_identical(sasonr:::sas_rt_active(), sasonr:::default_session())
})

test_that("a nested submission to the same session is refused; the outer run is intact (review dd816d3, 1)", {
  s <- sas_session()
  on.exit(sas_session_close(s))
  nested <- NULL
  orig <- sasonr:::sas_f_upcase
  local_mocked_bindings(sas_f_upcase = function(...) {
    if (is.null(nested)) nested <<- tryCatch(sas_run("%let mv = inner; %put inner;", session = s), error = function(e) e)
    orig(...)
  })
  res <- sas_run("%let mv = outer;
data a; length c $ 3; x = 0; c = upcase('ab'); y = 1 / x; put c=; run;", session = s, level = "L3")
  expect_s3_class(nested, "sasonr_error")
  expect_match(conditionMessage(nested), "another submission")
  expect_identical(res$work$a$c, "AB")
  expect_true("c=AB" %in% res$log)
  expect_true(any(grepl("Division by zero", res$log)))
  expect_false("inner" %in% res$log)
  expect_identical(res$macro_vars$mv, "outer")
})

test_that("a submission releases the session and drops its closures, also after an R error (review dd816d3, 1)", {
  s <- sas_session()
  on.exit(sas_session_close(s))
  released <- function() {
    expect_false(s$busy)
    expect_null(s$macro$exec)
    expect_null(s$macro$on_put)
    expect_null(s$rt$on_note)
    expect_null(s$rt$on_error)
    expect_null(s$rt$on_warn)
    expect_null(sasonr:::.active$session)
  }
  sas_run("data a; x = 1; put x=; run;", session = s)
  released()
  expect_error(sas_run("data b; set nosuch; run;", session = s, on_error = "abort"), class = "sasonr_runtime_error")
  released()
  expect_identical(sas_run("data c; set a; run;", session = s)$work$c$x, 1)
})

test_that("the SQL connection is opened on first use and closed with the session", {
  s <- sas_session()
  sas_run("data a; x = 1; output; x = 2; output; run;", session = s)
  expect_null(s$sql$con)
  r <- sas_run("proc sql; create table b as select sum(x) as t from a; quit;", session = s)
  expect_identical(r$work$b$t, 3)
  con <- s$sql$con
  expect_true(DBI::dbIsValid(con))
  # the connection persists across submissions
  sas_run("proc sql; create table c as select max(x) as m from a; quit;", session = s)
  expect_identical(s$sql$con, con)
  sas_session_close(s)
  expect_null(s$sql$con)
  expect_false(DBI::dbIsValid(con))
  # a throw-away session closes its connection when the run ends
  seen <- NULL
  orig <- sasonr:::sql_duck_close
  local_mocked_bindings(sql_duck_close = function(sql) {
    if (!is.null(sql$con)) seen <<- sql$con
    orig(sql)
  })
  sas_run("data a; x = 1; run; proc sql; create table b as select sum(x) as t from a; quit;")
  expect_false(is.null(seen))
  expect_false(DBI::dbIsValid(seen))
})

test_that("function-layer calls outside a run use the default session", {
  rt <- sasonr:::rt_diag()
  old <- rt$prx
  on.exit(rt$prx <- old)
  rt$prx <- list()
  expect_identical(sas_f_prxparse("/a+/"), 1)
  expect_identical(sas_f_prxparse("/b/"), 2)
  # a run does not see or change these ids
  res <- sas_run("data a; id = prxparse('/c/'); run;")
  expect_identical(res$work$a$id, 1)
  expect_length(rt$prx, 2L)
})

test_that("accessors reject objects that are not sessions", {
  expect_error(sas_work(list()), class = "sasonr_error")
  expect_error(sas_run("data a; run;", session = list()), class = "sasonr_error")
})
