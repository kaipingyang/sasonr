stmts <- function(code) sas_parse(code)$steps[[1]]$stmts
kinds <- function(code) vapply(stmts(code), `[[`, "", "stmt")

test_that("`name =` is an assignment even for keywords", {
  expect_equal(kinds("data _null_; set = 1; if = 2; end = 3; run;"), rep("assign", 3))
  expect_equal(kinds("data _null_; n + 1; run;"), "sum")
})

test_that("IF/ELSE keeps structure and the dangling ELSE binds to the nearest IF", {
  s <- stmts("data _null_; if x>1 then if x>10 then h=1; else h=2; run;")[[1]]
  expect_equal(s$stmt, "if")
  expect_null(s[["else"]])
  expect_equal(s$then$stmt, "if")
  expect_equal(s$then[["else"]]$stmt, "assign")
  expect_equal(kinds("data a; set b; if x; run;"), c("set", "subset_if"))
})

test_that("DO forms are parsed faithfully", {
  s <- stmts("data _null_; do i = 1 to 10 by 2 while (x < 3); y = i; end; run;")[[1]]
  expect_equal(s$kind, "iter")
  expect_equal(s$while_until$kind, "while")
  expect_length(s$body, 1)
  expect_equal(stmts("data _null_; do k = 1, 3, 5; end; run;")[[1]]$kind, "list")
  expect_equal(stmts("data _null_; do c = 'a', 'b'; end; run;")[[1]]$kind, "list")
  expect_equal(stmts("data _null_; do until (m > 3); m + 1; end; run;")[[1]]$kind, "until")
  expect_equal(stmts("data _null_; do; x = 1; do; y = 2; end; end; run;")[[1]]$body[[2]]$kind, "block")
})

test_that("SELECT keeps WHEN lists and OTHERWISE", {
  s <- stmts("data _null_; select (g); when ('A', 'B') x = 1; when ('C') do; x = 2; end; otherwise; end; run;")[[1]]
  expect_equal(s$stmt, "select")
  expect_length(s$whens, 2)
  expect_length(s$whens[[1]]$values, 2)
  expect_true(s$has_otherwise)
  expect_equal(s$otherwise$stmt, "null")
})

test_that("SET/MERGE data set options and statement options", {
  s <- stmts("data m; merge a(in=ina rename=(x=y) keep=id x where=(x > 1)) work.b(drop=z) end=eof; by id descending t; run;")
  expect_equal(s[[1]]$stmt, "merge")
  o <- s[[1]]$datasets[[1]]$options
  expect_equal(o$in_var, "ina")
  expect_equal(o$rename, c(x = "y"))
  expect_equal(s[[1]]$datasets[[2]]$name, "b")
  expect_equal(s[[1]]$end, "eof")
  expect_equal(s[[2]]$descending, c(FALSE, TRUE))
  expect_error(sas_parse("data a; set b(compress=yes); run;"), class = "sasonr_unsupported")
})

test_that("declarations: RETAIN, LENGTH, FORMAT, LABEL, ARRAY", {
  s <- stmts("data a; retain x y 0 z 'a'; length c $3 n 8; format d date9. x 8.2; label x='X lab';
              array t{3} _temporary_ (10 20 30); array a{*} $ 5 c1-c3; run;")
  expect_equal(s[[1]]$groups[[1]]$init[[1]]$value, 0)
  expect_equal(s[[2]]$specs[[1]]$len, 3L)
  expect_equal(s[[3]]$specs[[1]]$fmt$text, "date9.")
  expect_equal(s[[3]]$specs[[2]]$fmt$d, 2L)
  expect_true(s[[5]]$temporary)
  expect_length(s[[5]]$init, 3)
  expect_true(s[[6]]$char)
  expect_equal(s[[6]]$items[[1]]$kind, "range")
})

test_that("PUT and INPUT items", {
  p <- stmts("data _null_; put 'x' d= date9. n= s $char5.; run;")[[1]]
  expect_equal(vapply(p$items, `[[`, "", "kind"), c("lit", "named", "named", "value"))
  expect_equal(p$items[[2]]$fmt$name, "date")
  i <- stmts("data a; input n $ v s $char5. t :$10.; datalines;\nx 1 abc   q\n;\nrun;")[[1]]
  expect_equal(vapply(i$items, `[[`, "", "mode"), c("list", "list", "formatted", "modified"))
  expect_equal(i$items[[4]]$fmt$w, 10L)
})

test_that("step boundaries: RUN, the next DATA, and DATALINES", {
  p <- sas_parse("data a; x=1;\ndata b; set a;\ndata _null_; set b; put x=;")
  expect_length(p$steps, 3)
  expect_equal(p$steps[[2]]$outputs[[1]]$name, "b")
  p <- sas_parse("data hi lo; set a; run;")
  expect_equal(vapply(p$steps[[1]]$outputs, `[[`, "", "name"), c("hi", "lo"))
})

test_that("unsupported statements fail loud with the SAS line number", {
  err <- expect_error(sas_parse("data a;\n  x = 1;\n  file 'f.txt';\nrun;"), class = "sasonr_unsupported")
  expect_equal(err$sas_line, 3L)
  expect_error(sas_parse("proc means data=a; run;"), class = "sasonr_unsupported")
  expect_error(sas_parse("%let x = 1;"), class = "sasonr_unsupported")
  expect_error(sas_parse("options obs=3;"), class = "sasonr_unsupported")
  expect_error(sas_parse("data a; call execute('x'); run;"), class = "sasonr_unsupported")
})

test_that("syntax errors are classed", {
  expect_error(sas_parse("data a; do i = 1 to 3; x = 1; run;"), class = "sasonr_syntax_error")
  expect_error(sas_parse("data a; else x = 1; run;"), class = "sasonr_syntax_error")
  expect_error(sas_parse("data a; x = (1 + ; run;"), class = "sasonr_syntax_error")
})
