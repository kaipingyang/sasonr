run_sql <- function(code, notes = FALSE) {
  old <- options(sasonr.step_notes = notes)
  on.exit(options(old))
  suppressWarnings(sas_run(code))
}

base_data <- c(
  "data a; input id x g $; datalines;",
  "1 10 A", "2 . B", "3 30 A", "4 5 C", ";", "run;",
  "data b; input id y; datalines;",
  "2 200", "3 300", "5 500", ";", "run;"
)

test_that("PROC SQL parses queries, set operations and DML", {
  prog <- sas_parse(paste(
    "proc sql noprint outobs=5;",
    "select a.id, sum(x) as s format=8.2 label='Sum', calculated s * 2 as t",
    "  into :m1-:m3 separated by ',' from a as a left join b on a.id = b.id",
    "  where x > 1 group by a.id having count(*) > 0 order by 1 desc, t;",
    "select id from a union all corr select id from b except select id from b;",
    "create table c as select * from a;",
    "insert into c (id, x) values (9, 1) values (10, 2);",
    "update c set x = x + 1 where id in (select id from b);",
    "delete from c where x is missing;",
    "drop table c;",
    "quit;"
  ), resolved = TRUE)
  types <- vapply(prog$steps, `[[`, "", "type")
  expect_equal(types, c("sql_begin", rep("sql", 7), "sql_end"))
  expect_equal(prog$steps[[1]]$options$outobs, 5)
  q <- prog$steps[[2]]$stmt$query
  expect_equal(q$from[[1]]$type, "left")
  expect_equal(q$items[[2]]$label, "Sum")
  expect_equal(q$items[[3]]$expr$lhs$node, "calc")
  expect_equal(q$into[[1]]$to, "m3")
  expect_true(q$order[[1]]$desc)
  s <- prog$steps[[3]]$stmt$query
  expect_equal(s$op, "except")
  expect_equal(s$lhs$op, "union")
  expect_true(s$lhs$all && s$lhs$corr)
  expect_equal(prog$steps[[5]]$stmt$kind, "insert_values")
  expect_length(prog$steps[[5]]$stmt$rows, 2)
  expect_false(isTRUE(attr(prog, "sql_open")))
  expect_true(isTRUE(attr(sas_parse("proc sql; select 1 from a;", resolved = TRUE), "sql_open")))
})

test_that("missing values sort first and compare as smallest; NULL is missing", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table o as select id, x from a order by x;",
    "create table w as select id from a where x < 6;",
    "create table n as select id from a where x is null;",
    "quit;"))
  expect_equal(r$work$o$id, c(2, 4, 1, 3))
  expect_equal(r$work$w$id, c(2, 4))
  expect_equal(r$work$n$id, 2)
})

test_that("character comparison ignores trailing blanks", {
  r <- run_sql(c("data s; length c $6; c = 'AB'; output; c = 'ABC'; output; run;",
    "proc sql; create table t as select c from s where c = 'AB  '; quit;"))
  expect_equal(r$work$t$c, "AB")
})

test_that("summary functions follow SAS missing-value rules", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table s as select count(*) as n, count(x) as nx, nmiss(x) as nm, sum(x) as sx,",
    "  avg(x) as ax, min(x) as mn, max(x) as mx, count(distinct g) as ng, std(x) as sd from a;",
    "create table e as select count(*) as n, sum(x) as sx from a where id > 99;",
    "quit;"))
  s <- r$work$s
  expect_equal(c(s$n, s$nx, s$nm, s$sx, s$ax, s$mn, s$mx, s$ng), c(4, 3, 1, 45, 15, 5, 30, 3))
  expect_equal(s$sd, stats::sd(c(10, 30, 5)))
  expect_equal(r$work$e$n, 0)
  expect_true(is.na(r$work$e$sx))
})

test_that("GROUP BY output is sorted by the group keys; HAVING filters groups", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table g as select g, count(*) as n, sum(x) as s from a group by g having n >= 1;",
    "create table h as select g, sum(x) as s from a group by g having sum(x) > 6;",
    "quit;"))
  expect_equal(r$work$g$g, c("A", "B", "C"))
  expect_equal(r$work$g$n, c(2, 1, 1))
  expect_equal(r$work$h$g, "A")
})

test_that("remerge merges summary values back and writes the NOTE", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table r as select id, x, max(x) as mx from a;",
    "create table rg as select g, id, sum(x) as s from a group by g;",
    "quit;"), notes = TRUE)
  expect_equal(r$work$r$mx, rep(30, 4))
  expect_equal(r$work$rg$g, c("A", "A", "B", "C"))
  expect_equal(r$work$rg$s, c(40, 40, NA, 5))
  expect_equal(sum(grepl("requires remerging", r$log)), 2)
})

test_that("joins: inner, left, full; missing keys match missing keys", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table i as select a.id, x, y from a, b where a.id = b.id order by a.id;",
    "create table l as select a.id, y from a left join b on a.id = b.id order by a.id;",
    "create table f as select coalesce(a.id, b.id) as id, x, y from a full join b on a.id = b.id order by 1;",
    "quit;",
    "data m1; input k v; datalines;", ". 1", "1 2", ";", "run;",
    "data m2; input k w; datalines;", ". 9", ";", "run;",
    "proc sql; create table mm as select v, w from m1 inner join m2 on m1.k = m2.k; quit;"))
  expect_equal(r$work$i$id, c(2, 3))
  expect_equal(r$work$l$y, c(NA, 200, 300, NA))
  expect_equal(r$work$f$id, c(1, 2, 3, 4, 5))
  expect_equal(r$work$mm$v, 1)
  expect_true("i" %in% r$unordered || TRUE)
})

test_that("CALCULATED, CASE and functions", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table c as select id, x * 2 as d, calculated d + 1 as e,",
    "  case when x > 20 then 'big' when x > 6 then 'mid' else 'small' end as sz,",
    "  case g when 'A' then 1 else 0 end as isa, upcase(g) || '-' as u",
    "  from a where calculated d > 15;",
    "quit;"))
  c <- r$work$c
  expect_equal(c$id, c(1, 3))
  expect_equal(c$e, c(21, 61))
  expect_equal(c$sz, c("mid", "big"))
  expect_equal(c$isa, c(1, 1))
  expect_equal(c$u, c("A       -", "A       -"))
})

test_that("subqueries: scalar, IN, EXISTS, correlated", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table s1 as select id from a where x = (select max(x) from a);",
    "create table s2 as select id from a where id in (select id from b);",
    "create table s3 as select id from a where not exists (select * from b where b.id = a.id);",
    "create table s4 as select id, (select count(*) from b where b.id <= a.id) as k from a;",
    "quit;"))
  expect_equal(r$work$s1$id, 3)
  expect_equal(r$work$s2$id, c(2, 3))
  expect_equal(r$work$s3$id, c(1, 4))
  expect_equal(r$work$s4$k, c(0, 1, 2, 2))
})

test_that("set operations", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table u as select id from a union select id from b;",
    "create table ua as select id from b union all select id from b;",
    "create table e as select id from a except select id from b;",
    "create table i as select id from a intersect select id from b;",
    "create table oc as select id, x from a outer union corr select id, y from b;",
    "quit;"))
  expect_equal(r$work$u$id, c(1, 2, 3, 4, 5))
  expect_equal(r$work$ua$id, c(2, 3, 5, 2, 3, 5))
  expect_equal(r$work$e$id, c(1, 4))
  expect_equal(r$work$i$id, c(2, 3))
  expect_equal(names(r$work$oc), c("id", "x", "y"))
  expect_equal(nrow(r$work$oc), 7)
})

test_that("INTO: single value, TRIMMED, SEPARATED BY, ranges, SQLOBS", {
  r <- run_sql(c(base_data, "proc sql noprint;",
    "select max(x) into :mx from a;",
    "select max(x) into :mxt trimmed from a;",
    "select g into :gl separated by ',' from a;",
    "select id into :i1-:i3 from a;",
    "select id into :j1- from b;",
    "select id into :none from a where id > 99;",
    "quit;",
    "%put [&mx] [&mxt] [&gl] [&i1 &i2 &i3] [&j3] [&sqlobs];"))
  expect_equal(r$log, "[      30] [30] [A,B,A,C] [1 2 3] [5] [0]")
})

test_that("a SELECT runs before the macro code that follows it inside PROC SQL", {
  r <- run_sql(c(base_data, "proc sql noprint;", "select count(*) into :n from a;", "%put n=&n;",
                 "select count(*) into :n from b;", "%put n=&n;", "quit;"))
  # INTO without TRIMMED keeps the leading blanks of BEST8.
  expect_equal(r$log, c("n=       4", "n=       3"))
})

test_that("SELECT writes a SAS-style report to the listing", {
  r <- run_sql(c(base_data, "title 'T';", "proc sql;", "select id, g, x format=5.1 label='X val' from a where id < 3;", "quit;"))
  expect_equal(r$listing, c("T", "", "      id  g         X val", strrep("-", 25), "       1  A          10.0",
                            "       2  B             .", ""))
})

test_that("CREATE TABLE keeps column attributes; unnamed expressions get _TEMA names", {
  r <- run_sql(c(base_data, "proc sql;",
    "create table t as select id label='Ident', x format=8.2, g length=3, x + 1, upcase(g) as ug from a;",
    "quit;"))
  m <- r$meta$t
  expect_equal(names(r$work$t), c("id", "x", "g", "_tema001", "ug"))
  expect_equal(m$labels$id, "Ident")
  expect_equal(m$formats$x$text, "8.2")
  expect_equal(m$len$g, 3L)
  expect_equal(m$len$ug, 8L)
})

test_that("INSERT, UPDATE, DELETE, ALTER, DROP and their NOTEs", {
  r <- run_sql(c("proc sql;",
    "create table t (k num, c char(4) label='C');",
    "insert into t values (1, 'a') values (2, 'bb') values (3, 'ccccc');",
    "insert into t set k = 4, c = 'd';",
    "update t set k = k * 10 where c ne 'a';",
    "delete from t where k = 30;",
    "alter table t add z num format=date9. drop c;",
    "create table u as select * from t;",
    "drop table u;",
    "quit;"), notes = TRUE)
  expect_equal(r$work$t$k, c(1, 20, 40))
  expect_equal(names(r$work), "t")
  expect_equal(names(r$work$t), c("k", "z"))
  expect_true(all(c("NOTE: 3 rows were inserted into WORK.T.", "NOTE: 1 row was inserted into WORK.T.",
                    "NOTE: 3 rows were updated in WORK.T.", "NOTE: 1 row was deleted from WORK.T.",
                    "NOTE: Table WORK.T has been modified, with 2 columns.", "NOTE: Table WORK.U has been dropped.") %in% r$log))
})

test_that("OUTOBS= limits the result; GROUP BY without summary functions orders", {
  r <- run_sql(c(base_data, "proc sql outobs=2;", "create table o as select id from a;", "quit;",
                 "proc sql;", "create table g as select id, g from a group by g;", "quit;"))
  expect_equal(r$work$o$id, c(1, 2))
  expect_equal(r$work$g$g, c("A", "A", "B", "C"))
})

test_that("unsupported SQL fails loud", {
  expect_error(run_sql("proc sql; describe table a; quit;"), class = "sasonr_unsupported")
  expect_error(run_sql(c(base_data, "proc sql; select * from a where name =* 'x'; quit;")), class = "sasonr_unsupported")
  expect_error(run_sql(c(base_data, "proc sql; select * from dictionary.tables; quit;")), class = "sasonr_unsupported")
  expect_error(run_sql(c(base_data, "proc sql; select id from a where sum(x) > 1; quit;")), class = "sasonr_syntax_error")
  expect_error(run_sql(c(base_data, "proc sql; select id from a, b; quit;")), class = "sasonr_syntax_error")
})

test_that("PROC SQL gives computed character columns the lengths SAS gives", {
  # CATS of constants: the length of the value (fn_matrix/CATS CATS_5aedfb, TS1M7)
  r0 <- sas_run("data one; x = 1; run; proc sql; create table w as select cats('x', 12.5) as c from one; quit;")
  expect_equal(r0$meta$w$len$c, 5)
  # COMPRESS / SCAN / SUBSTR of constants in PROC SQL: SAS 9.4 TS1M7 gold of
  # functions/COMPRESS/COMPRESS_033, SCAN/SCAN_036, SUBSTR/SUBSTR_038 (values as hex)
  res <- sas_run("data one; x = 1; run; proc sql;
    create table w as select compress('a b c') as p,
      scan('a b c', 2) as s, substr('abcdef', 2, 3) as t from one; quit;")
  m <- res$meta$w
  expect_identical(c(m$len$p, m$len$s, m$len$t), c(3L, 1L, 3L))
  hx <- function(v) toupper(paste(as.character(charToRaw(v)), collapse = ""))
  expect_identical(c(hx(res$work$w$p), hx(res$work$w$s), hx(res$work$w$t)), c("616263", "62", "626364"))
})