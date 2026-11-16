# Translation levels: vectorised (L1/L2) and row-wise (L3) code of the same
# DATA steps give the same data sets.

test_that("vectorised (L1/L2) and row-wise (L3) code agree on data sets", {
  code <- "
data a; input g $ h $ v; datalines;
A x 1
A x .
A y 3
B y 4
B z 5
;
data s(rename=(tot=total)); set a; by g;
  retain cnt 10;
  if first.g then tot = 0;
  tot + v;
  cnt + 1;
  prev = lag(v);
  if last.g then output;
  keep g tot cnt prev;
run;
data t; set a; if v > 1; length cls $4;
  select (g); when ('A') cls = 'aa'; otherwise cls = 'other'; end;
  if v >= 4 then do; big = 1; note = 'big'; end; else note = 'small';
run;
"
  auto <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(vapply(auto$translation$steps, `[[`, "", "level"), c("L1", "L2", "L1"))
  expect_equal(auto$work, rows$work)
  expect_equal(auto$work$s$total, c(4, 9))
  expect_equal(auto$work$s$cnt, c(13, 15))
  expect_equal(auto$work$t$note, c("sma", "big", "big"))
})

test_that("a retained variable assigned on every row before use is L1 and equals L3", {
  code <- "
data a; input g $ v; datalines;
A 1
A .
B 3
B 4
;
data r1; retain b a; set a; a = v + 1; b = a * 2; run;
data r2; set a; retain k 5 c ''; if v > 3 then delete; k = v; c = g; w = k + 1; run;
data r3; set a; retain d; if v > 1; d = v; run;
"
  auto <- sas_run(code)
  rows <- sas_run(code, level = "L3")
  expect_equal(vapply(auto$translation$steps, `[[`, "", "level"), c("L1", "L1", "L1", "L1"))
  expect_false(any(grepl("sas_carry", vapply(auto$translation$steps, `[[`, "", "code"))))
  expect_equal(auto$work, rows$work)
  expect_equal(names(auto$work$r1), c("b", "a", "g", "v"))
  expect_equal(auto$work$r2$k, c(1, NA, 3))
})

test_that("LEAVE inside a SELECT inside a DO loop leaves only the SELECT", {
  r <- sas_run("
data _null_;
  do i = 1 to 3;
    select (i);
      when (2) leave;
      otherwise;
    end;
    put 'loop i=' i=;
  end;
  put 'after  i=' i=;
run;
")
  put <- msgs(r)[startsWith(msgs(r), "loop") | startsWith(msgs(r), "after")]
  expect_equal(put, c("loop i=i=1", "loop i=i=2", "loop i=i=3", "after  i=i=4"))
  # a LEAVE in a nested iterative DO still leaves that loop (j keeps its value
  # within the iteration of the step); CONTINUE beside a SELECT-level LEAVE
  # fails loud
  r <- sas_run("data a; do i = 1 to 2; select (i); when (1) do j = 1 to 5; if j = 2 then leave; end; otherwise; end; k = i; output; end; run;")
  expect_equal(r$work$a$j, c(2, 2))
  expect_equal(r$work$a$k, c(1, 2))
  expect_error(sas_run("data a; do i = 1 to 2; select (i); when (1) leave; otherwise continue; end; end; run;", on_error = "abort"),
               class = "sasonr_unsupported")
})

test_that("an iterative DO evaluates TO before it assigns the start to the index", {
  r <- sas_run("
data _null_;
  i = 10;
  put 'before i=' i=;
  do i = 1 to i;
    put 'in     i=' i=;
  end;
  put 'out    i=' i=;
run;
")
  put <- msgs(r)[grepl("^(before|in|out) ", msgs(r))]
  expect_equal(put, c("before i=i=10", sprintf("in     i=i=%d", 1:10), "out    i=i=11"))
  r <- sas_run("data a; i = 3; s = 0; do i = i to 2 * i by i - 2; s + i; end; run;")
  expect_equal(r$work$a$s, sum(3:6))
  expect_equal(r$work$a$i, 7)
})
