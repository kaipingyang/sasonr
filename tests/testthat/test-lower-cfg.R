cfg_meta <- list(
  a = meta_from_df(data.frame(g = c("A", "A", "B"), v = c(1, 2, 3))),
  b = meta_from_df(data.frame(g = c("A", "B"), w = c(10, 20)))
)
cfg_of <- function(code, meta = cfg_meta) {
  x <- dsir_lower(sas_analyze(sas_parse(code)$steps[[1]], meta))
  expect_s3_class(x, "sas_dsir")
  x
}
# terminator kind of each block, in block order
terms_of <- function(x) vapply(x$blocks, function(b) b$term$term, "")
# instruction ops of a block
ops_of <- function(x, id) vapply(x$blocks[[id]]$instrs, `[[`, "", "op")
# the block holding the first instruction that assigns `var`
block_assigning <- function(x, var) {
  for (b in x$blocks) for (i in b$instrs) {
    if (i$op %in% c("assign", "sum") && identical(i$target$name, var)) return(b$id)
  }
  NA_character_
}
cfg_check <- function(x) {
  ids <- names(x$blocks)
  expect_true(x$entry %in% ids)
  # every terminator target exists
  for (b in x$blocks) {
    expect_false(is.null(b$term))
    expect_true(all(dsir_succ(b$term) %in% ids), info = b$id)
  }
  # every block is reachable from the entry
  seen <- character()
  todo <- x$entry
  while (length(todo)) {
    id <- todo[1]
    todo <- todo[-1]
    if (id %in% seen) next
    seen <- c(seen, id)
    todo <- c(todo, dsir_succ(x$blocks[[id]]$term))
  }
  expect_setequal(seen, ids)
}

test_that("a straight-line step is one block ending the iteration", {
  x <- cfg_of("data c; set a; length z $ 3; w = v * 2; total + v; keep g w total; run;")
  expect_length(x$blocks, 1)
  expect_equal(x$entry, "B1")
  expect_equal(ops_of(x, "B1"), c("read", "assign", "sum"))
  expect_equal(x$blocks$B1$instrs[[1]]$reader, 1L)
  expect_equal(x$blocks$B1$term$term, "end_iteration")
  expect_length(x$labels, 0)
  # declarative statements give no instruction; the line of each instruction is kept
  x <- cfg_of("data c;\nset a;\nw = v;\nformat w 8.2;\nrun;")
  expect_equal(vapply(x$blocks$B1$instrs, `[[`, 1L, "line"), c(2L, 3L))
})

test_that("IF-THEN-ELSE is a diamond", {
  x <- cfg_of("data c; set a; if v > 1 then w = 1; else w = 2; u = 3; run;")
  expect_length(x$blocks, 4)
  t1 <- x$blocks$B1$term
  expect_equal(t1$term, "branch")
  expect_equal(t1$cond$node, "compare")
  expect_equal(c(t1$true, t1$false), c("B2", "B3"))
  expect_equal(x$blocks$B2$term, list(term = "jump", line = 1L, to = "B4"))
  expect_equal(x$blocks$B3$term$to, "B4")
  expect_equal(block_assigning(x, "u"), "B4")
  expect_equal(x$blocks$B4$term$term, "end_iteration")
  # IF-THEN without ELSE: the false edge goes to the join
  x <- cfg_of("data c; set a; if v > 1 then w = 1; run;")
  expect_length(x$blocks, 3)
  expect_equal(c(x$blocks$B1$term$true, x$blocks$B1$term$false), c("B2", "B3"))
})

test_that("a subsetting IF branches to a DELETE block", {
  x <- cfg_of("data c; set a; if v > 1; w = v; run;")
  expect_length(x$blocks, 3)
  t1 <- x$blocks$B1$term
  expect_equal(c(t1$term, t1$true, t1$false), c("branch", "B2", "B3"))
  expect_equal(x$blocks$B3$term$term, "delete")
  expect_length(x$blocks$B3$instrs, 0)
  expect_equal(block_assigning(x, "w"), "B2")
})

test_that("DELETE, STOP and RETURN end the block; what follows is unreachable", {
  for (k in c("delete", "stop", "return")) {
    x <- cfg_of(sprintf("data c;\nset a;\nif v = 1 then do;\n%s;\nw = 1;\nend;\nu = 2;\nrun;", k))
    expect_equal(x$blocks$B2$term$term, k)
    expect_true(is.na(block_assigning(x, "w")))
    expect_equal(x$unreachable, 5L)
    expect_equal(block_assigning(x, "u"), "B3")
    cfg_check(x)
  }
  x <- cfg_of("data c; set a; stop; run;")
  expect_equal(unname(terms_of(x)), "stop")
})

test_that("a DO group adds no block", {
  x <- cfg_of("data c; set a; do; w = 1; u = 2; end; run;")
  expect_length(x$blocks, 1)
  expect_equal(ops_of(x, "B1"), c("read", "assign", "assign"))
  expect_length(x$loops, 0)
})

test_that("iterative DO: init, test, body, increment", {
  x <- cfg_of("data c; do i = 1 to 3; x = i; end; y = 1; run;")
  lp <- x$loops[[1]]
  expect_equal(lp[c("kind", "var")], list(kind = "iter", var = "i"))
  # init in the entry block: start value, TO and BY evaluated once
  init <- x$blocks$B1$instrs
  expect_equal(vapply(init, function(i) i$target$node, ""), c("var", "loop_temp", "loop_temp"))
  expect_equal(vapply(init[2:3], function(i) i$target$name, ""), c("to", "by"))
  expect_equal(x$blocks$B1$term$to, lp$head)
  th <- x$blocks[[lp$head]]$term
  expect_equal(th$cond$node, "loop_test")
  expect_equal(c(th$true, th$false), c(lp$body, lp$exit))
  expect_equal(block_assigning(x, "x"), lp$body)
  expect_equal(x$blocks[[lp$body]]$loop, 1L)
  expect_equal(x$blocks[[lp$body]]$term$to, lp$latch)
  # the latch increments and jumps back to the test
  expect_equal(x$blocks[[lp$latch]]$instrs[[1]]$expr$node, "loop_next")
  expect_equal(x$blocks[[lp$latch]]$term$to, lp$head)
  expect_equal(block_assigning(x, "y"), lp$exit)
  cfg_check(x)
})

test_that("iterative DO with WHILE tests after the bound, with UNTIL before the increment", {
  x <- cfg_of("data c; do i = 1 to 9 while (i < 5); x = i; end; run;")
  lp <- x$loops[[1]]
  th <- x$blocks[[lp$head]]$term
  wt <- x$blocks[[th$true]]$term
  expect_equal(wt$cond$node, "compare")
  expect_equal(c(wt$true, wt$false), c(lp$body, lp$exit))
  expect_equal(th$false, lp$exit)
  cfg_check(x)
  x <- cfg_of("data c; do i = 1 to 9 until (i > 5); x = i; end; run;")
  lp <- x$loops[[1]]
  expect_equal(x$blocks[[lp$head]]$term$true, lp$body)
  ut <- x$blocks[[lp$latch]]$term
  expect_equal(ut$cond$node, "compare")
  expect_equal(ut$true, lp$exit)
  incr <- x$blocks[[ut$false]]
  expect_equal(incr$instrs[[1]]$expr$node, "loop_next")
  expect_equal(incr$term$to, lp$head)
  cfg_check(x)
})

test_that("DO value list keeps a position and assigns the value in the body", {
  x <- cfg_of("data c; do k = 1, 5, 7; x = k; output; end; stop; run;")
  lp <- x$loops[[1]]
  expect_equal(lp$kind, "list")
  expect_equal(x$blocks$B1$instrs[[1]]$target, list(node = "loop_temp", loop = 1L, name = "index"))
  b <- x$blocks[[lp$body]]
  expect_equal(b$instrs[[1]]$expr$node, "loop_value")
  expect_length(b$instrs[[1]]$expr$values, 3)
  expect_equal(ops_of(x, lp$body), c("assign", "assign", "output"))
  expect_equal(x$blocks[[lp$exit]]$term$term, "stop")
  cfg_check(x)
})

test_that("DO WHILE tests at the top, DO UNTIL at the bottom", {
  x <- cfg_of("data c; x = 0; do while (x < 3); x + 1; end; run;")
  lp <- x$loops[[1]]
  expect_equal(x$blocks$B1$term$to, lp$head)
  expect_equal(lp$head, lp$latch)
  t <- x$blocks[[lp$head]]$term
  expect_equal(c(t$term, t$true, t$false), c("branch", lp$body, lp$exit))
  expect_equal(x$blocks[[lp$body]]$term$to, lp$head)
  cfg_check(x)

  x <- cfg_of("data c; x = 0; do until (x > 3); x + 1; end; run;")
  lp <- x$loops[[1]]
  # the body runs before the first test
  expect_equal(x$blocks$B1$term$to, lp$body)
  expect_equal(x$blocks[[lp$body]]$term$to, lp$latch)
  t <- x$blocks[[lp$latch]]$term
  expect_equal(c(t$true, t$false), c(lp$exit, lp$body))
  cfg_check(x)
})

test_that("LEAVE jumps to the loop exit, CONTINUE to the latch", {
  x <- cfg_of("data c; do i = 1 to 5; if i = 2 then continue; if i = 4 then leave; x = i; end; run;")
  lp <- x$loops[[1]]
  jumps <- unlist(lapply(x$blocks, function(b) if (b$term$term == "jump" && identical(b$loop, 1L)) b$term$to))
  expect_true(lp$latch %in% jumps)
  expect_true(lp$exit %in% jumps)
  cfg_check(x)
})

test_that("DO UNTIL with LEAVE", {
  x <- cfg_of("data c; x = 0; do until (x > 9); x + 1; if x = 3 then leave; end; y = x; run;")
  lp <- x$loops[[1]]
  br <- x$blocks[[block_assigning(x, "x")]]
  expect_equal(br$id, "B1")
  body <- x$blocks[[lp$body]]
  expect_equal(ops_of(x, lp$body), "sum")
  leave_b <- x$blocks[[body$term$true]]
  expect_equal(leave_b$term$to, lp$exit)
  expect_equal(x$blocks[[body$term$false]]$term$to, lp$latch)
  expect_equal(block_assigning(x, "y"), lp$exit)
  cfg_check(x)
})

test_that("nested IF in a DO loop stays inside the loop", {
  x <- cfg_of("data c; do i = 1 to 3; if i > 1 then do; if i > 2 then x = 1; else x = 2; end; end; run;")
  b_then <- block_assigning(x, "x")
  expect_equal(x$blocks[[b_then]]$loop, 1L)
  inner <- Filter(function(b) identical(b$term$term, "branch") && identical(b$loop, 1L), x$blocks)
  expect_length(inner, 2)
  cfg_check(x)
})

test_that("SELECT with OTHERWISE is a branch chain", {
  x <- cfg_of("data c; set a; select (g); when ('A') x = 1; when ('B', 'C') x = 2; otherwise x = 3; end; y = 1; run;")
  expect_equal(ops_of(x, "B1"), c("read", "assign"))
  expect_equal(x$blocks$B1$instrs[[2]]$target$node, "select_temp")
  t1 <- x$blocks$B1$term
  expect_equal(t1$cond$node, "when_test")
  expect_true(t1$cond$has_expr)
  t2 <- x$blocks[[t1$false]]$term
  expect_length(t2$cond$values, 2)
  other <- x$blocks[[t2$false]]
  expect_equal(other$instrs[[1]]$expr$value, 3)
  join <- other$term$to
  expect_equal(x$blocks[[t1$true]]$term$to, join)
  expect_equal(x$blocks[[t2$true]]$term$to, join)
  expect_equal(block_assigning(x, "y"), join)
  expect_false("abort" %in% terms_of(x))
  cfg_check(x)
})

test_that("SELECT without OTHERWISE ends in a runtime-error terminator", {
  x <- cfg_of("data c; set a; select; when (v > 1) x = 1; end; run;")
  t1 <- x$blocks$B1$term
  expect_false(t1$cond$has_expr)
  ab <- x$blocks[[t1$false]]$term
  expect_equal(ab[c("term", "kind")], list(term = "abort", kind = "select"))
  expect_equal(sum(terms_of(x) == "abort"), 1L)
  cfg_check(x)
  # no WHEN at all: the OTHERWISE body or the error, without a chain
  x <- cfg_of("data c; set a; select (g); otherwise x = 1; end; run;")
  expect_length(x$blocks, 1)
})

test_that("OUTPUT records its targets", {
  x <- cfg_of("data c d; set a; output d; if v > 1 then output; run;")
  o <- x$blocks$B1$instrs[[2]]
  expect_equal(o[c("op", "targets", "index")], list(op = "output", targets = "d", index = 2L))
  o2 <- x$blocks[[x$blocks$B1$term$true]]$instrs[[1]]
  expect_equal(o2$index, 1:2)
})

test_that("PUT, CALL and INPUT are instructions; MERGE is a read", {
  x <- cfg_of("data _null_; set a; put v=; call symputx('m', v); run;")
  expect_equal(ops_of(x, "B1"), c("read", "put", "call"))
  expect_equal(x$blocks$B1$instrs[[3]]$routine, "symputx")
  x <- cfg_of("data c; input x y; datalines;\n1 2\n;\nrun;", list())
  expect_equal(ops_of(x, "B1"), "input")
  expect_equal(x$blocks$B1$instrs[[1]]$reader, 1L)
  x <- cfg_of("data c; merge a b; by g; run;")
  expect_equal(x$blocks$B1$instrs[[1]][c("op", "kind", "reader")], list(op = "read", kind = "merge", reader = 1L))
})

test_that("LAG sites: unconditional, conditional and in a loop test", {
  x <- cfg_of("data c; set a; p = lag(v); if v > 1 then q = dif2(v); run;")
  ls <- x$lag_sites
  expect_equal(ls$id, 1:2)
  expect_equal(ls$fn, c("lag", "dif"))
  expect_equal(ls$n, c(1L, 2L))
  expect_equal(ls$block, c("B1", block_assigning(x, "q")))
  expect_false(ls$block[2] == "B1")
  x <- cfg_of("data c; set a; if lag(v) > 1 then q = 1; run;")
  # a LAG in the condition runs before the branch
  expect_equal(x$lag_sites$block, "B1")
  x <- cfg_of("data c; set a; if v > 1 then do; delete; q = lag(v); end; run;")
  expect_true(is.na(x$lag_sites$block))
  x <- cfg_of("data c; set a; run;")
  expect_equal(nrow(x$lag_sites), 0)
})

test_that("LINK and GOTO stay rejected", {
  expect_error(cfg_of("data c; set a; link x; return; x: v = 1; return; run;"), "LINK|statement")
  expect_error(cfg_of("data c; set a; goto x; x: v = 1; run;"), "GOTO|statement")
})

test_that("every block is reachable and every target exists", {
  progs <- c(
    "data c; set a; run;",
    "data c; set a; if v > 1; run;",
    "data c; set a; if v > 1 then delete; else if v > 2 then stop; else return; run;",
    "data c; do i = 1 to 3 by 2 while (i < 9); if i = 3 then leave; x = i; end; run;",
    "data c; do j = 1 to 2; do i = 1 to 3 until (i > 1); if i = 2 then continue; x = i; end; end; run;",
    "data c; do k = 'a', 'b'; select (k); when ('a') x = 1; when ('b') leave; otherwise; end; end; run;",
    "data c; set a; select (g); when ('A') delete; when ('B') stop; end; run;",
    "data c; x = 0; do while (x < 3); do until (x > 1); x + 1; end; x + 1; end; stop; run;",
    "data c; set a end = e; total + v; if e then do; output; stop; end; run;",
    "data c; do until (e); set a end = e; if v > 1 then output; end; stop; run;",
    "data c; set a; array t{2} t1 t2; do i = 1 to 2; t{i} = lag(v); end; drop i; run;"
  )
  for (p in progs) {
    x <- cfg_of(p)
    cfg_check(x)
    expect_true(all(is.na(x$lag_sites$block) | x$lag_sites$block %in% names(x$blocks)))
  }
})

test_that("dsir_dump prints blocks, instructions and edges", {
  x <- cfg_of("data c; set a; if v > 1 then w = lag(v); run;")
  out <- dsir_dump(x)
  expect_equal(out[1], "B1 (entry):")
  expect_true(any(grepl("-> branch (v > 1) ? B2 : B3", out, fixed = TRUE)))
  expect_true(any(grepl("assign w = lag(v)", out, fixed = TRUE)))
  expect_true(any(grepl("lag sites: 1 lag1 in B2", out, fixed = TRUE)))
})
