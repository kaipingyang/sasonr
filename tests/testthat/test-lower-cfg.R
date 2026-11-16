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

test_that("a straight-line step: the read, the end-of-input stop, one block ending the iteration", {
  x <- cfg_of("data c; set a; length z $ 3; w = v * 2; total + v; keep g w total; run;")
  expect_length(x$blocks, 3)
  expect_equal(x$entry, "B1")
  expect_equal(ops_of(x, "B1"), "read")
  expect_equal(x$blocks$B1$instrs[[1]]$reader, 1L)
  expect_equal(x$blocks$B1$term[c("term", "true", "false", "eof")], list(term = "branch", true = "B2", false = "B3", eof = TRUE))
  expect_equal(x$blocks$B1$term$cond, list(node = "eof", reader = 1L))
  expect_equal(x$blocks$B2$term[c("term", "eof")], list(term = "stop", eof = TRUE))
  expect_equal(ops_of(x, "B3"), c("assign", "sum"))
  expect_equal(x$blocks$B3$term$term, "end_iteration")
  expect_length(x$labels, 0)
  # declarative statements give no instruction; the line of each instruction is kept
  x <- cfg_of("data c;\nset a;\nw = v;\nformat w 8.2;\nrun;")
  expect_equal(vapply(c(x$blocks$B1$instrs, x$blocks$B3$instrs), `[[`, 1L, "line"), c(2L, 3L))
})

test_that("IF-THEN-ELSE is a diamond", {
  x <- cfg_of("data c; set a; if v > 1 then w = 1; else w = 2; u = 3; run;")
  expect_length(x$blocks, 6)
  t1 <- x$blocks$B3$term
  expect_equal(t1$term, "branch")
  expect_equal(t1$cond$node, "compare")
  expect_equal(c(t1$true, t1$false), c("B4", "B5"))
  expect_equal(x$blocks$B4$term, list(term = "jump", line = 1L, to = "B6"))
  expect_equal(x$blocks$B5$term$to, "B6")
  expect_equal(block_assigning(x, "u"), "B6")
  expect_equal(x$blocks$B6$term$term, "end_iteration")
  # IF-THEN without ELSE: the false edge goes to the join
  x <- cfg_of("data c; set a; if v > 1 then w = 1; run;")
  expect_length(x$blocks, 5)
  expect_equal(c(x$blocks$B3$term$true, x$blocks$B3$term$false), c("B4", "B5"))
})

test_that("a subsetting IF branches to a DELETE block", {
  x <- cfg_of("data c; set a; if v > 1; w = v; run;")
  expect_length(x$blocks, 5)
  t1 <- x$blocks$B3$term
  expect_equal(c(t1$term, t1$true, t1$false), c("branch", "B4", "B5"))
  expect_equal(x$blocks$B5$term$term, "delete")
  expect_length(x$blocks$B5$instrs, 0)
  expect_equal(block_assigning(x, "w"), "B4")
})

test_that("DELETE, STOP and RETURN end the block; what follows is unreachable", {
  for (k in c("delete", "stop", "return")) {
    x <- cfg_of(sprintf("data c;\nset a;\nif v = 1 then do;\n%s;\nw = 1;\nend;\nu = 2;\nrun;", k))
    expect_equal(x$blocks$B4$term$term, k)
    expect_true(is.na(block_assigning(x, "w")))
    expect_equal(x$unreachable, 5L)
    expect_equal(block_assigning(x, "u"), "B5")
    cfg_check(x)
  }
  x <- cfg_of("data c; set a; stop; run;")
  expect_equal(unname(terms_of(x)), c("branch", "stop", "stop"))
})

test_that("a DO group adds no block", {
  x <- cfg_of("data c; set a; do; w = 1; u = 2; end; run;")
  expect_length(x$blocks, 3)
  expect_equal(ops_of(x, "B3"), c("assign", "assign"))
  expect_length(x$loops, 0)
})

test_that("iterative DO: init, test, body, increment", {
  x <- cfg_of("data c; do i = 1 to 3; x = i; end; y = 1; run;")
  lp <- x$loops[[1]]
  expect_equal(lp[c("kind", "var")], list(kind = "iter", var = "i"))
  # init in the entry block: start value, TO and BY evaluated once, then the index
  init <- x$blocks$B1$instrs
  expect_equal(vapply(init, function(i) i$target$node, ""), c("loop_temp", "loop_temp", "loop_temp", "var"))
  expect_equal(vapply(init[1:3], function(i) i$target$name, ""), c("from", "to", "by"))
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
  # the values are evaluated once in the pre-header, then the position
  expect_equal(vapply(x$blocks$B1$instrs, function(i) i$target$name, ""), c("v1", "v2", "v3", "index"))
  b <- x$blocks[[lp$body]]
  expect_equal(b$instrs[[1]]$expr$node, "loop_value")
  expect_equal(b$instrs[[1]]$expr$values[[3]], list(node = "loop_temp", loop = 1L, name = "v3"))
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
  expect_equal(ops_of(x, "B3"), "assign")
  expect_equal(x$blocks$B3$instrs[[1]]$target$node, "select_temp")
  t1 <- x$blocks$B3$term
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
  t1 <- x$blocks$B3$term
  expect_false(t1$cond$has_expr)
  ab <- x$blocks[[t1$false]]$term
  expect_equal(ab[c("term", "kind")], list(term = "abort", kind = "select"))
  expect_equal(sum(terms_of(x) == "abort"), 1L)
  cfg_check(x)
  # no WHEN at all: the OTHERWISE body or the error, without a chain (the
  # join block is the target of a LEAVE)
  x <- cfg_of("data c; set a; select (g); otherwise x = 1; end; run;")
  expect_length(x$blocks, 4)
})

test_that("OUTPUT records its targets", {
  x <- cfg_of("data c d; set a; output d; if v > 1 then output; run;")
  o <- x$blocks$B3$instrs[[1]]
  expect_equal(o[c("op", "targets", "index")], list(op = "output", targets = "d", index = 2L))
  o2 <- x$blocks[[x$blocks$B3$term$true]]$instrs[[1]]
  expect_equal(o2$index, 1:2)
})

test_that("PUT, CALL and INPUT are instructions; MERGE is a read", {
  x <- cfg_of("data _null_; set a; put v=; call symputx('m', v); run;")
  expect_equal(ops_of(x, "B3"), c("put", "call"))
  expect_equal(x$blocks$B3$instrs[[2]]$routine, "symputx")
  x <- cfg_of("data c; input x y; datalines;\n1 2\n;\nrun;", list())
  expect_equal(ops_of(x, "B1"), "input")
  expect_equal(x$blocks$B1$instrs[[1]]$reader, 1L)
  expect_true(isTRUE(x$blocks$B1$term$eof))
  x <- cfg_of("data c; merge a b; by g; run;")
  expect_equal(x$blocks$B1$instrs[[1]][c("op", "kind", "reader")], list(op = "read", kind = "merge", reader = 1L))
})

test_that("LAG sites: unconditional, conditional and in a loop test", {
  x <- cfg_of("data c; set a; p = lag(v); if v > 1 then q = dif2(v); run;")
  ls <- x$lag_sites
  expect_equal(ls$id, 1:2)
  expect_equal(ls$fn, c("lag", "dif"))
  expect_equal(ls$n, c(1L, 2L))
  expect_equal(ls$block, c("B3", block_assigning(x, "q")))
  expect_false(ls$block[2] == "B3")
  x <- cfg_of("data c; set a; if lag(v) > 1 then q = 1; run;")
  # a LAG in the condition runs before the branch
  expect_equal(x$lag_sites$block, "B3")
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
  expect_true(any(grepl("-> branch (v > 1) ? B4 : B5", out, fixed = TRUE)))
  expect_true(any(grepl("assign w = lag(v)", out, fixed = TRUE)))
  expect_true(any(grepl("lag sites: 1 lag1 in B4", out, fixed = TRUE)))
})

test_that("LAG in a DO value list is evaluated in the pre-header, not in the body", {
  x <- cfg_of("data c; set a; do i = lag(v), lag2(v); w = i; end; run;")
  lp <- x$loops[[1]]
  expect_equal(nrow(x$lag_sites), 2)
  expect_false(any(x$lag_sites$block == lp$body))
  pre <- x$blocks[[x$lag_sites$block[1]]]
  expect_null(pre$loop)
  expect_equal(pre$term$to, lp$head)
  cfg_check(x)
})

test_that("a read and an INPUT end their block with the end-of-input stop", {
  x <- cfg_of("data c; w = 1; set a; u = v; run;")
  expect_equal(ops_of(x, "B1"), c("assign", "read"))
  t <- x$blocks$B1$term
  expect_equal(x$blocks[[t$true]]$term$term, "stop")
  expect_equal(block_assigning(x, "u"), t$false)
  # a read inside a DO loop: the stop block belongs to the loop
  x <- cfg_of("data c; do until (e); set a end = e; end; run;")
  rb <- Filter(function(b) any(vapply(b$instrs, `[[`, "", "op") == "read"), x$blocks)[[1]]
  expect_equal(rb$loop, 1L)
  expect_equal(x$blocks[[rb$term$true]]$loop, 1L)
  cfg_check(x)
})

test_that("unreachable lists statements that give only a terminator or a head", {
  x <- cfg_of("data c;\nset a;\ndelete;\nstop;\nif v > 1 then w = 1;\ndo i = 1 to 2; end;\nselect (g); otherwise; end;\nreturn;\nrun;")
  expect_equal(x$unreachable, 4:8)
  # a DELETE in a THEN branch does not make the IF unreachable
  x <- cfg_of("data c;\nset a;\nif v > 1 then delete;\nw = 1;\nrun;")
  expect_length(x$unreachable, 0)
})

test_that("LEAVE inside a SELECT jumps to the end of the SELECT, not the loop exit", {
  x <- cfg_of("data c; do k = 'a', 'b'; select (k); when ('b') leave; otherwise; end; x = 1; end; run;")
  lp <- x$loops[[1]]
  jumps <- unlist(lapply(x$blocks, function(b) if (b$term$term == "jump") b$term$to))
  expect_false(lp$exit %in% jumps[names(jumps) != lp$head])
  # the block after the SELECT (x = 1) is reached from the LEAVE
  after <- block_assigning(x, "x")
  expect_true(sum(jumps == after) >= 2)
  # a LEAVE in an IF inside a DO group inside the SELECT acts on the SELECT too
  x <- cfg_of("data c; do i = 1 to 3; select; when (i = 2) do; if i > 1 then leave; end; otherwise; end; end; run;")
  lp <- x$loops[[1]]
  expect_false(any(vapply(x$blocks, function(b) identical(b$loop, 1L) && b$term$term == "jump" &&
                                     identical(b$term$to, lp$exit), NA)))
  cfg_check(x)
})

test_that("an iterative DO evaluates the start, TO and BY before it assigns the index", {
  x <- cfg_of("data c; i = 10; do i = 1 to i; x = i; end; run;")
  ins <- x$blocks$B1$instrs
  tg <- vapply(ins, function(i) if (i$target$node == "var") i$target$name else i$target$name, "")
  expect_equal(tg, c("i", "from", "to", "by", "i"))
  expect_equal(ins[[5]]$expr$node, "loop_temp")
  cfg_check(x)
})

facts_of <- function(code, meta = cfg_meta) {
  an <- sas_analyze(sas_parse(code)$steps[[1]], meta)
  f <- dsir_facts(dsir_lower(an), an)
  list(f = f, lv = dsir_level(f, an), cls = classify_step(an))
}

test_that("facts: explicit output, the dominating read, conditional sites", {
  r <- facts_of("data c; set a; p = lag(v); if v > 1 then q = dif(v); run;")
  expect_false(r$f$explicit_output)
  expect_true(r$f$read_dominates)
  expect_equal(r$f$conditional$lag, 2L)
  expect_false(r$f$conditional$read)
  expect_false(r$f$needs_state_machine)
  r <- facts_of("data c; w = 1; set a; output; run;")
  expect_true(r$f$explicit_output)
  expect_false(r$f$read_dominates)
  r <- facts_of("data c; do until (e); set a end = e; end; stop; run;")
  expect_false(r$f$read_dominates)
  r <- facts_of("data c; x = 1; run;")
  expect_true(is.na(r$f$read_dominates))
  # rows that a subsetting IF deletes are not paths that complete the iteration
  r <- facts_of("data c; set a; if v > 1; t + v; run;")
  expect_length(r$f$conditional$sum, 0)
  r <- facts_of("data c; set a; if v > 1 then t + v; run;")
  expect_equal(r$f$conditional$sum, "t")
})

test_that("facts: cross-row live variables follow the PDV reset rules", {
  # a sum variable read by its own sum statement
  r <- facts_of("data c; set a; t + v; run;")
  expect_equal(r$f$cross_row_live, "t")
  # a plain assigned variable is reset at the top of each iteration
  r <- facts_of("data c; set a; if v > 1 then w = v; run;")
  expect_length(r$f$cross_row_live, 0)
  # a retained variable assigned on some rows reaches the implicit output
  r <- facts_of("data c; set a; by g; retain b; if first.g then b = v; run;")
  expect_equal(r$f$cross_row_live, "b")
  expect_true(length(r$f$exposed$b) > 0)
  # assigned on every row: nothing is carried
  r <- facts_of("data c; set a; retain b; b = v * 2; run;")
  expect_length(r$f$cross_row_live, 0)
  # an input variable set by the read before any use
  r <- facts_of("data c; set a; v + 1; run;")
  expect_length(r$f$cross_row_live, 0)
  # an input variable read before the read is carried
  r <- facts_of("data c; w = v; set a; run;")
  expect_equal(r$f$cross_row_live, "v")
  # a retained variable never assigned holds its initial value: no state
  r <- facts_of("data c; set a; retain k 5; w = k; run;")
  expect_length(r$f$cross_row_live, 0)
})

test_that("dsir_level agrees with classify_step on the common shapes", {
  progs <- c(
    "data c; set a; w = v * 2; run;",
    "data c; set a; if v > 1; run;",
    "data c; set a; t + v; run;",
    "data c; set a; by g; if first.g then t = 0; t + v; run;",
    "data c; set a; by g; retain b; if first.g then b = v; run;",
    "data c; set a; p = lag(v); run;",
    "data c; set a; if v > 1 then p = lag(v); run;",
    "data c; set a; by g; if last.g then output; run;",
    "data c; set a; w = v; output; run;",
    "data c; set a; if v > 1 then t + v; run;",
    "data c; do i = 1 to 3; x = i; output; end; run;",
    "data c d; set a; run;",
    "data c; set a; call symputx('m', v); run;",
    "data c; x = 1; run;"
  )
  for (p in progs) {
    r <- facts_of(p)
    expect_equal(r$lv$level, r$cls$level, info = p)
  }
})

test_that("dsir_level is more precise where the data flow shows no carried value", {
  # the sum variable comes from the input data set: the read sets it on every
  # row, nothing is carried, but the generator emits a sum statement only as
  # an accumulator
  r <- facts_of("data c; set a; v + 1; run;")
  expect_length(r$f$cross_row_live, 0)
  expect_equal(r$lv$level, "L3")
  # a retained variable assigned on every row carries nothing
  r <- facts_of("data c; set a; retain b; b = v; run;")
  expect_equal(r$cls$level, "L2")
  expect_equal(r$lv$level, "L1")
})

test_that("dsir_level: RETURN forces L3 (the generators do not emit it)", {
  for (p in c("data c; set a; if v > 1 then return; w = 1; run;",
              "data _null_; x = 1; put 'x=' x; return; put 'never'; run;")) {
    r <- facts_of(p)
    expect_equal(r$lv$level, "L3", info = p)
    expect_true("RETURN statement" %in% r$lv$reasons, info = p)
  }
})

test_that("dsir_level: a carried value may be read after its last assignment", {
  r <- facts_of("data c; set a; by g; retain b; if first.g then b = v; w = v - b; run;")
  expect_equal(r$cls$level, "L2")
  expect_equal(r$lv$level, "L2")
  # read before the assignment: the previous row's value is used
  r <- facts_of("data c; set a; by g; retain b; w = b; if first.g then b = v; run;")
  expect_equal(r$cls$level, "L3")
  expect_equal(r$lv$level, "L3")
})

test_that("dsir_level: a carried assignment inside a DO group with other statements is not the pattern", {
  r <- facts_of("data c; set a; by g; retain b; if first.g then b = .; if v = 1 then do; f = 'Y'; b = v; end; else w = v - b; run;")
  expect_equal(r$cls$level, "L3")
  expect_equal(r$lv$level, "L3")
})

test_that("an undecided dsir_level() (NA) falls back to classify_step(); NA never reaches the code", {
  p <- "data c; set a; by g; retain b; if first.g then do; if v > 1 then b = v; end; run;"
  r <- facts_of(p)
  expect_true(is.na(r$lv$level))
  an <- sas_analyze(sas_parse(p)$steps[[1]], cfg_meta)
  expect_equal(an$level, r$cls$level)
  expect_equal(an$reasons, r$cls$reasons)
  tr <- translate_step(sas_parse(p)$steps[[1]], cfg_meta)
  expect_false(grepl("\\bNA\\b", strsplit(tr$code, "\n")[[1]][2]))
})
