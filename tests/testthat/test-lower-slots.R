slot_meta <- list(
  a = meta_from_df(data.frame(g = c("A", "A", "B"), v = c(1, 2, 3))),
  b = meta_from_df(data.frame(g = c("A", "B"), w = c(10, 20)))
)
slots_of <- function(code, meta = slot_meta) {
  d <- dsir_slots(sas_analyze(sas_parse(code)$steps[[1]], meta))
  expect_s3_class(d, "sas_dsir")
  d$slots
}

test_that("source: data set options, analysed reader info and the block flag", {
  s <- slots_of("data c; set a(keep = g v rename = (v = vv) where = (vv > 1)); run;")$source
  expect_length(s, 1)
  expect_equal(s[[1]]$kind, "set")
  expect_false(s[[1]]$in_block)
  expect_equal(s[[1]]$reader, 1L)
  d1 <- s[[1]]$datasets[[1]]
  expect_equal(d1$name, "a")
  expect_equal(d1$vars, c("g", "v"))
  expect_equal(d1$out_names, c("g", "vv"))
  expect_equal(d1$options$rename, c(v = "vv"))
  expect_equal(d1$options$where$node, "compare")
  # a SET inside a DO block (DOW loop)
  s <- slots_of("data c; do until (e); set b end = e; output; end; stop; run;")$source
  expect_true(s[[1]]$in_block)
  expect_equal(s[[1]]$end, "e")
})

test_that("source: MERGE with IN= keeps the options and END=", {
  s <- slots_of("data c; merge a(in = ina) b(in = inb) end = eof; by g; run;")$source
  expect_length(s, 1)
  expect_equal(s[[1]]$kind, "merge")
  expect_equal(vapply(s[[1]]$datasets, function(d) d$options$in_var, ""), c("ina", "inb"))
  expect_equal(s[[1]]$end, "eof")
  expect_equal(s[[1]]$datasets[[2]]$vars, c("g", "w"))
})

test_that("source: INPUT is a reading statement", {
  s <- slots_of("data c; input x y $; datalines;\n1 a\n;\nrun;", list())$source
  expect_length(s, 1)
  expect_equal(s[[1]]$kind, "input")
  expect_length(s[[1]]$datasets, 0)
})

test_that("where: every WHERE statement in order, the last one is effective", {
  an <- sas_analyze(sas_parse(
    "data c; set a; where v > 0; x = 1; do i = 1 to 2; where v > 1; end; where g = 'A'; run;"
  )$steps[[1]], slot_meta)
  w <- dsir_slots(an)$slots$where
  expect_length(w$stmts, 3)
  expect_equal(vapply(w$stmts, `[[`, NA, "in_block"), c(FALSE, TRUE, FALSE))
  expect_equal(w$effective$lhs$name, "g")
  top <- Filter(function(s) s$stmt == "where", an$stmts)
  expect_identical(w$effective, top[[length(top)]]$cond)
  expect_identical(an$dsir$slots$where$effective, w$effective)
  expect_null(slots_of("data c; set a; run;")$where$effective)
})

test_that("select_rename: statements in order and resolved per output", {
  sr <- slots_of(
    "data c(keep = g vv) d(keep = x); set a; x = 1; if x then do; drop x; end; rename v = vv; keep g v x; run;"
  )$select_rename
  expect_equal(vapply(sr$stmts, `[[`, "", "kind"), c("drop", "rename", "keep"))
  expect_equal(vapply(sr$stmts, `[[`, NA, "in_block"), c(TRUE, FALSE, FALSE))
  expect_equal(sr$stmts[[2]]$items, c(v = "vv"))
  expect_equal(sr$rename, c(v = "vv"))
  expect_equal(vapply(sr$per_output, `[[`, "", "name"), c("c", "d"))
  expect_equal(sr$per_output[[1]]$vars, c("g", "v"))
  expect_equal(sr$per_output[[1]]$out_names, c("g", "vv"))
  expect_length(sr$per_output[[2]]$vars, 0)
})

test_that("attrs: declarative statements keep statement order, also after executable ones and in blocks", {
  at <- slots_of(paste(
    "data c; set a; y = 1; length z $ 4; format v 8.2; if y then do; label v = 'Value'; end;",
    "retain r1 0 r2 'x'; array t{2} t1 t2 (5 6); informat q 8.; attrib k length = $ 3 label = 'K';",
    "do i = 1 to 2; length m 8; end; run;"
  ))$attrs
  expect_equal(vapply(at, `[[`, "", "kind"),
               c("length", "format", "label", "retain", "array", "informat", "attrib", "length"))
  expect_equal(vapply(at, `[[`, NA, "in_block"), c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(at[[3]]$content$labels, c(v = "Value"))
})

test_that("attrs: RETAIN initial values and the analysed ARRAY", {
  at <- slots_of("data c; set a; retain s 0 n (1 2) t 'ab'; array arr{2} a1 a2 (5 6); run;")$attrs
  r <- at[[1]]$content$groups
  expect_equal(lapply(r, `[[`, "init_values"), list(list(0), list(1, 2), list("ab")))
  arr <- at[[2]]$content
  expect_equal(arr$name, "arr")
  expect_equal(unname(as.vector(arr$elems)), c("a1", "a2"))
  expect_equal(arr$type, "num")
  expect_equal(arr$init, list(5, 6))
})

test_that("by: variables and DESCENDING", {
  expect_null(slots_of("data c; set a; run;")$by)
  by <- slots_of("data c; set a; by descending g v; run;")$by
  expect_equal(by$vars, c("g", "v"))
  expect_equal(by$descending, c(TRUE, FALSE))
  expect_false(by$notsorted)
  expect_false(by$groupformat)
})

test_that("outputs: two output data sets with KEEP= and their resolved columns", {
  o <- slots_of("data c(keep = g) d(keep = v x); set a; x = v * 2; run;")$outputs
  expect_equal(vapply(o, `[[`, "", "name"), c("c", "d"))
  expect_equal(o[[1]]$options$keep[[1]]$name, "g")
  expect_equal(o[[1]]$vars, "g")
  expect_equal(o[[2]]$vars, c("v", "x"))
  expect_length(slots_of("data _null_; set a; run;")$outputs, 0)
})

test_that("slot_where_effective: the last WHERE wins", {
  expect_null(slot_where_effective(list()))
  expect_equal(slot_where_effective(list("a", "b", "c")), "c")
})

test_that("a BY statement inside a block fails loud", {
  for (code in c(
    "data c; set a; if v > 1 then do; by g; end; run;",
    "data c; do until (e); set a end = e; by g; end; run;",
    "data c; set a; select (g); when ('A') by g; otherwise; end; run;"
  )) {
    expect_error(sas_analyze(sas_parse(code)$steps[[1]], slot_meta), "a BY statement inside a block",
                 class = "sasonr_unsupported")
  }
})
