# Verification tooling (round 15): regression tests for the findings of the
# independent review of 37e3ba6 (sasonr-conformance
# docs/conformance/review_37e3ba6_tools.md).  The tools are not part of the
# built package (.Rbuildignore): skipped there.

tools_dir <- test_path("..", "..", "tools")
skip_if_not(file.exists(file.path(tools_dir, "conf_impact.R")), "tools/ not available (built package)")
te <- new.env()
sys.source(file.path(tools_dir, "conf_archive.R"), envir = te)
sys.source(file.path(tools_dir, "conf_impact.R"), envir = te)
# the md5 cache of the case index in a temporary directory (not the user's)
md5_cache <- file.path(tempfile("conf_tools_"), "md5_index.rds")
dir.create(dirname(md5_cache))
te$imp_md5_cache_file <- function() md5_cache

test_that("finding 4: a gate under a non-symbol call head counts as caught", {
  run <- quote(sas_run("data a; run;"))
  gate <- quote(feature_check("function", "LOG"))
  caught <- function(...) te$imp_gate_caught(list(run, ..., gate), c(0L, seq_len(length(list(...)) + 1L) - 1L))
  # no handler between sas_run and the gate: not caught (the verdict is derived)
  expect_false(caught(quote(step(x))))
  # a plain tryCatch with an error handler: caught
  expect_true(caught(quote(tryCatch(f(), error = function(e) NULL))))
  # a tryCatch without an error handler does not catch the gate
  expect_false(caught(quote(tryCatch(f(), warning = function(w) NULL))))
  # namespace-qualified heads, do.call and anonymous functions: caught (rerun in strict mode)
  expect_true(caught(quote(base::tryCatch(f(), error = function(e) NULL))))
  expect_true(caught(quote(rlang::try_fetch(f(), error = function(e) NULL))))
  expect_true(caught(quote(do.call(tryCatch, list(quote(f()))))))
  # an R6 method or a namespace-qualified ordinary function is not a catcher
  expect_false(caught(quote(self$run(x)), quote(sasonr::step(x))))
  expect_true(caught(quote((function(x) x)(1))))
  # outside sas_run: caught (nothing to derive)
  expect_true(te$imp_gate_caught(list(gate), 0L))
})

test_that("finding 5: matrix blocks with effects on later blocks are not derived", {
  pure <- c("%put @@BEGIN A_1 data;", "data work.A_1;", "  r = abs(-3.5);", "  file 'results.txt' mod lrecl=1000000;",
            "  put '@@N A_1 hex=' r hex16.;", "run;", "%put @@END A_1;")
  expect_true(te$imp_block_pure(pure))
  expect_false(te$imp_block_pure(append(pure, "%let x = 1;", 1)))
  expect_false(te$imp_block_pure(append(pure, "  call symputx('x', r);", 3)))
  expect_false(te$imp_block_pure(append(pure, "options missing='*';", 1)))
  expect_false(te$imp_block_pure(c(pure, "proc format; value f 1 = 'a'; run;")))
  expect_false(te$imp_block_pure(append(pure, "  set work.other;", 2)))
  expect_false(te$imp_block_pure(c("proc print data=a; run;")))
})

test_that("finding 8: arc_write removes the older results of both modes of the root", {
  pkg <- withr::local_tempdir()
  df <- data.frame(case = "a/1", status = "pass", failed_layers = "", sas_version = "TS1M7")
  te$arc_write("k1", "own", list(strict = df, nonstrict = df), NULL, "x: 1", list(strict = "s", nonstrict = "n"), pkg)
  d <- file.path(pkg, ".claude", "results", "k1")
  expect_true(file.exists(file.path(d, "own-strict-false-TS1M7.csv")))
  te$arc_write("k1", "own", list(strict = df), NULL, "x: 2", list(strict = "s"), pkg)
  expect_false(file.exists(file.path(d, "own-strict-false-TS1M7.csv")))
  expect_false(file.exists(file.path(d, "own-strict-false.summary.txt")))
  expect_true(file.exists(file.path(d, "own-TS1M7.csv")))
  # info.txt: one block per root
  te$arc_write("k1", "internal", list(strict = df), NULL, "y: 1", list(strict = "s"), pkg)
  inf <- readLines(file.path(d, "info.txt"))
  expect_identical(inf, c("[own]", "x: 2", "[internal]", "y: 1"))
})

test_that("finding 1: the code hash follows the content of the code", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  dir.create(file.path(pkg, "inst", "registry"), recursive = TRUE)
  writeLines("f <- function() 1", file.path(pkg, "R", "a.R"))
  writeLines("name,status", file.path(pkg, "inst", "registry", "functions.csv"))
  writeLines("Package: x", file.path(pkg, "DESCRIPTION"))
  h1 <- te$arc_code_hash(pkg)
  writeLines("f <- function() 2", file.path(pkg, "R", "a.R"))
  h2 <- te$arc_code_hash(pkg)
  writeLines("name,status,x", file.path(pkg, "inst", "registry", "functions.csv"))
  h3 <- te$arc_code_hash(pkg)
  expect_false(identical(h1, h2))
  expect_false(identical(h2, h3))
  # the SASHELP copy is part of the fingerprint, not of the code hash
  dir.create(file.path(pkg, "inst", "sashelp"))
  writeLines("x", file.path(pkg, "inst", "sashelp", "class.csv"))
  expect_identical(te$arc_code_hash(pkg), h3)
})

test_that("finding 6: case hashes use the file content, symbolic links and dot files", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  case <- file.path(root, "area", "c1")
  dir.create(file.path(case, "expected"), recursive = TRUE)
  writeLines("data a; run;", file.path(case, "program.sas"))
  writeLines("case: area/c1", file.path(case, "meta.yaml"))
  writeLines("abc", file.path(case, "expected", "put.txt"))
  hash <- function() {
    rm(list = ls(te$imp_index_cache), envir = te$imp_index_cache)
    te$imp_unit_hash(normalizePath(case), root)
  }
  h1 <- hash()
  # same size and modification time, other content
  f <- file.path(case, "expected", "put.txt")
  mt <- file.mtime(f)
  writeLines("abd", f)
  Sys.setFileTime(f, mt)
  h2 <- hash()
  expect_false(identical(h1, h2))
  # a dot file and a file behind a symbolic link count
  writeLines("1", file.path(case, ".hidden"))
  h3 <- hash()
  expect_false(identical(h2, h3))
  other <- withr::local_tempdir()
  writeLines("v1", file.path(other, "d.csv"))
  file.symlink(other, file.path(case, "data"))
  h4 <- hash()
  writeLines("v2", file.path(other, "d.csv"))
  h5 <- hash()
  expect_false(identical(h3, h4))
  expect_false(identical(h4, h5))
})

test_that("finding 6: the environment fingerprint covers BLAS, threads, locale, time zone and the books root", {
  fp <- te$arc_fingerprint(test_path("..", ".."))
  expect_true(all(c("R", "packages", "sashelp", "books_root", "blas", "threads", "locale", "tz") %in% names(fp)))
  fp2 <- withr::with_envvar(c(OPENBLAS_NUM_THREADS = "7", SASONR_BOOKS_ROOT = tempdir()),
                            te$arc_fingerprint(test_path("..", "..")))
  expect_false(identical(fp$threads, fp2$threads))
  expect_false(identical(fp$books_root, fp2$books_root))
})

test_that("finding 7: the compile cache is keyed by NAMESPACE and the names of the namespace", {
  ns <- new.env()
  assign("f", function() 1, envir = ns)
  d <- withr::local_tempdir()
  nsf <- file.path(d, "NAMESPACE")
  writeLines("export(f)", nsf)
  c1 <- te$imp_compile_cache_file(ns, d, nsf)
  writeLines(c("export(f)", "importFrom(rlang, abort)"), nsf)
  c2 <- te$imp_compile_cache_file(ns, d, nsf)
  assign("paste", function(...) "masked", envir = ns)
  c3 <- te$imp_compile_cache_file(ns, d, nsf)
  expect_false(identical(c1, c2))
  expect_false(identical(c2, c3))
})
