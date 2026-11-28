# Generated code as language objects: exact display, runnable display text.

test_that("displayed literals parse back to identical values", {
  vals <- list(0.1 + 0.2, 1 / 3, 1e-300, .Machine$double.xmin / 4, -0, NA_real_, NA_character_, NA, NA_integer_,
               Inf, -Inf, 5L, "a\"b", "tab\there", "caf\xe9", 1e22, -123.456)
  for (v in vals) {
    e <- call("<-", as.name("x"), v)
    back <- eval(parse(text = cg_render(list(e)), keep.source = FALSE)[[1]][[3]])
    expect_identical(back, v, info = cg_render(list(e)))
  }
  # a signed zero survives display and execution
  expect_identical(1 / eval(parse(text = cg_render(list(-0)))[[1]]), -Inf)
})

test_that("a pipeline is displayed with |> and runs as the nested call", {
  p <- cg_pipe(quote(data.frame(x = 1:4)),
               list(quote(dplyr::filter(x > 1)), quote(dplyr::mutate(y = x * 2))), target = ".d")
  txt <- cg_render(list(p), header = "# ---- a pipeline")
  expect_equal(strsplit(txt, "\n")[[1]],
               c("# ---- a pipeline", ".d <- data.frame(x = 1:4) |>", "  dplyr::filter(x > 1) |>", "  dplyr::mutate(y = x * 2)"))
  e1 <- new.env()
  eval(cg_exec(list(p)), e1)
  e2 <- new.env()
  eval(parse(text = txt), e2)
  expect_identical(e1$.d, e2$.d)
  expect_equal(e1$.d$y, c(4, 6, 8))
  expect_identical(cg_exec(list(p))[[1]], parse(text = txt, keep.source = FALSE)[[1]])
})

cgl_progs <- c(
  l1 = "data a; input g $ v; datalines;\nA 1\nA .\nB 3\n;\ndata b; set a; w = v * 2 + 0.1; if v > 1 then c = 'big'; run;",
  l2 = "data a; input g $ v; datalines;\nA 1\nA .\nB 3\n;\ndata b; set a; by g; if first.g then t = 0; t + v; p = lag(v); run;",
  l3 = "data b; do i = 1 to 3; select (i); when (2) leave; otherwise; end; x = i / 3; output; end; run;",
  # the L3 statement forms: arrays, DO WHILE/UNTIL/list, CONTINUE, SELECT,
  # IF/ELSE IF/ELSE, CALL MISSING/SORTN, SUBSTR=, PUT, subsetting IF
  l3_forms = paste(
    "data a; input g $ v s $; datalines;\nA 1 xy\nA . zz\nB 3 qq\n;",
    "data b; set a; by g; array x{2} v w; array t{3} _temporary_ (1 2 3); retain k;",
    "if first.g then k = 0; do i = 1 to 2; x{i} = i * t{i}; end; call missing(w);",
    "substr(s, 1, 1) = 'Q'; select (g); when ('A') m = 1; when ('B', 'C') m = 2; otherwise m = 3; end;",
    "if v > 1 then put 'big ' v= s=; else if v > 0 then w = 1; else do; w = 2; z = 3; end;",
    "j = 0; do while (j < 2); j + 1; end; do until (j > 4); j + 1; if j = 3 then continue; end;",
    "do q = 'a', 'b'; end; call sortn(v, w); if m < 3; run;")
)

test_that("the displayed code of a DATA step parses to the objects that run", {
  for (nm in names(cgl_progs)) {
    tr <- sas_translate(cgl_progs[[nm]])
    for (s in Filter(function(s) !is.null(s$exprs), tr$steps)) {
      expect_identical(as.list(parse(text = s$code, keep.source = FALSE)), s$exprs, info = nm)
      expect_identical(cg_exec(s$exprs), parse(text = s$code, keep.source = FALSE), info = nm)
    }
  }
})

test_that("running the displayed code gives the same WORK as sas_run()", {
  for (nm in names(cgl_progs)) {
    r <- sas_run(cgl_progs[[nm]])
    tr <- sas_translate(cgl_progs[[nm]])
    # the environment engine_eval() gives a step
    env <- new.env(parent = asNamespace("sasonr"))
    env$work <- new.env(parent = emptyenv())
    log_buffer_init(env)
    env$.lst <- character()
    env$.meta <- list()
    for (s in tr$steps) eval(parse(text = s$code, keep.source = FALSE), envir = env)
    for (ds in names(r$work)) {
      got <- dplyr::mutate(env$work[[ds]], dplyr::across(dplyr::where(is.character), sas_rt_rtrim))
      expect_identical(as.data.frame(got), as.data.frame(r$work[[ds]]), info = paste(nm, ds))
    }
  }
})

test_that("displayed code of an L1, an L2 and an L3 step", {
  for (nm in c("l1", "l2", "l3")) {
    tr <- sas_translate(cgl_progs[[nm]])
    expect_snapshot(cat(tr$steps[[length(tr$steps)]]$code, sep = "\n"))
  }
})

test_that("a non-syntactic name keeps its backticks in vectorised code (_N_)", {
  r <- sas_run("data a; do i = 1 to 3; output; end; run; data b; set a; n = _n_; run;")
  expect_equal(r$translation$steps[[2]]$level, "L1")
  expect_equal(r$work$b$n, c(1, 2, 3))
  expect_match(r$translation$steps[[2]]$code, "n = `_n_`", fixed = TRUE)
})

test_that("cg_trim() matches trimws() and stays linear on long blank runs", {
  x <- c("  a b  ", "\tx\n", "", "   ", "a", paste0("'", strrep(" ", 3), "'  "))
  expect_identical(cg_trim(x), trimws(x))
  long <- paste0("x <- \"", strrep(" ", 32767), "\"")
  expect_lt(system.time(r <- cg_trim(long))[["elapsed"]], 1)
  expect_identical(r, long)
})
