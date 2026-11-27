# The layout of the generated code (spec 09 L6a, R/codegen_format.R).

fmt_cases <- function() {
  list(
    quote(x <- f(a, b)),
    quote(z <- sas_put_rows(list(sas_put_line(sas_put_named("r1", .d$r1), sas_put_named("r2", .d$r2))), list(TRUE), n = nrow(.d))),
    quote(.status <- tryCatch({
      a <- 1
      "ok"
    }, sasonr_delete = function(e) "delete", sasonr_stop = function(e) "stop")),
    quote(repeat {
      .n <- .n + 1L
      if (.status == "stop") break
      b <- 2
    }),
    quote(y <- aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa & bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb & ccccccccccccccccccccccccccccccc),
    quote(.reset <- pdv[c("id", "fmt", "w", "d", "x_hex", "x_spec", "x", "t", "tr", "q", "qr", "rh", "rr", "zz")]),
    quote(if (.status == "ok") {
      out <- 1
    } else {
      out <- 2
    }),
    cg_pipe(quote(tibble::tibble(.rows = 1L)), list(quote(dplyr::mutate(r1 = sas_f_abs(3.5))), quote(dplyr::select(r1))), target = "lab"),
    cg_pipe(quote(sas_read_input(sas_infile_open(c("S01 1 10", "S01 2 12", "S02 1 20", "S02 2 18"), c(5L, 6L, 7L, 8L)))),
            list(quote(select(usubjid, visit, aval))), target = "lab")
  )
}

test_that("long calls put one argument per line and short ones stay on one line", {
  expect_identical(cg_format(quote(x <- f(a, b))), "x <- f(a, b)")
  out <- cg_format(fmt_cases()[[2]])
  expect_identical(out, c("z <- sas_put_rows(",
                          "  list(sas_put_line(sas_put_named(\"r1\", .d$r1), sas_put_named(\"r2\", .d$r2))),",
                          "  list(TRUE),", "  n = nrow(.d)", ")"))
  expect_true(all(nchar(unlist(lapply(fmt_cases(), function(e) cg_format(cg_normalize(e))))) <= 80L))
})

test_that("pipelines break after |> only when they do not fit", {
  expect_identical(cg_format(cg_pipe(quote(a), list(quote(f()), quote(g(b))), target = "x")), "x <- a |> f() |> g(b)")
  expect_identical(cg_format(fmt_cases()[[8]]), c("lab <- tibble::tibble(.rows = 1L) |>", "  dplyr::mutate(r1 = sas_f_abs(3.5)) |>",
                                                  "  dplyr::select(r1)"))
  out <- cg_format(fmt_cases()[[9]])
  expect_identical(out[length(out)], "  select(usubjid, visit, aval)")
  expect_match(out[length(out) - 1L], "\\|>$")
})

test_that("an if without braces that is not the last statement of a block gets braces", {
  e <- cg_normalize(fmt_cases()[[4]])
  expect_identical(cg_format(e)[3:5], c("  if (.status == \"stop\") {", "    break", "  }"))
  expect_identical(cg_format(cg_normalize(quote(repeat {
    a <- 1
    if (s) break
  })))[3], "  if (s) break")
})

test_that("the text parses back to the code and is what air writes", {
  for (e in fmt_cases()) {
    e <- cg_normalize(e)
    txt <- cg_format(e)
    if (!inherits(e, "cg_pipe")) expect_identical(rlang::parse_exprs(paste(txt, collapse = "\n"))[[1]], e)
  }
  air <- Sys.which("air")
  skip_if(!nzchar(air), "air is not installed")
  f <- withr::local_tempfile(fileext = ".R")
  for (e in fmt_cases()) {
    txt <- cg_format(cg_normalize(e))
    writeLines(txt, f)
    system2(air, c("format", f))
    expect_identical(readLines(f), txt)
  }
})

test_that("the backend object carries the line width and dispatches the layout (spec 09 L6c)", {
  e <- quote(z <- sas_put_rows(list(sas_put_line(sas_put_named("r1", .d$r1))), list(TRUE), n = nrow(.d)))
  expect_identical(cg_emit(cg_backend(), e), cg_format(e))
  expect_length(cg_emit(cg_backend(width = 200L), e), 1L)
  expect_s3_class(cg_backend(), c("cg_backend_air", "cg_backend"))
})
