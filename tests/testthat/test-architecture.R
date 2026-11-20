# Dependency rules of the package source (R/ parsed, not run).

pkg_r_dir <- function() {
  d <- testthat::test_path("..", "..", "R")
  if (!dir.exists(d)) testthat::skip("package sources (R/) not available")
  d
}

# The log stores (R/log.R): only R/log.R, R/log_catalog.R, R/log_render.R ...
# may assign them.
log_store_fields <- c("log", ".log", "events", "log_programs", "program", "src_line", "step_no", "step_count", "approx")
log_files_re <- "^log(_[a-z]+)?\\.R$"

# The fields an assignment writes: for `a$b[[1]]$c <- v` the names b and c.
assigned_fields <- function(lhs) {
  out <- character()
  while (is.call(lhs)) {
    f <- lhs[[1]]
    if (identical(f, as.name("$")) || identical(f, as.name("@"))) {
      out <- c(out, as.character(lhs[[3]]))
    } else if (identical(f, as.name("[[")) && length(lhs) >= 3 && is.character(lhs[[3]])) {
      out <- c(out, lhs[[3]])
    }
    lhs <- lhs[[2]]
  }
  out
}

# The function a call calls ("assign" for assign() and base::assign()).
call_fn <- function(x) {
  f <- x[[1]]
  if (is.name(f)) return(as.character(f))
  if (is.call(f) && length(f) == 3 && (identical(f[[1]], as.name("::")) || identical(f[[1]], as.name(":::")))) {
    return(as.character(f[[3]]))
  }
  ""
}

# The literal names an assigning call writes: assign("x", ...) / assign(x = "x"),
# rlang::env_poke(env, "x", ...), rlang::env_bind(env, x = ...), list2env(list(x = ...), ...).
called_names <- function(x, fn) {
  a <- as.list(x)[-1]
  nm <- names(a) %||% rep("", length(a))
  lit <- function(v) if (is.character(v)) v else character()
  switch(fn,
    assign = , delayedAssign = lit(if ("x" %in% nm) a[[which(nm == "x")[1]]] else a[[which(!nzchar(nm))[1]]]),
    env_poke = lit(if ("nm" %in% nm) a[[which(nm == "nm")[1]]] else if (sum(!nzchar(nm)) >= 2) a[[which(!nzchar(nm))[2]]]),
    env_bind = , env_bind_lazy = setdiff(nm, c("", ".env")),
    list2env = {
      l <- a[[which(!nzchar(nm))[1]]]
      if (is.call(l) && call_fn(l) == "list") setdiff(names(as.list(l)[-1]) %||% character(), "") else character()
    },
    character()
  )
}

# Every write in `expr` to one of `fields`, as deparsed text.
log_store_writes <- function(expr, fields) {
  hits <- character()
  walk <- function(x) {
    if (!is.call(x)) return()
    fn <- call_fn(x)
    if (fn %in% c("<-", "=", "<<-") && length(x) == 3) {
      lhs <- x[[2]]
      bare <- is.name(lhs) && (fn == "<<-" || as.character(lhs) == ".log") && as.character(lhs) %in% fields
      if (bare || any(assigned_fields(lhs) %in% fields)) hits <<- c(hits, paste(deparse(lhs), collapse = ""))
    }
    if (fn %in% c("assign", "delayedAssign", "env_poke", "env_bind", "env_bind_lazy", "list2env")) {
      w <- intersect(called_names(x, fn), fields)
      if (length(w)) hits <<- c(hits, paste0(fn, "(\"", w, "\")"))
    }
    for (a in as.list(x)[-1]) if (!missing(a)) walk(a)
  }
  for (e in as.list(expr)) walk(e)
  hits
}

# Accepted gaps until spec 06 M4 (the message buffers and callbacks still
# carry text into the log outside R/log*.R): rt$notes, rt$proc_notes,
# ct$notes, rt$reads, rt$flow_note, and the rt$on_note / on_warn / on_error
# and ses$on_put callbacks called from other files.  Generated step code
# appends PUT lines and the PDV dump to the step buffer `.log` inside code
# templates (codegen_tidy.R). Since the code generator builds language
# objects, the PDV dump template is visible to the check; it stays an accepted
# gap (listed below) until generated code writes the log through R/log*.R.
test_that("only R/log*.R writes the log stores", {
  files <- list.files(pkg_r_dir(), pattern = "[.][Rr]$", full.names = TRUE)
  expect_gt(length(files), 0)
  bad <- character()
  for (f in files[!grepl(log_files_re, basename(files))]) {
    w <- log_store_writes(parse(f, keep.source = FALSE), log_store_fields)
    if (length(w)) bad <- c(bad, paste0(basename(f), ": ", w))
  }
  accepted <- "codegen_tidy.R: .log"
  expect_identical(setdiff(bad, accepted), character())
})

test_that("the log store check finds writes", {
  ex <- parse(text = c("eng$log <- c(eng$log, 'x')", "env$.log <- character()", "s$events[[1]] <- 1",
                       "assign('.log', 1, envir = e)", "base::assign(x = 'events', 1, envir = e)",
                       ".log <<- 1", "log <<- 2", "rlang::env_poke(e, 'program', 1)", "rlang::env_bind(e, step_no = 1)",
                       "list2env(list(src_line = 1), e)", "rt$approx <- 'f'",
                       "x <- eng$log", "f(eng$log)", "log <- 1", "list(log = 1)"))
  expect_length(log_store_writes(ex, log_store_fields), 11)
  expect_true(grepl(log_files_re, "log_render.R"))
  expect_false(grepl(log_files_re, "logistic.R"))
})

test_that("one function builds the columns of the TABULATE output data set", {
  files <- list.files(pkg_r_dir(), pattern = "[.][Rr]$", full.names = TRUE)
  env <- asNamespace("sasonr")
  hits <- character()
  for (f in files) {
    exprs <- parse(f, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function")) &&
          any(grepl("Table for Observation", deparse(e[[3]]), fixed = TRUE))) {
        hits <- c(hits, as.character(e[[2]]))
      }
    }
  }
  expect_identical(hits, "tab_output_data")
  expect_true(is.function(get("tab_output_data", envir = env)))
})
