# SAS error model of sas_run(on_error = "sas") (spec 01 §4.7, decision D4).
#
# SAS in batch: a step that raises an ERROR is not executed (or stops); SAS
# writes "NOTE: The SAS System stopped processing this step because of
# errors." and, outside PROC SQL, enters syntax-check mode (OBS=0): the steps
# that follow are compiled but not run -- a DATA step creates its output data
# sets with 0 observations, PROC steps produce nothing -- while global
# statements and the macro language are still processed.  An ERROR inside
# PROC SQL sets NOEXEC for the rest of that PROC SQL step only.  Output data
# sets of the failing step: a new one is created with 0 observations and the
# variables compiled so far; an existing one is not replaced.  SYSERR/SYSCC
# follow SAS (SAS gold tests/conformance/datastep/errors).
#
# Only SAS ERRORs (`sasonr_syntax_error`, `sasonr_runtime_error`) are handled
# this way.  `sasonr_unsupported` (fail loud) and R errors always abort.  An
# execution-time ERROR in a DATA step (data sets partly written) still aborts
# with `sasonr_unsupported`: the partial data sets are not reproduced yet.
#
# The variables of the 0-observation data sets come from the parser and the
# analyzer, never from scanning tokens: the step's AST (or, after a syntax
# error, a statement-level recovery parse) is translated with the failing
# statements replaced as SAS compiles them, and the output metadata of that
# translation (names, types, lengths, formats) is used.  SAS gold
# (datastep/errors) fixes what a failing statement contributes:
#   assignment  its target and the variables named before the error (e01, e09, e10)
#   SET / MERGE nothing; SAS ignores the statement (e05, e08, e11)
#   PROC SORT   the OUT= data set, with 0 variables (e06)
# Any other failing statement or PROC fails loud (sasonr_unsupported): what SAS
# leaves in WORK has not been verified.

err_message_lines <- function(e) {
  msg <- conditionMessage(e)
  msg <- sub("^(Syntax error|Runtime error): ", "", msg)
  msg <- sub(" \\(SAS line [0-9]+\\)$", "", msg)
  code <- regmatches(msg, regexpr("\\(ERROR [0-9]+-[0-9]+\\)$", msg))
  if (length(code) == 1L) {
    msg <- trimws(sub("\\(ERROR [0-9]+-[0-9]+\\)$", "", msg))
    return(paste0("ERROR ", gsub("[^0-9-]", "", code), ": ", msg))
  }
  paste0("ERROR: ", msg)
}

# Statement-level recovery parse of the one step in `text` that has a syntax
# error: the statements that parse are kept as AST, a failing statement becomes
# list(stmt = "failed", kind, vars) with the variables the parser had read when
# it failed (SAS skips to the end of the statement and goes on compiling).
# NULL when the step cannot be recovered (DATA/PROC statement itself fails,
# a failing block statement, more than one step in the text).
err_parse_recover <- function(text) {
  toks <- tryCatch(sas_lex(text), error = function(e) NULL)
  if (is.null(toks) || nrow(toks) == 0L) return(NULL)
  toks$type[toks$type == "str_macro"] <- "str"
  p <- new_parser(toks)
  while (!at_eof(p) && tk_type(p) == "semi") advance(p)
  if (at_eof(p) || tk_type(p) != "name") return(NULL)
  if (is_kw(p, "proc")) {
    # the PROC statement alone (its options name the output data sets)
    end <- p$pos
    while (end <= p$n && p$type[end] != "semi") end <- end + 1L
    if (end > p$n) return(NULL)
    sub <- toks[c(p$pos:end, end, end), , drop = FALSE]
    sub$type[nrow(sub) - 1L] <- "name"
    sub$value[nrow(sub) - 1L] <- "run"
    st <- tryCatch(sas_parse(sub, resolved = TRUE), error = function(e) NULL)
    if (is.null(st) || length(st$steps) != 1L || st$steps[[1]]$type != "proc") return(NULL)
    return(st$steps[[1]])
  }
  if (!is_kw(p, "data") || is_op(p, "=", 1L)) return(list(type = "global"))
  line <- tk_line(p)
  advance(p)
  outputs <- tryCatch({
    out <- list()
    while (tk_type(p) != "semi") {
      if (tk_type(p) != "name") abort_syntax("data set name", tk_line(p))
      out <- c(out, list(parse_ds_ref(p, "output")))
    }
    advance(p)
    out
  }, error = function(e) NULL)
  if (is.null(outputs) || !length(outputs)) return(NULL)
  is_null <- vapply(outputs, function(o) o$name == "_null_", logical(1))
  p$arrays <- character()
  stmts <- list()
  repeat {
    if (at_eof(p)) break
    if (is_kw(p, "run") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      if (!at_eof(p)) return(NULL)
      break
    }
    if (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L)) return(NULL)
    start <- p$pos
    p$seen <- character()
    s <- tryCatch(parse_statement(p), sasonr_syntax_error = function(e) e, error = function(e) NULL)
    seen <- p$seen
    p$seen <- NULL
    if (is.null(s)) return(NULL)
    if (inherits(s, "condition")) {
      kind <- if (p$type[start] == "name" && start < p$n && p$type[start + 1L] == "op" && p$value[start + 1L] == "=") "assign" else p$low[start]
      stmts <- c(stmts, list(list(stmt = "failed", kind = kind, vars = unique(seen), line = p$line[start])))
      while (!at_eof(p) && tk_type(p) != "semi") advance(p)
      advance(p)
      next
    }
    if (s$stmt == "end") return(NULL)
    stmts <- c(stmts, list(s))
    if (s$stmt == "datalines") break
  }
  list(type = "data", line = line, null = any(is_null), outputs = if (any(is_null)) list() else outputs, stmts = stmts)
}

# What a failing DATA step statement contributes as SAS compiles it: a list of
# statements, or NULL when that is not verified against SAS (fail loud).
err_failed_stmt <- function(kind, vars, target, line) {
  if (kind %in% c("set", "merge")) return(list())
  if (kind != "assign" || is.null(target)) return(NULL)
  tv <- list(node = "var", name = target, text = target, line = line)
  c(list(list(stmt = "assign", target = tv, value = list(node = "missing", code = ".", line = line), line = line)),
    lapply(setdiff(vars, target), function(v) {
      list(stmt = "subset_if", cond = list(node = "var", name = v, text = v, line = line), line = line)
    }))
}

# Output metadata (eng$meta form) of a DATA step as SAS compiled it, or NULL.
err_data_schema <- function(eng, step) {
  stmts <- list()
  for (s in step$stmts) {
    if (identical(s$stmt, "failed")) {
      r <- err_failed_stmt(s$kind, s$vars, if (s$kind == "assign") s$vars[1] else NULL, s$line)
      if (is.null(r)) return(NULL)
      stmts <- c(stmts, r)
    } else {
      stmts <- c(stmts, list(s))
    }
  }
  step$stmts <- stmts
  for (attempt in seq_len(length(stmts) + 1L)) {
    tr <- tryCatch(translate_step(step, eng$meta, eng$level),
                   sasonr_syntax_error = function(e) e, sasonr_runtime_error = function(e) e, error = function(e) NULL)
    if (is.null(tr)) return(NULL)
    if (!inherits(tr, "condition")) return(tr$meta[vapply(step$outputs, `[[`, "", "name")])
    # an ERROR of the analyzer at one statement: replace it as SAS compiles it
    ln <- tr$sas_line
    k <- which(vapply(step$stmts, function(s) identical(as.integer(s$line), as.integer(ln)), logical(1)))
    if (length(k) != 1L) return(NULL)
    s <- step$stmts[[k]]
    kind <- if (s$stmt == "assign" && identical(s$target$node, "var")) "assign" else s$stmt
    vars <- if (kind == "assign") c(s$target$name, expr_reads(s$value)) else character()
    r <- err_failed_stmt(kind, vars, if (kind == "assign") s$target$name else NULL, s$line)
    if (is.null(r)) return(NULL)
    step$stmts <- append(step$stmts[-k], r, after = k - 1L)
  }
  NULL
}

# Output data sets of the failing step and their metadata (eng$meta form) as
# SAS has compiled them when the step stops: list(kind, outputs), or NULL
# when they cannot be derived from the parsed step.
err_recover_step <- function(eng, text) {
  step <- eng$cur_step %||% err_parse_recover(text)
  if (is.null(step)) return(NULL)
  if (step$type == "proc") {
    outs <- list()
    if (step$name == "sort" && !is.null(step$options$out)) {
      outs[[step$options$out$name]] <- list(vars = character(), type = list(), len = list(), formats = list(), disp = list())
    } else if (step$name != "print") {
      return(NULL)
    }
    return(list(kind = "proc", outputs = outs))
  }
  if (step$type != "data") return(list(kind = "global", outputs = list()))
  if (isTRUE(step$null) || !length(step$outputs)) return(list(kind = "data", outputs = list()))
  meta <- err_data_schema(eng, step)
  if (is.null(meta) || any(vapply(meta, is.null, logical(1)))) return(NULL)
  list(kind = "data", outputs = meta)
}

err_set_macro <- function(eng, syserr = NULL, syscc = NULL) {
  g <- eng$ses$globals
  if (!is.null(syserr)) assign("syserr", as.character(syserr), envir = g)
  if (!is.null(syscc)) {
    old <- suppressWarnings(as.numeric(get0("syscc", envir = g, inherits = FALSE) %||% "0"))
    assign("syscc", as.character(max(old, syscc, na.rm = TRUE)), envir = g)
  }
}

# Handle a SAS ERROR raised while compiling or running the step in `text`.
engine_step_error <- function(eng, e, text) {
  if (identical(eng$phase, "execute_data")) {
    abort_unsupported(paste0("the data sets that an execution-time ERROR leaves in WORK (", conditionMessage(e), ")"),
                      e$sas_line %||% NULL)
  }
  in_sql <- isTRUE(eng$sql_open) || identical(eng$phase, "sql")
  rec <- if (in_sql) NULL else err_recover_step(eng, text)
  if (!in_sql && is.null(rec)) {
    abort_unsupported(paste0("the data sets that this ERROR leaves in WORK could not be derived from the parsed step (",
                             conditionMessage(e), ")"), e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, conditionMessage(e))
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  eng$log <- c(eng$log, err_message_lines(e))
  if (in_sql) {
    if (!isTRUE(eng$sql_noexec)) {
      eng$log <- c(eng$log, "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.")
    }
    eng$sql_noexec <- TRUE
    err_set_macro(eng, syscc = 1012)
    return(invisible())
  }
  eng$log <- c(eng$log, "NOTE: The SAS System stopped processing this step because of errors.")
  if (!isTRUE(eng$syntax_check) && !isTRUE(eng$nosyntaxcheck)) {
    eng$log <- c(eng$log, "NOTE: Due to ERROR(s) above, SAS set option OBS=0, enabling syntax check mode.",
                 "      This prevents execution of subsequent data modification statements.")
    eng$syntax_check <- TRUE
  }
  for (nm in names(rec$outputs)) {
    old <- get0(nm, envir = eng$wenv, inherits = FALSE)
    m <- rec$outputs[[nm]]
    eng$log <- c(eng$log, sprintf(
      "WARNING: The data set WORK.%s may be incomplete.  When this step was stopped there were 0 observations and %d variables.",
      toupper(nm), length(m$vars)))
    if (!is.null(old)) {
      eng$log <- c(eng$log, sprintf("WARNING: Data set WORK.%s was not replaced because this step was stopped.", toupper(nm)))
    } else {
      engine_empty_dataset(eng, nm, m)
    }
  }
  err_set_macro(eng, syserr = 1012, syscc = if (identical(rec$kind, "proc")) 3000 else 1012)
  invisible()
}

# A data set with 0 observations and the variables (types, lengths, formats)
# of the output metadata `m` (eng$meta form).
engine_empty_dataset <- function(eng, nm, m) {
  vars <- m$vars %||% character()
  cols <- lapply(vars, function(v) if (identical(m$type[[v]], "char")) character() else numeric())
  d <- tibble::as_tibble(stats::setNames(cols, vars), .rows = 0L)
  assign(nm, d, envir = eng$wenv)
  eng$meta[[nm]] <- utils::modifyList(meta_from_df(d), m)
  invisible()
}
