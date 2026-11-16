# Error model of sas_run(on_error = "sas").
#
# The batch-mode model: a step that raises an ERROR is not executed (or
# stops); the log gets "NOTE: sasonr stopped processing this step
# because of errors." and, outside PROC SQL, syntax-check mode (OBS=0)
# starts: the steps that follow are compiled but not run -- a DATA step
# creates its output data sets with 0 observations, PROC steps produce
# nothing -- while global statements and the macro language are still
# processed.  An ERROR inside PROC SQL sets NOEXEC for the rest of that PROC
# SQL step only.  Output data sets of the failing step: a new one is created
# with 0 observations and the variables compiled so far; an existing one is
# not replaced.  SYSERR and SYSCC are set.
#
# Only ERRORs of the program (`sasonr_syntax_error`, `sasonr_runtime_error`) are handled
# this way.  `sasonr_unsupported` (fail loud) and R errors always abort.  An
# execution-time ERROR in a DATA step (data sets partly written) still aborts
# with `sasonr_unsupported`: the partial data sets are not supported yet.
#
# The variables of the 0-observation data sets come from the parser and the
# analyzer, never from scanning tokens: the step's AST (or, after a syntax
# error, a statement-level recovery parse) is translated with the failing
# statements replaced by what they contribute at compile time, and the output
# metadata of that translation (names, types, lengths, formats) is used.  What
# a failing statement contributes:
#   assignment  its target and the variables named before the error; when the
#               value is complete and the statement goes on (`x = 1 y = 2;`, a
#               missing semicolon or operator), also the variables of the rest
#               of the statement for the rests err_assign_rest() supports
#   SET / MERGE nothing; the statement is ignored
#   PROC SORT   the OUT= data set, with 0 variables
# Any other failing statement or PROC fails loud (sasonr_unsupported): what
# WORK holds then is not supported.

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
      if (kind == "assign") {
        rest <- err_assign_rest(p, start)
        if (is.null(rest)) return(NULL)
        seen <- c(seen, rest)
      }
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

# Words that begin DATA step statements (and the step boundaries).
err_stmt_words <- c("abort", "array", "attrib", "by", "call", "cards", "cards4", "continue", "data", "datalines",
                    "datalines4", "delete", "dm", "do", "drop", "else", "end", "endsas", "error", "file", "filename",
                    "footnote", "format", "goto", "if", "infile", "informat", "input", "keep", "label", "leave", "length",
                    "libname", "link", "list", "lines", "lines4", "lostcard", "merge", "missing", "modify", "options",
                    "otherwise", "output", "proc", "put", "putlog", "remove", "rename", "replace", "retain", "return",
                    "run", "select", "set", "stop", "then", "title", "update", "when", "where", "window")

# The variables that the rest of a failing assignment adds, when the value is
# complete and the statement goes on without a semicolon (`x = 1 y = z + 2;`),
# in order and numeric when new.  The supported rests:
#   one or more assignments `name = value` (value: names, numbers, + - * / and
#   parentheses), optionally followed by `IF name` or `THEN name`: every name
#   but IF / THEN
#   `name`, `number name`: the name
#   `name name` and a rest that starts with IF: nothing
# Any other rest, and a rest naming an array or a statement keyword, is not
# supported: NULL (fail loud).  character() when the value is not complete.
err_assign_rest <- function(p, start) {
  pos0 <- p$pos
  on.exit(p$pos <- pos0)
  p$pos <- start
  complete <- tryCatch({
    advance(p)
    if (!is_op(p, "=")) stop("not an assignment")
    advance(p)
    parse_expr(p)
    TRUE
  }, error = function(e) FALSE)
  if (!complete || at_eof(p) || tk_type(p) == "semi") return(character())
  k <- p$pos
  end <- k
  while (end <= p$n && p$type[end] != "semi") end <- end + 1L
  if (end > p$n) return(NULL)
  ty <- p$type[k:(end - 1L)]
  low <- p$low[k:(end - 1L)]
  val <- p$value[k:(end - 1L)]
  m <- length(ty)
  if (ty[1] == "name" && low[1] == "if") return(character())
  bad_name <- function(i) low[i] %in% c(err_stmt_words, names(mnemonic_cmp), "and", "or", "not", "in", "min", "max") ||
    low[i] %in% p$arrays
  if (m == 1L && ty[1] == "name") return(if (bad_name(1)) NULL else low[1])
  if (m == 2L && identical(ty, c("num", "name"))) return(if (bad_name(2)) NULL else low[2])
  if (m == 2L && identical(ty, c("name", "name"))) return(if (bad_name(1) || bad_name(2)) NULL else character())
  # assignments, optionally followed by IF name / THEN name
  tail <- character()
  if (m >= 2L && ty[m - 1L] == "name" && low[m - 1L] %in% c("if", "then") && ty[m] == "name") {
    if (bad_name(m)) return(NULL)
    tail <- low[m]
    m <- m - 2L
  }
  out <- character()
  i <- 1L
  while (i <= m) {
    if (i + 1L > m || ty[i] != "name" || bad_name(i) || !(ty[i + 1L] == "op" && val[i + 1L] == "=")) return(NULL)
    out <- c(out, low[i])
    j <- i + 2L
    depth <- 0L
    first <- j
    while (j <= m) {
      if (depth == 0L && ty[j] == "name" && j + 1L <= m && ty[j + 1L] == "op" && val[j + 1L] == "=") break
      if (ty[j] == "lparen") depth <- depth + 1L
      if (ty[j] == "rparen") depth <- depth - 1L
      ok <- ty[j] %in% c("name", "num", "lparen", "rparen") || (ty[j] == "op" && val[j] %in% c("+", "-", "*", "/"))
      if (!ok || depth < 0L) return(NULL)
      if (ty[j] == "name") {
        if (bad_name(j) || (j + 1L <= m && ty[j + 1L] == "lparen")) return(NULL)
        out <- c(out, low[j])
      }
      j <- j + 1L
    }
    if (j == first || depth != 0L) return(NULL)
    i <- j
  }
  unique(c(out, tail))
}

# What a failing DATA step statement contributes at compile time: a list of
# statements, or NULL when that is not supported (fail loud).
err_failed_stmt <- function(kind, vars, target, line) {
  if (kind %in% c("set", "merge")) return(list())
  if (kind != "assign" || is.null(target)) return(NULL)
  tv <- list(node = "var", name = target, text = target, line = line)
  c(list(list(stmt = "assign", target = tv, value = list(node = "missing", code = ".", line = line), line = line)),
    lapply(setdiff(vars, target), function(v) {
      list(stmt = "subset_if", cond = list(node = "var", name = v, text = v, line = line), line = line, ref_only = TRUE)
    }))
}

# Output metadata (eng$session$meta form) of a DATA step as compiled, or NULL.
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
    tr <- tryCatch(translate_step(step, eng$session$meta, eng$level),
                   sasonr_syntax_error = function(e) e, sasonr_runtime_error = function(e) e, error = function(e) NULL)
    if (is.null(tr)) return(NULL)
    if (!inherits(tr, "condition")) return(tr$meta[vapply(step$outputs, `[[`, "", "name")])
    # an ERROR of the analyzer at one statement: replace it by what it contributes at compile time
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

# Output data sets of the failing step and their metadata (eng$session$meta form) as
# compiled when the step stops: list(kind, outputs, syntax_check), or NULL
# when they cannot be derived from the parsed step.  `syntax_check`: whether
# the stopped step starts syntax-check mode (OBS=0): a DATA step with an output
# data set and PROC SORT (with or without OUT=) do; DATA _NULL_ and PROC
# PRINT, MEANS, SUMMARY and FREQ do not.  Another PROC that stops with an ERROR
# is not supported: NULL.
err_recover_step <- function(eng, text) {
  step <- eng$cur_step %||% err_parse_recover(text)
  if (is.null(step)) return(NULL)
  if (step$type == "proc") {
    outs <- list()
    if (step$name == "sort") {
      if (!is.null(step$options$out)) {
        outs[[step$options$out$name]] <- list(vars = character(), type = list(), len = list(), formats = list(), disp = list())
      }
      return(list(kind = "proc", outputs = outs, syntax_check = TRUE, step = step))
    }
    if (step$name %in% c("means", "summary", "freq")) {
      # the output data sets (OUTPUT OUT=, TABLES / OUT=) a stopped PROC MEANS,
      # SUMMARY or FREQ leaves in WORK are not implemented
      if (grepl("\\bout(put)?\\b", text, ignore.case = TRUE, perl = TRUE)) return(NULL)
      # only an ERROR found while the statements are read (an invalid statement
      # or option) is known not to start syntax-check mode; others are not
      # implemented
      if (!is.null(eng$cur_step)) return(NULL)
    } else if (step$name != "print") {
      return(NULL)
    }
    return(list(kind = "proc", outputs = outs, syntax_check = FALSE, step = step))
  }
  # an ERROR in a global statement is not a step ERROR (engine_global_error())
  if (step$type != "data") return(NULL)
  # what a stopped `data a _null_;` leaves in WORK is not implemented
  if (isTRUE(step$null_mixed)) return(NULL)
  if (isTRUE(step$null) || !length(step$outputs)) {
    return(list(kind = "data", outputs = list(), syntax_check = !isTRUE(step$null)))
  }
  meta <- err_data_schema(eng, step)
  if (is.null(meta) || any(vapply(meta, is.null, logical(1)))) return(NULL)
  list(kind = "data", outputs = meta, syntax_check = TRUE)
}

err_set_macro <- function(eng, syserr = NULL, syscc = NULL) {
  g <- eng$session$macro$globals
  if (!is.null(syserr)) assign("syserr", as.character(syserr), envir = g)
  if (!is.null(syscc)) {
    old <- suppressWarnings(as.numeric(get0("syscc", envir = g, inherits = FALSE) %||% "0"))
    assign("syscc", as.character(max(old, syscc, na.rm = TRUE)), envir = g)
  }
}

# An ERROR in a global statement (`options ls=abc;` in open code, also when a
# macro generates the statement): the ERROR is written, SYSCC is set to 1012
# and SYSERR left alone, no "stopped processing this step" NOTE is written,
# and syntax-check mode starts at the next step, which writes "Due to ERROR(s)
# above, sasonr set option OBS=0 ..." (a DATA step; engine_exec_steps()).  Other
# errors, an error inside a step or PROC SQL, in syntax-check mode or under
# NOSYNTAXCHECK are not supported: fail loud.
engine_global_error <- function(eng, e) {
  if (!identical(e$what, "linesize") || !isTRUE(e$open_code) || isTRUE(eng$sql_open) ||
      isTRUE(eng$syntax_check) || isTRUE(eng$sc_pending) || isTRUE(eng$session$options$nosyntaxcheck)) {
    abort_unsupported(paste0("the error handling of this ERROR in a global statement (", err_message_lines(e), ")"),
                      e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, conditionMessage(e))
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  err_emit_error(eng, e)
  err_set_macro(eng, syscc = 1012)
  eng$sc_pending <- TRUE
  invisible()
}

# Handle an ERROR of the program raised while compiling or running the step in `text`.
engine_step_error <- function(eng, e, text) {
  if (inherits(e, "sasonr_global_error")) return(engine_global_error(eng, e))
  if (identical(eng$phase, "execute_data")) {
    abort_unsupported(paste0("the data sets that an execution-time ERROR leaves in WORK (", conditionMessage(e), ")"),
                      e$sas_line %||% NULL)
  }
  in_sql <- isTRUE(eng$sql_open) || identical(eng$phase, "sql")
  rec <- if (in_sql) NULL else err_recover_step(eng, text)
  if (!in_sql && is.null(rec)) {
    abort_unsupported(paste0("the data sets that this ERROR leaves in WORK, or whether it starts syntax-check mode, ",
                             "could not be derived from the parsed step (", conditionMessage(e), ")"), e$sas_line %||% NULL)
  }
  # a PROC step whose text does not parse: its lines and ERRORs belong to that
  # step (the blank line after a numbered ERROR depends on the procedure)
  if (identical(rec$kind, "proc") && is.null(eng$cur_step)) {
    log_step_start(eng, log_step_label(rec$step), rec$step$line, rec$step$line)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, conditionMessage(e))
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  err_emit_error(eng, e)
  if (in_sql) {
    if (!isTRUE(eng$sql_noexec)) {
      log_emit(eng, id = "sql.noexec")
    }
    eng$sql_noexec <- TRUE
    # how the step ends: an ERROR found while a statement is read leaves
    # syntax-check mode off; one found while a statement runs starts it at
    # QUIT; for other ERRORs this is not implemented
    kind <- if (inherits(e, "sasonr_syntax_error")) "syntax" else if (inherits(e, "sasonr_runtime_error")) "run" else "other"
    eng$sql_error_kind <- unique(c(eng$sql_error_kind, kind))
    err_set_macro(eng, syscc = 1012)
    return(invisible())
  }
  # the rest of the step's lines, then what the stopped step writes
  log_source_read(eng, eng$chunk_end)
  log_boundary(eng)
  log_emit(eng, id = "step.stopped")
  # one NOTE per step, also when the step has several ERRORs
  if (isTRUE(rec$syntax_check) && !isTRUE(eng$syntax_check) && !isTRUE(eng$session$options$nosyntaxcheck)) {
    log_emit(eng, id = "step.syntax_check")
    eng$syntax_check <- TRUE
  }
  kept <- FALSE
  for (nm in names(rec$outputs)) {
    old <- get0(nm, envir = eng$session$work, inherits = FALSE)
    m <- rec$outputs[[nm]]
        log_emit(eng, id = "ds.incomplete", name = toupper(nm), nobs = 0L, nvar = length(m$vars))
    if (!is.null(old)) {
      kept <- TRUE
      log_emit(eng, id = "ds.not_replaced_stopped", name = toupper(nm))
    } else {
      engine_empty_dataset(eng, nm, m)
    }
  }
  # _LAST_: the new output of the stopped DATA step; after a data set that was
  # kept, the output of a stopped PROC or a stopped in-place PROC SORT: not
  # supported
  if (length(rec$outputs) || (identical(rec$kind, "proc") && identical(rec$step$name, "sort"))) {
    if (identical(rec$kind, "data") && !kept) {
      session_set_last(eng$session, names(rec$outputs)[length(rec$outputs)])
    } else {
      mac_auto_unknown(eng$session$macro, c("syslast", "sysdsn"), paste0("a step that stopped with an ERROR (SAS line ",
                                                                         e$sas_line %||% "?", ")"))
    }
  }
  if (identical(rec$kind, "proc") && rec$step$name %in% c("means", "summary", "freq")) {
    # the SYSERR and SYSCC values after a stopped PROC MEANS, SUMMARY or FREQ
    # are not implemented: a reference fails loud
    mac_auto_unknown(eng$session$macro, c("syserr", "syscc"),
                     paste0("a PROC ", toupper(rec$step$name), " step that stopped with an ERROR (SAS line ", e$sas_line %||% "?", ")"))
  } else {
    err_set_macro(eng, syserr = 1012, syscc = if (identical(rec$kind, "proc")) 3000 else 1012)
  }
  invisible()
}

# A data set with 0 observations and the variables (types, lengths, formats)
# of the output metadata `m` (eng$session$meta form).
engine_empty_dataset <- function(eng, nm, m) {
  vars <- m$vars %||% character()
  cols <- lapply(vars, function(v) if (identical(m$type[[v]], "char")) character() else numeric())
  d <- tibble::as_tibble(stats::setNames(cols, vars), .rows = 0L)
  assign(nm, d, envir = eng$session$work)
  eng$session$meta[[nm]] <- utils::modifyList(meta_from_df(d), m)
  invisible()
}

# The ERROR message of condition `e`: a compile-time ERROR follows the program
# line it was found in (the source echo, log_items()), with its column and
# token when the condition knows them (`sas_col`, `sas_token`: the underscore
# marker).
err_emit_error <- function(eng, e) {
  line <- e$sas_line
  exact <- identical(eng$phase, "compile") && length(line) == 1L && !is.na(line)
  at <- if (exact && length(e$sas_col) == 1L && !is.na(e$sas_col)) c(line, e$sas_col)
  text <- err_message_lines(e)
  # the source echo around a numbered ERROR on a macro-affected line (SAS
  # shows the generated line; the column of the marker is not known) and
  # around an ERROR on a line that NOSOURCE hides is not implemented: the echo
  # of the submission is left out
  if (log_error_numbered(text) && length(line) == 1L && log_macro_line(line, eng$session$rt)) {
    log_echo_omit(eng, "a numbered ERROR on a line with macro references")
  }
  if (!is.null(at) && !log_source_on_all(eng$program$source, line)) {
    log_echo_omit(eng, "an ERROR on a line that OPTIONS NOSOURCE hides")
  }
  log_emit(eng, "error", text = text, at = at, token = e$sas_token,
           src = if (exact) line, phase = if (exact) "compile")
}
