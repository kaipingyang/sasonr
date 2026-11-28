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
    st <- tryCatch(sas_parse(sub, resolved = TRUE), error = function(e) e)
    # an option word that is ignored after its ERROR (22-322 + 202-322): the
    # statement without it
    if (inherits(st, "sasonr_syntax_error") && any(grepl("202-322", st$sas_also %||% ""))) {
      drop <- which(sub$line == st$sas_line & sub$col == st$sas_col)
      st <- if (length(drop) == 1L) tryCatch(sas_parse(sub[-drop, , drop = FALSE], resolved = TRUE), error = function(e) NULL)
    }
    if (inherits(st, "condition")) st <- NULL
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
      out <- c(out, list(parse_ds_ref(p, "data_output")))
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
      if (grepl("(ERROR 180-322)", conditionMessage(s), fixed = TRUE)) {
        # an invalid statement: the names it holds (checked against the compiled variables)
        end <- start
        while (end <= p$n && p$type[end] != "semi") end <- end + 1L
        k <- seq.int(start + 1L, length.out = max(0L, end - start - 1L))
        stmts <- c(stmts, list(list(stmt = "failed", kind = "invalid", vars = unique(p$low[k][p$type[k] == "name"]),
                                    line = p$line[start])))
        p$pos <- end
        advance(p)
        next
      }
      if (kind == "if") {
        # IF-THEN whose ELSE branch fails: the IF-THEN, then the failing branch
        r <- err_recover_if(p, start)
        if (is.null(r)) return(NULL)
        stmts <- c(stmts, r)
        next
      }
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

# An IF-THEN/ELSE statement at token `start` whose ELSE branch is an
# assignment that fails: list(the IF-THEN without ELSE, the failed
# assignment), with the parser after the failing statement; NULL when the
# failure is elsewhere (fail loud).
err_recover_if <- function(p, start) {
  p$pos <- start
  line <- p$line[start]
  head <- tryCatch({
    advance(p)
    cond <- parse_expr(p)
    expect_kw(p, "then")
    then_s <- parse_branch(p)
    list(stmt = "if", cond = cond, then = then_s, "else" = NULL, line = line)
  }, error = function(e) NULL)
  if (is.null(head) || !is_kw(p, "else") || is_op(p, "=", 1L)) return(NULL)
  advance(p)
  start2 <- p$pos
  if (!(p$type[start2] == "name" && start2 < p$n && p$type[start2 + 1L] == "op" && p$value[start2 + 1L] == "=")) return(NULL)
  p$seen <- character()
  s <- tryCatch(parse_statement(p), sasonr_syntax_error = function(e) e, error = function(e) NULL)
  seen <- p$seen
  if (!inherits(s, "sasonr_syntax_error")) return(NULL)
  rest <- err_assign_rest(p, start2)
  if (is.null(rest)) return(NULL)
  while (!at_eof(p) && tk_type(p) != "semi") advance(p)
  advance(p)
  list(head, list(stmt = "failed", kind = "assign", vars = unique(c(seen, rest)), line = p$line[start2]))
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
    if (!is_op(p, "=")) rlang::abort("not an assignment")
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
  # (`x = 1 run;`: RUN becomes a variable [measured])
  if (m == 1L && ty[1] == "name") return(if (bad_name(1) && low[1] != "run") NULL else low[1])
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
  if (kind %in% c("set", "merge", "invalid")) return(list())
  if (kind != "assign" || is.null(target)) return(NULL)
  tv <- list(node = "var", name = target, text = target, line = line)
  c(list(list(stmt = "assign", target = tv, value = list(node = "missing", code = ".", line = line), line = line)),
    lapply(setdiff(vars, target), function(v) {
      list(stmt = "subset_if", cond = list(node = "var", name = v, text = v, line = line), line = line, ref_only = TRUE)
    }))
}

# Output metadata (eng$session$meta form) of a DATA step as compiled, or NULL.
err_data_schema <- function(eng, step, ignored = FALSE) {
  stmts <- list()
  # (the names in an invalid statement, ERROR 180-322, add no variables:
  # a step of only such a statement stops with 0 variables [measured])
  failed <- any(vapply(step$stmts, function(s) identical(s$stmt, "failed"), logical(1)))
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
  # a call with too many arguments compiles with its declared arguments
  # (an_extra_args())
  step$extra_args <- TRUE
  replaced <- FALSE
  for (attempt in seq_len(length(stmts) + 1L)) {
    tr <- tryCatch(translate_step(step, eng$session$meta, eng$level),
                   sasonr_syntax_error = function(e) e, sasonr_runtime_error = function(e) e, error = function(e) NULL)
    if (is.null(tr)) return(NULL)
    if (!inherits(tr, "condition")) {
      meta <- tr$meta[vapply(step$outputs, `[[`, "", "name")]
      # the type conversion NOTE of a step stopped by a call with too many
      # arguments; unknown when a statement failed to parse or was replaced
      # below (what it converts, and the type it gives its target)
      if (isTRUE(ct_diag()$extra_args)) {
        if (replaced || failed) {
          abort_unsupported("the type conversion NOTE of a step with a call with too many arguments and another ERROR", step$line)
        }
        attr(meta, "conversion_notes") <- conversion_notes()
      } else if (failed && !replaced && ignored) {
        # an ignored assignment compiles as `target = .` (numeric); the
        # conversions of the other statements are written [measured,
        # cases]; a step parsed again from
        # the chunk text numbers its lines from 1
        if (is.null(eng$cur_step)) {
          off <- (eng$chunk_line %||% 1L) - 1L
          ct <- ct_diag()
          for (id in intersect(names(ct$places), c("conv.n2c", "conv.c2n"))) {
            k <- ct$places[[id]]
            p <- strsplit(k, ":", fixed = TRUE)
            ct$places[[id]] <- vapply(p, function(x) sprintf("%d:%s", as.integer(x[1]) + off, x[2]), "")
          }
        }
        attr(meta, "conversion_notes") <- conversion_notes()
      }
      # the later ERRORs of a step whose first ERROR ignored a statement
      # (76-322); after another first ERROR they are not measured (none in
      # the cases)
      if (length(ct_diag()$secondary) && failed && !replaced && ignored) attr(meta, "secondary") <- ct_diag()$secondary
      return(meta)
    }
    # an ERROR of the analyzer at one statement: replace it by what it contributes at compile time
    ln <- tr$sas_line
    k <- which(vapply(step$stmts, function(s) identical(vctrs::vec_cast(s$line, integer()), vctrs::vec_cast(ln, integer())), logical(1)))
    if (length(k) != 1L) return(NULL)
    s <- step$stmts[[k]]
    kind <- if (s$stmt == "assign" && identical(s$target$node, "var")) "assign" else s$stmt
    vars <- if (kind == "assign") c(s$target$name, expr_reads(s$value)) else character()
    r <- err_failed_stmt(kind, vars, if (kind == "assign") s$target$name else NULL, s$line)
    if (is.null(r)) return(NULL)
    step$stmts <- append(step$stmts[-k], r, after = k - 1L)
    replaced <- TRUE
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
err_recover_step <- function(eng, text, e = NULL) {
  step <- eng$cur_step %||% err_parse_recover(text)
  if (is.null(step)) return(NULL)
  if (step$type == "proc") {
    # PROC TABULATE: only the ERRORs of its own checks (proc_tabulate.R) are
    # known; they leave nothing in WORK and do not start syntax-check mode
    if (step$name == "tabulate") {
      if (!isTRUE(e$tab_error)) return(NULL)
      return(list(kind = "proc", outputs = list(), syntax_check = FALSE, step = step))
    }
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
  meta <- err_data_schema(eng, step, ignored = any(grepl("^ERROR 76-322", e$sas_also %||% "")))
  if (is.null(meta) || any(vapply(meta, is.null, logical(1)))) return(NULL)
  sec <- attr(meta, "secondary")
  # a step parsed again from the chunk text numbers its lines from 1
  if (is.null(eng$cur_step) && length(sec)) {
    off <- (eng$chunk_line %||% 1L) - 1L
    sec <- lapply(sec, function(s2) { s2$line <- s2$line + off; s2 })
  }
  list(kind = "data", outputs = meta, syntax_check = TRUE, conversion_notes = attr(meta, "conversion_notes"),
       secondary = sec)
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
  nosc <- isTRUE(eng$session$options$nosyntaxcheck)
  if (!e$what %in% c("linesize", "unknown_stmt") || !isTRUE(e$open_code) || isTRUE(eng$sql_open) ||
      isTRUE(eng$syntax_check) || isTRUE(eng$sc_pending) || (nosc && !identical(e$what, "unknown_stmt"))) {
    abort_unsupported(paste0("the error handling of this ERROR in a global statement (", err_message_lines(e), ")"),
                      e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, conditionMessage(e))
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  # ERROR 180-322 in open code has a blank line before it (measured; ERROR
  # 13-12 of OPTIONS has none)
  if (identical(e$what, "unknown_stmt")) log_emit(eng, "echo", phase = "compile", src = e$sas_line)
  err_emit_error(eng, e)
  err_set_macro(eng, syscc = 1012)
  # (measured for ERROR 180-322: under NOSYNTAXCHECK no syntax-check mode)
  if (!nosc) {
    eng$sc_pending <- TRUE
    eng$sc_pending_what <- e$what
  }
  invisible()
}

# Handle an ERROR of the program raised while compiling or running the step in `text`.
engine_step_error <- function(eng, e, text, sql = FALSE) {
  if (inherits(e, "sasonr_global_error")) return(engine_global_error(eng, e))
  if (identical(eng$phase, "execute_data")) {
    abort_unsupported(paste0("the data sets that an execution-time ERROR leaves in WORK (", conditionMessage(e), ")"),
                      e$sas_line %||% NULL)
  }
  in_sql <- sql || isTRUE(eng$sql_open) || identical(eng$phase, "sql")
  rec <- if (in_sql) NULL else err_recover_step(eng, text, e)
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
  # ERRORs that the step compiled again after this one finds, each after the
  # source of its line (with its WARNING first)
  for (s2 in rec$secondary) {
    if (identical(eng$phase, "compile") && !isTRUE(e$sas_step_end) && s2$line >= (e$sas_line %||% Inf)) {
      for (w in s2$before) log_emit(eng, "warning", text = w, src = s2$line, phase = "compile")
      log_emit(eng, "error", text = s2$text, at = c(s2$line, s2$col), token = s2$token, src = s2$line, phase = "compile")
    } else {
      abort_unsupported("a second ERROR of a stopped step before the first one", s2$line)
    }
  }
  if (in_sql) {
    if (!isTRUE(eng$sql_noexec)) {
      log_emit(eng, id = "sql.noexec")
    }
    eng$sql_noexec <- TRUE
    # how the step ends: an ERROR found while a statement is read leaves
    # syntax-check mode off; one found while a statement runs starts it at
    # QUIT; an ERROR may name its kind (sql_timing "syntax" or "run"); for
    # other ERRORs this is not implemented
    kind <- if (!is.null(e$sql_timing)) {
      if (e$sql_timing %in% c("syntax", "run")) e$sql_timing else "other"
    } else if (inherits(e, "sasonr_syntax_error")) "syntax" else if (inherits(e, "sasonr_runtime_error")) "run" else "other"
    eng$sql_error_kind <- unique(c(eng$sql_error_kind, kind))
    err_set_macro(eng, syscc = 1012)
    return(invisible())
  }
  # the rest of the step's lines, then what the stopped step writes
  log_source_read(eng, eng$chunk_end)
  log_boundary(eng)
  # the type conversion NOTEs of the compiled step come before it stops
  if (inherits(e, "sasonr_syntax_error") && isTRUE(eng$session$options$notes) && eng$step_notes) {
    log_emit_lines(eng, rec$conversion_notes, phase = "compile")
  }
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
  if (identical(rec$kind, "proc") && rec$step$name %in% c("means", "summary", "freq", "tabulate") && is.null(e$sas_syserr)) {
    # the SYSERR and SYSCC values after a stopped PROC MEANS, SUMMARY, FREQ or TABULATE
    # are not implemented: a reference fails loud
    mac_auto_unknown(eng$session$macro, c("syserr", "syscc"),
                     paste0("a PROC ", toupper(rec$step$name), " step that stopped with an ERROR (SAS line ", e$sas_line %||% "?", ")"))
  } else {
    err_set_macro(eng, syserr = 1012, syscc = if (identical(rec$kind, "proc")) 3000 else 1012)
  }
  # an ERROR that names its code (ERROR 180-322, PROC option ERRORs): SYSERR and SYSCC
  if (!is.null(e$sas_syserr)) err_set_macro(eng, syserr = e$sas_syserr, syscc = e$sas_syserr)
  invisible()
}

# Message `text` on lines of at most `ls` columns, broken after the commas
# of its list, continuation lines indented under the text after the number.
err_wrap_items <- function(text, ls) {
  indent <- strrep(" ", nchar(sub("^(ERROR [0-9]+-[0-9]+: ).*$", "\\1", text)))
  items <- strsplit(text, "(?<=,) ", perl = TRUE)[[1]]
  lines <- character()
  cur <- ""
  for (w in items) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(cand) > ls && nzchar(cur)) {
      lines <- c(lines, cur)
      cur <- paste0(indent, w)
    } else {
      cur <- cand
    }
  }
  c(lines, cur)
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
  if (!is.null(e$sas_block_msgs)) return(err_emit_block_errors(eng, e))
  line <- e$sas_line
  # (`sas_step_end`: the ERROR follows the source of the whole step)
  exact <- identical(eng$phase, "compile") && length(line) == 1L && !is.na(line) && !isTRUE(e$sas_step_end)
  at <- if (exact && length(e$sas_col) == 1L && !is.na(e$sas_col)) c(line, e$sas_col)
  text <- err_message_lines(e)
  # a numbered syntax ERROR longer than LINESIZE continues on lines indented
  # under its text ("ERROR 22-322: " is 14 columns), broken between the items
  # of the expecting list ("a missing value" stays on one line)
  if (isTRUE(e$sas_wrap) && log_error_numbered(text)) text <- err_wrap_items(text, rt_linesize())
  # the source echo around a numbered ERROR on a macro-affected line (SAS
  # shows the generated line; the column of the marker is not known) and
  # around an ERROR on a line that NOSOURCE hides is not implemented: the echo
  # of the submission is left out
  gen <- NULL
  if (log_error_numbered(text) && length(line) == 1L && log_macro_line(line, eng$session$rt)) {
    gen <- if (!is.null(at)) err_macro_echo(eng, line, at[2], e$sas_token)
    if (is.null(gen)) log_echo_omit(eng, "a numbered ERROR on a line with macro references")
  }
  # a numbered ERROR on a line of an included file (not echoed under
  # NOSOURCE2): that line is written as `n         +text` with its marker
  # [measured]
  lev <- eng$include_level
  if (is.null(gen) && !is.null(at) && log_error_numbered(text) && length(lev) >= line && lev[line] > 0L) {
    if (length(e$sas_also)) abort_unsupported("several numbered ERRORs at a token of an included line", line)
    code <- regmatches(text[1], regexec("^ERROR ([0-9]+)-[0-9]+:", text[1]))[[1]][2]
    pos <- 11L + at[2] - 1L
    src_txt <- sub("[[:space:]]+$", "", eng$program$lines[line])
    log_emit(eng, "put", text = c(paste0(formatC(line, width = -10), "+", src_txt), paste0(strrep(" ", pos), strrep("_", nchar(e$sas_token))),
                                  paste0(strrep(" ", pos), code)), src = line, phase = "compile", split = FALSE, gen_echo = TRUE)
    at <- NULL
  }
  if (!is.null(gen)) {
    # "Line generated by the invoked macro": the text the innermost call
    # generated, at the line of the call, the marker under the token there
    log_emit(eng, "note", text = paste0("NOTE: Line generated by the invoked macro \"", gen$name, "\"."), src = line,
             phase = "compile")
    code <- regmatches(text[1], regexec("^ERROR ([0-9]+)-[0-9]+:", text[1]))[[1]][2]
    pos <- 11L + gen$col - 1L
    log_emit(eng, "put", text = c(paste0(formatC(line, width = -10), " ", gen$text), paste0(strrep(" ", pos), strrep("_", nchar(e$sas_token))),
                                  paste0(strrep(" ", pos), code)), src = line, phase = "compile", split = FALSE, gen_echo = TRUE)
    at <- NULL
    if (length(e$sas_also)) abort_unsupported("several numbered ERRORs at a token in macro-generated text", line)
  }
  if (!is.null(at) && !log_source_on_all(eng$program$source, line)) {
    log_echo_omit(eng, "an ERROR on a line that OPTIONS NOSOURCE hides")
  }
  # an ERROR found when the whole step has been read follows its source and
  # the blank line after it, after the WARNINGs the step writes first
  # (`sas_before`)
  end_phase <- NULL
  if (isTRUE(e$sas_step_end)) {
    log_source_read(eng, eng$chunk_end)
    log_boundary(eng)
    end_phase <- "step_end"
  }
  if (length(e$sas_before)) log_emit_lines(eng, e$sas_before, phase = end_phase)
  log_emit(eng, "error", text = text, at = at, token = e$sas_token,
           src = if (exact) line, phase = if (exact) "compile" else end_phase)
  # further numbered ERRORs at the same token, their numbers stacked under the
  # marker (22-322 followed by 76-322 or 202-322)
  for (t in e$sas_also) {
    log_emit(eng, "error", text = t, at = at, token = e$sas_token, src = if (exact) line,
             phase = if (exact) "compile" else end_phase)
  }
  # a second numbered ERROR at the same token (PROC SQL statement options)
  if (!is.null(e$sql_also)) log_emit(eng, "error", text = e$sql_also, src = if (exact) line, phase = if (exact) "compile")
}

# The macro-generated line of a numbered ERROR at program line `line`, column
# `col` (spec 09 contract 2): the innermost macro call whose text holds the
# token, that text (line breaks as blanks, trailing blanks dropped) and the
# column of the token in it; NULL when the token is not wholly in the text
# of one call.
err_macro_echo <- function(eng, line, col, token) {
  prov <- eng$chunk_prov
  text <- eng$chunk_text
  if (is.null(prov) || is.null(text) || !is.character(token) || !nzchar(token)) return(NULL)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  k <- line - (eng$chunk_line %||% 1L) + 1L
  if (k < 1L || k > length(lines)) return(NULL)
  i <- sum(nchar(lines[seq_len(k - 1L)]) + 1L) + col
  j <- i + nchar(token) - 1L
  if (j > length(prov$call)) return(NULL)
  ids <- prov$call[i:j]
  if (anyNA(ids) || length(unique(ids)) != 1L) return(NULL)
  ct <- prov$ctext[[as.character(ids[1])]]
  # (the whole text of the call, also several statements [measured])
  gtxt <- sub("[[:space:]]+$", "", intToUtf8(ct$cp))
  # the leading blanks of the macro text are dropped and the line starts with
  # one blank, with three when the text had some, the call is inside a step
  # and it is alone on its program line, not followed by a semicolon
  # [measured]
  core <- sub("^[[:space:]]+", "", gtxt)
  n_lead <- nchar(gtxt) - nchar(core)
  alone <- grepl("^[[:space:]]*%[A-Za-z_][A-Za-z0-9_]*[[:space:]]*(\\(.*\\))?[[:space:]]*$", eng$program$lines[line] %||% "")
  starts_step <- grepl("^(data|proc)\\b", core, ignore.case = TRUE)
  pad <- if (n_lead > 0L && alone && !starts_step) "   " else " "
  list(name = toupper(ct$name), text = paste0(pad, core), col = prov$off[i] - n_lead + nchar(pad))
}

# The DO/SELECT block ERRORs that the parser recorded in `step`
# (parse_program(recover = TRUE)): an END without DO is ERROR 161-185 at the
# END, the step otherwise compiled without it; blocks still open at RUN are
# ERROR 117-185 after the source of the step, one per kind from the innermost
# block out ("There were 2 unclosed DO blocks."), each after the RUN line
# written again with the marker under its semicolon (measured).  Both kinds in
# one step, two ENDs without DO, and a kind in two groups are not measured:
# fail loud.
engine_block_errors <- function(eng, step) {
  be <- step$block_errors
  kinds <- vapply(be, `[[`, "", "kind")
  if (all(kinds == "nomatch")) {
    if (length(be) > 1L) abort_unsupported("two END statements without a matching DO or SELECT in one step", be[[2]]$line)
    abort_syntax_numbered("No matching DO/SELECT statement. (ERROR 161-185)", be[[1]]$line, be[[1]]$col, "end")
  }
  if (any(kinds == "nomatch")) abort_unsupported("an END without DO and a DO or SELECT block left open in one step", step$line)
  what <- vapply(be, `[[`, "", "what")
  runs <- rle(what)
  if (anyDuplicated(runs$values)) abort_unsupported("DO and SELECT blocks left open in alternating order", step$line)
  msgs <- sprintf("ERROR 117-185: There %s %d unclosed %s block%s.", ifelse(runs$lengths == 1L, "was", "were"),
                  runs$lengths, runs$values, ifelse(runs$lengths == 1L, "", "s"))
  rlang::abort(paste("Syntax error:", msgs[1]), class = c("sasonr_syntax_error", "sasonr_error"),
               sas_line = be[[1]]$line, sas_block_msgs = msgs, sas_reecho = c(be[[1]]$line, be[[1]]$col))
}

# The ERRORs of engine_block_errors() for blocks left open at RUN.
err_emit_block_errors <- function(eng, e) {
  ln <- e$sas_reecho[1]
  if (log_macro_line(ln, eng$session$rt) || !log_source_on_all(eng$program$source, ln)) {
    abort_unsupported("ERROR 117-185 at a RUN statement that a macro generated or NOSOURCE hides", ln)
  }
  log_source_read(eng, eng$chunk_end)
  log_boundary(eng)
  pos <- 11L + e$sas_reecho[2] - 1L
  src_txt <- sub("[[:space:]]+$", "", eng$program$lines[ln])
  for (m in e$sas_block_msgs) {
    log_emit(eng, "put", text = c(paste0(formatC(ln, width = -10), " ", src_txt), paste0(strrep(" ", pos), "_"),
                                  paste0(strrep(" ", pos), "117")), phase = "step_end", split = FALSE, gen_echo = TRUE)
    log_emit(eng, "error", text = m, phase = "step_end")
  }
  invisible()
}
