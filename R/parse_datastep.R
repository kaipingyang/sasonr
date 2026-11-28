# Statement-level recursive-descent parser producing a faithful AST.
#
# Nothing is desugared here: IF/ELSE, DO/END, SELECT/WHEN, IN, LENGTH and
# friends keep their source shape. Lowering happens in analyze/codegen.

#' Parse a SAS program
#'
#' @param code SAS source code, or a token tibble from [sas_lex()].
#' @param resolved `TRUE` when the text has already been through the macro
#'   processor: `&`/`%` left inside double-quoted strings are then plain text.
#' @param sql_open Internal: `TRUE` when a PROC SQL step is already open
#'   (statements before QUIT belong to it).
#' @return A `sasonr_program`: a list with element `steps`.
#' @examples
#' p <- sas_parse("data b; set a; if x > 1; run;")
#' str(p$steps[[1]]$stmts, max.level = 1)
#' @export
sas_parse <- function(code, resolved = FALSE, sql_open = FALSE) {
  parse_program(code, resolved, sql_open)
}

# The body of sas_parse().  `recover`: a DO or SELECT block left open at RUN
# and an END without DO do not stop the parse; they are recorded in the
# step (`block_errors`) for the engine to report (ERROR 117-185,
# 161-185; engine_block_errors()).
parse_program <- function(code, resolved = FALSE, sql_open = FALSE, recover = FALSE) {
  toks <- if (is.data.frame(code)) code else sas_lex(code)
  if (resolved) toks$type[toks$type == "str_macro"] <- "str"
  p <- new_parser(toks)
  p$resolved <- resolved
  p$recover <- recover
  steps <- list()
  # a chunk that continues an open PROC SQL (statements are executed one by one)
  if (sql_open) {
    res <- parse_sql_stmts(p)
    steps <- step_span(p, res$steps, 0L, 1L)
    sql_open <- res$open
  }
  n0 <- length(steps)
  from <- p$pos
  while (!at_eof(p)) {
    # the steps the previous pass added end where this one starts
    if (length(steps) > n0) steps <- step_span(p, steps, n0, from)
    n0 <- length(steps)
    from <- p$pos
    t <- tk_type(p)
    line <- tk_line(p)
    if (t == "semi") {
      advance(p)
      next
    }
    # a macro call left unresolved by the macro processor (it wrote its
    # WARNING) starts no statement: ERROR 180-322 (measured)
    if (t == "macro" && isTRUE(p$resolved)) {
      # (the marker is under the % alone)
      abort_global("Statement is not valid or it is used out of proper order. (ERROR 180-322)", line, "unknown_stmt",
                   TRUE, col = tk_col(p), token = "%")
    }
    if (t %in% c("macro", "macro_var", "str_macro")) {
      abort_unsupported(paste0("macro code ", tk_val(p), " (run the program with sas_run() to expand macros)"), line)
    }
    if (t == "num") {
      abort_global("Statement is not valid or it is used out of proper order. (ERROR 180-322)", line, "unknown_stmt",
                   TRUE, col = tk_col(p), token = tk_val(p))
    }
    if (t != "name") abort_syntax(paste("unexpected", tok_desc(p)), line)
    low <- tk_low(p)
    if (low == "data" && !is_op(p, "=", 1L)) {
      steps <- c(steps, list(parse_data_step(p)))
      next
    }
    if (low == "proc" && is_kw(p, "sql", 1L)) {
      res <- parse_sql_block(p)
      steps <- c(steps, res$steps)
      sql_open <- res$open
      next
    }
    if (low == "proc") {
      steps <- c(steps, list(parse_proc_step(p)))
      next
    }
    if (low %in% c("run", "quit") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      next
    }
    steps <- c(steps, list(parse_global_stmt(p, open_code = TRUE)))
  }
  if (length(steps) > n0) steps <- step_span(p, steps, n0, from)
  out <- structure(list(steps = steps), class = "sasonr_program", sql_open = sql_open)
  # the node checks of the tests (ast_validate.R)
  if (isTRUE(getOption("sasonr.validate_ast"))) validate_sas_node(out)
  out
}

# The source span of the steps `n0 + 1` ... that one pass of sas_parse()
# added: from token `from` to the token at the parser position (exclusive;
# NA at the end of the text).  The first of them gets the span, the others
# (the statements of a PROC SQL block) are covered by it (`span` NULL).
step_span <- function(p, steps, n0, from) {
  if (length(steps) <= n0) return(steps)
  to <- p$pos
  end <- if (to <= p$n) c(p$line[to], p$col[to]) else c(NA_integer_, NA_integer_)
  start <- if (from <= p$n) c(p$line[from], p$col[from]) else end
  steps[[n0 + 1L]]$span <- c(line = start[1], col = start[2], end_line = end[1], end_col = end[2])
  steps
}

parse_data_step <- function(p) {
  line <- tk_line(p)
  advance(p)
  outputs <- list()
  while (tk_type(p) != "semi") {
    if (tk_type(p) == "str") abort_unsupported("a quoted physical data set name (permanent data set)", tk_line(p))
    if (tk_type(p) != "name") abort_syntax(paste("expected a data set name but found", tok_desc(p)), tk_line(p))
    outputs <- c(outputs, list(parse_ds_ref(p, "data_output")))
  }
  advance(p)
  if (length(outputs) == 0) abort_unsupported("DATA statement without a data set name", line)
  is_null <- vapply(outputs, function(o) o$name == "_null_", logical(1))
  if (any(is_null) && length(outputs) > 1) abort_unsupported("_NULL_ combined with other outputs", line)

  p$arrays <- character()
  stmts <- list()
  p$block_errors <- list()
  repeat {
    if (at_eof(p)) break
    if (is_kw(p, "run") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    }
    # Implicit step boundary: the next DATA or PROC statement.
    if (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L)) break
    end_col <- tk_col(p)
    s <- parse_statement(p)
    if (s$stmt == "end" && isTRUE(p$recover)) {
      p$block_errors <- c(p$block_errors, list(list(kind = "nomatch", line = s$line, col = end_col)))
      next
    }
    if (s$stmt == "end") abort_syntax("END without a matching DO or SELECT", s$line)
    stmts <- c(stmts, list(s))
    if (s$stmt == "datalines") break
  }
  list(
    type = "data", line = line, null = any(is_null),
    # _NULL_ together with other output names (`data a _null_;`)
    null_mixed = any(is_null) && !all(is_null),
    outputs = if (any(is_null)) list() else outputs, stmts = stmts,
    block_errors = if (length(p$block_errors)) p$block_errors
  )
}

# Data set references and options ----------------------------------------

parse_ds_ref <- function(p, context) {
  line <- tk_line(p)
  if (tk_type(p) == "str") abort_unsupported("a quoted physical data set name (permanent data set)", line)
  name <- tk_low(p)
  advance(p)
  if (tk_type(p) == "dot" && tk_type(p, 1L) == "name" && tk_adjacent(p, -1L, 0L)) {
    lib <- name
    name <- tk_low(p, 1L)
    advance(p, 2L)
    if (lib != "work") {
      # a member of a read-only library (LIBNAME): read as WORK-like data set
      # `lib.member` (library.R); writing to it is not supported
      if (context %in% c("data_output", "output")) abort_unsupported(paste0("writing to library ", toupper(lib), " (only WORK is written)"), line)
      if (lib %in% c("sashelp", "sasuser")) abort_unsupported(paste0("library ", toupper(lib), " outside PROC SQL"), line)
      opts <- list()
      if (tk_type(p) == "lparen") opts <- parse_ds_options(p, context)
      return(list(name = paste0(lib, ".", name), libref = lib, member = name, options = opts, line = line))
    }
  }
  opts <- list()
  if (tk_type(p) == "lparen") opts <- parse_ds_options(p, context)
  list(name = name, options = opts, line = line)
}

parse_ds_options <- function(p, context) {
  advance(p)
  opts <- list()
  while (tk_type(p) != "rparen") {
    line <- tk_line(p)
    if (tk_type(p) != "name" || !is_op(p, "=", 1L)) {
      abort_syntax(paste("expected a data set option but found", tok_desc(p)), line)
    }
    opt <- tk_low(p)
    advance(p, 2L)
    if (opt %in% c("keep", "drop")) {
      items <- list()
      while (tk_type(p) == "name" && !is_op(p, "=", 1L)) items <- c(items, list(parse_varlist_item(p)))
      opts[[opt]] <- items
    } else if (opt == "rename") {
      opts$rename <- parse_rename_pairs(p, paren = TRUE)
    } else if (opt == "where") {
      expect_type(p, "lparen", "'(' after WHERE=")
      opts$where <- parse_where_expr(p)
      expect_type(p, "rparen", "')'")
    } else if (opt == "in" && context == "input") {
      opts$in_var <- tk_low(p)
      expect_type(p, "name", "a variable name after IN=")
    } else if (opt == "label" && context == "data_output") {
      # the data set label of a DATA step output (at most 256 characters)
      if (!tk_type(p) %in% c("str", "str_macro")) abort_syntax("expected a quoted label after LABEL=", line)
      opts$label <- substr(tk_val(p), 1L, 256L)
      advance(p)
    } else if (opt %in% c("firstobs", "obs") && context == "input") {
      # n, nK / nM / nG / nT (multiples of 1024), MIN or MAX (the largest count);
      # MIN is 1 for FIRSTOBS= and 0 for OBS=
      if (tk_type(p) == "name" && tk_low(p) %in% c("min", "max")) {
        opts[[opt]] <- if (tk_low(p) == "min") (if (opt == "firstobs") 1 else 0) else 2^63 - 1
        advance(p)
      } else {
        v <- as.numeric(tk_val(p))
        expect_type(p, "num", "a number")
        if (tk_type(p) == "name" && tk_adjacent(p, -1L, 0L)) {
          k <- match(tk_low(p), c("k", "m", "g", "t"))
          if (is.na(k)) abort_syntax(paste0("invalid value for data set option ", toupper(opt), "="), line)
          v <- v * 1024^k
          advance(p)
        }
        opts[[opt]] <- v
      }
    } else {
      abort_unsupported(paste0("data set option ", toupper(opt), "= on ", sub("data_", "", context), " data set"), line)
    }
  }
  advance(p)
  opts
}

parse_rename_pairs <- function(p, paren) {
  if (paren) expect_type(p, "lparen", "'(' after RENAME=")
  out <- character()
  while (tk_type(p) == "name" && is_op(p, "=", 1L) && tk_type(p, 2L) == "name") {
    out[tk_low(p)] <- tk_low(p, 2L)
    advance(p, 3L)
  }
  if (paren) expect_type(p, "rparen", "')'")
  out
}

# Formats -----------------------------------------------------------------

is_format_ahead <- function(p) {
  k <- 0L
  if (is_op(p, "$")) {
    if (!tk_adjacent(p, 0L, 1L)) return(FALSE)
    k <- 1L
  }
  t <- tk_type(p, k)
  if (t == "num") return(grepl(".", tk_val(p, k), fixed = TRUE))
  if (t == "name") {
    if (tk_type(p, k + 1L) == "dot" && tk_adjacent(p, k, k + 1L)) return(TRUE)
    if (tk_type(p, k + 1L) == "num" && tk_adjacent(p, k, k + 1L) && startsWith(tk_val(p, k + 1L), ".")) return(TRUE)
  }
  FALSE
}

parse_format <- function(p) {
  line <- tk_line(p)
  char <- FALSE
  if (is_op(p, "$")) {
    char <- TRUE
    advance(p)
  }
  name <- ""
  w <- NA_integer_
  d <- NA_integer_
  if (tk_type(p) == "num") {
    parts <- strsplit(tk_val(p), ".", fixed = TRUE)[[1]]
    w <- as.integer(parts[1])
    if (length(parts) > 1 && nzchar(parts[2])) d <- as.integer(parts[2])
    advance(p)
  } else if (tk_type(p) == "name") {
    sp <- split_trailing_digits(tk_low(p))
    name <- sp$base
    w <- sp$width
    advance(p)
    if (tk_type(p) == "dot" && tk_adjacent(p, -1L, 0L)) {
      advance(p)
      if (tk_type(p) == "num" && tk_adjacent(p, -1L, 0L) && !grepl(".", tk_val(p), fixed = TRUE)) {
        d <- as.integer(tk_val(p))
        advance(p)
      }
    } else if (tk_type(p) == "num" && tk_adjacent(p, -1L, 0L) && startsWith(tk_val(p), ".")) {
      d <- as.integer(substring(tk_val(p), 2L))
      advance(p)
    } else if (isTRUE(p$fmt_no_period) && tk_type(p) == "rbrace") {
      # a format in brackets (PROC FORMAT label [fmt]) needs no period
    } else {
      abort_syntax("a format name must end with a period", line)
    }
  } else {
    abort_syntax(paste("expected a format but found", tok_desc(p)), line)
  }
  text <- paste0(if (char) "$", name, if (!is.na(w)) w, ".", if (!is.na(d)) d)
  list(name = name, w = w, d = d, char = char, text = text)
}

# Statements ----------------------------------------------------------------

# Every statement node gets its source span, up to its semicolon, and the class
# c("sas_stmt_<stmt>", "sas_stmt") (spec 09 contract 3, L3b; node_span()).
parse_statement <- function(p) {
  start <- p$pos
  s <- parse_statement_body(p)
  if (!is.list(s) || !is.character(s$stmt)) return(s)
  last <- max(start, p$pos - 1L)
  s$span <- c(tok = start, tok_end = last, line = p$line[start], col = p$col[start],
              end_line = p$line[last], end_col = p$col[last])
  class(s) <- c(paste0("sas_stmt_", s$stmt), "sas_stmt")
  s
}

parse_statement_body <- function(p) {
  line <- tk_line(p)
  t <- tk_type(p)
  if (t == "semi") {
    advance(p)
    return(list(stmt = "null", line = line))
  }
  if (t %in% c("macro", "macro_var", "str_macro")) {
    abort_unsupported(paste0("macro code ", tk_val(p), " (macro language not implemented)"), line)
  }
  if (t == "eof") abort_syntax("unexpected end of program inside a block", line)
  if (t != "name") abort_syntax(paste("unexpected", tok_desc(p), "at start of statement"), line)
  low <- tk_low(p)

  # `name =` is always an assignment, so `set = 1; if = 2; end = 3;` are legal.
  if (is_op(p, "=", 1L)) return(parse_assign(p))
  # SUBSTR(variable, position <, length>) = value;  (left-hand SUBSTR)
  if (low == "substr" && tk_type(p, 1L) == "lparen") return(parse_substr_assign(p))
  if (tk_type(p, 1L) == "lbrace" || (tk_type(p, 1L) == "lparen" && low %in% p$arrays)) {
    return(parse_assign(p))
  }
  if (is_op(p, "+", 1L) && !low %in% c("put", "input")) return(parse_sum(p))
  # obj.method(...); a method call statement of a component object
  if (tk_type(p, 1L) == "dot" && tk_adjacent(p, 0L, 1L) && tk_type(p, 2L) == "name" && tk_type(p, 3L) == "lparen") {
    call <- parse_obj_ref(p)
    expect_semi(p, "method call")
    return(list(stmt = "method", call = call, line = line))
  }

  switch(low,
    "if" = parse_if(p),
    "do" = parse_do(p),
    "end" = {
      advance(p)
      expect_semi(p, "END")
      list(stmt = "end", line = line)
    },
    leave = ,
    continue = ,
    delete = {
      advance(p)
      expect_semi(p, toupper(low))
      list(stmt = low, line = line)
    },
    select = parse_select(p),
    declare = ,
    dcl = parse_declare(p),
    set = ,
    merge = ,
    update = parse_set(p, low),
    by = parse_by(p),
    retain = parse_retain(p),
    length = parse_length(p),
    format = parse_format_stmt(p),
    informat = {
      s <- parse_format_stmt(p)
      s$stmt <- "informat"
      s
    },
    attrib = parse_attrib(p),
    stop = ,
    "return" = {
      advance(p)
      expect_semi(p, toupper(low))
      list(stmt = low, line = line)
    },
    label = parse_label(p),
    keep = ,
    drop = {
      advance(p)
      items <- list()
      while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
      expect_semi(p, toupper(low))
      list(stmt = low, items = items, line = line)
    },
    rename = {
      advance(p)
      pairs <- parse_rename_pairs(p, paren = FALSE)
      expect_semi(p, "RENAME")
      list(stmt = "rename", pairs = pairs, line = line)
    },
    array = parse_array(p),
    output = {
      advance(p)
      targets <- character()
      while (tk_type(p) == "name") {
        targets <- c(targets, tk_low(p))
        advance(p)
      }
      expect_semi(p, "OUTPUT")
      list(stmt = "output", targets = targets, line = line)
    },
    where = parse_where(p),
    put = parse_put(p),
    call = parse_call_stmt(p),
    input = parse_input(p),
    infile = parse_infile(p),
    file = parse_file(p),
    datalines = ,
    cards = ,
    lines = ,
    datalines4 = ,
    cards4 = ,
    lines4 = parse_datalines(p),
    "else" = abort_syntax("ELSE without a matching IF-THEN", line),
    when = ,
    otherwise = abort_syntax(paste(toupper(low), "outside a SELECT group"), line),
    run = abort_syntax("RUN before the enclosing DO/SELECT block was closed", line),
    {
      # a word that begins no SAS statement, followed by a name (`foo x;`):
      # ERROR 180-322 at the word; any other unknown statement fails loud
      if (!low %in% sas_statement_words && tk_type(p, 1L) == "name") {
        abort_syntax_numbered("Statement is not valid or it is used out of proper order. (ERROR 180-322)",
                              line, tk_col(p), tk_val(p), syserr = 3000)
      }
      abort_unsupported(paste0("DATA step statement ", toupper(tk_val(p))), line)
    }
  )
}

# The words that begin a statement of the SAS 9.4 language in a DATA step:
# the DATA step statements, the global statements and the words of the
# statements that sasonr does not implement, so that only a word that is no
# statement at all is ERROR 180-322.
sas_statement_words <- c(
  "abort", "array", "attrib", "by", "call", "cards", "cards4", "catname", "checkpoint", "comment", "continue",
  "data", "datalines", "datalines4", "dcl", "declare", "delete", "describe", "display", "dm", "do", "drop",
  "else", "end", "endrsubmit", "endsas", "error", "execute", "file", "filename", "footnote", "format", "go",
  "goto", "if", "infile", "informat", "input", "keep", "label", "leave", "length", "libname", "link", "list",
  "lines", "lines4", "lock", "lostcard", "merge", "missing", "modify", "ods", "options", "otherwise", "output",
  "page", "proc", "put", "putlog", "redirect", "remove", "rename", "replace", "resetline", "retain", "return",
  "rsubmit", "run", "sasfile", "select", "set", "signoff", "signon", "skip", "stop", "sysecho", "title",
  "update", "when", "where", "window", "x",
  paste0("footnote", 1:10), paste0("title", 1:10))

# SUBSTR(target, position <, length>) = value; replaces part of a character
# variable in place (the "left-hand SUBSTR").
parse_substr_assign <- function(p) {
  line <- tk_line(p)
  col <- tk_col(p)
  advance(p) # substr
  expect_type(p, "lparen", "'(' after SUBSTR")
  if (tk_type(p) != "name") abort_syntax("expected a variable name as the first argument of the SUBSTR statement", line)
  var <- tk_low(p)
  var_text <- tk_val(p)
  advance(p)
  if (tk_type(p) != "comma") abort_syntax("expected ',' after the variable of the SUBSTR statement", line)
  advance(p)
  pos <- parse_expr(p)
  len <- NULL
  if (tk_type(p) == "comma") {
    advance(p)
    len <- parse_expr(p)
  }
  expect_type(p, "rparen", "')'")
  if (!is_op(p, "=")) abort_syntax(paste("expected '=' but found", tok_desc(p)), line)
  advance(p)
  value <- parse_expr(p)
  expect_semi(p, "SUBSTR assignment")
  list(stmt = "substr_assign", var = var, var_text = var_text, pos = pos, len = len, value = value,
       line = line, col = col)
}

parse_assign <- function(p) {
  line <- tk_line(p)
  if (tk_type(p, 1L) %in% c("lbrace", "lparen")) {
    target <- parse_array_ref(p)
  } else {
    target <- list(node = "var", name = tk_low(p), text = tk_val(p), line = line)
    parser_saw(p, target$name)
    advance(p)
  }
  # a sum statement on an array element: a[1] + x;
  if (is_op(p, "+") && target$node == "array_ref") {
    advance(p)
    value <- parse_expr(p)
    expect_semi(p, "sum statement")
    return(list(stmt = "sum", target = target, value = value, line = line))
  }
  if (!is_op(p, "=")) abort_syntax(paste("expected '=' but found", tok_desc(p)), line)
  advance(p)
  p$assign_value <- TRUE
  value <- parse_expr(p)
  if (tk_type(p) == "name" && p$pos > 1L && p$type[p$pos - 1L] == "num") assign_missing_semi(p)
  expect_semi(p, "assignment")
  list(stmt = "assign", target = target, value = value, line = line)
}

parse_sum <- function(p) {
  line <- tk_line(p)
  target <- list(node = "var", name = tk_low(p), text = tk_val(p), line = line)
  advance(p, 2L)
  value <- parse_expr(p)
  expect_semi(p, "sum statement")
  list(stmt = "sum", target = target, value = value, line = line)
}

parse_if <- function(p) {
  line <- tk_line(p)
  advance(p)
  cond <- parse_expr(p)
  if (tk_type(p) == "semi") {
    advance(p)
    return(list(stmt = "subset_if", cond = cond, line = line))
  }
  expect_kw(p, "then")
  then_s <- parse_branch(p)
  else_s <- NULL
  if (is_kw(p, "else") && !is_op(p, "=", 1L)) {
    advance(p)
    else_s <- parse_branch(p)
  }
  list(stmt = "if", cond = cond, then = then_s, "else" = else_s, line = line)
}

# DECLARE HASH name<(arguments)>; / DECLARE HITER name(arguments); (DCL too)
parse_declare <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (!is_kw(p, c("hash", "hiter"))) abort_unsupported(paste0("DECLARE of the component object type ", toupper(tk_val(p))), line)
  cls <- tk_low(p)
  advance(p)
  if (tk_type(p) != "name") abort_syntax("expecting the object name after DECLARE", line)
  name <- tk_low(p)
  advance(p)
  args <- if (tk_type(p) == "lparen") parse_obj_args(p) else list()
  expect_semi(p, "DECLARE")
  list(stmt = "declare", class = cls, name = name, args = args, line = line)
}

parse_branch <- function(p) {
  s <- parse_statement(p)
  if (s$stmt == "end") abort_syntax("END without a matching DO", s$line)
  s
}

parse_block <- function(p, what) {
  body <- list()
  repeat {
    if (at_eof(p)) abort_syntax(paste("missing END for", what), tk_line(p))
    if (parse_unclosed_at_run(p, what)) break
    s <- parse_statement(p)
    if (s$stmt == "end") break
    if (s$stmt %in% c("datalines")) abort_syntax(paste("DATALINES inside", what), s$line)
    body <- c(body, list(s))
  }
  body
}

parse_while_until <- function(p) {
  kind <- tk_low(p)
  advance(p)
  expect_type(p, "lparen", paste0("'(' after ", toupper(kind)))
  cond <- parse_expr(p)
  expect_type(p, "rparen", "')'")
  list(kind = kind, cond = cond)
}

parse_do <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) == "semi") {
    advance(p)
    return(list(stmt = "do", kind = "block", body = parse_block(p, "DO"), line = line))
  }
  if (is_kw(p, c("while", "until")) && tk_type(p, 1L) == "lparen") {
    wu <- parse_while_until(p)
    expect_semi(p, "DO")
    return(list(stmt = "do", kind = wu$kind, cond = wu$cond, body = parse_block(p, "DO"), line = line))
  }
  # DO OVER array; (an implicitly subscripted array, index _I_)
  if (is_kw(p, "over") && tk_type(p, 1L) == "name" && tk_type(p, 2L) == "semi") {
    arr <- tk_low(p, 1L)
    advance(p, 3L)
    return(list(stmt = "do", kind = "over", array = arr, body = parse_block(p, "DO"), line = line))
  }
  if (!(tk_type(p) == "name" && is_op(p, "=", 1L))) {
    abort_syntax(paste("invalid DO statement near", tok_desc(p)), line)
  }
  var <- tk_low(p)
  var_text <- tk_val(p)
  advance(p, 2L)
  first <- parse_expr(p)
  if (is_kw(p, "to")) {
    advance(p)
    to <- parse_expr(p)
    by <- NULL
    if (is_kw(p, "by")) {
      advance(p)
      by <- parse_expr(p)
    }
    wu <- NULL
    if (is_kw(p, c("while", "until"))) wu <- parse_while_until(p)
    if (tk_type(p) == "comma") abort_unsupported("DO lists mixing TO ranges and values", line)
    expect_semi(p, "DO")
    return(list(
      stmt = "do", kind = "iter", var = var, var_text = var_text, from = first, to = to, by = by,
      while_until = wu, body = parse_block(p, "DO"), line = line
    ))
  }
  if (is_kw(p, "by")) abort_unsupported("DO with BY but no TO", line)
  values <- list(first)
  while (tk_type(p) == "comma") {
    advance(p)
    values <- c(values, list(parse_expr(p)))
    if (is_kw(p, "to")) abort_unsupported("DO lists mixing TO ranges and values", line)
  }
  if (is_kw(p, c("while", "until"))) abort_unsupported("WHILE/UNTIL on a DO value list", line)
  expect_semi(p, "DO")
  list(stmt = "do", kind = "list", var = var, var_text = var_text, values = values,
       body = parse_block(p, "DO"), line = line)
}

parse_select <- function(p) {
  line <- tk_line(p)
  advance(p)
  sel <- NULL
  if (tk_type(p) == "lparen") {
    advance(p)
    sel <- parse_expr(p)
    expect_type(p, "rparen", "')'")
  }
  expect_semi(p, "SELECT")
  whens <- list()
  otherwise <- NULL
  has_otherwise <- FALSE
  repeat {
    if (is_kw(p, "when") && tk_type(p, 1L) == "lparen") {
      if (has_otherwise) abort_syntax("a WHEN clause cannot follow OTHERWISE in a SELECT group", tk_line(p))
      wline <- tk_line(p)
      advance(p, 2L)
      vals <- list(parse_expr(p))
      while (tk_type(p) == "comma") {
        advance(p)
        vals <- c(vals, list(parse_expr(p)))
      }
      expect_type(p, "rparen", "')'")
      whens <- c(whens, list(list(values = vals, body = parse_branch(p), line = wline)))
    } else if (is_kw(p, "otherwise") && !is_op(p, "=", 1L)) {
      advance(p)
      if (has_otherwise) abort_syntax("only one OTHERWISE clause is allowed in a SELECT group", tk_line(p))
      otherwise <- parse_branch(p)
      has_otherwise <- TRUE
    } else if (is_kw(p, "end") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    } else {
      if (parse_unclosed_at_run(p, "SELECT")) break
      abort_syntax(paste("expected WHEN, OTHERWISE or END in SELECT but found", tok_desc(p)), tk_line(p))
    }
  }
  list(stmt = "select", expr = sel, whens = whens, otherwise = otherwise,
       has_otherwise = has_otherwise, line = line)
}

parse_set <- function(p, kind) {
  line <- tk_line(p)
  advance(p)
  datasets <- list()
  opts <- list()
  while (tk_type(p) != "semi") {
    if (tk_type(p) == "str") abort_unsupported("a quoted physical data set name (permanent data set)", tk_line(p))
    if (tk_type(p) == "name" && is_op(p, "=", 1L)) {
      opt <- tk_low(p)
      oline <- tk_line(p)
      advance(p, 2L)
      set_opts <- c("end", "nobs", "point", "key", "indsname", "curobs", "open", "keyreset", if (kind == "update") "updatemode")
      if (!opt %in% set_opts) {
        # not an option of the SET/MERGE statement: syntax ERROR 22-322
        abort_syntax(paste0("expected a data set name or a ", toupper(kind), " statement option but found ", toupper(opt), "="), oline)
      }
      if (!opt %in% c("end", "nobs", "point", "updatemode") || (kind %in% c("merge", "update") && opt %in% c("nobs", "point"))) {
        abort_unsupported(paste0(toupper(kind), " statement option ", toupper(opt), "="), oline)
      }
      opts[[opt]] <- tk_low(p)
      expect_type(p, "name", paste0("a variable name after ", toupper(opt), "="))
      next
    }
    if (tk_type(p) == "name") {
      ref <- parse_ds_ref(p, "input")
      if (is_op(p, "-") && tk_type(p, 1L) == "name") {
        # numbered data set list: d1-d3
        if (length(ref$options) > 0) abort_unsupported("data set options inside a data set list", line)
        to <- tk_low(p, 1L)
        advance(p, 2L)
        if (tk_type(p) == "lparen") abort_unsupported("data set options on a data set list", line)
        nms <- expand_numbered_range(ref$name, to, line)
        # zero-padded endpoints (ds00-ds02, d08-d12): members keep the padding
        dg <- function(s) sub("^.*?([0-9]+)$", "\\1", s, perl = TRUE)
        if (any(startsWith(c(dg(ref$name), dg(to)), "0") & nchar(c(dg(ref$name), dg(to))) > 1L)) {
          base <- substr(ref$name, 1L, nchar(ref$name) - nchar(dg(ref$name)))
          width <- max(nchar(dg(ref$name)), nchar(dg(to)))
          nms <- paste0(base, formatC(as.integer(substring(nms, nchar(base) + 1L)), width = width, flag = "0"))
        }
        for (nm in nms) {
          datasets <- c(datasets, list(list(name = nm, options = list(), line = ref$line)))
        }
        next
      }
      datasets <- c(datasets, list(ref))
      if (is_op(p, ":")) abort_unsupported("data set prefix lists", line)
      next
    }
    abort_syntax(paste("unexpected", tok_desc(p), "in", toupper(kind), "statement"), tk_line(p))
  }
  advance(p)
  if (length(datasets) == 0) abort_unsupported(paste(toupper(kind), "without a data set (_LAST_)"), line)
  if (!is.null(opts$updatemode) && !opts$updatemode %in% c("missingcheck", "nomissingcheck")) {
    abort_syntax(paste0("UPDATEMODE=", toupper(opts$updatemode), " is not MISSINGCHECK or NOMISSINGCHECK"), line)
  }
  list(stmt = kind, datasets = datasets, end = opts$end, nobs = opts$nobs, point = opts$point,
       updatemode = opts$updatemode, line = line)
}

parse_by <- function(p) {
  line <- tk_line(p)
  advance(p)
  vars <- character()
  desc <- logical()
  pending_desc <- FALSE
  while (tk_type(p) == "name") {
    low <- tk_low(p)
    if (low %in% c("notsorted", "groupformat")) abort_unsupported(paste("BY option", toupper(low)), line)
    if (low == "descending") {
      pending_desc <- TRUE
    } else {
      vars <- c(vars, low)
      desc <- c(desc, pending_desc)
      pending_desc <- FALSE
    }
    advance(p)
  }
  expect_semi(p, "BY")
  if (length(vars) == 0) abort_syntax("BY statement without variables", line)
  list(stmt = "by", vars = vars, descending = desc, line = line)
}

parse_retain <- function(p) {
  line <- tk_line(p)
  advance(p)
  groups <- list()
  cur <- list()
  repeat {
    t <- tk_type(p)
    if (t == "semi") break
    if (t == "name") {
      cur <- c(cur, list(parse_varlist_item(p)))
      next
    }
    if (t == "lparen") {
      advance(p)
      vals <- list()
      while (tk_type(p) != "rparen") {
        if (tk_type(p) == "comma") {
          advance(p)
          next
        }
        vals <- c(vals, list(parse_constant(p, "RETAIN initial value")))
      }
      advance(p)
      groups <- c(groups, list(list(vars = cur, init = vals, paren = TRUE)))
      cur <- list()
      next
    }
    init <- parse_constant(p, "RETAIN initial value")
    groups <- c(groups, list(list(vars = cur, init = list(init))))
    cur <- list()
  }
  if (length(cur) > 0) groups <- c(groups, list(list(vars = cur, init = NULL)))
  expect_semi(p, "RETAIN")
  list(stmt = "retain", groups = groups, line = line)
}

parse_length <- function(p) {
  line <- tk_line(p)
  advance(p)
  specs <- list()
  cur <- list()
  repeat {
    t <- tk_type(p)
    if (t == "semi") break
    if (t == "name") {
      if (tk_low(p) == "default" && is_op(p, "=", 1L)) abort_unsupported("LENGTH DEFAULT=", line)
      cur <- c(cur, list(parse_varlist_item(p)))
      next
    }
    if (is_op(p, "$")) {
      advance(p)
      if (tk_type(p) != "num") abort_syntax("expected a length after '$'", line)
      len <- as.integer(tk_val(p))
      if (is.na(len) || len < 1 || len > 32767) abort_syntax("the length of character variables is 1-32767", line)
      specs <- c(specs, list(list(vars = cur, type = "char", len = len)))
      advance(p)
      cur <- list()
      next
    }
    if (t == "num") {
      # the range is checked on the value as written (8.5 is an ERROR), then truncated (3.5 is 3)
      raw <- suppressWarnings(as.numeric(tk_val(p)))
      if (is.na(raw) || raw < 3 || raw > 8) abort_syntax("the length of numeric variables is 3-8", line)
      len <- as.integer(raw)
      specs <- c(specs, list(list(vars = cur, type = "num", len = len)))
      advance(p)
      cur <- list()
      next
    }
    abort_syntax(paste("unexpected", tok_desc(p), "in LENGTH statement"), line)
  }
  if (length(cur) > 0) abort_syntax("LENGTH variable without a length", line)
  expect_semi(p, "LENGTH")
  list(stmt = "length", specs = specs, line = line)
}

parse_format_stmt <- function(p) {
  line <- tk_line(p)
  advance(p)
  specs <- list()
  cur <- list()
  while (tk_type(p) != "semi") {
    if (is_format_ahead(p)) {
      if (length(cur) == 0) abort_syntax("FORMAT without variables", line)
      specs <- c(specs, list(list(vars = cur, fmt = parse_format(p))))
      cur <- list()
    } else if (tk_type(p) == "name") {
      cur <- c(cur, list(parse_varlist_item(p)))
    } else {
      abort_syntax(paste("unexpected", tok_desc(p), "in FORMAT statement"), line)
    }
  }
  if (length(cur) > 0) specs <- c(specs, list(list(vars = cur, fmt = NULL)))
  advance(p)
  list(stmt = "format", specs = specs, line = line)
}

parse_attrib <- function(p) {
  line <- tk_line(p)
  advance(p)
  groups <- list()
  while (tk_type(p) != "semi") {
    vars <- list()
    while (tk_type(p) == "name" && !is_op(p, "=", 1L)) vars <- c(vars, list(parse_varlist_item(p)))
    if (length(vars) == 0) abort_syntax(paste("expected a variable in ATTRIB but found", tok_desc(p)), line)
    g <- list(vars = vars)
    while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
      o <- tk_low(p)
      advance(p, 2L)
      if (o == "length") {
        char <- is_op(p, "$")
        if (char) advance(p)
        if (tk_type(p) != "num") abort_syntax("expected a length in ATTRIB", line)
        # a fractional length is truncated ($8.5 is 8, 3.5 is 3); a numeric length is
        # range-checked on the value as written (8.5 and 2.5 are ERRORs), as in LENGTH
        raw <- suppressWarnings(as.numeric(tk_val(p)))
        g$len <- list(type = if (char) "char" else "num", len = suppressWarnings(as.integer(raw)))
        if (char && (is.na(g$len$len) || g$len$len < 1 || g$len$len > 32767)) abort_syntax("The length of character variables is 1-32767", line)
        if (!char && (is.na(raw) || raw < 3 || raw > 8)) abort_syntax("The length of numeric variables is 3-8", line)
        advance(p)
      } else if (o %in% c("format", "informat")) {
        g[[o]] <- parse_format(p)
      } else if (o == "label") {
        if (!tk_type(p) %in% c("str", "str_macro")) abort_syntax("expected a quoted label in ATTRIB", line)
        g$label <- tk_val(p)
        advance(p)
      } else {
        abort_unsupported(paste0("ATTRIB option ", toupper(o), "="), line)
      }
    }
    groups <- c(groups, list(g))
  }
  advance(p)
  list(stmt = "attrib", groups = groups, line = line)
}

parse_label <- function(p) {
  line <- tk_line(p)
  advance(p)
  labels <- character()
  texts <- character()
  while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
    nm <- tk_low(p)
    texts[nm] <- tk_val(p)
    advance(p, 2L)
    if (tk_type(p) != "str") abort_syntax("expected a quoted label", line)
    labels[nm] <- tk_val(p)
    advance(p)
  }
  expect_semi(p, "LABEL")
  list(stmt = "label", labels = labels, texts = texts, line = line)
}

parse_array <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name") abort_syntax("expected an array name", line)
  name <- tk_low(p)
  advance(p)
  # an implicitly subscripted array: no dimension (DO OVER, index _I_)
  implicit <- tk_type(p) == "name" || is_op(p, "$")
  if (!implicit && !tk_type(p) %in% c("lbrace", "lparen")) abort_syntax("expected an array dimension", line)
  close <- if (tk_type(p) == "lparen") "rparen" else "rbrace"
  if (!implicit) advance(p)
  dim <- NA_integer_
  lo <- 1L
  dims <- NULL
  if (implicit) {
  } else if (is_op(p, "*")) {
    advance(p)
  } else {
    # bounds may be negative: array b{-2:2}
    signed_int <- function(what) {
      neg <- is_op(p, "-")
      if (neg) advance(p)
      if (tk_type(p) == "num" && !tk_int(p)) abort_syntax("expecting one of the following: an integer constant, *.", line)
      v <- as.integer(tk_val(p))
      expect_type(p, "num", what)
      if (neg) -v else v
    }
    one <- function() {
      n1 <- signed_int("an array dimension")
      if (is_op(p, ":")) {
        advance(p)
        n2 <- signed_int("an array upper bound")
        return(c(n1, n2 - n1 + 1L))
      }
      c(1L, n1)
    }
    # a multi-dimensional array: the bounds of each dimension (row-major)
    los <- integer()
    dims <- integer()
    repeat {
      b <- one()
      los <- c(los, b[1])
      dims <- c(dims, b[2])
      if (tk_type(p) != "comma") break
      advance(p)
    }
    lo <- los[1]
    dim <- as.integer(prod(dims))
    if (length(dims) == 1L) dims <- NULL else attr(dims, "lo") <- los
  }
  if (!implicit) expect_type(p, close, "closing bracket")
  char <- FALSE
  len <- NA_integer_
  if (is_op(p, "$")) {
    char <- TRUE
    advance(p)
    if (tk_type(p) == "num") {
      len <- as.integer(tk_val(p))
      advance(p)
    }
  } else if (tk_type(p) == "num") {
    advance(p) # numeric length, ignored
  }
  temporary <- FALSE
  items <- list()
  while (tk_type(p) == "name") {
    if (tk_low(p) == "_temporary_") {
      temporary <- TRUE
      advance(p)
    } else {
      items <- c(items, list(parse_varlist_item(p)))
    }
  }
  init <- NULL
  if (tk_type(p) == "lparen") {
    advance(p)
    init <- list()
    while (tk_type(p) != "rparen") {
      if (tk_type(p) == "comma") {
        advance(p)
        next
      }
      reps <- 1L
      if (tk_type(p) == "num" && is_op(p, "*", 1L)) {
        if (!tk_int(p)) abort_syntax("Expecting iterator.", line)
        reps <- as.integer(tk_val(p))
        advance(p, 2L)
      }
      val <- parse_constant(p, "array initial value")
      init <- c(init, rep(list(val), reps))
    }
    advance(p)
  }
  expect_semi(p, "ARRAY")
  p$arrays <- c(p$arrays, name)
  list(stmt = "array", name = name, dim = dim, lo = lo, dims = dims, implicit = implicit, char = char, len = len,
       temporary = temporary, items = items, init = init, line = line)
}

parse_where <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (is_kw(p, c("also", "same"))) abort_unsupported("WHERE ALSO / WHERE SAME", line)
  cond <- parse_where_expr(p)
  expect_semi(p, "WHERE")
  list(stmt = "where", cond = cond, line = line)
}

# A WHERE expression: parsed with the WHERE rules; the parser state is restored
# also when the expression has an error (the statement-level error recovery
# goes on with the same parser).
parse_where_expr <- function(p) {
  old <- p$where_ctx
  p$where_ctx <- TRUE
  on.exit(p$where_ctx <- old, add = TRUE)
  parse_expr(p)
}

parse_put <- function(p) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  while (tk_type(p) != "semi") {
    t <- tk_type(p)
    if (t == "str") {
      items <- c(items, list(list(kind = "lit", text = tk_val(p))))
      advance(p)
      next
    }
    if (t == "name") {
      name <- tk_low(p)
      text <- tk_val(p)
      if (name %in% c("first", "last") && tk_type(p, 1L) == "dot" && tk_adjacent(p, 0L, 1L)) {
        name <- paste0(name, ".", tk_low(p, 2L))
        text <- name
        advance(p, 2L)
      }
      # PUT _ALL_; alone
      if (name == "_all_" && !length(items) && tk_type(p, 1L) == "semi") {
        advance(p, 2L)
        return(list(stmt = "put", items = list(list(kind = "all")), line = line))
      }
      if (name %in% c("_all_", "_infile_", "_page_", "_character_", "_numeric_", "_char_")) abort_unsupported(paste("PUT", toupper(name)), line)
      # an array element (a[2], a{i}) or all elements (a[*])
      if (name %in% p$arrays && tk_type(p, 1L) %in% c("lbrace", "lparen")) {
        if (is_op(p, "*", 2L)) {
          advance(p, 4L)
          if (is_op(p, "=") || is_format_ahead(p)) abort_unsupported(paste0("PUT ", toupper(name), "{*} with = or a format"), line)
          items <- c(items, list(list(kind = "array_all", name = name, text = text)))
          next
        }
        ref <- parse_array_ref(p)
        kind <- "value"
        if (is_op(p, "=")) {
          kind <- "named"
          advance(p)
        }
        if (is_format_ahead(p)) abort_unsupported(paste0("PUT of an array element with a format"), line)
        items <- c(items, list(list(kind = kind, name = name, text = text, ref = ref)))
        next
      }
      advance(p)
      kind <- "value"
      if (is_op(p, "=")) {
        kind <- "named"
        advance(p)
      }
      fmt_col <- tk_col(p)
      fmt_text <- if (is_format_ahead(p)) tk_val(p) else NULL
      fmt <- if (is_format_ahead(p)) parse_format(p) else NULL
      items <- c(items, list(list(kind = kind, name = name, text = text, fmt = fmt, fmt_col = fmt_col)))
      next
    }
    if (is_op(p, "/")) {
      advance(p)
      items <- c(items, list(list(kind = "ptr", k = "next")))
      next
    }
    if (is_op(p, "@")) {
      advance(p)
      if (tk_type(p) == "semi" || is_op(p, "@")) abort_unsupported("PUT line-hold specifiers (trailing @)", line)
      items <- c(items, list(list(kind = "ptr", k = "col", v = parse_ptr_operand(p, line))))
      next
    }
    if (is_op(p, "+")) {
      advance(p)
      items <- c(items, list(list(kind = "ptr", k = "rel", v = parse_ptr_operand(p, line))))
      next
    }
    if (t == "op" && tk_val(p) == "#") abort_unsupported("PUT line pointer #", line)
    abort_unsupported(paste("PUT item", tok_desc(p)), line)
  }
  advance(p)
  put_check_empty_lines(items, line)
  list(stmt = "put", items = items, line = line)
}

# The empty lines a PUT statement writes by its form (no items; a `/` at the
# start, at the end or after another `/`, or between pointers only) are not
# implemented yet: they fail loud.
put_check_empty_lines <- function(items, line) {
  # `PUT;` (when its empty line is written: not before the step's NOTEs) is
  # not implemented
  if (!length(items)) abort_unsupported("PUT with no items", line)
  seg <- cumsum(vapply(items, function(it) it$kind == "ptr" && it$k == "next", logical(1)))
  text <- vapply(items, function(it) it$kind != "ptr", logical(1))
  # segment k holds the items after the k-th `/`; segments 0..max(seg) are the lines
  filled <- vapply(0:max(seg), function(k) any(text[seg == k]), logical(1))
  if (!all(filled)) abort_unsupported("PUT line pointer / that writes an empty line", line)
}

parse_call_stmt <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name" || tk_type(p, 1L) != "lparen") abort_syntax("expected CALL routine(...)", line)
  routine <- tk_low(p)
  if (!routine %in% c("symput", "symputx", "execute", "missing", "sortn", "sortc", "cats", "catt", "catx", "cat")) {
    abort_unsupported(paste0("CALL ", toupper(routine)), line)
  }
  call <- parse_call(p)
  expect_semi(p, "CALL")
  nargs <- length(call$args)
  if (routine %in% c("cats", "catt", "catx", "cat")) {
    # CALL CATS(result, items...) is result = CATS(result, items...)
    k <- if (routine == "catx") 2L else 1L
    if (nargs < k || call$args[[k]]$node != "var") abort_syntax(paste0("CALL ", toupper(routine), " needs a result variable"), line)
    return(list(stmt = "assign", target = call$args[[k]], value = call, line = line))
  }
  if (routine == "execute") {
    if (nargs != 1) abort_syntax("wrong number of arguments to CALL EXECUTE", line)
    return(list(stmt = "call", routine = routine, args = call$args, line = line))
  }
  if (routine %in% c("missing", "sortn", "sortc")) {
    if (nargs < 1) abort_syntax(paste0("CALL ", toupper(routine), " requires arguments"), line)
    return(list(stmt = "call", routine = routine, args = call$args, line = line))
  }
  if (nargs < 2 || nargs > (if (routine == "symputx") 3 else 2)) {
    abort_syntax(paste0("wrong number of arguments to CALL ", toupper(routine)), line)
  }
  list(stmt = "call", routine = routine, args = call$args, line = line)
}

# INPUT ----------------------------------------------------------------------

# Pointer operand: a number, a variable or a parenthesised expression.
parse_ptr_operand <- function(p, line) {
  t <- tk_type(p)
  if (t == "num") {
    v <- as.numeric(tk_val(p))
    advance(p)
    return(list(node = "num", value = v, text = as.character(v), kind = "num", line = line))
  }
  if (t == "name") {
    e <- list(node = "var", name = tk_low(p), text = tk_val(p), line = line)
    advance(p)
    return(e)
  }
  if (t == "lparen") {
    advance(p)
    e <- parse_expr(p)
    expect_type(p, "rparen", "')'")
    return(e)
  }
  if (is_op(p, "-") && tk_type(p, 1L) == "num") {
    v <- -as.numeric(tk_val(p, 1L))
    advance(p, 2L)
    return(list(node = "num", value = v, text = as.character(v), kind = "num", line = line))
  }
  abort_syntax(paste("expected a pointer position but found", tok_desc(p)), line)
}

parse_input_modifiers <- function(p, item) {
  repeat {
    if (is_op(p, "?")) {
      if (is_op(p, "?", 1L) && tk_adjacent(p, 0L, 1L)) {
        item$qq <- "??"
        advance(p, 2L)
      } else {
        item$qq <- "?"
        advance(p)
      }
    } else if (is_op(p, "&")) {
      item$amp <- TRUE
      advance(p)
    } else if (is_op(p, "^")) {
      item$tilde <- TRUE
      advance(p)
    } else if (is_op(p, ":")) {
      item$colon <- TRUE
      advance(p)
    } else {
      break
    }
  }
  item
}

parse_input_var <- function(p, line) {
  item <- list(k = "var", name = tk_low(p), text = tk_val(p), char = FALSE, mode = "list", fmt = NULL,
               amp = FALSE, tilde = FALSE, qq = "", colon = FALSE)
  if (item$name %in% c("_infile_", "_all_")) abort_unsupported(paste("INPUT", toupper(item$name)), line)
  advance(p)
  if (is_op(p, "=")) abort_unsupported("named INPUT (var=)", line)
  item <- parse_input_modifiers(p, item)
  if (is_op(p, "$") && !is_format_ahead(p)) {
    item$char <- TRUE
    advance(p)
  }
  item <- parse_input_modifiers(p, item)
  if (is_format_ahead(p)) {
    item$fmt <- parse_format(p)
    item$mode <- if (item$colon || item$amp || item$tilde) "modified" else "formatted"
  } else if (tk_type(p) == "num") {
    item$mode <- "column"
    item$from <- as.integer(tk_val(p))
    item$to <- item$from
    advance(p)
    if (is_op(p, "-") && tk_type(p, 1L) == "num") {
      if (!tk_int(p, 1L)) abort_syntax("Invalid decimal specification. Expecting an integer constant.", line)
      item$to <- as.integer(tk_val(p, 1L))
      advance(p, 2L)
    }
    if (tk_type(p) == "num" && startsWith(tk_val(p), ".")) {
      item$cd <- as.integer(substring(tk_val(p), 2L))
      advance(p)
    }
    if (is.na(item$from) || is.na(item$to) || item$from < 1 || item$to < item$from) {
      abort_syntax("invalid column range in INPUT", line)
    }
  } else if (item$colon) {
    abort_syntax("expected an informat after ':'", line)
  }
  if (item$amp && item$mode == "list") item$mode <- "list"
  if (!is.null(item$fmt)) item$char <- item$char || item$fmt$char
  item
}

parse_input_group <- function(p, line) {
  advance(p)
  vars <- list()
  while (tk_type(p) == "name") vars <- c(vars, list(parse_varlist_item(p)))
  expect_type(p, "rparen", "')' after the INPUT variable list")
  if (tk_type(p) != "lparen") abort_syntax("expected an informat list after the INPUT variable list", line)
  advance(p)
  fmts <- list()
  while (tk_type(p) != "rparen") {
    if (is_op(p, "+")) {
      advance(p)
      fmts <- c(fmts, list(list(k = "rel", v = parse_ptr_operand(p, line))))
    } else if (is_op(p, "@")) {
      advance(p)
      fmts <- c(fmts, list(list(k = "col", v = parse_ptr_operand(p, line))))
    } else if (tk_type(p) == "comma") {
      advance(p)
    } else if (is_format_ahead(p) || is_op(p, "$")) {
      fmts <- c(fmts, list(list(k = "fmt", fmt = parse_format(p))))
    } else {
      abort_unsupported(paste("INPUT informat list item", tok_desc(p)), line)
    }
  }
  advance(p)
  # stored names keep the spelling of the program (a range takes it from its first name)
  texts <- character()
  names <- vctrs::list_unchop(lapply(vars, function(it) {
    nms <- switch(it$kind, name = it$name, range = expand_numbered_range(it$from, it$to, line),
                  abort_unsupported("this variable list form in an INPUT group", line))
    tx <- if (it$kind == "name") it$text %||% it$name else {
      base <- if (is.null(it$text)) NULL else split_trailing_digits(it$text)$base
      if (is.null(base)) nms else paste0(base, substring(nms, nchar(base) + 1L))
    }
    texts <<- c(texts, tx)
    nms
  }), ptype = character(), name_spec = rlang::zap())
  if (!any(vapply(fmts, function(f) f$k == "fmt", logical(1)))) abort_syntax("INPUT informat list without an informat", line)
  out <- list()
  j <- 0L
  for (q in seq_along(names)) {
    nm <- names[q]
    repeat {
      j <- j %% length(fmts) + 1L
      f <- fmts[[j]]
      if (f$k == "fmt") break
      out <- c(out, list(f))
    }
    out <- c(out, list(list(k = "var", name = nm, text = texts[q], char = f$fmt$char, mode = "formatted", fmt = f$fmt,
                            amp = FALSE, tilde = FALSE, qq = "", colon = FALSE)))
  }
  out
}

parse_input <- function(p) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  hold <- ""
  while (tk_type(p) != "semi") {
    if (tk_type(p) == "eof") abort_syntax("missing semicolon after INPUT", line)
    if (tk_type(p) == "name") {
      items <- c(items, list(parse_input_var(p, line)))
      next
    }
    if (tk_type(p) == "lparen") {
      items <- c(items, parse_input_group(p, line))
      next
    }
    if (is_op(p, "@")) {
      if (is_op(p, "@", 1L) && tk_adjacent(p, 0L, 1L)) {
        advance(p, 2L)
        if (tk_type(p) != "semi") abort_syntax("@@ must be the last item of an INPUT statement", line)
        hold <- "@@"
        next
      }
      advance(p)
      if (tk_type(p) == "semi") {
        hold <- "@"
        next
      }
      if (tk_type(p) %in% c("str", "str_macro")) {
        items <- c(items, list(list(k = "col_str", v = list(node = "str", value = tk_val(p), line = line))))
        advance(p)
        next
      }
      items <- c(items, list(list(k = "col", v = parse_ptr_operand(p, line))))
      next
    }
    if (is_op(p, "+")) {
      advance(p)
      items <- c(items, list(list(k = "rel", v = parse_ptr_operand(p, line))))
      next
    }
    if (is_op(p, "#")) {
      advance(p)
      if (tk_type(p) == "num" && !tk_int(p)) abort_syntax("expecting one of the following: a name, an integer constant, (.", line)
      items <- c(items, list(list(k = "line", v = parse_ptr_operand(p, line))))
      next
    }
    if (is_op(p, "/")) {
      advance(p)
      items <- c(items, list(list(k = "next")))
      next
    }
    abort_unsupported(paste("INPUT item", tok_desc(p)), line)
  }
  advance(p)
  list(stmt = "input", items = items, hold = hold, line = line)
}

infile_flags <- c(dsd = "dsd", missover = "mode", truncover = "mode", flowover = "mode", stopover = "mode",
                  pad = "pad", nopad = "pad")
infile_ignored <- c("lrecl", "encoding", "termstr", "recfm", "ignoredoseof", "linesize", "ls", "blksize")

parse_infile <- function(p) {
  line <- tk_line(p)
  advance(p)
  src <- NULL
  if (tk_type(p) %in% c("str", "str_macro")) {
    src <- list(kind = "file", path = tk_val(p))
    advance(p)
  } else if (tk_type(p) == "name") {
    nm <- tk_low(p)
    src <- if (nm %in% c("datalines", "cards", "datalines4", "cards4", "lines", "lines4")) {
      list(kind = "datalines")
    } else {
      list(kind = "fileref", ref = nm)
    }
    advance(p)
  } else {
    abort_syntax("expected a file specification after INFILE", line)
  }
  opts <- list()
  while (tk_type(p) != "semi") {
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in INFILE statement"), line)
    o <- tk_low(p)
    if (o %in% names(infile_flags) && !is_op(p, "=", 1L)) {
      kind <- infile_flags[[o]]
      opts[[kind]] <- if (kind == "mode") o else if (kind == "pad") o == "pad" else TRUE
      advance(p)
      next
    }
    if (!is_op(p, "=", 1L)) abort_unsupported(paste0("INFILE option ", toupper(o)), line)
    advance(p, 2L)
    t <- tk_type(p)
    v <- tk_val(p)
    if (o %in% c("dlm", "delimiter")) {
      if (t == "hexstr") v <- hex_to_str(v, line) else if (t != "str") abort_unsupported("INFILE DLM= given by a variable", line)
      opts$dlm <- v
    } else if (o %in% c("firstobs", "obs", "n")) {
      if (t != "num") abort_syntax(paste0("expected a number after ", toupper(o), "="), line)
      opts[[if (o == "n") "lines" else o]] <- as.integer(v)
    } else if (o == "end") {
      if (t != "name") abort_syntax("expected a variable name after END=", line)
      opts$end <- tolower(v)
    } else if (o %in% c("ls", "linesize")) {
      if (t != "num" || as.numeric(v) < 10 || as.numeric(v) > 32767) abort_syntax("10 <= LS <= 32767", line)
      opts$ls <- as.integer(v)
    } else if (o == "lrecl" && identical(src$kind, "datalines")) {
      abort_syntax("Invalid context (INFILE CARDS) for option name LRECL", line)
    } else if (o %in% infile_ignored) {
      NULL
    } else {
      abort_unsupported(paste0("INFILE option ", toupper(o), "="), line)
    }
    advance(p)
  }
  advance(p)
  list(stmt = "infile", source = src, opts = opts, line = line)
}

# FILE destination [MOD] [LRECL=n];  destination: 'path', fileref, LOG or PRINT.
parse_file <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) %in% c("str", "str_macro")) {
    dest <- list(kind = "path", path = tk_val(p))
  } else if (tk_type(p) == "name") {
    nm <- tk_low(p)
    dest <- if (nm %in% c("log", "print")) list(kind = nm) else list(kind = "fileref", ref = nm)
  } else {
    abort_syntax("expected a file specification after FILE", line)
  }
  advance(p)
  opts <- list(mod = FALSE, lrecl = 32767L)
  while (tk_type(p) != "semi") {
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in FILE statement"), line)
    o <- tk_low(p)
    if (o == "mod" && !is_op(p, "=", 1L)) {
      opts$mod <- TRUE
      advance(p)
      next
    }
    if (o == "lrecl" && is_op(p, "=", 1L) && tk_type(p, 2L) == "num") {
      if (!tk_int(p, 2L)) abort_syntax("Invalid value for the LRECL option.", line)
      opts$lrecl <- as.integer(tk_val(p, 2L))
      if (is.na(opts$lrecl) || opts$lrecl < 1L || opts$lrecl > 1073741823L) abort_syntax("invalid LRECL= value", line)
      advance(p, 3L)
      next
    }
    abort_unsupported(paste0("FILE option ", toupper(o)), line)
  }
  advance(p)
  list(stmt = "file", dest = dest, opts = opts, line = line)
}

hex_to_str <- function(h, line = NULL) {
  h <- gsub(" ", "", h, fixed = TRUE)
  if (nchar(h) %% 2 != 0 || grepl("[^0-9A-Fa-f]", h)) abort_syntax("invalid hexadecimal constant", line)
  bytes <- strtoi(substring(h, seq(1, nchar(h), 2), seq(2, nchar(h), 2)), 16L)
  rawToChar(as.raw(bytes))
}

parse_datalines <- function(p) {
  line <- tk_line(p)
  four <- endsWith(tk_low(p), "4")
  advance(p)
  expect_semi(p, "DATALINES")
  lines <- character()
  line_no <- integer()
  while (tk_type(p) == "datalines") {
    lines <- c(lines, tk_val(p))
    line_no <- c(line_no, tk_line(p))
    advance(p)
  }
  end_no <- tk_line(p)
  expect_type(p, "datalines_end", "end of DATALINES block")
  list(stmt = "datalines", lines = lines, line_no = line_no, end_no = end_no, four = four, line = line)
}

# A name right after a numeric constant that ends an assignment's value
# (`x = 1 y = 2;`, `x = 1 run;`): ERROR 22-322 with the operators that may
# follow the constant, the marker under the name.  The list holds `;` except
# before RUN (measured, both forms); other places fail with the plain message.
assign_missing_semi <- function(p) {
  ops <- c("!", "!!", "&", "*", "**", "+", "-", "/", ";", "<", "<=", "<>", "=", ">", "><", ">=", "AND", "EQ", "GE",
           "GT", "IN", "LE", "LT", "MAX", "MIN", "NE", "NG", "NL", "NOTIN", "OR", "^=", "|", "||", "~=")
  if (tk_low(p) == "run") ops <- setdiff(ops, ";")
  abort_syntax_numbered(paste0("Syntax error, expecting one of the following: ", paste(ops, collapse = ", "),
                               ". (ERROR 22-322)"), tk_line(p), tk_col(p), tk_val(p))
}

# RUN; where a DO or SELECT block (`what`) is still open: under `p$recover`
# the block ends there and is recorded for ERROR 117-185 (the marker goes
# under the semicolon of RUN); TRUE when so.
parse_unclosed_at_run <- function(p, what) {
  if (!isTRUE(p$recover) || !is_kw(p, "run") || tk_type(p, 1L) != "semi") return(FALSE)
  p$block_errors <- c(p$block_errors, list(list(kind = "unclosed", what = what, line = tk_line(p, 1L), col = tk_col(p, 1L))))
  TRUE
}
