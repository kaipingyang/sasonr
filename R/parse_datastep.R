# Statement-level recursive-descent parser producing a faithful AST.
#
# Nothing is desugared here: IF/ELSE, DO/END, SELECT/WHEN, IN, LENGTH and
# friends keep their source shape. Lowering happens in analyze/codegen.

#' Parse a SAS program
#'
#' @param code SAS source code, or a token tibble from [sas_lex()].
#' @param resolved `TRUE` when the text has already been through the macro
#'   processor: `&`/`%` left inside double-quoted strings are then plain text.
#' @return A `sasonr_program`: a list with element `steps`.
#' @export
sas_parse <- function(code, resolved = FALSE, sql_open = FALSE) {
  toks <- if (is.data.frame(code)) code else sas_lex(code)
  if (resolved) toks$type[toks$type == "str_macro"] <- "str"
  p <- new_parser(toks)
  steps <- list()
  # a chunk that continues an open PROC SQL (statements are executed one by one)
  if (sql_open) {
    res <- parse_sql_stmts(p)
    steps <- res$steps
    sql_open <- res$open
  }
  while (!at_eof(p)) {
    t <- tk_type(p)
    line <- tk_line(p)
    if (t == "semi") {
      advance(p)
      next
    }
    if (t %in% c("macro", "macro_var", "str_macro")) {
      abort_unsupported(paste0("macro code ", tk_val(p), " (run the program with sas_run() to expand macros)"), line)
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
    steps <- c(steps, list(parse_global_stmt(p)))
  }
  structure(list(steps = steps), class = "sasonr_program", sql_open = sql_open)
}

parse_data_step <- function(p) {
  line <- tk_line(p)
  advance(p)
  outputs <- list()
  while (tk_type(p) != "semi") {
    if (tk_type(p) != "name") abort_syntax(paste("expected a data set name but found", tok_desc(p)), tk_line(p))
    outputs <- c(outputs, list(parse_ds_ref(p, "output")))
  }
  advance(p)
  if (length(outputs) == 0) abort_unsupported("DATA statement without a data set name", line)
  is_null <- vapply(outputs, function(o) o$name == "_null_", logical(1))
  if (any(is_null) && length(outputs) > 1) abort_unsupported("_NULL_ combined with other outputs", line)

  p$arrays <- character()
  stmts <- list()
  repeat {
    if (at_eof(p)) break
    if (is_kw(p, "run") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    }
    # Implicit step boundary: the next DATA or PROC statement.
    if (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L)) break
    s <- parse_statement(p)
    if (s$stmt == "end") abort_syntax("END without a matching DO or SELECT", s$line)
    stmts <- c(stmts, list(s))
    if (s$stmt == "datalines") break
  }
  list(
    type = "data", line = line, null = any(is_null),
    outputs = if (any(is_null)) list() else outputs, stmts = stmts
  )
}

# Data set references and options ----------------------------------------

parse_ds_ref <- function(p, context) {
  line <- tk_line(p)
  name <- tk_low(p)
  advance(p)
  if (tk_type(p) == "dot" && tk_type(p, 1L) == "name" && tk_adjacent(p, -1L, 0L)) {
    lib <- name
    name <- tk_low(p, 1L)
    advance(p, 2L)
    if (lib != "work") abort_unsupported(paste0("library ", toupper(lib), " (only WORK is available)"), line)
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
      p$where_ctx <- TRUE
      opts$where <- parse_expr(p)
      p$where_ctx <- FALSE
      expect_type(p, "rparen", "')'")
    } else if (opt == "in" && context == "input") {
      opts$in_var <- tk_low(p)
      expect_type(p, "name", "a variable name after IN=")
    } else if (opt %in% c("firstobs", "obs") && context == "input") {
      opts[[opt]] <- as.numeric(tk_val(p))
      expect_type(p, "num", "a number")
    } else {
      abort_unsupported(paste0("data set option ", toupper(opt), "= on ", context, " data set"), line)
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

parse_statement <- function(p) {
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
  if (tk_type(p, 1L) == "lbrace" || (tk_type(p, 1L) == "lparen" && low %in% p$arrays)) {
    return(parse_assign(p))
  }
  if (is_op(p, "+", 1L) && !low %in% c("put", "input")) return(parse_sum(p))

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
    set = ,
    merge = parse_set(p, low),
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
    abort_unsupported(paste0("DATA step statement ", toupper(tk_val(p))), line)
  )
}

parse_assign <- function(p) {
  line <- tk_line(p)
  if (tk_type(p, 1L) %in% c("lbrace", "lparen")) {
    target <- parse_array_ref(p)
  } else {
    target <- list(node = "var", name = tk_low(p), text = tk_val(p), line = line)
    advance(p)
  }
  if (is_op(p, "+") && target$node == "array_ref") abort_unsupported("sum statement on an array element", line)
  if (!is_op(p, "=")) abort_syntax(paste("expected '=' but found", tok_desc(p)), line)
  advance(p)
  value <- parse_expr(p)
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

parse_branch <- function(p) {
  s <- parse_statement(p)
  if (s$stmt == "end") abort_syntax("END without a matching DO", s$line)
  if (s$stmt %in% c("datalines", "set", "merge", "by", "array", "length", "retain")) {
    if (s$stmt %in% c("set", "merge")) abort_unsupported("conditional SET/MERGE", s$line)
  }
  s
}

parse_block <- function(p, what) {
  body <- list()
  repeat {
    if (at_eof(p)) abort_syntax(paste("missing END for", what), tk_line(p))
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
  if (is_kw(p, "over")) abort_unsupported("DO OVER", line)
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
    if (tk_type(p) == "name" && is_op(p, "=", 1L)) {
      opt <- tk_low(p)
      oline <- tk_line(p)
      advance(p, 2L)
      if (!opt %in% c("end", "nobs") || (kind == "merge" && opt == "nobs")) {
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
        for (nm in expand_numbered_range(ref$name, to, line)) {
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
  list(stmt = kind, datasets = datasets, end = opts$end, nobs = opts$nobs, line = line)
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
      len <- as.integer(tk_val(p))
      if (is.na(len) || len < 3 || len > 8) abort_syntax("the length of numeric variables is 3-8", line)
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
        g$len <- list(type = if (char) "char" else "num", len = as.integer(sub(".", "", tk_val(p), fixed = TRUE)))
        if (char && (is.na(g$len$len) || g$len$len < 1 || g$len$len > 32767)) abort_syntax("The length of character variables is 1-32767", line)
        if (!char && (is.na(g$len$len) || g$len$len < 3 || g$len$len > 8)) abort_syntax("The length of numeric variables is 3-8", line)
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
  while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
    nm <- tk_low(p)
    advance(p, 2L)
    if (tk_type(p) != "str") abort_syntax("expected a quoted label", line)
    labels[nm] <- tk_val(p)
    advance(p)
  }
  expect_semi(p, "LABEL")
  list(stmt = "label", labels = labels, line = line)
}

parse_array <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name") abort_syntax("expected an array name", line)
  name <- tk_low(p)
  advance(p)
  if (!tk_type(p) %in% c("lbrace", "lparen")) abort_syntax("expected an array dimension", line)
  close <- if (tk_type(p) == "lparen") "rparen" else "rbrace"
  advance(p)
  dim <- NA_integer_
  lo <- 1L
  if (is_op(p, "*")) {
    advance(p)
  } else {
    n1 <- as.integer(tk_val(p))
    expect_type(p, "num", "an array dimension")
    if (is_op(p, ":")) {
      advance(p)
      n2 <- as.integer(tk_val(p))
      expect_type(p, "num", "an array upper bound")
      lo <- n1
      dim <- n2 - n1 + 1L
    } else {
      dim <- n1
    }
  }
  if (tk_type(p) == "comma") abort_unsupported("multi-dimensional arrays", line)
  expect_type(p, close, "closing bracket")
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
  list(stmt = "array", name = name, dim = dim, lo = lo, char = char, len = len,
       temporary = temporary, items = items, init = init, line = line)
}

parse_where <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (is_kw(p, c("also", "same"))) abort_unsupported("WHERE ALSO / WHERE SAME", line)
  p$where_ctx <- TRUE
  cond <- parse_expr(p)
  p$where_ctx <- FALSE
  expect_semi(p, "WHERE")
  list(stmt = "where", cond = cond, line = line)
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
      if (name %in% c("_all_", "_infile_", "_page_")) abort_unsupported(paste("PUT", toupper(name)), line)
      advance(p)
      kind <- "value"
      if (is_op(p, "=")) {
        kind <- "named"
        advance(p)
      }
      fmt <- if (is_format_ahead(p)) parse_format(p) else NULL
      items <- c(items, list(list(kind = kind, name = name, text = text, fmt = fmt)))
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
  list(stmt = "put", items = items, line = line)
}

parse_call_stmt <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (tk_type(p) != "name" || tk_type(p, 1L) != "lparen") abort_syntax("expected CALL routine(...)", line)
  routine <- tk_low(p)
  if (!routine %in% c("symput", "symputx", "missing", "sortn", "sortc", "cats", "catt", "catx", "cat")) {
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
  names <- unlist(lapply(vars, function(it) {
    switch(it$kind, name = it$name, range = expand_numbered_range(it$from, it$to, line),
           abort_unsupported("this variable list form in an INPUT group", line))
  }))
  if (!any(vapply(fmts, function(f) f$k == "fmt", logical(1)))) abort_syntax("INPUT informat list without an informat", line)
  out <- list()
  j <- 0L
  for (nm in names) {
    repeat {
      j <- j %% length(fmts) + 1L
      f <- fmts[[j]]
      if (f$k == "fmt") break
      out <- c(out, list(f))
    }
    out <- c(out, list(list(k = "var", name = nm, text = nm, char = f$fmt$char, mode = "formatted", fmt = f$fmt,
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
