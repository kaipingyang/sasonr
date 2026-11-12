# PROC SQL parser: recursive descent over the shared token stream, with its own
# Pratt expression parser (SQL adds qualified columns, CALCULATED, CASE,
# subqueries, EXISTS, quantified comparisons, COUNT(*) and DISTINCT
# aggregates). Expression nodes reuse the DATA step node types where the
# meaning is the same (num, str, missing, unary, binary, compare, in,
# between, contains, is_missing, like, call); SQL-only nodes are
#   col      table (alias or NULL), name, text
#   calc     CALCULATED name
#   case     operand (NULL for a searched CASE), whens [ {cond, value} ], else
#   subq     scalar subquery: query
#   exists   query
#   in_subq  lhs, query, negate
#   quant    op, quant (any/all), lhs, query
#   star     the argument of COUNT(*)
#
# Steps produced by sas_parse() for PROC SQL:
#   list(type = "sql_begin", options, line)  the PROC SQL statement
#   list(type = "sql", stmt, line)           one SQL statement
#   list(type = "sql_end", line)             QUIT

sql_proc_options <- c("noprint", "print", "number", "nonumber", "double", "nodouble", "flow", "noflow",
                      "feedback", "nofeedback", "stimer", "nostimer", "errorstop", "noerrorstop",
                      "warnrecurs", "nowarnrecurs", "threads", "nothreads", "exec", "remerge", "noremerge",
                      "sortmsg", "nosortmsg", "nowarn", "warn")

parse_sql_options <- function(p, line) {
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC SQL", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC SQL options"), line)
    opt <- tk_low(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (!opt %in% c("outobs", "inobs", "loops", "undo_policy")) abort_unsupported(paste0("PROC SQL option ", toupper(opt), "="), line)
      if (opt %in% c("outobs", "inobs", "loops")) {
        if (tk_type(p) != "num") abort_syntax(paste0("expected a number after ", toupper(opt), "="), line)
        opts[[opt]] <- as.numeric(tk_val(p))
      }
      advance(p)
      next
    }
    if (opt == "noexec") abort_unsupported("PROC SQL option NOEXEC", line)
    if (!opt %in% sql_proc_options) abort_unsupported(paste0("PROC SQL option ", toupper(opt)), line)
    opts[[opt]] <- TRUE
    advance(p)
  }
  advance(p)
  opts
}

# The PROC SQL statement and the statements that follow it.
parse_sql_block <- function(p) {
  line <- tk_line(p)
  advance(p, 2L)
  opts <- parse_sql_options(p, line)
  res <- parse_sql_stmts(p)
  res$steps <- c(list(list(type = "sql_begin", options = opts, line = line)), res$steps)
  res
}

# SQL statements up to QUIT, the next DATA/PROC statement or the end of text.
parse_sql_stmts <- function(p) {
  steps <- list()
  repeat {
    if (at_eof(p)) return(list(steps = steps, open = TRUE))
    if (tk_type(p) == "semi") {
      advance(p)
      next
    }
    line <- tk_line(p)
    if (is_kw(p, "quit") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      steps <- c(steps, list(list(type = "sql_end", line = line)))
      return(list(steps = steps, open = FALSE))
    }
    if (is_kw(p, "run") && tk_type(p, 1L) == "semi") {
      advance(p, 2L) # RUN has no effect in PROC SQL
      next
    }
    if (is_kw(p, c("data", "proc")) && !is_op(p, c("=", "+"), 1L)) return(list(steps = steps, open = FALSE))
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC SQL"), line)
    low <- tk_low(p)
    if (startsWith(low, "title") || low %in% c("footnote", "options", "option", "filename", "libname")) {
      steps <- c(steps, list(parse_global_stmt(p)))
      next
    }
    st <- parse_sql_statement(p)
    steps <- c(steps, list(list(type = "sql", stmt = st, line = line)))
  }
}

parse_sql_statement <- function(p) {
  line <- tk_line(p)
  low <- tk_low(p)
  st <- switch(low,
    select = list(kind = "select", query = parse_sql_query(p)),
    create = parse_sql_create(p),
    insert = parse_sql_insert(p),
    update = parse_sql_update(p),
    delete = parse_sql_delete(p),
    drop = parse_sql_drop(p),
    alter = parse_sql_alter(p),
    reset = {
      advance(p)
      list(kind = "reset", options = parse_sql_options(p, line))
    },
    abort_unsupported(paste0("PROC SQL statement ", toupper(tk_val(p))), line)
  )
  st$line <- line
  if (low != "reset") expect_semi(p, paste("PROC SQL", toupper(low), "statement"))
  st
}

# Table names ---------------------------------------------------------------

parse_sql_table_name <- function(p) {
  line <- tk_line(p)
  if (tk_type(p) != "name") abort_syntax(paste("expected a table name but found", tok_desc(p)), line)
  name <- tk_low(p)
  text <- tk_val(p)
  advance(p)
  if (tk_type(p) == "dot" && tk_type(p, 1L) == "name") {
    if (name == "dictionary" || name == "sashelp") abort_unsupported(paste0(toupper(name), " tables"), line)
    if (name != "work") abort_unsupported(paste0("library ", toupper(name), " (only WORK is available)"), line)
    advance(p)
    name <- tk_low(p)
    text <- tk_val(p)
    advance(p)
  }
  list(name = name, text = text, line = line)
}

# Queries -------------------------------------------------------------------

sql_reserved <- c("from", "where", "group", "having", "order", "into", "union", "except", "intersect", "outer",
                  "on", "join", "inner", "left", "right", "full", "cross", "natural", "as", "format", "label",
                  "length", "informat", "transcode", "and", "or", "not", "then", "else", "end", "when", "set",
                  "values", "select", "quit", "run", "desc", "asc", "by", "using", "corr", "all", "distinct")

parse_sql_query <- function(p) {
  q <- parse_sql_union(p)
  if (is_kw(p, "order") && is_kw(p, "by", 1L)) {
    advance(p, 2L)
    q$order <- parse_sql_order(p)
  }
  q
}

parse_sql_setop_tail <- function(p) {
  all <- FALSE
  corr <- FALSE
  repeat {
    if (is_kw(p, "all")) {
      all <- TRUE
      advance(p)
    } else if (is_kw(p, c("corr", "corresponding"))) {
      corr <- TRUE
      advance(p)
    } else {
      break
    }
  }
  list(all = all, corr = corr)
}

parse_sql_union <- function(p) {
  q <- parse_sql_intersect(p)
  repeat {
    line <- tk_line(p)
    if (is_kw(p, c("union", "except"))) {
      op <- tk_low(p)
      advance(p)
    } else if (is_kw(p, "outer") && is_kw(p, "union", 1L)) {
      op <- "outer union"
      advance(p, 2L)
    } else {
      break
    }
    t <- parse_sql_setop_tail(p)
    rhs <- parse_sql_intersect(p)
    q <- list(node = "setop", op = op, all = t$all, corr = t$corr, lhs = q, rhs = rhs, line = line)
  }
  q
}

parse_sql_intersect <- function(p) {
  q <- parse_sql_query_term(p)
  while (is_kw(p, "intersect")) {
    line <- tk_line(p)
    advance(p)
    t <- parse_sql_setop_tail(p)
    rhs <- parse_sql_query_term(p)
    q <- list(node = "setop", op = "intersect", all = t$all, corr = t$corr, lhs = q, rhs = rhs, line = line)
  }
  q
}

parse_sql_query_term <- function(p) {
  if (tk_type(p) == "lparen") {
    advance(p)
    q <- parse_sql_query(p)
    expect_type(p, "rparen", "')'")
    return(q)
  }
  parse_sql_select(p)
}

parse_sql_select <- function(p) {
  line <- tk_line(p)
  expect_kw(p, "select")
  distinct <- FALSE
  if (is_kw(p, c("distinct", "unique"))) {
    distinct <- TRUE
    advance(p)
  }
  items <- list()
  repeat {
    items <- c(items, list(parse_sql_select_item(p)))
    if (tk_type(p) != "comma") break
    advance(p)
  }
  q <- list(node = "select", distinct = distinct, items = items, into = NULL, from = list(), where = NULL,
            group = list(), having = NULL, order = list(), line = line)
  if (is_kw(p, "into")) {
    advance(p)
    q$into <- parse_sql_into(p)
  }
  if (is_kw(p, "from")) {
    advance(p)
    q$from <- parse_sql_from(p)
  } else if (!is.null(q$into) || !is_kw(p, c("where", "group", "having"))) {
    # SELECT without FROM: one row of constants
  }
  if (is_kw(p, "where")) {
    advance(p)
    q$where <- parse_sql_expr(p)
  }
  if (is_kw(p, "group") && is_kw(p, "by", 1L)) {
    advance(p, 2L)
    repeat {
      q$group <- c(q$group, list(parse_sql_expr(p)))
      if (tk_type(p) != "comma") break
      advance(p)
    }
  }
  if (is_kw(p, "having")) {
    advance(p)
    q$having <- parse_sql_expr(p)
  }
  q
}

parse_sql_col_attrs <- function(p, item, line) {
  repeat {
    if (is_kw(p, c("format", "informat")) && is_op(p, "=", 1L)) {
      k <- tk_low(p)
      advance(p, 2L)
      item[[k]] <- parse_format(p)
    } else if (is_kw(p, "label") && is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (tk_type(p) != "str") abort_syntax("expected a quoted label after LABEL=", line)
      item$label <- tk_val(p)
      advance(p)
    } else if (is_kw(p, "length") && is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (tk_type(p) != "num") abort_syntax("expected a number after LENGTH=", line)
      item$length <- as.integer(tk_val(p))
      advance(p)
    } else if (is_kw(p, "transcode") && is_op(p, "=", 1L)) {
      advance(p, 3L)
    } else if (tk_type(p) == "str") {
      item$label <- tk_val(p) # column 'label' shorthand
      advance(p)
    } else {
      break
    }
  }
  item
}

parse_sql_select_item <- function(p) {
  line <- tk_line(p)
  if (is_op(p, "*")) {
    advance(p)
    return(list(star = TRUE, table = NULL, line = line))
  }
  if (tk_type(p) == "name" && tk_type(p, 1L) == "dot" && is_op(p, "*", 2L)) {
    tab <- tk_low(p)
    advance(p, 3L)
    return(list(star = TRUE, table = tab, line = line))
  }
  e <- parse_sql_expr(p)
  item <- list(star = FALSE, expr = e, alias = NULL, line = line)
  if (is_kw(p, "as")) {
    advance(p)
    if (tk_type(p) != "name") abort_syntax(paste("expected a column alias after AS but found", tok_desc(p)), line)
    item$alias <- tk_low(p)
    item$alias_text <- tk_val(p)
    advance(p)
  } else if (tk_type(p) == "name" && !tk_low(p) %in% sql_reserved) {
    item$alias <- tk_low(p)
    item$alias_text <- tk_val(p)
    advance(p)
  }
  parse_sql_col_attrs(p, item, line)
}

parse_sql_into <- function(p) {
  line <- tk_line(p)
  targets <- list()
  sep <- NULL
  repeat {
    if (!is_op(p, ":")) abort_syntax(paste("expected ':macro-variable' in INTO but found", tok_desc(p)), line)
    advance(p)
    if (tk_type(p) != "name") abort_syntax("expected a macro variable name after ':'", line)
    nm <- tk_low(p)
    advance(p)
    tg <- list(name = nm, to = NULL, open = FALSE, trimmed = FALSE)
    if (is_op(p, "-")) {
      advance(p)
      if (is_op(p, ":")) {
        advance(p)
        tg$to <- tk_low(p)
        advance(p)
      } else {
        tg$open <- TRUE
      }
    } else if (is_kw(p, "thru", 0L) || is_kw(p, "through")) {
      advance(p)
      if (is_op(p, ":")) advance(p)
      tg$to <- tk_low(p)
      advance(p)
    }
    if (is_kw(p, "separated") && is_kw(p, "by", 1L)) {
      advance(p, 2L)
      if (tk_type(p) != "str") abort_syntax("expected a quoted separator after SEPARATED BY", line)
      tg$sep <- tk_val(p)
      advance(p)
    }
    while (is_kw(p, c("trimmed", "notrim"))) {
      tg[[tk_low(p)]] <- TRUE
      advance(p)
    }
    targets <- c(targets, list(tg))
    if (tk_type(p) != "comma") break
    advance(p)
  }
  targets
}

parse_sql_from <- function(p) {
  srcs <- list()
  repeat {
    srcs <- c(srcs, list(parse_sql_join(p)))
    if (tk_type(p) != "comma") break
    advance(p)
  }
  srcs
}

parse_sql_join <- function(p) {
  lhs <- parse_sql_source(p)
  repeat {
    line <- tk_line(p)
    type <- NULL
    if (is_kw(p, "join")) {
      type <- "inner"
      advance(p)
    } else if (is_kw(p, "inner") && is_kw(p, "join", 1L)) {
      type <- "inner"
      advance(p, 2L)
    } else if (is_kw(p, c("left", "right", "full"))) {
      type <- tk_low(p)
      advance(p)
      if (is_kw(p, "outer")) advance(p)
      expect_kw(p, "join")
    } else if (is_kw(p, "cross") && is_kw(p, "join", 1L)) {
      type <- "cross"
      advance(p, 2L)
    } else if (is_kw(p, "natural")) {
      abort_unsupported("NATURAL joins", line)
    } else {
      break
    }
    rhs <- parse_sql_source(p)
    on <- NULL
    if (type != "cross") {
      if (is_kw(p, "using")) abort_unsupported("JOIN ... USING", line)
      expect_kw(p, "on")
      on <- parse_sql_expr(p)
    }
    lhs <- list(kind = "join", type = type, lhs = lhs, rhs = rhs, on = on, line = line)
  }
  lhs
}

parse_sql_source <- function(p) {
  line <- tk_line(p)
  if (tk_type(p) == "lparen") {
    if (is_kw(p, "select", 1L) || tk_type(p, 1L) == "lparen") {
      advance(p)
      q <- parse_sql_query(p)
      expect_type(p, "rparen", "')' after an in-line view")
      src <- list(kind = "subquery", query = q, alias = NULL, line = line)
    } else {
      advance(p)
      src <- parse_sql_join(p)
      expect_type(p, "rparen", "')'")
      return(src)
    }
  } else {
    tn <- parse_sql_table_name(p)
    src <- list(kind = "table", name = tn$name, text = tn$text, alias = NULL, options = list(), line = line)
    if (tk_type(p) == "lparen") src$options <- parse_ds_options(p, "input")
  }
  if (is_kw(p, "as")) {
    advance(p)
    src$alias <- tk_low(p)
    advance(p)
  } else if (tk_type(p) == "name" && !tk_low(p) %in% sql_reserved) {
    src$alias <- tk_low(p)
    advance(p)
  }
  src
}

parse_sql_order <- function(p) {
  items <- list()
  repeat {
    e <- parse_sql_expr(p)
    desc <- FALSE
    if (is_kw(p, c("desc", "descending"))) {
      desc <- TRUE
      advance(p)
    } else if (is_kw(p, c("asc", "ascending"))) {
      advance(p)
    }
    items <- c(items, list(list(expr = e, desc = desc)))
    if (tk_type(p) != "comma") break
    advance(p)
  }
  items
}

# DDL / DML -------------------------------------------------------------------

parse_sql_create <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (is_kw(p, c("unique", "index"))) abort_unsupported("CREATE INDEX", line)
  what <- tk_low(p)
  if (!what %in% c("table", "view")) abort_syntax(paste("expected TABLE or VIEW after CREATE but found", tok_desc(p)), line)
  advance(p)
  tn <- parse_sql_table_name(p)
  if (tk_type(p) == "lparen" && !is_kw(p, "select", 1L)) {
    if (tk_type(p, 1L) == "name" && is_op(p, "=", 2L)) abort_unsupported("data set options on a CREATE TABLE target", line)
    if (what == "view") abort_syntax("column definitions are not allowed in CREATE VIEW", line)
    advance(p)
    cols <- list()
    repeat {
      if (is_kw(p, c("constraint", "primary", "foreign", "unique", "check"))) abort_unsupported("integrity constraints", line)
      cols <- c(cols, list(parse_sql_coldef(p)))
      if (tk_type(p) != "comma") break
      advance(p)
    }
    expect_type(p, "rparen", "')' after column definitions")
    return(list(kind = "create_cols", table = tn$name, text = tn$text, cols = cols))
  }
  if (is_kw(p, "like")) {
    advance(p)
    src <- parse_sql_table_name(p)
    return(list(kind = "create_like", table = tn$name, text = tn$text, source = src$name))
  }
  expect_kw(p, "as")
  q <- parse_sql_query(p)
  if (is_kw(p, "using")) abort_unsupported("CREATE VIEW ... USING", line)
  list(kind = if (what == "view") "create_view" else "create_table", table = tn$name, text = tn$text, query = q)
}

parse_sql_coldef <- function(p) {
  line <- tk_line(p)
  if (tk_type(p) != "name") abort_syntax(paste("expected a column name but found", tok_desc(p)), line)
  col <- list(name = tk_low(p), text = tk_val(p), line = line)
  advance(p)
  if (tk_type(p) != "name") abort_syntax(paste("expected a data type but found", tok_desc(p)), line)
  ty <- tk_low(p)
  advance(p)
  width <- NULL
  if (tk_type(p) == "lparen") {
    advance(p)
    width <- as.integer(tk_val(p))
    advance(p)
    if (tk_type(p) == "comma") advance(p, 2L)
    expect_type(p, "rparen", "')'")
  }
  if (ty %in% c("char", "character", "varchar")) {
    col$type <- "char"
    col$len <- width %||% 8L
  } else if (ty %in% c("num", "numeric", "decimal", "float", "real", "double", "int", "integer", "smallint", "date")) {
    col$type <- "num"
    col$len <- 8L
    if (ty == "date") col$format <- list(name = "date", w = 9L, d = NA_integer_, char = FALSE, text = "date9.")
  } else {
    abort_unsupported(paste0("column type ", toupper(ty)), line)
  }
  if (is_kw(p, "not") && is_kw(p, "null", 1L)) {
    col$not_null <- TRUE
    advance(p, 2L)
  }
  if (is_kw(p, c("primary", "unique", "check", "references"))) abort_unsupported("column constraints", line)
  col <- parse_sql_col_attrs(p, col, line)
  if (!is.null(col$length)) col$len <- col$length
  col
}

parse_sql_insert <- function(p) {
  line <- tk_line(p)
  advance(p)
  expect_kw(p, "into")
  tn <- parse_sql_table_name(p)
  cols <- NULL
  if (tk_type(p) == "lparen") {
    advance(p)
    cols <- character()
    repeat {
      cols <- c(cols, tk_low(p))
      advance(p)
      if (tk_type(p) != "comma") break
      advance(p)
    }
    expect_type(p, "rparen", "')'")
  }
  if (is_kw(p, "values")) {
    rows <- list()
    while (is_kw(p, "values")) {
      advance(p)
      expect_type(p, "lparen", "'(' after VALUES")
      vals <- list()
      repeat {
        vals <- c(vals, list(parse_sql_expr(p)))
        if (tk_type(p) != "comma") break
        advance(p)
      }
      expect_type(p, "rparen", "')'")
      rows <- c(rows, list(vals))
    }
    return(list(kind = "insert_values", table = tn$name, cols = cols, rows = rows))
  }
  if (is_kw(p, "set")) {
    rows <- list()
    while (is_kw(p, "set")) {
      advance(p)
      sets <- list()
      repeat {
        cn <- tk_low(p)
        advance(p)
        if (!is_op(p, "=")) abort_syntax("expected '=' in INSERT ... SET", line)
        advance(p)
        sets[[cn]] <- parse_sql_expr(p)
        if (tk_type(p) != "comma") break
        advance(p)
      }
      rows <- c(rows, list(sets))
    }
    return(list(kind = "insert_set", table = tn$name, rows = rows))
  }
  if (is_kw(p, "select") || tk_type(p) == "lparen") {
    return(list(kind = "insert_query", table = tn$name, cols = cols, query = parse_sql_query(p)))
  }
  abort_syntax(paste("expected VALUES, SET or a query in INSERT but found", tok_desc(p)), line)
}

parse_sql_update <- function(p) {
  line <- tk_line(p)
  advance(p)
  tn <- parse_sql_table_name(p)
  alias <- NULL
  if (is_kw(p, "as")) advance(p)
  if (tk_type(p) == "name" && !is_kw(p, "set")) {
    alias <- tk_low(p)
    advance(p)
  }
  expect_kw(p, "set")
  sets <- list()
  repeat {
    cn <- tk_low(p)
    advance(p)
    if (tk_type(p) == "dot") abort_unsupported("qualified column in UPDATE ... SET", line)
    if (!is_op(p, "=")) abort_syntax("expected '=' in UPDATE ... SET", line)
    advance(p)
    sets <- c(sets, list(list(name = cn, expr = parse_sql_expr(p))))
    if (tk_type(p) != "comma") break
    advance(p)
  }
  where <- NULL
  if (is_kw(p, "where")) {
    advance(p)
    where <- parse_sql_expr(p)
  }
  list(kind = "update", table = tn$name, alias = alias, sets = sets, where = where)
}

parse_sql_delete <- function(p) {
  line <- tk_line(p)
  advance(p)
  if (is_op(p, "*")) advance(p)
  expect_kw(p, "from")
  tn <- parse_sql_table_name(p)
  alias <- NULL
  if (is_kw(p, "as")) advance(p)
  if (tk_type(p) == "name" && !is_kw(p, "where")) {
    alias <- tk_low(p)
    advance(p)
  }
  where <- NULL
  if (is_kw(p, "where")) {
    advance(p)
    where <- parse_sql_expr(p)
  }
  list(kind = "delete", table = tn$name, alias = alias, where = where)
}

parse_sql_drop <- function(p) {
  line <- tk_line(p)
  advance(p)
  what <- tk_low(p)
  if (!what %in% c("table", "view")) abort_unsupported(paste0("DROP ", toupper(tk_val(p))), line)
  advance(p)
  names <- character()
  repeat {
    names <- c(names, parse_sql_table_name(p)$name)
    if (tk_type(p) != "comma") break
    advance(p)
  }
  list(kind = "drop", what = what, tables = names)
}

parse_sql_alter <- function(p) {
  line <- tk_line(p)
  advance(p)
  expect_kw(p, "table")
  tn <- parse_sql_table_name(p)
  actions <- list()
  repeat {
    if (is_kw(p, "add")) {
      advance(p)
      if (is_kw(p, c("constraint", "primary", "foreign", "unique", "check"))) abort_unsupported("integrity constraints", line)
      repeat {
        actions <- c(actions, list(list(op = "add", col = parse_sql_coldef(p))))
        if (tk_type(p) != "comma") break
        advance(p)
      }
    } else if (is_kw(p, "drop")) {
      advance(p)
      if (is_kw(p, c("constraint", "primary", "foreign"))) abort_unsupported("integrity constraints", line)
      repeat {
        actions <- c(actions, list(list(op = "drop", name = tk_low(p))))
        advance(p)
        if (tk_type(p) != "comma") break
        advance(p)
      }
    } else if (is_kw(p, "modify")) {
      advance(p)
      repeat {
        actions <- c(actions, list(list(op = "modify", col = parse_sql_modify_col(p))))
        if (tk_type(p) != "comma") break
        advance(p)
      }
    } else {
      break
    }
  }
  if (length(actions) == 0) abort_syntax("expected ADD, DROP or MODIFY in ALTER TABLE", line)
  list(kind = "alter", table = tn$name, actions = actions)
}

# MODIFY: a column name followed by an optional type and attributes.
parse_sql_modify_col <- function(p) {
  line <- tk_line(p)
  if (tk_type(p, 1L) == "name" && !tk_low(p, 1L) %in% c("format", "informat", "label", "length")) {
    return(parse_sql_coldef(p))
  }
  col <- list(name = tk_low(p), text = tk_val(p), line = line)
  advance(p)
  col <- parse_sql_col_attrs(p, col, line)
  if (!is.null(col$length)) col$len <- col$length
  col
}

# Expressions -----------------------------------------------------------------

parse_sql_expr <- function(p, min_bp = 1L) {
  lhs <- parse_sql_prefix(p)
  chain_rhs <- NULL
  repeat {
    line <- tk_line(p)
    col <- tk_col(p)
    # NOT IN / NOT BETWEEN / NOT LIKE / NOT CONTAINS
    if (is_kw(p, "not") && is_kw(p, c("in", "between", "like", "contains"), 1L) && min_bp <= 3L) {
      advance(p)
      lhs <- parse_sql_postfix(p, lhs, tk_low(p), TRUE, line)
      next
    }
    if (is_kw(p, c("in", "between", "like", "contains", "is")) && min_bp <= 3L) {
      lhs <- parse_sql_postfix(p, lhs, tk_low(p), FALSE, line)
      next
    }
    if (is_op(p, "?") && min_bp <= 3L) {
      advance(p)
      lhs <- list(node = "contains", lhs = lhs, rhs = parse_sql_expr(p, 4L), negate = FALSE, line = line)
      next
    }
    info <- sql_peek_infix(p)
    if (is.null(info) || info$bp < min_bp) break
    advance(p, info$ntok)
    if (info$kind == "cmp") {
      if (is_kw(p, c("any", "all", "some")) && tk_type(p, 1L) == "lparen") {
        quant <- if (tk_low(p) == "all") "all" else "any"
        advance(p, 2L)
        q <- parse_sql_query(p)
        expect_type(p, "rparen", "')'")
        lhs <- list(node = "quant", op = info$op, quant = quant, lhs = lhs, query = q, line = line)
        next
      }
      rhs <- parse_sql_expr(p, 4L)
      cmp <- list(node = "compare", op = sub(":", "", info$op, fixed = TRUE), colon = endsWith(info$op, ":"),
                  lhs = if (is.null(chain_rhs)) lhs else chain_rhs, rhs = rhs, line = line)
      # a <= b <= c is (a <= b) and (b <= c), as in the DATA step
      lhs <- if (is.null(chain_rhs)) cmp else list(node = "binary", op = "and", lhs = lhs, rhs = cmp, line = line, col = col)
      chain_rhs <- rhs
      next
    }
    chain_rhs <- NULL
    rhs <- parse_sql_expr(p, if (info$right) info$bp else info$bp + 1L)
    lhs <- list(node = "binary", op = info$op, lhs = lhs, rhs = rhs, line = line, col = col)
  }
  lhs
}

sql_peek_infix <- function(p) {
  t <- tk_type(p)
  if (t == "op") {
    v <- tk_val(p)
    if (v == "=" && is_op(p, "*", 1L) && tk_adjacent(p, 0L, 1L)) abort_unsupported("the sounds-like operator =*", tk_line(p))
    base <- sub(":", "", v, fixed = TRUE)
    if (base %in% cmp_ops) return(infix("cmp", v, 3L))
    return(switch(v,
      "|" = infix("binary", "or", 1L),
      "&" = infix("binary", "and", 2L),
      "||" = infix("binary", "||", 4L),
      "+" = infix("binary", "+", 5L),
      "-" = infix("binary", "-", 5L),
      "*" = infix("binary", "*", 6L),
      "/" = infix("binary", "/", 6L),
      "**" = infix("binary", "**", 7L, right = TRUE),
      "<>" = infix("cmp", "^=", 3L),
      "><" = abort_syntax("The >< (MIN) operator is not valid in PROC SQL.", tk_line(p)),
      NULL
    ))
  }
  if (t != "name") return(NULL)
  low <- tk_low(p)
  if (low %in% names(mnemonic_cmp)) return(infix("cmp", mnemonic_cmp[[low]], 3L))
  trunc_ops <- c(eqt = "=:", net = "^=:", ltt = "<:", let = "<=:", gtt = ">:", get = ">=:")
  if (low %in% names(trunc_ops)) return(infix("cmp", trunc_ops[[low]], 3L))
  switch(low,
    or = infix("binary", "or", 1L),
    and = infix("binary", "and", 2L),
    # the MIN / MAX word operators (a min b) are accepted by PROC SQL, not implemented
    min = ,
    max = abort_unsupported(paste0("the ", toupper(low), " operator in PROC SQL"), tk_line(p)),
    NULL
  )
}

parse_sql_postfix <- function(p, lhs, op, negate, line) {
  force(op)
  advance(p)
  switch(op,
    "in" = {
      expect_type(p, "lparen", "'(' after IN")
      if (is_kw(p, "select")) {
        q <- parse_sql_query(p)
        expect_type(p, "rparen", "')'")
        return(list(node = "in_subq", lhs = lhs, query = q, negate = negate, line = line))
      }
      items <- list()
      repeat {
        items <- c(items, list(parse_sql_expr(p, 4L)))
        if (tk_type(p) != "comma") break
        advance(p)
      }
      expect_type(p, "rparen", "')'")
      list(node = "in_list", lhs = lhs, items = items, negate = negate, line = line)
    },
    between = {
      lo <- parse_sql_expr(p, 4L)
      expect_kw(p, "and")
      hi <- parse_sql_expr(p, 4L)
      list(node = "between", lhs = lhs, lo = lo, hi = hi, negate = negate, line = line)
    },
    like = {
      rhs <- parse_sql_expr(p, 4L)
      esc <- NULL
      if (is_kw(p, "escape")) {
        advance(p)
        esc <- parse_sql_expr(p, 4L)
      }
      list(node = "like", lhs = lhs, rhs = rhs, escape = esc, negate = negate, line = line)
    },
    contains = list(node = "contains", lhs = lhs, rhs = parse_sql_expr(p, 4L), negate = negate, line = line),
    is = {
      neg <- FALSE
      if (is_kw(p, "not")) {
        neg <- TRUE
        advance(p)
      }
      if (!is_kw(p, c("missing", "null"))) abort_syntax("expected MISSING or NULL after IS", line)
      advance(p)
      list(node = "is_missing", lhs = lhs, negate = neg, line = line)
    }
  )
}

parse_sql_prefix <- function(p) {
  t <- tk_type(p)
  line <- tk_line(p)
  col <- tk_col(p)
  if (t == "lparen") {
    if (is_kw(p, "select", 1L)) {
      advance(p)
      q <- parse_sql_query(p)
      expect_type(p, "rparen", "')' after a subquery")
      return(list(node = "subq", query = q, line = line))
    }
    advance(p)
    e <- parse_sql_expr(p)
    expect_type(p, "rparen", "')'")
    e$paren <- TRUE
    return(e)
  }
  if (t == "op" && tk_val(p) %in% c("-", "+", "^")) {
    v <- tk_val(p)
    advance(p)
    arg <- parse_sql_expr(p, 7L)
    return(list(node = "unary", op = if (v == "^") "not" else v, arg = arg, line = line, col = col))
  }
  if (t == "missing" || (t == "dot" && !(tk_type(p, 1L) == "name" && tk_adjacent(p, 0L, 1L)))) {
    v <- tk_val(p)
    advance(p)
    return(list(node = "missing", code = if (t == "dot") "." else v, line = line))
  }
  if (t != "name") return(parse_prefix(p))
  low <- tk_low(p)
  if (low == "not") {
    advance(p)
    return(list(node = "unary", op = "not", arg = parse_sql_expr(p, 3L), line = line, col = col))
  }
  if (low == "exists" && tk_type(p, 1L) == "lparen") {
    advance(p, 2L)
    q <- parse_sql_query(p)
    expect_type(p, "rparen", "')'")
    return(list(node = "exists", query = q, line = line))
  }
  if (low == "calculated" && tk_type(p, 1L) == "name") {
    advance(p)
    nm <- tk_low(p)
    advance(p)
    return(list(node = "calc", name = nm, line = line))
  }
  if (low == "case") return(parse_sql_case(p))
  if (low == "null" && !tk_type(p, 1L) %in% c("lparen", "dot")) abort_unsupported("the NULL keyword as a value", line)
  if (tk_type(p, 1L) == "lparen") {
    if (low %in% c("put", "input")) {
      # the first argument is an SQL expression (columns, table.column)
      advance(p, 2L)
      return(parse_put_input_call(p, low, line, col, arg_parser = function(p) parse_sql_expr(p)))
    }
    return(parse_sql_call(p))
  }
  if (tk_type(p, 1L) == "dot" && tk_type(p, 2L) == "name" && tk_adjacent(p, 0L, 1L)) {
    tab <- low
    advance(p, 2L)
    nm <- tk_low(p)
    text <- tk_val(p)
    advance(p)
    return(list(node = "col", table = tab, name = nm, text = text, line = line, col = col))
  }
  advance(p)
  list(node = "col", table = NULL, name = low, text = tk_val(p, -1L), line = line, col = col)
}

parse_sql_call <- function(p) {
  name <- tk_low(p)
  line <- tk_line(p)
  col <- tk_col(p)
  advance(p, 2L)
  distinct <- FALSE
  args <- list()
  if (is_op(p, "*") && tk_type(p, 1L) == "rparen") {
    advance(p, 2L)
    return(list(node = "call", name = name, args = list(list(node = "star", line = line)), distinct = FALSE, line = line, col = col))
  }
  # UNIQUE is a synonym of DISTINCT in summary functions: count(unique x)
  if (is_kw(p, "distinct") || (is_kw(p, "unique") && tk_type(p, 1L) %in% c("name", "lparen"))) {
    distinct <- TRUE
    advance(p)
  }
  if (tk_type(p) == "rparen") {
    advance(p)
    return(list(node = "call", name = name, args = args, distinct = distinct, line = line, col = col))
  }
  repeat {
    if (is_kw(p, "of") && tk_type(p, 1L) == "name") abort_unsupported("OF lists in PROC SQL", line)
    if (tk_type(p) %in% c("comma", "rparen")) {
      args <- c(args, list(list(node = "omitted", line = line)))
    } else {
      args <- c(args, list(parse_sql_expr(p)))
    }
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    expect_type(p, "rparen", "')' or ','")
    break
  }
  list(node = "call", name = name, args = args, distinct = distinct, line = line, col = col)
}

parse_sql_case <- function(p) {
  line <- tk_line(p)
  advance(p)
  operand <- NULL
  if (!is_kw(p, "when")) operand <- parse_sql_expr(p)
  whens <- list()
  while (is_kw(p, "when")) {
    advance(p)
    cond <- parse_sql_expr(p)
    expect_kw(p, "then")
    whens <- c(whens, list(list(cond = cond, value = parse_sql_expr(p))))
  }
  if (length(whens) == 0) abort_syntax("CASE without WHEN", line)
  else_ <- NULL
  if (is_kw(p, "else")) {
    advance(p)
    else_ <- parse_sql_expr(p)
  }
  expect_kw(p, "end")
  list(node = "case", operand = operand, whens = whens, else_ = else_, line = line)
}
