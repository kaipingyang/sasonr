# Pratt (precedence-climbing) expression parser for SAS expressions of the
# DATA step and of WHERE.  Binding powers, associativity, comparison chains and
# the rules of conditions and prefix operators come from the operator table of
# operators.R (shared with the PROC SQL parser, sql_parse.R).

# Parser state ------------------------------------------------------------

new_parser <- function(tokens) {
  p <- new.env(parent = emptyenv())
  p$type <- tokens$type
  p$value <- tokens$value
  p$low <- tolower(tokens$value)
  p$line <- tokens$line
  p$col <- tokens$col
  p$n <- nrow(tokens)
  p$pos <- 1L
  p$arrays <- character()
  p$where_ctx <- FALSE
  # variables named so far (recorded only during the statement-level error
  # recovery of errors.R: the variables compiled when a statement fails)
  p$seen <- NULL
  p
}

parser_saw <- function(p, name) {
  if (!is.null(p$seen)) p$seen <- c(p$seen, name)
  invisible()
}

tk_type <- function(p, k = 0L) {
  j <- p$pos + k
  if (j > p$n) "eof" else p$type[j]
}
tk_val <- function(p, k = 0L) {
  j <- p$pos + k
  if (j > p$n) "" else p$value[j]
}
tk_low <- function(p, k = 0L) {
  j <- p$pos + k
  if (j > p$n) "" else p$low[j]
}
tk_line <- function(p, k = 0L) {
  if (p$n == 0) return(NA_integer_)
  p$line[min(p$pos + k, p$n)]
}
# An integer constant: a number token with no decimal point or exponent
# (where the grammar takes an integer constant, `2.5` is a syntax error, not 2).
tk_int <- function(p, k = 0L) tk_type(p, k) == "num" && grepl("^[0-9]+$", tk_val(p, k))
is_kw <- function(p, kw, k = 0L) tk_type(p, k) == "name" && tk_low(p, k) %in% kw
is_op <- function(p, op, k = 0L) tk_type(p, k) == "op" && tk_val(p, k) %in% op
advance <- function(p, k = 1L) {
  p$pos <- p$pos + k
  invisible(p)
}
at_eof <- function(p) p$pos > p$n

tok_desc <- function(p, k = 0L) {
  if (tk_type(p, k) == "eof") "end of program" else paste0("'", tk_val(p, k), "'")
}

expect_type <- function(p, type, what = type) {
  if (tk_type(p) != type) {
    abort_syntax(paste0("expected ", what, " but found ", tok_desc(p)), tk_line(p))
  }
  advance(p)
}
expect_semi <- function(p, what = "statement") {
  if (tk_type(p) != "semi") {
    abort_syntax(paste0("expected ';' to end ", what, " but found ", tok_desc(p)), tk_line(p))
  }
  advance(p)
}
expect_kw <- function(p, kw) {
  if (!is_kw(p, kw)) {
    abort_syntax(paste0("expected ", toupper(kw), " but found ", tok_desc(p)), tk_line(p))
  }
  advance(p)
}

# Two tokens touch (no whitespace between them).
tk_adjacent <- function(p, k1 = 0L, k2 = 1L) {
  a <- p$pos + k1
  b <- p$pos + k2
  if (b > p$n) return(FALSE)
  p$line[a] == p$line[b] && p$col[b] == p$col[a] + nchar(p$value[a])
}

# The text of tokens `from`..`to`, one blank between tokens that do not touch,
# strings in single quotes; NULL when a token has another form (date, hex and
# name literals, macro-quoted strings).  Read back by the lexer it gives the
# same tokens.
tok_render <- function(p, from, to) {
  k <- seq.int(from, to)
  if (!all(p$type[k] %in% c("name", "num", "op", "lparen", "rparen", "dot", "comma", "str"))) return(NULL)
  txt <- ifelse(p$type[k] == "str", paste0("'", gsub("'", "''", p$value[k], fixed = TRUE), "'"), p$value[k])
  touch <- c(FALSE, p$line[k[-1]] == p$line[k[-length(k)]] & p$col[k[-1]] == p$col[k[-length(k)]] + nchar(p$value[k[-length(k)]]) &
               p$type[k[-length(k)]] != "str")
  paste0(ifelse(touch, "", " "), txt, collapse = "") |> sub(pattern = "^ ", replacement = "")
}

# Public entry ------------------------------------------------------------

#' Parse a single SAS expression
#'
#' @param text SAS expression source.
#' @param where Parse in WHERE context (enables BETWEEN, CONTAINS, IS MISSING).
#' @return Expression AST (nested lists with a `node` field).
#' @examples
#' e <- sas_parse_expr("round(x * 2, 0.1) > 3")
#' e$node
#' sas_parse_expr("age between 18 and 65", where = TRUE)$node
#' @export
sas_parse_expr <- function(text, where = FALSE) {
  p <- new_parser(sas_lex(text))
  p$where_ctx <- where
  e <- parse_expr(p)
  if (!at_eof(p)) abort_syntax(paste("unexpected", tok_desc(p)), tk_line(p))
  e
}

# Pratt loop ---------------------------------------------------------------

cmp_ops <- c("=", "^=", "<", "<=", ">", ">=")
mnemonic_cmp <- c(eq = "=", ne = "^=", lt = "<", le = "<=", gt = ">", ge = ">=")

# The context of the operator table for a parser state.
expr_ctx <- function(p) if (isTRUE(p$where_ctx)) "where" else "data"

peek_infix <- function(p) {
  ctx <- expr_ctx(p)
  t <- tk_type(p)
  if (t == "op") {
    v <- tk_val(p)
    base <- sub(":", "", v, fixed = TRUE)
    if (base %in% cmp_ops) return(sas_infix("cmp", v, "cmp", ctx))
    return(switch(v,
      "|" = sas_infix("binary", "or", "or", ctx),
      "&" = sas_infix("binary", "and", "and", ctx),
      "||" = ,
      "+" = ,
      "-" = ,
      "*" = ,
      "/" = ,
      "**" = sas_infix("binary", v, v, ctx),
      "><" = if (p$where_ctx) {
        abort_unsupported("the >< operator in a WHERE expression", tk_line(p))
      } else sas_infix("binary", "min", "min", ctx),
      "<>" = if (p$where_ctx) sas_infix("cmp", "^=", "cmp", ctx) else sas_infix("binary", "max", "max", ctx),
      "?" = if (p$where_ctx) sas_infix("contains", "contains", "contains", ctx) else NULL, # WHERE: ? is CONTAINS
      NULL
    ))
  }
  if (t != "name") return(NULL)
  low <- tk_low(p)
  if (low %in% names(mnemonic_cmp)) {
    # GE: / EQ: ... the colon modifier on a mnemonic operator (prefix compare)
    if (tk_type(p, 1L) == "op" && identical(tk_val(p, 1L), ":")) {
      return(sas_infix("cmp", paste0(mnemonic_cmp[[low]], ":"), "cmp", ctx, ntok = 2L))
    }
    return(sas_infix("cmp", mnemonic_cmp[[low]], "cmp", ctx))
  }
  switch(low,
    or = sas_infix("binary", "or", "or", ctx),
    and = sas_infix("binary", "and", "and", ctx),
    min = sas_infix("binary", "min", "min", ctx),
    max = sas_infix("binary", "max", "max", ctx),
    "in" = sas_infix("in", "in", "in", ctx),
    not = {
      nxt <- tk_low(p, 1L)
      if (tk_type(p, 1L) != "name") return(NULL)
      if (nxt == "in") return(sas_infix("in", "in", "in", ctx, ntok = 2L, negate = TRUE))
      if (p$where_ctx && nxt %in% c("between", "contains", "like")) {
        return(sas_infix(nxt, nxt, nxt, ctx, ntok = 2L, negate = TRUE))
      }
      NULL
    },
    between = ,
    contains = ,
    like = if (p$where_ctx) sas_infix(low, low, low, ctx) else NULL,
    is = if (p$where_ctx) sas_infix("is", "is", "is", ctx) else NULL,
    NULL
  )
}

# The source span of an expression node (spec 09 contract 3): the first and
# last token of the node in the parser's token table, with their positions.
# Every node that parse_expr() returns carries one, with the class
# c("sas_<node>", "sas_node").
node_span <- function(p, node, start) {
  if (!is.list(node) || !is.character(node$node)) return(node)
  last <- max(start, p$pos - 1L)
  node$span <- c(tok = start, tok_end = last, line = p$line[start], col = p$col[start],
                 end_line = p$line[last], end_col = p$col[last])
  class(node) <- c(paste0("sas_", node$node), "sas_node")
  node
}

parse_expr <- function(p, min_bp = 1L) {
  start <- p$pos
  lhs <- node_span(p, parse_prefix(p, min_bp), start)
  last_cmp <- FALSE
  st <- list(cond_bp = NA_integer_, used = "")
  repeat {
    info <- peek_infix(p)
    if (is.null(info) || info$bp < min_bp) break
    sas_op_check(st, info, p)
    line <- tk_line(p)
    col <- tk_col(p)
    advance(p, info$ntok)

    if (info$kind == "cmp") {
      rhs <- parse_expr(p, info$bp + 1L)
      lhs <- node_span(p, sas_op_compare(lhs, sub(":", "", info$op, fixed = TRUE), endsWith(info$op, ":"), rhs, line, last_cmp,
                                         expr_ctx(p)), start)
      last_cmp <- TRUE
      st <- sas_op_after(info)
      next
    }
    last_cmp <- FALSE

    lhs <- switch(info$kind,
      binary = {
        p$operand_of <- info$op
        rhs <- parse_expr(p, if (info$right) info$bp else info$bp + 1L)
        list(node = "binary", op = info$op, lhs = lhs, rhs = rhs, line = line, col = col)
      },
      "in" = parse_in_list(p, lhs, info$negate, line),
      between = {
        lo <- parse_expr(p, info$bp + 1L)
        expect_kw(p, "and")
        hi <- parse_expr(p, info$bp + 1L)
        list(node = "between", lhs = lhs, lo = lo, hi = hi, negate = info$negate, line = line)
      },
      contains = list(node = "contains", lhs = lhs, rhs = parse_expr(p, info$bp + 1L), negate = info$negate, line = line),
      like = list(node = "like", lhs = lhs, rhs = parse_expr(p, info$bp + 1L), negate = info$negate, line = line),
      is = {
        negate <- FALSE
        if (is_kw(p, "not")) {
          negate <- TRUE
          advance(p)
        }
        if (!is_kw(p, c("missing", "null"))) abort_syntax("expected MISSING or NULL after IS", line)
        advance(p)
        list(node = "is_missing", lhs = lhs, negate = negate, line = line)
      }
    )
    lhs <- node_span(p, lhs, start)
    st <- sas_op_after(info)
  }
  lhs
}

parse_prefix <- function(p, min_bp = 1L) {
  # the prefix sign right before this operand (two signs in a row) and the
  # operator whose operand this is (set by the caller)
  sign <- p$prefix_sign
  p$prefix_sign <- NULL
  operand_of <- p$operand_of
  p$operand_of <- NULL
  # the first operand of an assignment's value (parse_assign())
  value_start <- isTRUE(p$assign_value)
  p$assign_value <- NULL
  t <- tk_type(p)
  v <- tk_val(p)
  line <- tk_line(p)
  col <- tk_col(p)
  switch(t,
    num = {
      advance(p)
      return(list(node = "num", value = sas_num_literal(v, line), text = v, kind = "num", line = line, col = col))
    },
    str = {
      advance(p)
      return(list(node = "str", value = v, line = line, col = col))
    },
    date = ,
    time = ,
    datetime = {
      val <- sas_parse_dt_literal(v, t)
      if (is.na(val)) abort_syntax(paste0("invalid ", t, " constant '", v, "'"), line)
      advance(p)
      return(list(node = "num", value = val, text = v, kind = t, line = line, col = col))
    },
    missing = {
      advance(p)
      return(list(node = "missing", code = v, line = line, col = col))
    },
    str_macro = ,
    macro = ,
    macro_var = abort_unsupported(paste0("macro reference ", v, " (macro language not implemented)"), line),
    namelit = abort_unsupported("name literals ('...'n)", line),
    hexstr = {
      advance(p)
      return(list(node = "str", value = hex_to_str(v, line), line = line))
    },
    bitstr = abort_unsupported("bit-mask constants", line),
    lparen = {
      advance(p)
      p$paren_depth <- (p$paren_depth %||% 0L) + 1L
      on.exit(p$paren_depth <- p$paren_depth - 1L, add = TRUE)
      e <- parse_expr(p)
      expect_type(p, "rparen", "')'")
      return(e)
    },
    op = {
      if (v %in% c("-", "+", "^")) {
        name <- switch(v, "-" = "neg", "+" = "pos", "^" = "not")
        o <- sas_prefix_op(name, expr_ctx(p), min_bp, p, sign, operand_of)
        advance(p)
        if (name != "not") p$prefix_sign <- name
        on.exit(p$prefix_sign <- NULL, add = TRUE)
        p$operand_of <- name
        arg <- parse_expr(p, o$bp)
        return(list(node = "unary", op = if (v == "^") "not" else v, arg = arg, line = line))
      }
    },
    name = return(parse_name_primary(p, min_bp, operand_of))
  )
  if (t == "semi") expr_missing_operand(p, value_start, operand_of, line, col)
  abort_syntax(paste("unexpected", tok_desc(p), "in expression"), line)
}

# A semicolon where an operand is expected: the numbered ERRORs of the two
# places whose expecting lists are known -- the first operand of an
# assignment's value (`w = ;`) and the right operand of a binary `+` inside
# parentheses (`x = (1 + ;`, followed by 76-322).  Elsewhere: returns, and the
# caller raises the plain message.
expr_missing_operand <- function(p, value_start, operand_of, line, col) {
  head <- "Syntax error, expecting one of the following: a name, a quoted string, a numeric constant, a datetime constant, a missing value,"
  if (value_start) {
    abort_syntax_numbered(paste(head, "INPUT, PUT. (ERROR 22-322)"), line, col, ";")
  }
  if (identical(operand_of, "+") && (p$paren_depth %||% 0L) >= 1L) {
    abort_syntax_numbered(paste(head, "arrayname, (, +, -, INPUT, NOT, PUT, ^, _NEW_, ~. (ERROR 22-322)"), line, col, ";",
                          also = "ERROR 76-322: Syntax error, statement will be ignored.")
  }
  invisible()
}

parse_name_primary <- function(p, min_bp = 1L, operand_of = NULL) {
  low <- tk_low(p)
  line <- tk_line(p)
  col <- tk_col(p)
  if (low == "not" && !tk_type(p, 1L) %in% c("semi", "rparen", "comma", "eof")) {
    o <- sas_prefix_op("not", expr_ctx(p), min_bp, p, operand_of = operand_of)
    advance(p)
    p$operand_of <- "not"
    return(list(node = "unary", op = "not", arg = parse_expr(p, o$bp), line = line))
  }
  # first.x / last.x
  if (low %in% c("first", "last") && tk_type(p, 1L) == "dot" && tk_type(p, 2L) == "name" &&
      tk_adjacent(p, 0L, 1L) && tk_adjacent(p, 1L, 2L)) {
    name <- paste0(low, ".", tk_low(p, 2L))
    advance(p, 3L)
    parser_saw(p, name)
    return(list(node = "var", name = name, text = name, line = line))
  }
  if (tk_type(p, 1L) == "dot" && tk_adjacent(p, 0L, 1L)) return(parse_obj_ref(p))
  if (low == "_new_") abort_unsupported("the _NEW_ operator (component objects are created by DECLARE)", line)
  if (tk_type(p, 1L) == "lbrace" || (tk_type(p, 1L) == "lparen" && low %in% p$arrays)) {
    return(parse_array_ref(p))
  }
  if (tk_type(p, 1L) == "lparen") return(parse_call(p))
  advance(p)
  parser_saw(p, low)
  list(node = "var", name = low, text = tk_val(p, -1L), line = line, col = col)
}

# A component object reference (DATA step hash and hash iterator objects):
# `obj.method(<arg>, ...)` (node "method") or `obj.attribute` (node
# "obj_attr", NUM_ITEMS ...). An argument is an expression, also with its tag
# (`key: k`, `dataset: 'ds'`).
parse_obj_ref <- function(p) {
  line <- tk_line(p)
  col <- tk_col(p)
  obj <- tk_low(p)
  if (tk_type(p, 2L) != "name" || !tk_adjacent(p, 1L, 2L)) abort_syntax(paste0("expecting a method or attribute name after ", toupper(obj), "."), line)
  m <- tk_low(p, 2L)
  advance(p, 3L)
  if (tk_type(p) != "lparen") return(list(node = "obj_attr", obj = obj, attr = m, line = line, col = col))
  list(node = "method", obj = obj, method = m, args = parse_obj_args(p), line = line, col = col)
}

# The arguments of a method call or of DECLARE, from `(` to `)`: each a list
# of `tag` (NULL without one) and `value`.
parse_obj_args <- function(p) {
  expect_type(p, "lparen", "(")
  args <- list()
  if (tk_type(p) == "rparen") {
    advance(p)
    return(args)
  }
  repeat {
    tag <- NULL
    if (tk_type(p) == "name" && is_op(p, ":", 1L)) {
      tag <- tk_low(p)
      advance(p, 2L)
    }
    args[[length(args) + 1L]] <- list(tag = tag, value = parse_expr(p))
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    break
  }
  expect_type(p, "rparen", ")")
  args
}

parse_array_ref <- function(p) {
  name <- tk_low(p)
  line <- tk_line(p)
  close <- if (tk_type(p, 1L) == "lparen") "rparen" else "rbrace"
  advance(p, 2L)
  if (is_op(p, "*")) abort_unsupported(paste0("array reference ", name, "{*} outside an OF list"), line)
  idx <- list(parse_expr(p))
  while (tk_type(p) == "comma") {
    advance(p)
    idx <- c(idx, list(parse_expr(p)))
  }
  expect_type(p, close, "closing bracket")
  list(node = "array_ref", name = name, index = idx, line = line)
}

tk_col <- function(p, k = 0L) {
  if (p$n == 0) return(NA_integer_)
  p$col[min(p$pos + k, p$n)]
}

# PUT(x, format [-l|-r|-c]) and INPUT(s, [?|??] informat): the second argument
# is a format, not an expression.
parse_put_input_call <- function(p, name, line, col, arg_parser = parse_expr) {
  arg <- arg_parser(p)
  # PUT(x) / INPUT(s): the parser expects an operator at `)` and ignores the
  # statement [measured]
  if (tk_type(p) == "rparen") {
    abort_syntax_numbered("Expecting an arithmetic operator. (ERROR 388-185)", line, tk_col(p), ")",
                          also = "ERROR 76-322: Syntax error, statement will be ignored.")
  }
  if (tk_type(p) != "comma") abort_syntax(paste0("expected ',' and a format in ", toupper(name), "()"), line)
  advance(p)
  qq <- ""
  if (name == "input" && is_op(p, "?")) {
    qq <- if (is_op(p, "?", 1L) && tk_adjacent(p, 0L, 1L)) "??" else "?"
    advance(p, nchar(qq))
  }
  if (!is_format_ahead(p) && !(is_op(p, "$") && tk_type(p, 1L) %in% c("num", "dot"))) {
    abort_syntax(paste0("expected a format in ", toupper(name), "()"), line)
  }
  fmt <- parse_format(p)
  align <- NULL
  if (name == "put" && is_op(p, "-") && tk_type(p, 1L) == "name" && tk_low(p, 1L) %in% c("l", "r", "c")) {
    align <- tk_low(p, 1L)
    advance(p, 2L)
  }
  expect_type(p, "rparen", paste0("')' to close ", toupper(name), "()"))
  list(node = "call", name = name, args = list(arg), fmt = fmt, qq = qq, align = align, line = line, col = col)
}

parse_call <- function(p) {
  name <- tk_low(p)
  line <- tk_line(p)
  col <- tk_col(p)
  advance(p, 2L)
  if (name %in% c("put", "input")) return(parse_put_input_call(p, name, line, col))
  args <- list()
  if (tk_type(p) == "rparen") {
    advance(p)
    return(list(node = "call", name = name, args = args, line = line, col = col))
  }
  repeat {
    if (is_kw(p, "of") && tk_type(p, 1L) == "name") {
      args <- c(args, list(parse_of_list(p)))
    } else {
      if (tk_type(p) %in% c("comma", "rparen")) {
        # an omitted (empty) argument, as in compress(s, , 'd')
        args <- c(args, list(list(node = "omitted", line = line)))
      } else {
        args <- c(args, list(parse_expr(p)))
      }
    }
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    expect_type(p, "rparen", "')' or ','")
    break
  }
  list(node = "call", name = name, args = args, line = line, col = col)
}

# Variable-list items shared by OF lists and KEEP/DROP/RETAIN statements.
parse_varlist_item <- function(p) {
  nm <- tk_low(p)
  if (tk_type(p, 1L) == "lbrace" && is_op(p, "*", 2L) && tk_type(p, 3L) == "rbrace") {
    advance(p, 4L)
    return(list(kind = "array", name = nm))
  }
  if (is_op(p, "-", 1L) && is_op(p, "-", 2L) && tk_type(p, 3L) == "name") {
    to <- tk_low(p, 3L)
    advance(p, 4L)
    return(list(kind = "pos_range", from = nm, to = to))
  }
  if (is_op(p, "-", 1L) && tk_type(p, 2L) == "name") {
    to <- tk_low(p, 2L)
    text <- tk_val(p)
    advance(p, 3L)
    return(list(kind = "range", from = nm, to = to, text = text))
  }
  if (is_op(p, ":", 1L) && tk_adjacent(p, 0L, 1L)) {
    advance(p, 2L)
    return(list(kind = "prefix", prefix = nm))
  }
  advance(p)
  if (nm %in% c("_numeric_", "_character_", "_all_")) return(list(kind = "special", name = nm))
  list(kind = "name", name = nm, text = tk_val(p, -1L))
}

parse_of_list <- function(p) {
  line <- tk_line(p)
  advance(p) # OF
  items <- list()
  while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
  list(node = "of", items = items, line = line)
}

parse_constant <- function(p, what = "constant") {
  line <- tk_line(p)
  sign <- 1
  if (is_op(p, c("-", "+"))) {
    if (tk_val(p) == "-") sign <- -1
    advance(p)
    if (tk_type(p) != "num") abort_syntax(paste("expected a number in", what), line)
  }
  t <- tk_type(p)
  v <- tk_val(p)
  if (t == "num") {
    advance(p)
    return(list(node = "num", value = sign * sas_num_literal(v, line), text = paste0(if (sign < 0) "-", v), kind = "num", line = line))
  }
  if (t == "str") {
    advance(p)
    return(list(node = "str", value = v, line = line))
  }
  if (t %in% c("date", "time", "datetime")) {
    advance(p)
    return(list(node = "num", value = sas_parse_dt_literal(v, t), text = v, kind = t, line = line))
  }
  if (t == "dot" && tk_type(p, 1L) == "name" && tk_adjacent(p, 0L, 1L) && nchar(tk_val(p, 1L)) == 1L &&
      grepl("^[A-Za-z_]$", tk_val(p, 1L))) {
    # special missing value such as .A in a list of constants (RETAIN, ARRAY)
    code <- paste0(".", toupper(tk_val(p, 1L)))
    advance(p, 2L)
    return(list(node = "missing", code = code, line = line))
  }
  if (t == "missing" || t == "dot") {
    advance(p)
    return(list(node = "missing", code = if (t == "dot") "." else v, line = line))
  }
  abort_syntax(paste0("expected a ", what, " but found ", tok_desc(p)), line)
}

parse_in_list <- function(p, lhs, negate, line) {
  # x IN arrayname searches the elements of an array (not implemented); a
  # name that is not an array is a SAS syntax error, which needs the symbol table
  if (tk_type(p) == "name" || (is_op(p, ":") && tk_type(p, 1L) == "name")) {
    abort_unsupported("IN with an array name as the right operand", line)
  }
  if (is_op(p, ":")) {
    # IN: compares prefixes, as =: does: x in: (a, b) is (x =: a) or (x =: b)
    advance(p)
    if (!lhs$node %in% c("var", "str", "num")) abort_unsupported("IN: with an operand other than a variable or a constant", line)
    e <- parse_in_list(p, lhs, FALSE, line)
    cmp <- lapply(e$items, function(it) {
      if (!it$node %in% c("str", "num")) abort_unsupported("IN: with a value range or a non-constant item", line)
      list(node = "compare", op = "=", colon = TRUE, lhs = lhs, rhs = it, line = line)
    })
    if (length(cmp) == 0) abort_unsupported("IN: with an empty list", line)
    out <- Reduce(function(a, b) list(node = "binary", op = "or", lhs = a, rhs = b, line = line), cmp)
    return(if (negate) list(node = "unary", op = "not", arg = out, line = line) else out)
  }
  expect_type(p, "lparen", "'(' after IN")
  items <- list()
  repeat {
    if (tk_type(p) == "rparen") {
      advance(p)
      break
    }
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    item <- parse_constant(p, "IN list value")
    if (is_op(p, ":")) {
      advance(p)
      to <- parse_constant(p, "IN range bound")
      if (item$node != "num" || to$node != "num") abort_syntax("IN range bounds must be numbers", line)
      item <- list(node = "range", from = item$value, to = to$value)
    }
    items <- c(items, list(item))
  }
  list(node = "in", lhs = lhs, items = items, negate = negate, line = line)
}

# Compact S-expression rendering of an expression AST (for tests and debugging).
expr_to_sexpr <- function(e) {
  s <- function(x) expr_to_sexpr(x)
  switch(e$node,
    num = e$text,
    str = paste0("'", e$value, "'"),
    missing = e$code,
    var = e$name,
    unary = paste0("(", if (e$op == "-") "neg" else e$op, " ", s(e$arg), ")"),
    binary = paste0("(", e$op, " ", s(e$lhs), " ", s(e$rhs), ")"),
    compare = paste0("(", e$op, if (isTRUE(e$colon)) ":", " ", s(e$lhs), " ", s(e$rhs), ")"),
    chain = paste0("(chain ", paste(e$ops, collapse = " "), " ", paste(vapply(e$args, s, ""), collapse = " "), ")"),
    "in" = paste0("(", if (e$negate) "not-in" else "in", " ", s(e$lhs), " ",
                  paste(vapply(e$items, function(it) if (it$node == "range") paste0(it$from, ":", it$to) else s(it), ""), collapse = " "), ")"),
    between = paste0("(", if (isTRUE(e$negate)) "not-between" else "between", " ", s(e$lhs), " ", s(e$lo), " ", s(e$hi), ")"),
    contains = paste0("(", if (isTRUE(e$negate)) "not-contains" else "contains", " ", s(e$lhs), " ", s(e$rhs), ")"),
    like = paste0("(", if (isTRUE(e$negate)) "not-like" else "like", " ", s(e$lhs), " ", s(e$rhs), ")"),
    # PROC SQL nodes
    col = if (is.null(e$table)) e$name else paste0(e$table, ".", e$name),
    in_list = paste0("(", if (isTRUE(e$negate)) "not-in" else "in", " ", s(e$lhs), " ",
                     paste(vapply(e$items, s, ""), collapse = " "), ")"),
    is_missing = paste0("(", if (isTRUE(e$negate)) "is-not-missing" else "is-missing", " ", s(e$lhs), ")"),
    call = paste0("(call ", e$name, paste0(" ", vapply(e$args, function(a) {
      if (a$node == "of") {
        paste0("(of ", paste(vapply(a$items, function(it) switch(it$kind,
          name = it$name, range = paste0(it$from, "-", it$to), prefix = paste0(it$prefix, ":"),
          array = paste0(it$name, "{*}"), pos_range = paste0(it$from, "--", it$to), it$name), ""), collapse = " "), ")")
      } else s(a)
    }, ""), collapse = ""), ")"),
    array_ref = paste0("(aref ", e$name, " ", s(e$index[[1]]), ")"),
    e$node
  )
}

# A numeric constant; values outside the double range and subnormal values
# are an error.
sas_num_literal <- function(v, line = NULL) {
  val <- as.numeric(v)
  if (is.infinite(val)) abort_syntax(paste0("Invalid number ", v), line)
  if (val != 0 && abs(val) < .Machine$double.xmin) {
    abort_syntax(paste0("Invalid number ", v), line)
  }
  val
}
