# Pratt (precedence-climbing) expression parser for SAS expressions.
#
# Binding powers follow SAS Table 6.6 (larger binds tighter):
#   1 OR |          2 AND &          3 comparisons, IN (chains allowed)
#   4 ||            5 + -            6 * /
#   7 Group I: ** >< <> and prefix + - NOT (right associative)

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
  # recovery of errors.R: the variables SAS has compiled when a statement fails)
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

infix <- function(kind, op, bp, right = FALSE, ntok = 1L, negate = FALSE) {
  list(kind = kind, op = op, bp = bp, right = right, ntok = ntok, negate = negate)
}

peek_infix <- function(p) {
  t <- tk_type(p)
  if (t == "op") {
    v <- tk_val(p)
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
      "><" = if (p$where_ctx) {
        abort_unsupported("the >< operator in a WHERE expression", tk_line(p))
      } else infix("binary", "min", 7L, right = TRUE),
      "<>" = if (p$where_ctx) infix("cmp", "^=", 3L) else infix("binary", "max", 7L, right = TRUE),
      NULL
    ))
  }
  if (t != "name") return(NULL)
  low <- tk_low(p)
  if (low %in% names(mnemonic_cmp)) return(infix("cmp", mnemonic_cmp[[low]], 3L))
  switch(low,
    or = infix("binary", "or", 1L),
    and = infix("binary", "and", 2L),
    min = infix("binary", "min", 7L, right = TRUE),
    max = infix("binary", "max", 7L, right = TRUE),
    "in" = infix("in", "in", 3L),
    not = {
      nxt <- tk_low(p, 1L)
      if (tk_type(p, 1L) != "name") return(NULL)
      if (nxt == "in") return(infix("in", "in", 3L, ntok = 2L, negate = TRUE))
      if (p$where_ctx && nxt %in% c("between", "contains", "like")) {
        return(infix(nxt, nxt, 3L, ntok = 2L, negate = TRUE))
      }
      NULL
    },
    between = ,
    contains = ,
    like = if (p$where_ctx) infix(low, low, 3L) else NULL,
    is = if (p$where_ctx) infix("is", "is", 3L) else NULL,
    NULL
  )
}

parse_expr <- function(p, min_bp = 1L) {
  lhs <- parse_prefix(p)
  last_cmp <- FALSE
  repeat {
    info <- peek_infix(p)
    if (is.null(info) || info$bp < min_bp) break
    line <- tk_line(p)
    col <- tk_col(p)
    advance(p, info$ntok)

    if (info$kind == "cmp") {
      rhs <- parse_expr(p, 4L)
      colon <- endsWith(info$op, ":")
      op <- sub(":", "", info$op, fixed = TRUE)
      chainable <- last_cmp && !colon && !isTRUE(lhs$colon)
      if (chainable && lhs$node == "compare") {
        lhs <- list(node = "chain", ops = c(lhs$op, op), args = list(lhs$lhs, lhs$rhs, rhs), line = lhs$line)
      } else if (chainable && lhs$node == "chain") {
        lhs$ops <- c(lhs$ops, op)
        lhs$args <- c(lhs$args, list(rhs))
      } else {
        lhs <- list(node = "compare", op = op, colon = colon, lhs = lhs, rhs = rhs, line = line)
      }
      last_cmp <- TRUE
      next
    }
    last_cmp <- FALSE

    lhs <- switch(info$kind,
      binary = {
        rhs <- parse_expr(p, if (info$right) info$bp else info$bp + 1L)
        list(node = "binary", op = info$op, lhs = lhs, rhs = rhs, line = line, col = col)
      },
      "in" = parse_in_list(p, lhs, info$negate, line),
      between = {
        lo <- parse_expr(p, 4L)
        expect_kw(p, "and")
        hi <- parse_expr(p, 4L)
        list(node = "between", lhs = lhs, lo = lo, hi = hi, negate = info$negate, line = line)
      },
      contains = list(node = "contains", lhs = lhs, rhs = parse_expr(p, 4L), negate = info$negate, line = line),
      like = list(node = "like", lhs = lhs, rhs = parse_expr(p, 4L), negate = info$negate, line = line),
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
  }
  lhs
}

parse_prefix <- function(p) {
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
      e <- parse_expr(p)
      expect_type(p, "rparen", "')'")
      return(e)
    },
    op = {
      if (v %in% c("-", "+", "^")) {
        advance(p)
        arg <- parse_expr(p, 7L)
        return(list(node = "unary", op = if (v == "^") "not" else v, arg = arg, line = line))
      }
    },
    name = return(parse_name_primary(p))
  )
  abort_syntax(paste("unexpected", tok_desc(p), "in expression"), line)
}

parse_name_primary <- function(p) {
  low <- tk_low(p)
  line <- tk_line(p)
  col <- tk_col(p)
  if (low == "not" && !tk_type(p, 1L) %in% c("semi", "rparen", "comma", "eof")) {
    advance(p)
    return(list(node = "unary", op = "not", arg = parse_expr(p, 7L), line = line))
  }
  # first.x / last.x
  if (low %in% c("first", "last") && tk_type(p, 1L) == "dot" && tk_type(p, 2L) == "name" &&
      tk_adjacent(p, 0L, 1L) && tk_adjacent(p, 1L, 2L)) {
    name <- paste0(low, ".", tk_low(p, 2L))
    advance(p, 3L)
    parser_saw(p, name)
    return(list(node = "var", name = name, text = name, line = line))
  }
  if (tk_type(p, 1L) == "dot" && tk_adjacent(p, 0L, 1L)) {
    abort_unsupported(paste0("dotted reference '", tk_val(p), ".' (component objects)"), line)
  }
  if (tk_type(p, 1L) == "lbrace" || (tk_type(p, 1L) == "lparen" && low %in% p$arrays)) {
    return(parse_array_ref(p))
  }
  if (tk_type(p, 1L) == "lparen") return(parse_call(p))
  advance(p)
  parser_saw(p, low)
  list(node = "var", name = low, text = tk_val(p, -1L), line = line, col = col)
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
  if (length(idx) > 1) abort_unsupported("multi-dimensional array references", line)
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
    between = paste0("(between ", s(e$lhs), " ", s(e$lo), " ", s(e$hi), ")"),
    contains = paste0("(contains ", s(e$lhs), " ", s(e$rhs), ")"),
    is_missing = paste0("(is-missing ", s(e$lhs), ")"),
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

# A numeric constant.  SAS accepts a subnormal literal and keeps the value:
# 1e-310 is stored as 000012688B70E62B, and 1e-310 + 0, 1e-310 * 1 and
# 1e-310 / 1e-310 = 1 all behave as ordinary IEEE arithmetic (SAS 9.4 does not
# run denormals-are-zero).  Only values outside the double range are an error.
sas_num_literal <- function(v, line = NULL) {
  val <- as.numeric(v)
  if (is.infinite(val)) abort_syntax(paste0("Invalid number ", v), line)
  val
}
