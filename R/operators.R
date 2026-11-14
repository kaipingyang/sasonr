# Operators of SAS expressions ------------------------------------------------
#
# One table for the expression parsers of the DATA step (parse_expr.R: DATA
# step statements, and WHERE expressions when `p$where_ctx` is set) and of
# PROC SQL (sql_parse.R).  Each operator has, per context, a binding power
# (larger binds tighter; NA: not an operator of that context) and a kind:
#   left, right  binary operator and its associativity
#   chain        comparison operator: a < b < c is (a < b) and (b < c)
#   cond         condition (DATA step IN): binds tighter than the comparison
#                operators and is no left operand of an operator that binds
#                tighter than itself (a in (1, 2) + c is a syntax error)
#   excl         condition of WHERE and PROC SQL (IN, BETWEEN-AND, CONTAINS,
#                LIKE, IS MISSING): at the level of the comparison operators,
#                no operand of a comparison or of another condition
#                (a < b in (1, 2) and a in (1, 2) < b are syntax errors)
#   prefix       prefix operator; `bp` binds its operand, `level` is the
#                tightest operand position it may stand in (NOT inside an
#                arithmetic operand is a syntax error in WHERE and PROC SQL)
#
# DATA step: SAS Programmer's Guide: Essentials, "Order of Operation in
# Compound Expressions" (Table 9.7): group 1, right to left, ** prefix + - NOT
# >< <> (MIN, MAX); group 2 * /; group 3 + -; group 4 ||; group 5 the
# comparison operators and IN; group 6 &; group 7 |.  IN binds tighter than the
# other group 5 operators: a <= b in (0, 5) is a <= (b in (0, 5)).
# PROC SQL: SQL Procedure User's Guide, "Operators and the Order of Evaluation"
# (Table 8.2): 2 ** and unary + -; 3 * /; 4 + -; 5 ||; 6 BETWEEN, CONTAINS,
# EXISTS, IN, IS, LIKE; 7 the comparison operators; 8 NOT; 9 AND; 10 OR.
# WHERE: the prefix signs bind tighter than **, ** tighter than MIN and MAX,
# which group left to right, and NOT takes a MIN / MAX operand (not a ** b is
# not (a ** b), not a * b is (not a) * b).  In WHERE and PROC SQL two different
# prefix signs in a row (- + a) are a syntax error.
sas_operators <- tibble::tribble(
  ~op,        ~data,          ~where,          ~sql,
  "or",       "10 left",      "10 left",       "10 left",
  "and",      "20 left",      "20 left",       "20 left",
  "not",      "90 prefix",    "90 prefix 50",  "40 prefix 30",
  "cmp",      "40 chain",     "40 chain",      "40 chain",
  "in",       "50 cond",      "40 excl",       "40 excl",
  "between",  NA,             "40 excl",       "40 excl",
  "contains", NA,             "40 excl",       "40 excl",
  "like",     NA,             "40 excl",       "40 excl",
  "is",       NA,             "40 excl",       "40 excl",
  "||",       "60 left",      "60 left",       "60 left",
  "+",        "70 left",      "70 left",       "70 left",
  "-",        "70 left",      "70 left",       "70 left",
  "*",        "80 left",      "80 left",       "80 left",
  "/",        "80 left",      "80 left",       "80 left",
  "min",      "90 right",     "90 left",       NA,
  "max",      "90 right",     "90 left",       NA,
  "**",       "90 right",     "100 right",     "90 right",
  "neg",      "90 prefix",    "110 prefix",    "90 prefix",
  "pos",      "90 prefix",    "110 prefix",    "90 prefix"
)

# contexts whose prefix signs may not alternate (- + a)
sas_op_strict_signs <- c(data = FALSE, where = TRUE, sql = TRUE)

# sas_operators as lookup lists: sas_op_info[[ctx]][[op]] =
# list(bp, kind, level) (NULL when `op` is not an operator of `ctx`)
sas_op_info <- local({
  out <- list()
  for (ctx in c("data", "where", "sql")) {
    tab <- list()
    for (i in seq_len(nrow(sas_operators))) {
      cell <- sas_operators[[ctx]][i]
      if (is.na(cell)) next
      w <- strsplit(cell, " ", fixed = TRUE)[[1]]
      tab[[sas_operators$op[i]]] <- list(bp = as.integer(w[1]), kind = w[2],
                                         level = if (length(w) > 2L) as.integer(w[3]) else .Machine$integer.max)
    }
    out[[ctx]] <- tab
  }
  out
})

# Description of an infix operator for the Pratt loops: `kind` is the node
# family the loop builds ("binary", "cmp", "in", "between", ...), `name` the
# row of sas_operators.  NULL when the operator does not exist in `ctx`.
sas_infix <- function(kind, op, name, ctx, ntok = 1L, negate = FALSE) {
  o <- sas_op_info[[ctx]][[name]]
  if (is.null(o)) return(NULL)
  list(kind = kind, op = op, bp = o$bp, right = identical(o$kind, "right"), cond = o$kind %in% c("cond", "excl"),
       excl = identical(o$kind, "excl"), ntok = ntok, negate = negate)
}

# The checks of the Pratt loops that come from the table.  `st` is the state of
# one loop level: `cond_bp` the binding power of the condition just applied (NA
# after anything else), `used` what the level holds ("" / "cmp" / "cond").
# Stops with a syntax error when `info` may not follow.
sas_op_check <- function(st, info, p) {
  if (!is.na(st$cond_bp) && info$bp > st$cond_bp) {
    abort_syntax(paste0("unexpected ", tok_desc(p), " after a condition (IN, BETWEEN, LIKE, CONTAINS or IS)"), tk_line(p))
  }
  if (isTRUE(info$excl) && nzchar(st$used)) {
    abort_syntax(paste0("unexpected ", tok_desc(p), ": a condition cannot take a comparison or condition as its operand"),
                 tk_line(p))
  }
  if (identical(info$kind, "cmp") && identical(st$used, "excl")) {
    abort_syntax(paste0("unexpected ", tok_desc(p), " after a condition (IN, BETWEEN, LIKE, CONTAINS or IS)"), tk_line(p))
  }
  invisible()
}

# The state of a loop level after `info` was applied.
sas_op_after <- function(info) {
  if (isTRUE(info$cond)) return(list(cond_bp = info$bp, used = if (isTRUE(info$excl)) "excl" else "cond"))
  list(cond_bp = NA_integer_, used = if (identical(info$kind, "cmp")) "cmp" else "")
}

# A prefix operator (`name`: not, neg, pos) at an operand position of binding
# power `min_bp`: its row, after the checks of the table (`sign`: the prefix
# sign just before it, for the alternating-sign rule).
sas_prefix_op <- function(name, ctx, min_bp, p, sign = NULL) {
  o <- sas_op_info[[ctx]][[name]]
  if (min_bp > o$level) abort_syntax(paste0("unexpected ", tok_desc(p), " inside an operand"), tk_line(p))
  if (isTRUE(sas_op_strict_signs[[ctx]]) && !is.null(sign) && name %in% c("neg", "pos") && !identical(sign, name)) {
    abort_syntax(paste0("unexpected ", tok_desc(p), " after a prefix sign"), tk_line(p))
  }
  o
}

# Add a comparison to `lhs`: a second comparison operator next to a comparison
# makes a chain (a < b < c is (a < b) and (b < c)); colon modifiers do not
# chain.  `last_cmp`: the previous operator of this level was a comparison.
sas_op_compare <- function(lhs, op, colon, rhs, line, last_cmp) {
  chainable <- last_cmp && !colon && !isTRUE(lhs$colon)
  if (chainable && identical(lhs$node, "compare")) {
    return(list(node = "chain", ops = c(lhs$op, op), args = list(lhs$lhs, lhs$rhs, rhs), line = lhs$line))
  }
  if (chainable && identical(lhs$node, "chain")) {
    lhs$ops <- c(lhs$ops, op)
    lhs$args <- c(lhs$args, list(rhs))
    return(lhs)
  }
  list(node = "compare", op = op, colon = colon, lhs = lhs, rhs = rhs, line = line)
}
