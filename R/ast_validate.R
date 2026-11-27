# Validation of AST nodes (spec 09 L3d): the fields each kind of node always
# has, checked in tests and when options(sasonr.validate_ast = TRUE), not on
# every parse. The tables come from the fields the parser writes on every node
# of a kind [measured: every node of the parsable programs of the test cases,
# 2026-11-27]; a kind not listed is checked only for its class and span.

ast_expr_fields <- list(
  array_ref = c("index", "line", "name"),
  between = c("hi", "lhs", "line", "lo", "negate"),
  # (an IN list rewritten as ORs has no column)
  binary = c("lhs", "line", "op", "rhs"),
  calc = c("line", "name"),
  call = c("args", "col", "line", "name"),
  case = c("else_", "line", "operand", "whens"),
  chain = c("args", "line", "ops"),
  col = c("col", "line", "name", "table", "text"),
  compare = c("colon", "lhs", "line", "op", "rhs"),
  concat = c("items"),
  cross = c("items"),
  elem = c("col", "denom", "label", "line", "name", "text"),
  fmt = c("col", "fmt", "line"),
  `in` = c("items", "lhs", "line", "negate"),
  method = c("args", "col", "line", "method", "obj"),
  obj_attr = c("attr", "col", "line", "obj"),
  in_list = c("items", "lhs", "line", "negate"),
  is_missing = c("lhs", "line", "negate"),
  like = c("lhs", "line", "negate", "rhs"),
  missing = c("code", "line"),
  num = c("kind", "line", "text", "value"),
  omitted = c("line"),
  paren = c("col", "inner", "line"),
  quant = c("lhs", "line", "op", "quant", "query"),
  select = c("distinct", "from", "group", "having", "into", "items", "line", "order", "where"),
  setop = c("all", "corr", "lhs", "line", "op", "order", "rhs"),
  star = c("line"),
  str = c("line", "value"),
  unary = c("arg", "line", "op"),
  var = c("line", "name", "text")
)

ast_stmt_fields <- list(
  array = c("char", "dim", "init", "items", "len", "line", "lo", "name", "temporary"),
  assign = c("line", "target", "value"),
  by = c("descending", "line", "vars"),
  class = c("items", "line"),
  contents = c("line", "opts"),
  continue = c("line"),
  datalines = c("end_no", "four", "line", "line_no", "lines"),
  declare = c("args", "class", "line", "name"),
  do = c("body", "kind", "line"),
  drop = c("items", "line"),
  exact = c("kws", "line"),
  file = c("dest", "line", "opts"),
  format = c("line", "specs"),
  id = c("items", "line"),
  `if` = c("cond", "else", "line", "then"),
  infile = c("line", "opts", "source"),
  informat = c("line", "specs"),
  input = c("hold", "items", "line"),
  keep = c("items", "line"),
  label = c("labels", "line", "texts"),
  leave = c("line"),
  length = c("line", "specs"),
  merge = c("datasets", "end", "line", "nobs"),
  method = c("call", "line"),
  null = c("line"),
  output = c("line"),
  put = c("items", "line"),
  retain = c("groups", "line"),
  set = c("datasets", "end", "line", "nobs"),
  stop = c("line"),
  substr_assign = c("col", "len", "line", "pos", "value", "var", "var_text"),
  sum = c("line"),
  sumby = c("items", "line"),
  table = c("box", "dims", "line", "misstext", "printmiss"),
  tables = c("line", "opts", "reqs"),
  weight = c("line"),
  where = c("cond", "line")
)

# The problems of one node and of the nodes below it, as text ("" none).
ast_problems <- function(x, path = "") {
  out <- character()
  if (inherits(x, "sas_node") || inherits(x, "sas_stmt")) {
    stmt <- inherits(x, "sas_stmt")
    kind <- if (stmt) x$stmt else x$node
    want <- if (stmt) paste0("sas_stmt_", kind) else paste0("sas_", kind)
    here <- paste0(path, if (stmt) " stmt " else " ", kind)
    if (!is.character(kind) || length(kind) != 1L) return(paste0(path, ": a node without its kind"))
    if (!identical(class(x), c(want, if (stmt) "sas_stmt" else "sas_node"))) out <- c(out, paste0(here, ": class ", paste(class(x), collapse = "/")))
    sp <- x$span
    if (!is.integer(sp) || length(sp) != 6L || !identical(names(sp), c("tok", "tok_end", "line", "col", "end_line", "end_col")) ||
        anyNA(sp) || sp[["tok"]] > sp[["tok_end"]]) {
      out <- c(out, paste0(here, ": span"))
    }
    req <- (if (stmt) ast_stmt_fields else ast_expr_fields)[[kind]]
    miss <- setdiff(req, names(x))
    if (length(miss)) out <- c(out, paste0(here, ": no field ", paste(miss, collapse = ", ")))
    path <- here
  }
  if (is.list(x)) {
    nm <- names(x) %||% rep("", length(x))
    for (k in seq_along(x)) if (is.list(x[[k]])) out <- c(out, ast_problems(x[[k]], paste0(path, "$", if (nzchar(nm[k])) nm[k] else k)))
  }
  out
}

# Stops with the problems of a parsed program (sas_parse()) or a node.
validate_sas_node <- function(x) {
  p <- ast_problems(x)
  if (length(p)) sasonr_abort(paste(c("internal: invalid AST nodes", utils::head(p, 20L)), collapse = "\n"), "sasonr_internal_error")
  invisible(x)
}
