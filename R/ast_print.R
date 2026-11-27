# Printing of AST nodes (spec 09 L3d): an indented tree, one line per node with
# its kind, its scalar fields and its source span; the child nodes below it.

# a node: a classed node, or a list with a `node` or `stmt` field (the parts a
# statement parser builds without parse_expr(), e.g. an assignment target)
ast_is_node <- function(x) {
  inherits(x, c("sas_node", "sas_stmt")) || (is.list(x) && !is.object(x) && (is.character(x$node) || is.character(x$stmt)))
}

ast_kind <- function(x) if (is.character(x$stmt)) paste0("stmt ", x$stmt) else x$node

ast_scalar <- function(v) {
  (is.character(v) || is.numeric(v) || is.logical(v)) && length(v) == 1L && is.null(attributes(v))
}

ast_format <- function(x, indent = "") {
  sp <- x$span
  at <- if (length(sp)) sprintf(" <%d:%d-%d:%d>", sp[["line"]], sp[["col"]], sp[["end_line"]], sp[["end_col"]]) else ""
  skip <- c("node", "stmt", "span", "line", "col")
  flds <- setdiff(names(x), skip)
  sc <- flds[vapply(flds, function(f) ast_scalar(x[[f]]), NA)]
  txt <- vapply(sc, function(f) {
    v <- x[[f]]
    paste0(f, "=", if (is.character(v)) encodeString(v, quote = "\"") else format(v))
  }, "")
  out <- paste0(indent, ast_kind(x), if (length(txt)) paste0(" ", paste(txt, collapse = " ")), at)
  for (f in setdiff(flds, sc)) {
    v <- x[[f]]
    kids <- if (ast_is_node(v)) list(v) else if (is.list(v) && !is.object(v)) Filter(ast_is_node, v) else list()
    if (!length(kids)) next
    out <- c(out, paste0(indent, "  ", f, ":"))
    for (k in kids) out <- c(out, ast_format(k, paste0(indent, "    ")))
  }
  out
}

#' @export
format.sas_node <- function(x, ...) ast_format(x)

#' @export
format.sas_stmt <- function(x, ...) ast_format(x)

#' @export
print.sas_node <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
print.sas_stmt <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
