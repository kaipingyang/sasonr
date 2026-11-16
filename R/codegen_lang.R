# Generated code as R language objects (plans/p0_1_slots_cfg.md block 1b-4).
#
# The emitters build language objects; they are executed as they are and
# deparsed once, by cg_render(), for display (sas_translate()). Deparsing
# keeps every value exact (digits17, keepNA, keepInteger), so the displayed
# code parses back to the executed objects. A vectorised pipeline is kept as
# a cg_pipe (its source and stages) and displayed with `|>`; it runs as the
# nested call.

cg_deparse_opts <- c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")

#' A pipeline: `target <- src |> stage1 |> stage2 ...`
#'
#' @param src The source expression.
#' @param stages Calls without their first argument: the previous value is
#'   inserted as the first argument.
#' @param target Name of the variable assigned, or NULL.
#' @noRd
cg_pipe <- function(src, stages = list(), target = NULL) {
  structure(list(src = src, stages = stages, target = target), class = "cg_pipe")
}

# The executable form of one element: a cg_pipe becomes the nested call.
cg_lang <- function(x) {
  if (!inherits(x, "cg_pipe")) return(x)
  val <- Reduce(function(acc, st) as.call(c(as.list(st)[1], list(acc), as.list(st)[-1])), x$stages, x$src)
  if (is.null(x$target)) val else call("<-", as.name(x$target), val)
}

#' The expression vector that runs the elements
#' @noRd
cg_exec <- function(exprs) as.expression(lapply(exprs, cg_lang))

cg_deparse <- function(x) {
  paste(deparse(x, width.cutoff = 80L, control = cg_deparse_opts), collapse = "\n")
}

#' The display text of the elements, after the header comment lines
#' @noRd
cg_render <- function(exprs, header = character()) {
  body <- vapply(exprs, function(x) {
    if (!inherits(x, "cg_pipe")) return(cg_deparse(x))
    parts <- c(cg_deparse(x$src), vapply(x$stages, cg_deparse, ""))
    txt <- paste(parts, collapse = " |>\n  ")
    if (is.null(x$target)) txt else paste(cg_deparse(as.name(x$target)), "<-", txt)
  }, "")
  paste(c(header, body), collapse = "\n")
}
