# Generated code as R language objects (plans/p0_1_slots_cfg.md block 1b-4).
#
# The emitters build language objects; they are executed as they are and
# deparsed once, by cg_render(), for display (sas_translate()). Deparsing
# keeps every value exact (15 digits where they suffice, else 17; keepNA,
# keepInteger), so the displayed
# code parses back to the executed objects. A vectorised pipeline is kept as
# a cg_pipe (its source and stages) and displayed with `|>`; it runs as the
# nested call.

cg_deparse_opts15 <- c("keepNA", "keepInteger", "niceNames", "showAttributes")
cg_deparse_opts <- c(cg_deparse_opts15, "digits17")

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

# 15 significant digits where they give the same object back (readable
# literals), else 17
# deparse() indents by 4 spaces; the generated text uses 2 (a string never
# spans lines: deparse() writes its newlines as \n).
cg_deparse <- function(x, width = 80L, collapse = "\n") {
  lines <- deparse(x, width.cutoff = width, backtick = TRUE, control = cg_deparse_opts15)
  back <- tryCatch(parse(text = lines, keep.source = FALSE), error = function(e) NULL)
  if (!(length(back) == 1L && identical(back[[1]], x, num.eq = FALSE))) {
    lines <- deparse(x, width.cutoff = width, backtick = TRUE, control = cg_deparse_opts)
  }
  # `}` and `else` on one line
  k <- which(grepl("^ *\\}$", lines[-length(lines)]) & grepl("^ *else\\b", lines[-1L]))
  for (i in rev(k)) {
    lines[i] <- paste0(lines[i], " ", sub("^ +", "", lines[i + 1L]))
    lines <- lines[-(i + 1L)]
  }
  # `if (c) stmt` without braces: deparse() puts stmt on the next line
  k <- which(grepl("^ *(\\} )?(if \\(.*\\)|else) $", lines))
  for (i in rev(k)) if (i < length(lines)) {
    lines[i] <- paste0(lines[i], sub("^ +", "", lines[i + 1L]))
    lines <- lines[-(i + 1L)]
  }
  lead <- nchar(lines) - nchar(sub("^ +", "", lines))
  lines <- paste0(strrep(" ", lead %/% 2L), substring(lines, lead + 1L))
  paste(lines, collapse = collapse)
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

# Values as language ------------------------------------------------------
#
# A constant embedded in a language object must deparse to text that parses
# back to the same object: a negative number becomes the call -(x), Inf and
# NaN the symbols, a vector a c() call, a list a list() call.

cg_num <- function(x) {
  if (is.na(x) && !is.nan(x)) return(if (is.integer(x)) NA_integer_ else NA_real_)
  if (is.nan(x)) return(as.name("NaN"))
  if (x < 0 || (x == 0 && 1 / x < 0)) return(call("-", cg_num(-x)))
  if (is.infinite(x)) return(as.name("Inf"))
  x
}

cg_val <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) return(as.call(c(list(as.name("list")), lapply(x, function(v) cg_val(v) %||% quote(NULL)))))
  x <- unname(x)
  if (length(x) == 1) return(if (is.numeric(x)) cg_num(x) else x)
  if (length(x) == 0) return(call(switch(typeof(x), character = "character", double = "numeric", integer = "integer", logical = "logical")))
  as.call(c(list(as.name("c")), lapply(x, cg_val)))
}

# A call of `fun` ("f" or "pkg::f") with the arguments in `args` (a list,
# names kept; a NULL element is a NULL argument).
cg_call <- function(fun, args = list()) {
  f <- if (grepl("::", fun, fixed = TRUE)) {
    p <- strsplit(fun, "::", fixed = TRUE)[[1]]
    call("::", as.name(p[1]), as.name(p[2]))
  } else as.name(fun)
  as.call(c(list(f), args))
}

# `.at = c(line, col)`
cg_at <- function(line, col) list(.at = call("c", as.integer(line), as.integer(col %||% NA_integer_)))

# The text of an expression on one line (the display layouts of the generated code).
# A call longer than deparse()'s widest line is joined without the line
# break's indentation.
cg_text <- function(x) paste(trimws(cg_deparse(x, width = 500L, collapse = NULL)), collapse = " ")

# A function literal as parse() gives it (the fourth element, the source
# reference, is NULL).
cg_fn <- function(args, body) as.call(list(as.name("function"), as.pairlist(args), body, NULL))

# A piece of generated code: the language object that runs and its display
# text (built from the deparsed parts in the generated layout).
cgi <- function(lang, text = cg_text(lang)) list(lang = lang, text = text)

# `x |> stage`: the stage call gets `x` as its first argument.
cgi_pipe <- function(x, stage, sep = " |> ") {
  cgi(as.call(c(as.list(stage)[1], list(x$lang), as.list(stage)[-1])), paste0(x$text, sep, cg_text(stage)))
}

# `name <- x`
cgi_assign <- function(name, x) cgi(call("<-", as.name(name), x$lang), paste0(name, " <- ", x$text))
