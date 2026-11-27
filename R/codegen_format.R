# Layout of the generated R code (spec 09 L6a): one renderer from the language
# objects (and cg_pipe pipelines) to the display text, with the rules of the
# air formatter (line width 80, indent 2):
# - an expression that fits in the rest of the line stays on one line;
# - otherwise a call puts each argument on a line of its own, indented, with
#   the closing parenthesis on a line of its own; a chain of one binary
#   operator (`&`, `+`, `|>` ...) breaks after each operator, the operands after
#   the first indented once;
# - braces always span lines (`{`, one statement per line, `}`), as do the
#   bodies of `if`, `repeat` and `function` written with braces.
# The tests check the text against air where it is installed (test-codegen-format.R);
# the text parses back to the object it shows.

# The backend of the display (spec 09 L6c): an object that carries the layout
# choices; cg_emit() dispatches on its class. One backend today, the layout of
# the air formatter (`cg_backend()`, line width 80).
cg_backend <- function(name = "air", width = 80L) {
  structure(list(name = name, width = as.integer(width)), class = c(paste0("cg_backend_", name), "cg_backend"))
}

#' The display lines of an item for a backend
#' @noRd
cg_emit <- function(backend, x) UseMethod("cg_emit")

#' @export
cg_emit.cg_backend_air <- function(backend, x) {
  old <- cg_state$width
  cg_state$width <- backend$width
  on.exit(cg_state$width <- old)
  cg_format(cg_normalize(x))
}

cg_state <- new.env(parent = emptyenv())
cg_state$width <- 80L

# A mutate() of two or more columns puts one column per line, like the SAS
# statements it comes from (air keeps a call broken after its opening
# parenthesis, so the layout is stable under air).
cg_force_break <- function(e) {
  if (!is.call(e)) return(FALSE)
  f <- e[[1]]
  is_mut <- identical(f, as.name("mutate")) || (cg_is_ns(f) && identical(f[[3]], as.name("mutate")))
  is_mut && sum(nzchar(names(e)[-1] %||% character())) >= 2L
}

# One line of `e`, or NA when it does not fit on one line by construction (braces).
cg_flat <- function(e) {
  if (inherits(e, "cg_pipe")) {
    parts <- c(cg_flat(e$src), vapply(e$stages, cg_flat, ""))
    if (anyNA(parts)) return(NA_character_)
    txt <- paste(parts, collapse = " |> ")
    return(if (is.null(e$target)) txt else paste(cg_flat(cg_target(e$target)), "<-", txt))
  }
  if (cg_has_brace(e) || cg_has_forced(e)) return(NA_character_)
  txt <- cg_deparse(e, width = 500L, collapse = NULL)
  if (length(txt) != 1L) NA_character_ else txt
}

cg_has_forced <- function(e) {
  if (!is.call(e)) return(FALSE)
  if (cg_force_break(e)) return(TRUE)
  for (a in as.list(e)) if (!missing(a) && cg_has_forced(a)) return(TRUE)
  FALSE
}

cg_has_brace <- function(e) {
  if (!is.call(e)) return(FALSE)
  if (identical(e[[1]], as.name("{"))) return(TRUE)
  for (a in as.list(e)) if (!missing(a) && cg_has_brace(a)) return(TRUE)
  FALSE
}

cg_binary_ops <- c("|>", "&", "|", "&&", "||", "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "%in%", "%%", "%/%")

# The operands of a left-nested chain of one operator: a & b & c -> a, b, c.
cg_chain <- function(e, op) {
  if (is.call(e) && length(e) == 3L && identical(e[[1]], as.name(op))) c(cg_chain(e[[2]], op), list(e[[3]])) else list(e)
}

#' The display lines of an expression or a cg_pipe
#' @param e A language object or a cg_pipe.
#' @param col The column where it starts.
#' @param tail Characters that follow it on its last line (a comma, a `|>`).
#' @param base The indentation of the line it starts on; its other lines are
#'   returned relative to it (the caller indents them).
#' @noRd
cg_format <- function(e, col = 0L, tail = 0L, base = 0L) {
  flat <- if (cg_force_break(e)) NA_character_ else cg_flat(e)
  if (!is.na(flat) && col + nchar(flat) + tail <= cg_state$width) return(flat)
  pad <- function(lines) c(lines[1], if (length(lines) > 1L) paste0("  ", lines[-1]))
  if (inherits(e, "cg_pipe") && !is.null(e$target)) {
    lhs <- paste0(cg_flat(cg_target(e$target)), " <- ")
    return(cg_prefix(lhs, cg_format(cg_pipe(e$src, e$stages), col + nchar(lhs), tail, base)))
  }
  if (inherits(e, "cg_pipe")) {
    steps <- c(list(e$src), e$stages)
    body <- character()
    for (k in seq_along(steps)) {
      last <- k == length(steps)
      lines <- if (k == 1L) cg_format(steps[[k]], col, if (last) tail else 3L, base) else
        cg_format(steps[[k]], base + 2L, if (last) tail else 3L, base + 2L)
      if (!last) lines[length(lines)] <- paste0(lines[length(lines)], " |>")
      body <- c(body, if (k == 1L) lines else paste0("  ", lines))
    }
    return(body)
  }
  if (!is.call(e)) return(cg_deparse(e, collapse = NULL))
  f <- e[[1]]
  fn <- if (is.name(f)) as.character(f) else ""
  if (fn %in% c("<-", "=") && length(e) == 3L) {
    lhs <- paste0(cg_flat(e[[2]]), " ", fn, " ")
    return(cg_prefix(lhs, cg_format(e[[3]], col + nchar(lhs), tail, base)))
  }
  if (fn == "{") {
    stmts <- as.list(e)[-1]
    return(c("{", unlist(lapply(stmts, function(s) paste0("  ", cg_format(s, base + 2L, 0L, base + 2L)))), "}"))
  }
  if (fn == "if") {
    cond <- paste0("if (", cg_flat(e[[2]]), ") ")
    out <- cg_prefix(cond, cg_format(e[[3]], col + nchar(cond), 0L, base))
    if (length(e) == 4L) {
      at <- if (length(out) > 1L) base + nchar(out[length(out)]) else col + nchar(out)
      els <- cg_format(e[[4]], at + 6L, tail, base)
      out[length(out)] <- paste0(out[length(out)], " else ", els[1])
      out <- c(out, els[-1])
    }
    return(out)
  }
  if (fn == "repeat") return(cg_prefix("repeat ", cg_format(e[[2]], col + 7L, tail, base)))
  if (fn == "while" && length(e) == 3L) {
    head <- paste0("while (", cg_flat(e[[2]]), ") ")
    return(cg_prefix(head, cg_format(e[[3]], col + nchar(head), tail, base)))
  }
  if (fn == "for" && length(e) == 4L) {
    head <- paste0("for (", cg_flat(e[[2]]), " in ", cg_flat(e[[3]]), ") ")
    return(cg_prefix(head, cg_format(e[[4]], col + nchar(head), tail, base)))
  }
  if (fn %in% cg_binary_ops && length(e) == 3L) {
    ops <- cg_chain(e, fn)
    out <- character()
    for (k in seq_along(ops)) {
      last <- k == length(ops)
      tl <- if (last) tail else nchar(fn) + 1L
      lines <- if (k == 1L) cg_format(ops[[k]], col, tl, base) else cg_format(ops[[k]], base + 2L, tl, base + 2L)
      if (!last) lines[length(lines)] <- paste0(lines[length(lines)], " ", fn)
      out <- c(out, if (k == 1L) lines else pad(c("", lines))[-1])
    }
    return(out)
  }
  # an index with one argument too wide: the argument broken in place, the
  # bracket after its last line (`x[c(` ... `)]`)
  if (fn %in% c("[", "[[") && length(e) == 3L && !is.na(cg_flat(e[[2]])) && is.null(names(e))) {
    open <- paste0(cg_flat(e[[2]]), fn)
    close <- if (fn == "[") "]" else "]]"
    lines <- cg_prefix(open, cg_format(e[[3]], col + nchar(open), tail + nchar(close), base))
    lines[length(lines)] <- paste0(lines[length(lines)], close)
    return(lines)
  }
  if (fn %in% c("function", "(", "[", "[[", "$", "@", "::", ":::", "-", "!") || (!is.name(f) && !cg_is_ns(f))) {
    return(cg_deparse(e, collapse = NULL))
  }
  # a call: one argument per line
  head <- paste0(cg_flat(f), "(")
  args <- as.list(e)[-1]
  nm <- names(args) %||% rep("", length(args))
  if (!length(args)) return(paste0(head, ")"))
  # a last argument in braces hugs the call when the arguments before it fit
  # on the line: `f(a, {` ... `})`
  last <- args[[length(args)]]
  if (is.call(last) && identical(last[[1]], as.name("{")) && !nzchar(nm[length(args)])) {
    before <- vapply(seq_len(length(args) - 1L), function(k) {
      fl <- cg_flat(args[[k]])
      if (is.na(fl)) NA_character_ else paste0(if (nzchar(nm[k])) paste0(cg_name(nm[k]), " = ") else "", fl, ", ")
    }, "")
    open <- paste0(head, paste(before, collapse = ""))
    if (!anyNA(before) && col + nchar(open) + 1L <= cg_state$width) {
      lines <- cg_format(last, base, 0L, base)
      lines[1] <- paste0(open, lines[1])
      lines[length(lines)] <- paste0(lines[length(lines)], ")")
      return(lines)
    }
  }
  body <- character()
  for (k in seq_along(args)) {
    pre <- if (nzchar(nm[k])) paste0(cg_name(nm[k]), " = ") else ""
    lines <- cg_prefix(pre, cg_format(args[[k]], base + 2L + nchar(pre), if (k < length(args)) 1L else 0L, base + 2L))
    if (k < length(args)) lines[length(lines)] <- paste0(lines[length(lines)], ",")
    body <- c(body, paste0("  ", lines))
  }
  c(head, body, ")")
}

cg_is_ns <- function(f) is.call(f) && length(f) == 3L && (identical(f[[1]], as.name("::")) || identical(f[[1]], as.name(":::")))

cg_name <- function(x) if (make.names(x) == x || x == "...") x else paste0("`", x, "`")

cg_prefix <- function(pre, lines) {
  lines[1] <- paste0(pre, lines[1])
  lines
}

# The code as air writes it (cg_format()): inside braces, an `if` without
# braces and without `else` that is not the last statement gets braces
# (`if (c) {` / body / `}`); the last one keeps its one-line form. The object
# changes the same way, so the text parses back to the code that runs.
cg_normalize <- function(e) {
  if (inherits(e, "cg_pipe")) {
    e$src <- cg_normalize(e$src)
    e$stages <- lapply(e$stages, cg_normalize)
    return(e)
  }
  if (!is.call(e)) return(e)
  for (k in seq_along(e)[-1]) if (!is.null(e[[k]]) && !identical(e[[k]], quote(expr = ))) e[[k]] <- cg_normalize(e[[k]])
  if (identical(e[[1]], as.name("{")) && length(e) > 2L) {
    for (k in 2:(length(e) - 1L)) {
      s <- e[[k]]
      if (is.call(s) && identical(s[[1]], as.name("if")) && length(s) == 3L && !(is.call(s[[3]]) && identical(s[[3]][[1]], as.name("{")))) {
        s[[3]] <- call("{", s[[3]])
        e[[k]] <- s
      }
    }
  }
  e
}

# The target of a pipeline: a name, or an expression such as work$ex.
cg_target <- function(t) if (is.character(t)) rlang::sym(t) else t
