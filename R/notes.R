# Log NOTEs about steps and data: data set sizes, observations read,
# missing values generated, mathematical errors, uninitialized variables.
#
# Run-time counters live in the run-time state of the active session
# (`rt_diag()`, see diag.R) and are reset per step evaluation; compile-time
# notes (constant folding, type conversions) are collected in its compile-time
# state (`ct_diag()`) while a step is translated.

# `places`: the places of the merged compile-time messages (log_place()).
ct_reset <- function(ct = ct_diag()) {
  ct$notes <- character()
  log_places_reset(ct)
  invisible()
}

# The type conversion NOTEs (conv.n2c, conv.c2n), in the order the first
# place of each was found; the place order is a catalogue field (log_catalog.R).
conversion_notes <- function() {
  ct <- ct_diag()
  ids <- intersect(names(ct$places), c("conv.n2c", "conv.c2n"))
  vctrs::list_unchop(lapply(ids, function(id) log_place_lines(id, ct$places[[id]])), ptype = character(), name_spec = rlang::zap())
}

at_key <- function(.at) {
  if (is.null(.at) || length(.at) < 2 || anyNA(.at)) return("0:0")
  sprintf("%d:%d", as.integer(.at[1]), as.integer(.at[2]))
}

# Functions whose missing numeric arguments are reported in the
# "Missing values were generated" NOTE (ROUND family, INT/FLOOR/CEIL family,
# INTCK, INTNX).
mv_note_fns <- c("round", "rounde", "roundz", "int", "intz", "floor", "floorz", "ceil", "ceilz", "intck", "intnx")

#' Arguments of a function call; missing numeric arguments are counted for the
#' "Missing values were generated" NOTE at the call's position
#' @export
#' @keywords internal
sas_mv_args <- function(.at, ..., .exact = TRUE) {
  args <- list(...)
  num <- Filter(function(a) is.numeric(a) && length(a) > 0, args)
  if (length(num)) rlang::exec(sas_mv_count, .at, !!!unname(num), .exact = .exact)
  args
}

# Functions whose missing *result* is counted in the NOTE (ABS, MIN, MAX, SUM,
# MEAN; SUM/MIN/... ignore missing arguments, so only an all-missing call is
# counted)
mv_result_fns <- c("abs", "min", "max", "sum", "mean")

#' Count missing results of a function call for the missing-value NOTE
#' @export
#' @keywords internal
sas_mv_res <- function(res, .at, .exact = TRUE) {
  # a special missing result (SUM(.A) is .A) is not counted
  if (anyNA(res)) {
    plain <- ifelse(sas_miss_code(res) %in% 1L, res, 0)
    sas_mv_count(.at, plain, .exact = .exact)
  }
  res
}

#' Count "missing values were generated" for one operation site
#'
#' @param .at `c(line, column)` of the operator or function name.
#' @param ... The operands; rows where any operand is missing are counted.
#' @param .exact `FALSE` when vectorised code may over-count (the step is re-run row by row).
#' @keywords internal
sas_mv_count <- function(.at, ..., .exact = TRUE) {
  rt <- rt_diag()
  args <- list(...)
  n <- max(lengths(args))
  if (n == 0) return(invisible())
  miss <- Reduce(`|`, lapply(args, function(x) rep_len(is.na(x), n)))
  k <- sum(miss)
  if (k > 0) {
    if (!.exact) rt$mv_rerun <- TRUE
    log_place("mv.generated", at = .at, n = k)
  }
  invisible()
}

#' Arithmetic operators `+ - *` with SAS missing-value semantics and NOTEs
#' @export
#' @keywords internal
sas_arith <- function(a, b, op, .at = NULL, .exact = TRUE, .gen_a = FALSE, .gen_b = FALSE) {
  r <- switch(op, "+" = a + b, "-" = a - b, "*" = a * b)
  if (anyNA(a) || anyNA(b)) {
    n <- max(length(a), length(b))
    ma <- rep_len(is.na(a), n)
    mb <- rep_len(is.na(b), n)
    # rows where a missing operand is the missing result of an earlier operation are not counted
    cnt <- (ma | mb) & !(.gen_a & ma) & !(.gen_b & mb)
    if (any(cnt)) sas_mv_count(.at, ifelse(cnt, NA_real_, 0), .exact = .exact)
  }
  sas_num(r, .at)
}

# Sort "line:col" keys by position.
sorted_places <- function(keys) {
  parts <- strsplit(keys, ":", fixed = TRUE)
  ln <- vapply(parts, function(p) as.integer(p[1]), 1L)
  cl <- vapply(parts, function(p) as.integer(p[2]), 1L)
  keys[order(ln, cl)]
}

place_lines <- function(entries) {
  # the places go on lines of at most LINESIZE columns, indented by 6
  out <- character()
  cur <- "     "
  ls <- rt_linesize()
  for (e in entries) {
    if (nchar(cur) + nchar(e) + 3L > ls && nchar(cur) > 5L) {
      out <- c(out, cur)
      cur <- "     "
    }
    cur <- paste0(cur, " ", e, "  ")
  }
  c(out, cur)
}

# Compile-time NOTEs of a step (type conversions, constant folding).
compile_notes <- function() {
  ct <- ct_diag()
  mv <- ct$places[["mv.compile"]]
  c(conversion_notes(), ct$notes, if (length(mv)) log_place_lines("mv.compile", mv))
}

#' Record the observations read from an input data set (for the log NOTE)
#'
#' Returns `d` unchanged.
#' @export
#' @keywords internal
sas_note_read <- function(d, name, where = NULL) {
  rt <- rt_diag()
  rt$reads[[length(rt$reads) + 1L]] <- list(name = name, n = nrow(d), where = where, nvar = sum(names(d) != ".obs"))
  d
}

#' Set the observation counts of the SET data sets to the rows consumed
#' @export
#' @keywords internal
sas_reads_consumed <- function(p) {
  rt <- rt_diag()
  left <- p
  for (k in seq_along(rt$reads)) {
    n <- rt$reads[[k]]$n
    rt$reads[[k]]$n <- min(n, max(0, left))
    left <- left - n
  }
  invisible()
}

dataset_note <- function(name, d) {
  log_render("ds.created", list(name = toupper(name), nobs = nrow(d), nvar = ncol(d)))
}

# Variables that are referenced but never given a value (compile-time NOTE).
uninit_vars <- function(an) {
  data_vars <- an$order[vapply(an$order, function(v) identical(an$info[[v]]$kind, "data"), logical(1))]
  assigned <- unique(vctrs::list_unchop(lapply(an$stmts, stmt_assigns_all), ptype = character(), name_spec = rlang::zap()))
  inputs <- vctrs::list_unchop(lapply(an$readers, function(r) {
    if (r$stmt == "input") input_var_names(r) else vctrs::list_unchop(lapply(r$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap())
  }), ptype = character(), name_spec = rlang::zap())
  in_arrays <- vctrs::list_unchop(lapply(an$arrays, `[[`, "elems"), ptype = character(), name_spec = rlang::zap())
  inits <- Filter(function(v) !is.null(an$info[[v]]$init), data_vars)
  setdiff(data_vars, c(assigned, inputs, in_arrays, inits, names(an$sum_plan)))
}

# Like stmt_assigns(), but also counts CALL routine arguments and sum targets
# inside nested blocks.
stmt_assigns_all <- function(s) {
  out <- switch(s$stmt,
    assign = if (s$target$node == "var") s$target$name,
    sum = s$target$name,
    substr_assign = s$var,
    do = s$var,
    call = c(vctrs::list_unchop(lapply(s$targets, function(t) if (t$node == "var") t$name), ptype = character(), name_spec = rlang::zap()),
             vctrs::list_unchop(lapply(s$args, function(a) if (identical(a$node, "var")) a$name), ptype = character(), name_spec = rlang::zap())),
    input = input_var_names(s),
    NULL
  )
  c(out, vctrs::list_unchop(lapply(stmt_children(s), stmt_assigns_all), ptype = character(), name_spec = rlang::zap()))
}

# A WHERE expression as its NOTE shows it: a prefix plus is dropped and a
# double minus cancels (+b is b, - - a is a).
where_fold_signs <- function(x) {
  if (!is.list(x) || is.null(x$node)) return(x)
  for (nm in c("lhs", "rhs", "arg", "lo", "hi")) if (!is.null(x[[nm]])) x[[nm]] <- where_fold_signs(x[[nm]])
  if (!is.null(x$args)) x$args <- lapply(x$args, where_fold_signs)
  if (identical(x$node, "unary") && identical(x$op, "+")) return(x$arg)
  if (identical(x$node, "unary") && identical(x$op, "-") && identical(x$arg$node, "unary") && identical(x$arg$op, "-")) {
    return(x$arg$arg)
  }
  x
}

# The text of a WHERE expression (for the "WHERE ...;" line of a NOTE).
where_text <- function(e) {
  e <- where_fold_signs(e)
  w <- function(x, top = FALSE) {
    switch(x$node,
      num = x$text %||% r_num(x$value),
      str = paste0("'", gsub("'", "''", x$value, fixed = TRUE), "'"),
      missing = x$code,
      var = x$text %||% x$name,
      unary = if (x$op == "not") paste0("not ", w(x$arg)) else paste0(x$op, w(x$arg)),
      binary = {
        if (x$op %in% c("and", "or")) {
          wrap <- function(y) if (y$node %in% c("compare", "binary") && !y$node %in% c("var")) paste0("(", w(y), ")") else w(y)
          paste(wrap(x$lhs), x$op, wrap(x$rhs))
        } else {
          # the MIN and MAX operators are written >< and <>
          paste0(w(x$lhs), switch(x$op, min = "><", max = "<>", x$op), w(x$rhs))
        }
      },
      compare = {
        # a variable compared with anything but a variable is written first
        # ("x+1 = y" is echoed "y=x+1", "2 = y" "y=2", "abs(x) < y" "y>ABS(x)",
        # "(x*2) >= y" "y<=x*2"); two non-variable operands keep their order
        # ("x+1=ABS(y)", "SUBSTR(s,1,3)='van'"); a constant compared with an
        # expression other than a call, and colon comparisons, are not supported
        is_var <- function(y) identical(y$node, "var")
        is_const <- function(y) y$node %in% c("num", "str", "missing") || (identical(y$node, "unary") && y$arg$node == "num")
        if (!is_var(x$lhs)) {
          if (isTRUE(x$colon) && is_var(x$rhs)) {
            abort_unsupported("the WHERE note of a colon comparison with the variable on the right (not supported)")
          }
          if (is_var(x$rhs)) {
            flip <- c("<" = ">", ">" = "<", "<=" = ">=", ">=" = "<=")
            x <- list(node = "compare", op = if (x$op %in% names(flip)) flip[[x$op]] else x$op, lhs = x$rhs, rhs = x$lhs,
                      colon = x$colon)
          } else if (is_const(x$lhs) || (is_const(x$rhs) && !identical(x$lhs$node, "call"))) {
            abort_unsupported("the WHERE note of a constant compared with an expression (operand order not supported)")
          }
        }
        paste0(w(x$lhs), if (x$op == "^=") " not = " else x$op, if (isTRUE(x$colon)) ":" else "", w(x$rhs))
      },
      # a < b < c is written a<b and b<c
      chain = paste(vapply(seq_along(x$ops), function(k) {
        w(list(node = "compare", op = sub(":", "", x$ops[k], fixed = TRUE), colon = endsWith(x$ops[k], ":"),
               lhs = x$args[[k]], rhs = x$args[[k + 1L]]))
      }, ""), collapse = " and "),
      "in" = if (length(x$items) == 1L && !isTRUE(x$negate) && x$items[[1]]$node != "range") {
        # an IN list of one value is written as an equality, the operand first
        paste0(w(x$lhs), "=", w(x$items[[1]]))
      } else {
        paste0(w(x$lhs), if (isTRUE(x$negate)) " not in (" else " in (",
               paste(vapply(x$items, function(it) if (it$node == "range") paste0(it$from, ":", it$to) else w(it), ""),
                     collapse = ", "), ")")
      },
      like = paste0(w(x$lhs), if (isTRUE(x$negate)) " not like " else " like ", w(x$rhs)),
      between = {
        # constant bounds are shown as a range; expression bounds keep BETWEEN
        const <- function(y) y$node %in% c("num", "str", "missing") || (y$node == "unary" && y$arg$node == "num")
        if (const(x$lo) && const(x$hi)) {
          # numeric bounds in ascending order (missing is the lowest)
          num_of <- function(y) switch(y$node, num = y$value, missing = -Inf, unary = -y$arg$value, NA_real_)
          lo <- num_of(x$lo)
          hi <- num_of(x$hi)
          if (anyNA(c(lo, hi)) && !identical(x$lo$node, x$hi$node)) {
            abort_unsupported("the WHERE note of BETWEEN with a character and a numeric bound")
          }
          if (!anyNA(c(lo, hi)) && lo > hi) {
            x[c("lo", "hi")] <- x[c("hi", "lo")]
          } else if (identical(x$lo$node, "str") && x$lo$value > x$hi$value) {
            abort_unsupported("the WHERE note of BETWEEN with character bounds in descending order")
          }
          txt <- paste0("(", w(x$lhs), ">=", w(x$lo), " and ", w(x$lhs), "<=", w(x$hi), ")")
          if (isTRUE(x$negate)) paste0("not ", txt) else txt
        } else {
          paste0(w(x$lhs), if (isTRUE(x$negate)) " not between " else " between ", w(x$lo), " and ", w(x$hi))
        }
      },
      contains = paste0(w(x$lhs), if (isTRUE(x$negate)) " not contains " else " contains ", w(x$rhs)),
      is_missing = paste0(w(x$lhs), if (isTRUE(x$negate)) " is not null" else " is null"),
      call = if (x$name != "sqllike" && length(x$args) && all(vapply(x$args, function(a) a$node %in% c("num", "str", "missing"), logical(1)))) {
        # a call with constant arguments is evaluated when the WHERE clause is
        # compiled and the NOTE shows the value ("contains trim('smith')" is
        # echoed "contains 'smith'"); that folding is not implemented
        abort_unsupported(paste0("the WHERE note of ", toupper(x$name), "() with constant arguments (the folded value is not supported)"))
      } else if (x$name == "sqllike") paste0(w(x$args[[1]]), " like ", w(x$args[[2]])) else paste0(toupper(x$name), "(", paste(vapply(x$args, function(a) if (a$node == "of") "of" else w(a), ""), collapse = ", "), ")"),
      "?"
    )
  }
  w(e, TRUE)
}
