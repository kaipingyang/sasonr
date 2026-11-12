# SAS log NOTEs about steps and data: data set sizes, observations read,
# missing values generated, mathematical errors, uninitialized variables.
#
# Run-time counters live in `rt` (see diag.R) and are reset per step
# evaluation; compile-time notes (constant folding) are collected in `ct`
# while a step is translated.

ct <- new.env(parent = emptyenv())
ct$notes <- character()
ct$mv <- character()

ct_reset <- function() {
  ct$notes <- character()
  ct$mv <- character()
  ct$conv <- list()
  invisible()
}

# Record an implicit type conversion found at compile time ("n2c" or "c2n").
ct_conversion <- function(kind, line, col) {
  ct$conv[[kind]] <- c(ct$conv[[kind]], sprintf("%d:%d", as.integer(line), as.integer(col %||% 0L)))
  invisible()
}

# SAS lists the places by line, and within a line by the column as text
# ("95:143 95:195 95:25"), each in a 9-column field.
conversion_notes <- function() {
  conv <- ct$conv %||% list()
  if (length(conv) == 0) return(character())
  head <- c(n2c = "NOTE: Numeric values have been converted to character values at the places given by: (Line):(Column).",
            c2n = "NOTE: Character values have been converted to numeric values at the places given by: (Line):(Column).")
  out <- character()
  for (k in names(conv)) {
    places <- unique(conv[[k]])
    ln <- as.integer(sub(":.*$", "", places))
    places <- places[order(ln, sub("^.*:", "", places), method = "radix")]
    out <- c(out, head[[k]], place_lines(sprintf("%-6s", places)))
  }
  out
}

at_key <- function(.at) {
  if (is.null(.at) || length(.at) < 2 || anyNA(.at)) return("0:0")
  sprintf("%d:%d", as.integer(.at[1]), as.integer(.at[2]))
}

rt_count <- function(slot, .at, n = 1L) {
  v <- rt[[slot]]
  key <- at_key(.at)
  v[key] <- (if (key %in% names(v)) v[[key]] else 0L) + as.integer(n)
  rt[[slot]] <- v
  invisible()
}

# Functions whose missing numeric arguments SAS reports in the
# "Missing values were generated" NOTE (verified against SAS: ROUND family,
# INT/FLOOR/CEIL family, INTCK, INTNX).
mv_note_fns <- c("round", "rounde", "roundz", "int", "intz", "floor", "floorz", "ceil", "ceilz", "intck", "intnx")

#' Arguments of a function call; missing numeric arguments are counted for the
#' "Missing values were generated" NOTE at the call's position
#' @export
#' @keywords internal
sas_mv_args <- function(.at, ..., .exact = TRUE) {
  args <- list(...)
  num <- Filter(function(a) is.numeric(a) && length(a) > 0, args)
  if (length(num)) do.call(sas_mv_count, c(list(.at), unname(num), list(.exact = .exact)))
  args
}

# Functions whose missing *result* SAS counts in the NOTE (verified: ABS, MIN,
# MAX, SUM, MEAN; SUM/MIN/... ignore missing arguments, so only an all-missing
# call is counted)
mv_result_fns <- c("abs", "min", "max", "sum", "mean")

#' Count missing results of a function call for the missing-value NOTE
#' @export
#' @keywords internal
sas_mv_res <- function(res, .at, .exact = TRUE) {
  # a special missing result (SUM(.A) is .A) is not counted (SAS 9.4)
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
#' @export
sas_mv_count <- function(.at, ..., .exact = TRUE) {
  args <- list(...)
  n <- max(lengths(args))
  if (n == 0) return(invisible())
  miss <- Reduce(`|`, lapply(args, function(x) rep_len(is.na(x), n)))
  k <- sum(miss)
  if (k > 0) {
    if (!.exact) rt$mv_rerun <- TRUE
    rt_count("mv", .at, k)
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
  # SAS writes the places on lines of at most LINESIZE columns, indented by 6
  out <- character()
  cur <- "     "
  ls <- rt$ls %||% 132L
  for (e in entries) {
    if (nchar(cur) + nchar(e) + 3L > ls && nchar(cur) > 5L) {
      out <- c(out, cur)
      cur <- "     "
    }
    cur <- paste0(cur, " ", e, "  ")
  }
  c(out, cur)
}

count_entries <- function(counts) {
  keys <- sorted_places(names(counts))
  sprintf("%d at %s", as.integer(counts[keys]), keys)
}

note_mv_lines <- function(entries) {
  c("NOTE: Missing values were generated as a result of performing an operation on missing values.",
    "      Each place is given by: (Number of times) at (Line):(Column).",
    place_lines(entries))
}

# NOTEs written at the end of a step from the run-time counters.
step_end_notes <- function() {
  out <- character()
  if (isTRUE(rt$flow_note)) out <- "NOTE: SAS went to a new line when INPUT statement reached past the end of a line."
  if (length(rt$mv) > 0) out <- c(out, note_mv_lines(count_entries(rt$mv)))
  if (length(rt$math) > 0) {
    head <- "NOTE: Mathematical operations could not be performed at the following places. The results of the operations have been set to missing values."
    out <- c(out,
      if (nchar(head) <= (rt$ls %||% 132L)) head else
        c("NOTE: Mathematical operations could not be performed at the following places. The results of the operations have been set to ",
          "      missing values."),
      "      Each place is given by: (Number of times) at (Line):(Column).",
      place_lines(count_entries(rt$math)))
  }
  out
}

# Compile-time NOTEs of a step (constant folding).
compile_notes <- function() {
  out <- c(conversion_notes(), ct$notes)
  if (length(ct$mv) > 0) out <- c(out, note_mv_lines(sorted_places(unique(ct$mv))))
  out
}

#' Record the observations read from an input data set (for the log NOTE)
#'
#' Returns `d` unchanged.
#' @export
#' @keywords internal
sas_note_read <- function(d, name, where = NULL) {
  rt$reads[[length(rt$reads) + 1L]] <- list(name = name, n = nrow(d), where = where)
  d
}

#' Set the observation counts of the SET data sets to the rows consumed
#' @export
#' @keywords internal
sas_reads_consumed <- function(p) {
  left <- p
  for (k in seq_along(rt$reads)) {
    n <- rt$reads[[k]]$n
    rt$reads[[k]]$n <- min(n, max(0, left))
    left <- left - n
  }
  invisible()
}

read_notes <- function() {
  unlist(lapply(rt$reads, function(r) {
    c(sprintf("NOTE: There were %d observations read from the data set WORK.%s.", as.integer(r$n), toupper(r$name)),
      if (!is.null(r$where)) paste0("      WHERE ", r$where, ";"))
  }))
}

# PROC PRINT reports an empty input differently.
print_read_notes <- function() {
  unlist(lapply(rt$reads, function(r) {
    if (r$n == 0) return(sprintf("NOTE: No observations in data set WORK.%s.", toupper(r$name)))
    c(sprintf("NOTE: There were %d observations read from the data set WORK.%s.", as.integer(r$n), toupper(r$name)),
      if (!is.null(r$where)) paste0("      WHERE ", r$where, ";"))
  }))
}

dataset_note <- function(name, d) {
  sprintf("NOTE: The data set WORK.%s has %d observations and %d variables.", toupper(name), nrow(d), ncol(d))
}

# Variables that are referenced but never given a value (compile-time NOTE).
uninit_vars <- function(an) {
  data_vars <- an$order[vapply(an$order, function(v) identical(an$info[[v]]$kind, "data"), logical(1))]
  assigned <- unique(unlist(lapply(an$stmts, stmt_assigns_all)))
  inputs <- unlist(lapply(an$readers, function(r) {
    if (r$stmt == "input") input_var_names(r) else unlist(lapply(r$datasets, `[[`, "out_names"))
  }))
  in_arrays <- unlist(lapply(an$arrays, `[[`, "elems"))
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
    call = c(unlist(lapply(s$targets, function(t) if (t$node == "var") t$name)),
             unlist(lapply(s$args, function(a) if (identical(a$node, "var")) a$name))),
    input = input_var_names(s),
    NULL
  )
  c(out, unlist(lapply(stmt_children(s), stmt_assigns_all)))
}

# SAS-like text of a WHERE expression (for the "WHERE ...;" line of a NOTE).
where_text <- function(e) {
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
          paste0(w(x$lhs), x$op, w(x$rhs))
        }
      },
      compare = paste0(w(x$lhs), if (x$op == "^=") " not = " else x$op, if (isTRUE(x$colon)) ":" else "", w(x$rhs)),
      chain = paste(vapply(x$args, w, ""), collapse = " "),
      "in" = paste0(w(x$lhs), if (isTRUE(x$negate)) " not in (" else " in (",
                    paste(vapply(x$items, function(it) if (it$node == "range") paste0(it$from, ":", it$to) else w(it), ""),
                          collapse = ", "), ")"),
      between = {
        # constant bounds are shown as a range; expression bounds keep BETWEEN
        const <- function(y) y$node %in% c("num", "str", "missing") || (y$node == "unary" && y$arg$node == "num")
        txt <- if (const(x$lo) && const(x$hi)) {
          paste0("(", w(x$lhs), ">=", w(x$lo), " and ", w(x$lhs), "<=", w(x$hi), ")")
        } else {
          paste0(w(x$lhs), " between ", w(x$lo), " and ", w(x$hi))
        }
        if (isTRUE(x$negate)) paste0("not ", txt) else txt
      },
      contains = paste0(w(x$lhs), if (isTRUE(x$negate)) " not contains " else " contains ", w(x$rhs)),
      is_missing = paste0(w(x$lhs), if (isTRUE(x$negate)) " is not null" else " is null"),
      call = if (x$name == "sqllike") paste0(w(x$args[[1]]), " like ", w(x$args[[2]])) else paste0(toupper(x$name), "(", paste(vapply(x$args, function(a) if (a$node == "of") "of" else w(a), ""), collapse = ", "), ")"),
      "?"
    )
  }
  w(e, TRUE)
}
