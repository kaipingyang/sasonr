# Code generation: DATA step AST -> R code (a character string).
#
# L1/L2 produce a dplyr pipeline over the input (`.d`), L3 produces an explicit
# row loop over a PDV list. Expressions share one generator; only variable
# references differ by mode:
#   vec  bare column names inside dplyr verbs
#   out  `.d$x` after the pipeline (PUT text)
#   row  `pdv$x` inside the L3 loop

cmp_helper <- c("=" = "sas_eq", "^=" = "sas_ne", "<" = "sas_lt", "<=" = "sas_le",
                ">" = "sas_gt", ">=" = "sas_ge")

new_cx <- function(mode, an) {
  cx <- new.env(parent = emptyenv())
  cx$mode <- mode
  cx$an <- an
  cx$n_guard <- 0L
  cx$n_tmp <- 0L
  cx$stages <- list()
  cx$puts <- list()
  cx$sink <- if (is.null(an$file)) ".log" else ".fput"
  cx$ls <- if (is.null(an$file)) 132L else an$file$lrecl
  cx
}

with_mode <- function(cx, mode) {
  cx2 <- new_cx(mode, cx$an)
  cx2
}

var_ref <- function(name, cx) {
  switch(cx$mode,
    vec = r_name(name),
    out = paste0(".d$", r_name(name)),
    row = paste0("pdv$", r_name(name))
  )
}

negate_if <- function(code, negate) if (isTRUE(negate)) paste0("sas_not(", code, ")") else code

# Expressions -------------------------------------------------------------

gen_expr <- function(e, cx) {
  g <- function(x) gen_expr(x, cx)
  switch(e$node,
    num = r_num(e$value),
    str = r_str(if (nzchar(e$value)) e$value else " "),
    missing = if (e$code == ".") "NA_real_" else paste0("sas_missing(", r_str(e$code), ")"),
    var = var_ref(e$name, cx),
    array_ref = gen_array_ref(e, cx),
    unary = switch(e$op,
      "-" = {
        # SAS records the place of a unary minus at the operand's position; the
        # minus itself is not recorded when the operand is the missing result of
        # an arithmetic operation (-(a + b) records the "+")
        at <- if (!is.null(e$arg$col)) at_code(list(line = e$arg$line, col = e$arg$col)) else ""
        is_arith_arg <- e$arg$node == "binary" && e$arg$op %in% c("+", "-", "*", "/", "**")
        if (is_arith_arg && nzchar(at)) at <- paste0(at, ", .gen_a = TRUE")
        if (isTRUE(cx$inexact)) at <- paste0(at, ", .exact = FALSE")
        paste0("sas_neg(", g(e$arg), at, ")")
      },
      "+" = g(e$arg),
      not = paste0("sas_not(", g(e$arg), ")")
    ),
    binary = {
      ct <- ct_diag()
      # constant division by zero is folded at compile time (no _ERROR_ at run time)
      if (e$op == "/" && e$lhs$node == "num" && e$rhs$node == "num" && e$rhs$value == 0) {
        ct$notes <- c(ct$notes, sprintf("NOTE: Division by zero detected during the compilation phase, detected at line %d column %d.",
                                        as.integer(e$line), as.integer(e$col %||% 0L)))
        return("NA_real_")
      }
      arith <- e$op %in% c("+", "-", "*", "/", "**")
      if (arith && e$lhs$node == "num" && e$rhs$node == "num") {
        # compile-time diagnostics of constant operations
        a0 <- e$lhs$value
        b0 <- e$rhs$value
        if (e$op == "**" && a0 == 0 && b0 <= 0) {
          ct$notes <- c(ct$notes, "NOTE: Invalid argument(s) to the exponential operator \"**\".")
        } else {
          v0 <- switch(e$op, "+" = a0 + b0, "-" = a0 - b0, "*" = a0 * b0, "/" = a0 / b0, "**" = a0^b0)
          if (is.infinite(v0)) {
            ct$notes <- c(ct$notes, sprintf("NOTE: A number has become too large during the compilation phase, detected at line %d column %d. ",
                                            as.integer(e$line), as.integer(e$col %||% 0L)))
          }
        }
      }
      # an operation on constants with a missing operand is folded as well
      if (arith && e$lhs$node %in% c("num", "missing") && e$rhs$node %in% c("num", "missing") &&
          (e$lhs$node == "missing" || e$rhs$node == "missing")) {
        ct$mv <- c(ct$mv, at_key(c(e$line, e$col %||% NA_integer_)))
        return("NA_real_")
      }
      a <- g(e$lhs)
      b <- g(e$rhs)
      fun <- switch(e$op,
        "/" = "sas_div", "**" = "sas_pow", "||" = "sas_concat", and = "sas_and",
        or = "sas_or", min = "sas_min_op", max = "sas_max_op", NA_character_
      )
      at <- if (arith && !is.null(e$col)) at_code(e) else ""
      # vectorised code evaluates guarded expressions for every row, so the
      # missing-value counts there are not exact (the engine re-runs as L3)
      if (arith && isTRUE(cx$inexact)) at <- paste0(at, ", .exact = FALSE")
      # an operand that is itself an arithmetic result: its missing values were already
      # counted, SAS does not count this operation again (a + b + c with b missing: 1 place).
      # Division and exponentiation need the flags as well: (a + b) / 3 is one place,
      # the division is not counted again.
      is_arith <- function(x) x$node == "binary" && x$op %in% c("+", "-", "*", "/", "**")
      if ((is.na(fun) || fun %in% c("sas_div", "sas_pow")) && nzchar(at)) {
        if (is_arith(e$lhs)) at <- paste0(at, ", .gen_a = TRUE")
        if (is_arith(e$rhs)) at <- paste0(at, ", .gen_b = TRUE")
      }
      if (is.na(fun)) paste0("sas_arith(", a, ", ", b, ", ", r_str(e$op), at, ")") else paste0(fun, "(", a, ", ", b, at, ")")
    },
    compare = if (isTRUE(e$colon)) {
      sprintf("sas_cmp_trunc(%s, %s, %s)", r_str(e$op), g(e$lhs), g(e$rhs))
    } else {
      sprintf("%s(%s, %s)", cmp_helper[[e$op]], g(e$lhs), g(e$rhs))
    },
    chain = sprintf("sas_chain(list(%s), %s)", paste(vapply(e$args, g, ""), collapse = ", "), r_chr_vec(e$ops)),
    "in" = negate_if(sprintf("sas_in(%s, %s)", g(e$lhs), gen_in_values(e)), e$negate),
    between = {
      x <- g(e$lhs)
      negate_if(sprintf("sas_and(sas_ge(%s, %s), sas_le(%s, %s))", x, g(e$lo), x, g(e$hi)), e$negate)
    },
    contains = negate_if(sprintf("as.numeric(sas_f_index(%s, sas_rtrim(%s)) > 0)", g(e$lhs), g(e$rhs)), e$negate),
    is_missing = negate_if(sprintf("sas_f_missing(%s)", g(e$lhs)), e$negate),
    call = gen_call(e, cx),
    omitted = "NULL",
    abort_unsupported(paste("expression node", e$node), e$line)
  )
}

gen_in_values <- function(e) {
  vals <- unlist(lapply(e$items, function(it) {
    switch(it$node,
      num = r_num(it$value),
      str = r_str(it$value),
      missing = if (it$code == ".") "NA_real_" else paste0("sas_missing(", r_str(it$code), ")"),
      range = {
        if (it$to - it$from > 10000) abort_unsupported("IN range wider than 10000 values", e$line)
        vapply(seq(it$from, it$to), r_num, "")
      }
    )
  }))
  paste0("c(", paste(vals, collapse = ", "), ")")
}

at_code <- function(e) sprintf(", .at = c(%dL, %dL)", as.integer(e$line), as.integer(e$col %||% NA_integer_))

fmt_lit <- function(fmt) r_lit(fmt[c("name", "w", "d", "char", "text")])

# VNAME(), VLENGTH() ... : compile-time facts about a variable (or an array element).
gen_var_info <- function(e, cx) {
  an <- cx$an
  a <- e$args[[1]]
  info_of <- function(v) {
    inf <- an$info[[v]]
    fmt <- an$formats[[v]]
    char <- identical(inf$type, "char")
    len <- if (char) inf$len else 8L
    fmt_text <- if (!is.null(fmt)) toupper(fmt$text) else if (char) paste0("$", len, ".") else "BEST12."
    switch(e$name,
      vname = r_str(inf$disp %||% v),
      vlength = r_num(len),
      vtype = r_str(if (char) "C" else "N"),
      vlabel = r_str(if (!is.na(an$labels[v] %|NA|% NA)) an$labels[[v]] else (inf$disp %||% v)),
      vformat = r_str(fmt_text),
      vformatn = r_str(if (!is.null(fmt)) toupper(paste0(if (fmt$char) "$", fmt$name)) else if (char) "$" else "BEST"),
      vformatw = r_num(if (!is.null(fmt) && !is.na(fmt$w)) fmt$w else if (char) len else 12),
      vformatd = r_num(if (!is.null(fmt) && !is.na(fmt$d)) fmt$d else 0),
      vinformat = r_str(if (char) paste0("$", len, ".") else "BEST32."),
      vvalue = sprintf("sas_rtrim(sas_put_value(%s%s))", var_ref(v, cx), fmt_code(fmt))
    )
  }
  if (a$node == "var") return(info_of(a$name))
  arr <- an$arrays[[a$name]]
  if (arr$temporary) abort_unsupported(paste0(toupper(e$name), " of a temporary array element"), e$line)
  if (cx$mode != "row") abort_unsupported("array reference in vectorised code", e$line)
  idx <- sprintf("sas_idx(%s, %dL, %dL)", gen_expr(a$index[[1]], cx), as.integer(arr$lo), as.integer(arr$dim))
  if (e$name == "vvalue") return(sprintf("sas_rtrim(sas_put_value(%s))", gen_array_ref(a, cx)))
  sprintf("c(%s)[%s]", paste(vapply(arr$elems, info_of, ""), collapse = ", "), idx)
}

gen_call <- function(e, cx) {
  if (e$name %in% c("dim", "lbound", "hbound")) {
    arr <- cx$an$arrays[[e$args[[1]]$name]]
    return(switch(e$name, dim = paste0(arr$dim, "L"), lbound = paste0(as.integer(arr$lo), "L"),
                  hbound = paste0(as.integer(arr$lo + arr$dim - 1L), "L")))
  }
  if (e$name %in% var_info_fns) return(gen_var_info(e, cx))
  if (e$name == "put") {
    return(sprintf("sas_f_put(%s, %s%s)", gen_expr(e$args[[1]], cx), fmt_lit(e$fmt),
                   if (is.null(e$align)) "" else paste0(", align = ", r_str(e$align))))
  }
  if (e$name == "input") {
    return(sprintf("sas_f_input(%s, %s%s%s)", gen_expr(e$args[[1]], cx), fmt_lit(e$fmt),
                   if (nzchar(e$qq)) paste0(", qq = ", r_str(e$qq)) else "",
                   if (cx$mode == "row" && !is.null(e$col)) at_code(e) else ""))
  }
  if (e$name == "exist") {
    # EXIST(member <, type>): the generated DATA step environment carries the
    # WORK library (`work`, see engine_eval()); SASHELP is read from inst/sashelp
    a1 <- gen_expr(e$args[[1]], cx)
    a2 <- if (length(e$args) > 1) gen_expr(e$args[[2]], cx) else "NULL"
    return(sprintf("sas_f_exist(%s, %s, work)", a1, a2))
  }
  args <- unlist(lapply(e$args, function(a) {
    if (a$node == "of") vapply(a$vars, var_ref, "", cx = cx) else gen_expr(a, cx)
  }))
  if (!is.null(e$lag)) {
    x <- args[1]
    n <- e$lag$n
    if (cx$mode == "row") {
      fun <- if (e$lag$kind == "lag") "sas_lag" else "sas_dif"
      return(sprintf("%s(.lags, \"site%d\", %s, %dL)", fun, e$site, x, n))
    }
    if (e$lag$kind == "lag") return(sprintf("sas_lag_vec(%s, %dL)", x, n))
    return(sprintf("(%s - sas_lag_vec(%s, %dL))", x, x, n))
  }
  if (e$name == "symget") args <- c(".symtab", args)
  in_sql <- identical(cx$an$context, "sql")
  if (e$name %in% names(fn_policy) && (!is.null(e$col) || in_sql)) {
    # missing / invalid arguments: log NOTEs and _ERROR_ as in SAS (fn_policy.R).
    # In PROC SQL the expressions carry no line/column; the invalid-argument
    # NOTE of PROC SQL does not use them either (diag.R).
    at_arg <- if (!is.null(e$col)) sprintf(", .at = c(%dL, %dL)", as.integer(e$line), as.integer(e$col)) else ""
    return(sprintf("sas_fcall(\"%s\", sas_f_%s%s%s%s)", e$name, e$name,
                   paste0(", ", args, collapse = ""), at_arg,
                   if (isTRUE(cx$inexact)) ", .exact = FALSE" else ""))
  }
  at <- if (cx$mode == "row" && e$name %in% err_fns && !is.null(e$col)) at_code(e) else ""
  if (e$name == "catx") at <- paste0(at, sprintf(", .len = %dL", as.integer(e$buf_len %||% 200L)))
  if (e$name %in% c("cat", "cats", "catt")) at <- paste0(at, sprintf(", .len = %dL", as.integer(e$buf_len %||% 200L)))
  if (e$name %in% mv_note_fns && !is.null(e$col) && length(args) > 0) {
    # count missing arguments once, then call with the evaluated arguments
    extra <- if (nzchar(at)) sprintf(", list(%s)", sub("^, ", "", at)) else ""
    return(sprintf("do.call(sas_f_%s, c(sas_mv_args(c(%dL, %dL), %s%s)%s))", e$name, as.integer(e$line),
                   as.integer(e$col), paste(args, collapse = ", "), if (isTRUE(cx$inexact)) ", .exact = FALSE" else "", extra))
  }
  call <- sprintf("sas_f_%s(%s%s)", e$name, paste(args, collapse = ", "), at)
  if (e$name %in% mv_result_fns && !is.null(e$col)) {
    call <- sprintf("sas_mv_res(%s, c(%dL, %dL)%s)", call, as.integer(e$line), as.integer(e$col),
                    if (isTRUE(cx$inexact)) ", .exact = FALSE" else "")
  }
  call
}

# Can evaluating this expression raise a SAS data error (sets _ERROR_)?
expr_may_error <- function(e) {
  if (is.null(e)) return(FALSE)
  hit <- FALSE
  walk_expr(e, function(x) {
    if (x$node == "call" && (x$name %in% err_fns || x$name %in% names(fn_policy))) hit <<- TRUE
    if (x$node == "binary" && x$op %in% c("/", "**", "*", "+", "-")) hit <<- TRUE
  })
  hit
}

stmt_may_error <- function(s) {
  if (s$stmt %in% c("input", "substr_assign")) return(TRUE)
  any(vapply(Filter(Negate(is.null), stmt_exprs(s)), expr_may_error, logical(1)))
}

gen_array_ref <- function(e, cx) {
  if (cx$mode != "row") abort_unsupported("array reference in vectorised code", e$line)
  arr <- cx$an$arrays[[e$name]]
  idx <- sprintf("sas_idx(%s, %dL, %dL)", gen_expr(e$index[[1]], cx), as.integer(arr$lo), as.integer(arr$dim))
  if (arr$temporary) sprintf(".ta_%s[%s]", arr$name, idx) else sprintf("pdv[[.arr_%s[%s]]]", arr$name, idx)
}

coerce_to <- function(code, type, len) {
  if (type == "char") sprintf("sas_char(%s, %dL)", code, as.integer(len)) else code
}

coerce_var <- function(code, name, an) {
  inf <- an$info[[name]]
  coerce_to(code, inf$type, inf$len)
}

proto_code <- function(inf, use_init = TRUE) {
  init <- if (use_init) inf$init else NULL
  if (inf$type == "char") {
    return(r_str(sas_char(if (is.null(init)) "" else init, inf$len)))
  }
  if (is.null(init)) return("NA_real_")
  code <- sas_miss_code(init)
  if (!is.na(code) && code != 1L) return(sprintf("sas_missing(%s)", r_str(paste0(".", miss_label(code)))))
  r_num(init)
}

fmt_code <- function(fmt) {
  if (is.null(fmt)) return("")
  paste0(", ", r_lit(fmt[c("name", "w", "d", "char", "text")]))
}

put_line_code <- function(s, cx) {
  if (any(vapply(s$items, function(it) it$kind == "ptr", logical(1)))) return(put_ptr_code(s, cx))
  items <- vapply(s$items, function(it) {
    if (it$kind == "lit") return(r_str(it$text))
    fmt <- it$fmt %||% cx$an$formats[[it$name]]
    ref <- var_ref(it$name, cx)
    if (it$kind == "named") {
      disp <- cx$an$info[[it$name]]$disp %||% it$name
      sprintf("sas_put_named(%s, %s%s)", r_str(disp), ref, fmt_code(fmt))
    } else {
      if (!is.null(it$fmt)) sprintf("sas_put_fvalue(%s%s)", ref, fmt_code(fmt)) else sprintf("sas_put_value(%s%s)", ref, fmt_code(fmt))
    }
  }, "")
  # literals and formatted (unnamed, explicit format) items add no trailing blank
  lit <- vapply(s$items, function(it) it$kind == "lit" || (it$kind == "value" && !is.null(it$fmt)), logical(1))
  ls <- if (identical(cx$ls, 132L) || is.null(cx$ls)) "" else paste0(", .ls = ", cx$ls, "L")
  if (!any(lit)) return(paste0("sas_put_line(", paste(items, collapse = ", "), ls, ")"))
  paste0("sas_put_line(", paste(items, collapse = ", "), ", .lit = ", r_lit(lit), ls, ")")
}

# PUT with pointer controls: every item becomes a positioned piece of text.
put_ptr_code <- function(s, cx) {
  pieces <- vapply(s$items, function(it) {
    if (it$kind == "ptr") {
      if (it$k == "next") return("list(k = \"next\")")
      return(sprintf("list(k = %s, v = %s)", r_str(it$k), gen_expr(it$v, cx)))
    }
    if (it$kind == "lit") return(sprintf("list(k = \"text\", s = %s, blank = FALSE)", r_str(it$text)))
    fmt <- it$fmt %||% cx$an$formats[[it$name]]
    ref <- var_ref(it$name, cx)
    if (it$kind == "named") {
      disp <- cx$an$info[[it$name]]$disp %||% it$name
      return(sprintf("list(k = \"text\", s = sas_put_named(%s, %s%s), blank = TRUE)", r_str(disp), ref, fmt_code(fmt)))
    }
    if (!is.null(it$fmt)) return(sprintf("list(k = \"text\", s = sas_put_fvalue(%s%s), blank = FALSE)", ref, fmt_code(fmt)))
    sprintf("list(k = \"text\", s = sas_put_value(%s%s), blank = TRUE)", ref, fmt_code(fmt))
  }, "")
  paste0("sas_put_layout(list(", paste(pieces, collapse = ", "), "))")
}

# Input sources (shared by L1/L2 pipelines and the L3 loop) ---------------

ds_code <- function(d, an, obs_col = FALSE, wheres = list()) {
  o <- d$options
  args <- c("work", r_str(d$name))
  if (!is.null(o$keep) || !is.null(o$drop)) args <- c(args, paste0("keep = ", r_chr_vec(d$vars)))
  if (!is.null(o$rename)) {
    args <- c(args, paste0("rename = c(", paste0(r_str(names(o$rename)), " = ", r_str(o$rename), collapse = ", "), ")"))
  }
  code <- sprintf("sas_ds(%s)", paste(args, collapse = ", "))
  if (obs_col) code <- paste0(code, " |> dplyr::mutate(.obs = dplyr::row_number())")
  if (!is.null(o$where)) {
    code <- sprintf("%s |> dplyr::filter(sas_true(%s))", code, gen_expr(o$where, new_cx("vec", an)))
  }
  # WHERE statement conditions apply to every input data set without a WHERE=
  # option (the data set option takes precedence), before FIRSTOBS=/OBS=
  if (length(wheres) > 0 && is.null(o$where)) {
    cx <- new_cx("vec", an)
    conds <- vapply(wheres, function(w) sprintf("sas_true(%s)", gen_expr(w, cx)), "")
    code <- sprintf("%s |> dplyr::filter(%s)", code, paste(conds, collapse = " & "))
  }
  # FIRSTOBS= / OBS= count observations that satisfy WHERE= and WHERE
  if (!is.null(o$firstobs) || !is.null(o$obs)) {
    code <- sprintf("%s |> sas_obs_window(%s, %s)", code,
                    if (is.null(o$firstobs)) "NULL" else r_num(o$firstobs), if (is.null(o$obs)) "NULL" else r_num(o$obs))
  }
  # values from a data set whose character variable is longer than the PDV
  # variable (defined by an earlier data set) are truncated
  m <- an$meta[[d$name]]
  if (!is.null(an$info) && !is.null(m)) {
    tr <- list()
    for (j in seq_along(d$vars)) {
      inf <- an$info[[d$out_names[j]]]
      ml <- m$len[[d$vars[j]]]
      if (identical(m$type[[d$vars[j]]], "char") && identical(inf$type, "char") && !is.null(ml) && ml > inf$len) {
        tr[[d$out_names[j]]] <- as.integer(inf$len)
      }
    }
    if (length(tr) > 0) code <- sprintf("%s |> sas_trunc_chars(%s)", code, r_lit(tr))
  }
  wexpr <- o$where %||% (if (length(wheres) > 0) wheres[[length(wheres)]])
  sprintf("%s |> sas_note_read(%s%s)", code, r_str(d$name),
          if (is.null(wexpr)) "" else paste0(", where = ", r_str(where_text(wexpr))))
}

# One INPUT statement as a run-time spec for sas_input_stmt().
input_spec_code <- function(rd, an) {
  rcx <- new_cx("row", an)
  ptr <- function(v) {
    if (v$node %in% c("num", "str")) return(if (v$node == "num") r_num(v$value) else r_str(v$value))
    paste0("function(pdv) ", gen_expr(v, rcx))
  }
  items <- vapply(rd$items, function(it) {
    if (it$k != "var") {
      return(sprintf("list(k = %s%s)", r_str(it$k), if (is.null(it$v)) "" else paste0(", v = ", ptr(it$v))))
    }
    inf <- an$info[[it$name]]
    char <- identical(inf$type, "char")
    fields <- c(
      paste0("name = ", r_str(it$name)), paste0("disp = ", r_str(inf$disp %||% it$name)),
      paste0("char = ", char), paste0("len = ", if (char) paste0(inf$len, "L") else "NA_integer_"),
      paste0("mode = ", r_str(it$mode)),
      if (!is.null(it$fmt)) paste0("fmt = ", fmt_lit(it$fmt)),
      if (it$mode == "column") sprintf("from = %dL, to = %dL", it$from, it$to),
      if (!is.null(it$cd)) sprintf("cd = %dL", it$cd),
      if (isTRUE(it$amp)) "amp = TRUE", if (isTRUE(it$tilde)) "tilde = TRUE",
      if (nzchar(it$qq %||% "")) paste0("qq = ", r_str(it$qq))
    )
    paste0("list(k = \"var\", ", paste(fields, collapse = ", "), ")")
  }, "")
  sprintf("list(items = list(\n    %s\n  ), hold = %s)", paste(items, collapse = ",\n    "), r_str(rd$hold %||% ""))
}

# Code that opens the input stream of a step (INFILE or DATALINES).
infile_open_code <- function(an) {
  inf <- an$infile
  opts <- if (is.null(inf)) list() else inf$opts
  n_lines <- max(c(1L, unlist(lapply(an$readers, function(r) {
    if (r$stmt != "input") return(NULL)
    unlist(lapply(r$items, function(it) if (it$k == "line" && it$v$node == "num") as.integer(it$v$value)))
  }))))
  ropts <- list(dlm = opts$dlm, dsd = opts$dsd, mode = opts$mode, firstobs = opts$firstobs, obs = opts$obs, ls = opts$ls,
                pad = opts$pad, lines = max(n_lines, opts$lines %||% 1L))
  ropts <- Filter(Negate(is.null), ropts)
  if (!is.null(inf) && identical(inf$source$kind, "file")) {
    return(sprintf("sas_infile_open(sas_read_file(%s), opts = %s, instream = FALSE)", r_str(inf$source$path), r_lit(ropts)))
  }
  dl <- an$dl
  if (is.null(dl)) abort_unsupported("INFILE DATALINES without a DATALINES block", inf$line)
  ropts$end_text <- if (isTRUE(dl$four)) ";;;;" else ";"
  sprintf("sas_infile_open(%s, %s, opts = %s)", r_chr_vec(dl$lines),
          if (length(dl$line_no) == 0) "integer()" else paste0("c(", paste0(dl$line_no, "L", collapse = ", "), ")"),
          r_lit(ropts))
}

source_code <- function(an, rd = if (length(an$readers) > 0) an$readers[[1]] else NULL) {
  if (is.null(rd)) return("tibble::tibble(.rows = 1L)")
  if (rd$stmt == "input") {
    end_var <- an$infile$opts$end
    return(sprintf("sas_read_input(%s, %s%s)", infile_open_code(an), input_spec_code(rd, an),
                   if (is.null(end_var)) "" else paste0(", end_var = ", r_str(end_var))))
  }
  codes <- vapply(rd$datasets, ds_code, "", an = an, wheres = an$wheres)
  in_vars <- vapply(rd$datasets, function(d) d$options$in_var %||% NA_character_, "")
  if (length(codes) == 1 && all(is.na(in_vars))) return(codes)
  by <- ""
  if (!is.null(an$by) && length(codes) > 1) {
    by <- sprintf(", by = %s, descending = %s", r_chr_vec(an$by$vars), r_lit(an$by$descending))
  }
  sprintf("sas_set_input(list(%s), in_vars = %s%s)", paste(codes, collapse = ", "), r_lit(in_vars), by)
}

input_columns <- function(an) {
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  if (is.null(rd)) return(character())
  if (rd$stmt == "input") return(unique(unlist(lapply(an$readers, function(r) if (r$stmt == "input") input_var_names(r)))))
  c(unlist(lapply(rd$datasets, `[[`, "out_names")),
    unlist(lapply(rd$datasets, function(d) d$options$in_var)))
}

# Stages common to L1/L2/L3 input preparation: WHERE, _N_, END=, FIRST./LAST.
add_input_stages <- function(cx, include_n = TRUE) {
  an <- cx$an
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  if (include_n && "_n_" %in% an$order) add_stage(cx, "mutate", "`_n_` = as.numeric(dplyr::row_number())")
  if (!is.null(rd) && !is.null(rd$end) && rd$stmt == "set") {
    add_stage(cx, "mutate", sprintf("%s = as.numeric(dplyr::row_number() == dplyr::n())", r_name(rd$end)))
  }
  if (!is.null(an$by) && !is.null(rd) && rd$stmt == "set") {
    add_stage(cx, "raw", sprintf("sas_check_sorted(%s, %s)", r_chr_vec(an$by$vars), r_lit(an$by$descending)))
    flags <- character()
    for (k in seq_along(an$by$vars)) {
      keys <- paste(r_name(an$by$vars[seq_len(k)]), collapse = ", ")
      v <- an$by$vars[k]
      flags <- c(flags,
        sprintf("%s = sas_first(%s)", r_name(paste0("first.", v)), keys),
        sprintf("%s = sas_last(%s)", r_name(paste0("last.", v)), keys))
    }
    add_stage(cx, "mutate", flags)
  }
}

add_stage <- function(cx, kind, code) {
  cx$stages <- c(cx$stages, list(list(kind = kind, code = code)))
}

render_pipeline <- function(target, src, stages) {
  merged <- list()
  for (s in stages) {
    k <- length(merged)
    if (s$kind == "mutate" && k > 0 && merged[[k]]$kind == "mutate") {
      merged[[k]]$code <- c(merged[[k]]$code, s$code)
    } else {
      merged <- c(merged, list(s))
    }
  }
  texts <- vapply(merged, function(s) {
    switch(s$kind,
      mutate = paste0("dplyr::mutate(\n", paste0("    ", s$code, collapse = ",\n"), "\n  )"),
      filter = paste0("dplyr::filter(", s$code, ")"),
      raw = s$code
    )
  }, "")
  paste0(target, " <- ", paste(c(src, texts), collapse = " |>\n  "))
}

# L1 / L2: vectorised pipeline -------------------------------------------

new_guard <- function(cx) {
  cx$n_guard <- cx$n_guard + 1L
  paste0(".g", cx$n_guard)
}

and_guard <- function(guard, cond) if (is.null(guard)) cond else paste(guard, "&", cond)

gen_vec_step <- function(an) {
  cx <- new_cx("vec", an)
  add_input_stages(cx)
  # Variables created by the step start missing (or at their RETAIN value).
  inputs <- input_columns(an)
  init_vars <- Filter(function(v) {
    inf <- an$info[[v]]
    inf$kind == "data" && !v %in% inputs && !v %in% names(an$sum_plan)
  }, an$order)
  if (length(init_vars) > 0) {
    add_stage(cx, "mutate", vapply(init_vars, function(v) paste0(r_name(v), " = ", proto_code(an$info[[v]])), ""))
  }
  gen_vec_block(an$stmts, NULL, cx, top = TRUE)

  out <- render_pipeline(".d", source_code(an), cx$stages)
  out <- c(out, vec_put_code(cx), vec_output_code(an))
  out
}

vec_put_code <- function(cx) {
  if (length(cx$puts) == 0) return(character())
  lines <- vapply(cx$puts, `[[`, "", "line")
  guards <- vapply(cx$puts, function(p) if (is.null(p$guard)) "TRUE" else paste0(".d$", p$guard), "")
  sink <- cx$sink
  if (length(lines) == 1 && guards == "TRUE") return(sprintf("%s <- c(%s, %s)", sink, sink, lines))
  sprintf("%s <- c(%s, sas_put_rows(\n  list(%s),\n  list(%s)\n))", sink, sink,
          paste(lines, collapse = ",\n       "), paste(guards, collapse = ", "))
}

select_code <- function(o) {
  cols <- ifelse(o$vars == o$out_names, r_name(o$vars), paste0(r_name(o$out_names), " = ", r_name(o$vars)))
  paste0("dplyr::select(", paste(cols, collapse = ", "), ")")
}

vec_output_code <- function(an) {
  if (length(an$outputs) == 0) return(character())
  o <- an$outputs[[1]]
  stages <- character()
  if (!is.null(an$output_pattern$cond)) {
    stages <- c(stages, sprintf("dplyr::filter(sas_true(%s))", r_name(an$output_pattern$cond)))
  }
  stages <- c(stages, select_code(o))
  paste0("work$", r_name(o$name), " <- ", paste(c(".d", stages), collapse = " |>\n  "))
}

gen_vec_block <- function(stmts, guard, cx, top = FALSE) {
  for (i in seq_along(stmts)) {
    if (top && i %in% cx$an$skip_top) next
    if (top && gen_vec_carry(i, cx)) next
    gen_vec_stmt(stmts[[i]], guard, cx)
  }
}

# Carry-forward assignments (see carry_plan_for()): each assignment records its
# value and firing flag where it stands; the last one fills the column forward.
gen_vec_carry <- function(i, cx) {
  an <- cx$an
  hit <- FALSE
  for (v in names(an$carry_plan)) {
    plan <- an$carry_plan[[v]]
    k <- which(vapply(plan$sets, `[[`, 0L, "idx") == i)
    if (length(k) == 0) next
    hit <- TRUE
    set <- plan$sets[[k]]
    cv <- sprintf(".cv_%s_%d", v, seq_along(plan$sets))
    cs <- sprintf(".cs_%s_%d", v, seq_along(plan$sets))
    cond <- if (is.null(set$cond)) "TRUE" else sprintf("sas_true(%s)", gen_expr(set$cond, cx))
    cx$inexact <- !is.null(set$cond)
    val <- coerce_var(gen_expr(set$value, cx), v, an)
    cx$inexact <- FALSE
    add_stage(cx, "mutate", c(
      paste0(r_name(cv[k]), " = ", val),
      paste0(r_name(cs[k]), " = ", cond)
    ))
    if (i == plan$last) {
      add_stage(cx, "mutate", sprintf("%s = sas_carry(list(%s), list(%s), %s)", r_name(v),
        paste(r_name(cv), collapse = ", "), paste(r_name(cs), collapse = ", "), proto_code(an$info[[v]])))
    }
  }
  hit
}

assign_code <- function(v, val, guard) {
  if (is.null(guard)) return(paste0(r_name(v), " = ", val))
  sprintf("%s = dplyr::if_else(%s, %s, %s)", r_name(v), guard, val, r_name(v))
}

gen_vec_stmt <- function(s, guard, cx) {
  an <- cx$an
  old <- cx$inexact
  cx$inexact <- isTRUE(old) || !is.null(guard)
  on.exit(cx$inexact <- old)
  switch(s$stmt,
    assign = {
      v <- s$target$name
      add_stage(cx, "mutate", assign_code(v, coerce_var(gen_expr(s$value, cx), v, an), guard))
    },
    sum = {
      v <- s$target$name
      plan <- an$sum_plan[[v]]
      e <- gen_expr(s$value, cx)
      if (is.null(plan$reset_var)) {
        add_stage(cx, "mutate", sprintf("%s = sas_cumsum(%s, %s, dplyr::n())", r_name(v), e, r_num(plan$init)))
      } else {
        add_stage(cx, "raw", sprintf("dplyr::group_by(.grp = cumsum(%s))", r_name(plan$reset_var)))
        add_stage(cx, "mutate", sprintf("%s = sas_cumsum(%s, %s, dplyr::n())", r_name(v), e, r_num(plan$reset_value)))
        add_stage(cx, "raw", "dplyr::ungroup()")
      }
    },
    substr_assign = {
      v <- s$var
      at <- sprintf(", .at = c(%dL, %dL)", as.integer(s$line), as.integer(s$col %||% NA_integer_))
      val <- sprintf("sas_substr_assign(%s, %s, %s, %s%s)", r_name(v), gen_expr(s$pos, cx),
                     if (is.null(s$len)) "NULL" else gen_expr(s$len, cx), gen_expr(s$value, cx), at)
      add_stage(cx, "mutate", assign_code(v, val, guard))
    },
    "if" = gen_vec_if(s, guard, cx),
    subset_if = {
      cond <- sprintf("sas_true(%s)", gen_expr(s$cond, cx))
      add_stage(cx, "filter", if (is.null(guard)) cond else sprintf("!(%s & !%s)", guard, cond))
    },
    delete = add_stage(cx, "filter", if (is.null(guard)) "FALSE" else paste0("!", guard)),
    do = gen_vec_block(s$body, guard, cx),
    select = gen_vec_select(s, guard, cx),
    put = cx$puts <- c(cx$puts, list(list(guard = guard, line = put_line_code(s, with_mode(cx, "out"))))),
    NULL
  )
  invisible()
}

# Collect an IF / ELSE IF chain whose branches all assign one variable.
if_chain <- function(s) {
  conds <- list()
  vals <- list()
  cur <- s
  repeat {
    th <- cur$then
    if (th$stmt != "assign" || th$target$node != "var") return(NULL)
    conds <- c(conds, list(cur$cond))
    vals <- c(vals, list(th))
    e <- cur[["else"]]
    if (is.null(e)) break
    if (e$stmt == "if") {
      cur <- e
      next
    }
    if (e$stmt == "assign" && e$target$node == "var") return(list(conds = conds, vals = vals, default = e))
    return(NULL)
  }
  list(conds = conds, vals = vals, default = NULL)
}

case_when_code <- function(target, conds, vals, default, guard) {
  parts <- paste0(vapply(conds, function(c) and_guard(guard, c), ""), " ~ ", vals)
  if (!is.null(default)) {
    if (is.null(guard)) {
      parts <- c(parts, paste0(".default = ", default))
    } else {
      parts <- c(parts, paste0(guard, " ~ ", default), paste0(".default = ", r_name(target)))
    }
  } else {
    parts <- c(parts, paste0(".default = ", r_name(target)))
  }
  sprintf("%s = dplyr::case_when(\n      %s\n    )", r_name(target), paste(parts, collapse = ",\n      "))
}

gen_vec_if <- function(s, guard, cx) {
  an <- cx$an
  ch <- if_chain(s)
  targets <- unique(vapply(c(ch$vals, if (!is.null(ch$default)) list(ch$default)), function(a) a$target$name, ""))
  if (!is.null(ch) && length(targets) == 1) {
    v <- targets
    old <- cx$inexact
    on.exit(cx$inexact <- old)
    conds <- vapply(seq_along(ch$conds), function(k) {
      if (k > 1) cx$inexact <- TRUE
      sprintf("sas_true(%s)", gen_expr(ch$conds[[k]], cx))
    }, "")
    cx$inexact <- TRUE
    vals <- vapply(ch$vals, function(a) coerce_var(gen_expr(a$value, cx), v, an), "")
    default <- if (is.null(ch$default)) NULL else coerce_var(gen_expr(ch$default$value, cx), v, an)
    if (length(conds) == 1 && is.null(default)) {
      add_stage(cx, "mutate", assign_code(v, vals, and_guard(guard, conds)))
    } else {
      add_stage(cx, "mutate", case_when_code(v, conds, vals, default, guard))
    }
    return(invisible())
  }
  g <- new_guard(cx)
  add_stage(cx, "mutate", paste0(g, " = ", and_guard(guard, sprintf("sas_true(%s)", gen_expr(s$cond, cx)))))
  gen_vec_stmt(s$then, g, cx)
  if (!is.null(s[["else"]])) {
    g2 <- new_guard(cx)
    add_stage(cx, "mutate", paste0(g2, " = ", and_guard(guard, paste0("!", g))))
    gen_vec_stmt(s[["else"]], g2, cx)
  }
}

when_cond_code <- function(w, sel_code, cx) {
  vals <- vapply(w$values, gen_expr, "", cx = cx)
  if (!is.null(sel_code)) return(sprintf("sas_true(sas_in(%s, list(%s)))", sel_code, paste(vals, collapse = ", ")))
  if (length(vals) == 1) return(sprintf("sas_true(%s)", vals))
  paste0("(", paste(sprintf("sas_true(%s)", vals), collapse = " | "), ")")
}

gen_vec_select <- function(s, guard, cx) {
  an <- cx$an
  bodies <- c(lapply(s$whens, `[[`, "body"), list(s$otherwise))
  all_assign <- all(vapply(bodies, function(b) b$stmt == "assign" && b$target$node == "var", logical(1)))
  sel_code <- if (is.null(s$expr)) NULL else gen_expr(s$expr, cx)
  old <- cx$inexact
  on.exit(cx$inexact <- old)
  cx$inexact <- TRUE
  if (all_assign && length(unique(vapply(bodies, function(b) b$target$name, ""))) == 1) {
    v <- bodies[[1]]$target$name
    conds <- vapply(s$whens, when_cond_code, "", sel_code = sel_code, cx = cx)
    vals <- vapply(s$whens, function(w) coerce_var(gen_expr(w$body$value, cx), v, an), "")
    default <- coerce_var(gen_expr(s$otherwise$value, cx), v, an)
    add_stage(cx, "mutate", case_when_code(v, conds, vals, default, guard))
    return(invisible())
  }
  if (!is.null(sel_code)) {
    cx$n_tmp <- cx$n_tmp + 1L
    tmp <- paste0(".s", cx$n_tmp)
    add_stage(cx, "mutate", paste0(tmp, " = ", sel_code))
    sel_code <- tmp
  }
  rem <- guard
  for (w in s$whens) {
    g <- new_guard(cx)
    add_stage(cx, "mutate", paste0(g, " = ", and_guard(rem, when_cond_code(w, sel_code, cx))))
    gen_vec_stmt(w$body, g, cx)
    r <- new_guard(cx)
    add_stage(cx, "mutate", paste0(r, " = ", and_guard(rem, paste0("!", g))))
    rem <- r
  }
  gen_vec_stmt(s$otherwise, rem, cx)
}

# L3: explicit PDV loop ---------------------------------------------------

has_stmt <- function(stmts, kind) {
  any(vapply(stmts, function(s) s$stmt %in% kind || has_stmt(stmt_children(s), kind), logical(1)))
}

gen_row_step <- function(an) {
  cx <- new_cx("row", an)
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  L <- character()

  has_input <- any(vapply(an$readers, function(r) r$stmt == "input", logical(1)))
  if (has_input) L <- c(L, paste0(".inf <- ", infile_open_code(an)))
  set_rd <- Find(function(r) r$stmt == "set", an$readers)
  if (!is.null(set_rd)) {
    pcx <- new_cx("vec", an)
    if (identical(rd, set_rd)) add_input_stages(pcx, include_n = FALSE)
    L <- c(L, render_pipeline(".in", source_code(an, set_rd), pcx$stages),
           ".rows <- sas_rows(.in)", ".n_in <- length(.rows)", ".p <- 0L")
  }
  if (!is.null(rd) && rd$stmt == "merge") {
    codes <- vapply(rd$datasets, ds_code, "", an = an, wheres = an$wheres)
    merged_vars <- unique(unlist(lapply(rd$datasets, `[[`, "out_names")))
    L <- c(L,
      paste0(".m_in <- list(\n  ", paste(codes, collapse = ",\n  "), "\n)"),
      ".m <- lapply(.m_in, sas_rows)",
      sprintf(".plan <- sas_merge_plan(.m_in, %s, %s)", r_chr_vec(an$by$vars), r_lit(an$by$descending)),
      ".n_in <- nrow(.plan)", ".p <- 0L"
    )
    cx$merge_reset <- merged_vars
  }
  for (arr in an$arrays) {
    if (arr$temporary) {
      miss <- if (arr$type == "char") r_str(strrep(" ", arr$len)) else "NA_real_"
      vals <- vapply(seq_len(arr$dim), function(k) {
        if (!is.null(arr$init) && k <= length(arr$init)) {
          if (arr$type == "char") r_str(sas_char(arr$init[[k]], arr$len)) else r_num(arr$init[[k]])
        } else miss
      }, "")
      L <- c(L, sprintf(".ta_%s <- c(%s)", arr$name, paste(vals, collapse = ", ")))
    } else {
      L <- c(L, sprintf(".arr_%s <- %s", arr$name, r_chr_vec(arr$elems)))
    }
  }

  pdv_order <- union(an$order, c("_error_", "_n_"))
  pdv_vals <- vapply(pdv_order, function(v) {
    inf <- an$info[[v]]
    if (is.null(inf) || inf$kind == "auto") return(if (v == "_error_") "0" else "NA_real_")
    proto_code(inf, use_init = isTRUE(inf$retained))
  }, "")
  pdv_code <- paste0("pdv <- list(\n", paste0("  ", r_name(pdv_order), " = ", pdv_vals, collapse = ",\n"), "\n)")
  dump_vars <- setdiff(an$order, c("_error_", "_n_"))
  dump_names <- vapply(dump_vars, function(v) an$info[[v]]$disp %||% v, "", USE.NAMES = FALSE)
  is_flag <- startsWith(dump_vars, "first.") | startsWith(dump_vars, "last.")
  if (any(is_flag)) {
    base <- sub("^[^.]*[.]", "", dump_vars[is_flag])
    dump_names[is_flag] <- paste0(toupper(sub("[.].*$", "", dump_vars[is_flag])), ".",
                                  vapply(base, function(v) an$info[[v]]$disp %||% v, "", USE.NAMES = FALSE))
  }
  dump_fmts <- Filter(Negate(is.null), lapply(stats::setNames(dump_vars, dump_vars), function(v) an$formats[[v]]))
  L <- c(L, pdv_code,
         ".proto <- pdv",
         sprintf(".dump <- sas_dump_spec(%s, %s, %s)", r_chr_vec(dump_vars), r_chr_vec(dump_names),
                 if (length(dump_fmts) == 0) "list()" else r_lit(lapply(dump_fmts, function(f) f[c("name", "w", "d", "char", "text")]))))
  if (!is.null(rd) && !is.null(rd$nobs)) {
    nobs <- paste0("nrow(work[[", r_str(vapply(rd$datasets, `[[`, "", "name")), "]])", collapse = " + ")
    L <- c(L, sprintf("pdv$%s <- %s", r_name(rd$nobs), nobs))
  }
  reset_vars <- Filter(function(v) an$info[[v]]$kind == "data" && !isTRUE(an$info[[v]]$retained), an$order)
  if (length(reset_vars) > 0) L <- c(L, sprintf(".reset <- pdv[%s]", r_chr_vec(reset_vars)))
  if (!is.null(cx$merge_reset)) L <- c(L, sprintf(".m_reset <- pdv[%s]", r_chr_vec(cx$merge_reset)))
  if (an$lag_sites > 0) L <- c(L, ".lags <- new.env()")
  for (k in seq_along(an$outputs)) {
    L <- c(L, sprintf(".out%d <- list()", k), sprintf(".vars%d <- %s", k, r_chr_vec(an$outputs[[k]]$vars)))
  }

  body <- gen_row_block(an$stmts, cx)
  implicit_output <- length(an$outputs) > 0 && !has_stmt(an$stmts, "output")
  loop <- c(
    ".n_iter <- 0L",
    "repeat {",
    "  .n_iter <- .n_iter + 1L",
    if (length(reset_vars) > 0) "  pdv[names(.reset)] <- .reset",
    "  pdv$`_n_` <- .n_iter",
    "  pdv$`_error_` <- 0",
    if (!is.null(rd)) "  .read <- FALSE",
    "  .status <- tryCatch({",
    indent_lines(body, 2L),
    "    \"ok\"",
    "  }, sasonr_delete = function(e) \"delete\", sasonr_stop = function(e) \"stop\", sasonr_return = function(e) \"ok\")",
    if (has_input) "  if (.status == \"stop\" && !is.null(.inf$pdv)) pdv[names(.inf$pdv)] <- .inf$pdv",
    "  pdv <- sas_err_sync(pdv)",
    sprintf("  .log <- c(.log, sas_iter_dump(pdv, .dump%s))", if (has_input) ", .inf" else ""),
    if (has_input) "  sas_input_iter_end(.inf)",
    "  if (.status == \"stop\") break",
    if (implicit_output) {
      c("  if (.status == \"ok\") {", indent_lines(output_lines(seq_along(an$outputs)), 2L), "  }")
    },
    if (!is.null(rd)) "  if (!.read) break" else "  break",
    "}"
  )
  L <- c(L, loop)
  if (!is.null(set_rd)) L <- c(L, "sas_reads_consumed(.p)")
  for (k in seq_along(an$outputs)) {
    o <- an$outputs[[k]]
    code <- sprintf("work$%s <- sas_rows_to_tibble(.out%d, .proto[.vars%d])", r_name(o$name), k, k)
    if (any(o$vars != o$out_names)) code <- paste0(code, " |>\n  ", select_code(o))
    L <- c(L, code)
  }
  L
}

output_lines <- function(ks) sprintf(".out%d[[length(.out%d) + 1L]] <- pdv[.vars%d]", ks, ks, ks)

gen_row_block <- function(stmts, cx) {
  unlist(lapply(stmts, function(s) {
    code <- gen_row_stmt(s, cx)
    if (length(code) > 0 && !s$stmt %in% c("if", "do", "select") && stmt_may_error(s)) code <- c(code, "pdv <- sas_err_sync(pdv)")
    code
  }))
}

# A condition whose evaluation may raise a data error: evaluate it first so
# _ERROR_ is set before the branch runs.
row_cond <- function(e, cx, out_name) {
  if (!expr_may_error(e)) return(list(pre = character(), cond = sprintf("sas_true(%s)", gen_expr(e, cx))))
  cx$n_tmp <- cx$n_tmp + 1L
  v <- paste0(".c", cx$n_tmp)
  list(pre = c(sprintf("%s <- sas_true(%s)", v, gen_expr(e, cx)), "pdv <- sas_err_sync(pdv)"), cond = v)
}

block <- function(head, body, tail = "}") c(head, indent_lines(body, 1L), tail)

gen_row_stmt <- function(s, cx) {
  an <- cx$an
  g <- function(e) gen_expr(e, cx)
  switch(s$stmt,
    assign = {
      if (s$target$node == "var") {
        v <- s$target$name
        sprintf("%s <- %s", var_ref(v, cx), coerce_var(g(s$value), v, an))
      } else {
        arr <- an$arrays[[s$target$name]]
        sprintf("%s <- %s", gen_array_ref(s$target, cx), coerce_to(g(s$value), arr$type, arr$len))
      }
    },
    sum = {
      ref <- var_ref(s$target$name, cx)
      sprintf("%s <- sas_f_sum(%s, %s)", ref, ref, g(s$value))
    },
    substr_assign = {
      at <- sprintf(".at = c(%dL, %dL)", as.integer(s$line), as.integer(s$col %||% NA_integer_))
      sprintf("%s <- sas_substr_assign(%s, %s, %s, %s, %s)", var_ref(s$var, cx), var_ref(s$var, cx),
              g(s$pos), if (is.null(s$len)) "NULL" else g(s$len), g(s$value), at)
    },
    "if" = {
      rc <- row_cond(s$cond, cx)
      out <- block(sprintf("if (%s) {", rc$cond), gen_row_block(list(s$then), cx))
      if (!is.null(s[["else"]])) {
        out[length(out)] <- "} else {"
        out <- c(out, indent_lines(gen_row_block(list(s[["else"]]), cx), 1L), "}")
      }
      c(rc$pre, out)
    },
    subset_if = sprintf("if (!sas_true(%s)) sas_delete()", g(s$cond)),
    stop = "sas_stop_step()",
    "return" = "sas_return()",
    delete = "sas_delete()",
    leave = "break",
    continue = "next",
    do = gen_row_do(s, cx),
    select = gen_row_select(s, cx),
    set = c(
      ".p <- .p + 1L",
      "if (.p > .n_in) sas_stop_step()",
      "pdv <- sas_read_row(pdv, .rows, .p)",
      ".read <- TRUE"
    ),
    input = c(
      paste0("pdv <- sas_input_stmt(.inf, pdv, ", input_spec_code(s, an), ")"),
      ".read <- TRUE",
      if (!is.null(an$infile$opts$end)) sprintf("pdv$%s <- sas_input_at_end(.inf)", r_name(an$infile$opts$end))
    ),
    merge = c(
      ".p <- .p + 1L",
      "if (.p > .n_in) sas_stop_step()",
      sprintf("pdv <- sas_merge_read(pdv, .m, .plan, .p, .m_reset, %s, %s)",
              r_lit(vapply(s$datasets, function(d) d$options$in_var %||% NA_character_, "")),
              r_chr_vec(an$by$vars)),
      if (!is.null(s$end)) sprintf("pdv$%s <- as.numeric(.p == .n_in)", r_name(s$end)),
      ".read <- TRUE"
    ),
    output = {
      targets <- if (length(s$targets) == 0) seq_along(an$outputs) else {
        k <- match(s$targets, vapply(an$outputs, `[[`, "", "name"))
        if (anyNA(k)) abort_syntax(paste0("OUTPUT to ", toupper(s$targets[is.na(k)][1]), " which is not on the DATA statement"), s$line)
        k
      }
      output_lines(targets)
    },
    put = sprintf("%s <- c(%s, %s)", cx$sink, cx$sink, put_line_code(s, cx)),
    call = if (s$routine %in% c("missing", "sortn", "sortc")) gen_row_call_vars(s, cx) else {
      args <- vapply(s$args, g, "")
      scope <- if (length(args) >= 3) paste0(", scope = ", args[3]) else ""
      sprintf("sas_symput(.symtab, %s, %s, trim = %s%s)", args[1], args[2], s$routine == "symputx", scope)
    },
    character()
  )
}

# CALL MISSING / SORTN / SORTC on variables and array elements.
gen_row_call_vars <- function(s, cx) {
  an <- cx$an
  refs <- vapply(s$targets, function(t) if (t$node == "var") var_ref(t$name, cx) else gen_array_ref(t, cx), "")
  if (s$routine == "missing") {
    vals <- vapply(s$targets, function(t) {
      ty <- expr_type(t, an)
      if (ty$type == "char") r_str(strrep(" ", ty$len)) else "NA_real_"
    }, "")
    return(paste(refs, "<-", vals))
  }
  cx$n_tmp <- cx$n_tmp + 1L
  tmp <- paste0(".srt", cx$n_tmp)
  c(sprintf("%s <- sas_sort_values(list(%s))", tmp, paste(refs, collapse = ", ")),
    sprintf("%s <- %s[[%dL]]", refs, tmp, seq_along(refs)))
}

gen_row_do <- function(s, cx) {
  g <- function(e) gen_expr(e, cx)
  body <- gen_row_block(s$body, cx)
  if (s$kind == "block") return(body)
  cx$n_tmp <- cx$n_tmp + 1L
  k <- cx$n_tmp
  if (s$kind == "while") return(block("repeat {", c(sprintf("if (!sas_true(%s)) break", g(s$cond)), body)))
  if (s$kind == "until") {
    return(c(sprintf(".first%d <- TRUE", k), block("repeat {", c(
      sprintf("if (!.first%d && sas_true(%s)) break", k, g(s$cond)),
      sprintf(".first%d <- FALSE", k), body
    ))))
  }
  ref <- var_ref(s$var, cx)
  if (s$kind == "list") {
    vals <- vapply(s$values, g, "")
    head <- sprintf("for (.v%d in list(%s)) {", k, paste(vals, collapse = ", "))
    return(block(head, c(sprintf("%s <- %s", ref, coerce_var(paste0(".v", k), s$var, cx$an)), body)))
  }
  wu <- s$while_until
  c(
    sprintf("%s <- %s", ref, g(s$from)),
    sprintf(".to%d <- %s", k, g(s$to)),
    sprintf(".by%d <- %s", k, if (is.null(s$by)) "1" else g(s$by)),
    sprintf(".first%d <- TRUE", k),
    block("repeat {", c(
      block(sprintf("if (!.first%d) {", k), c(
        if (identical(wu$kind, "until")) sprintf("if (sas_true(%s)) break", g(wu$cond)),
        sprintf("%s <- %s + .by%d", ref, ref, k)
      )),
      sprintf(".first%d <- FALSE", k),
      sprintf("if (!sas_do_test(%s, .to%d, .by%d)) break", ref, k, k),
      if (identical(wu$kind, "while")) sprintf("if (!sas_true(%s)) break", g(wu$cond)),
      body
    ))
  )
}

gen_row_select <- function(s, cx) {
  out <- character()
  sel <- NULL
  if (!is.null(s$expr)) {
    cx$n_tmp <- cx$n_tmp + 1L
    sel <- paste0(".s", cx$n_tmp)
    out <- sprintf("%s <- %s", sel, gen_expr(s$expr, cx))
  }
  first <- TRUE
  for (w in s$whens) {
    cond <- when_cond_code(w, sel, cx)
    head <- if (first) sprintf("if (%s) {", cond) else sprintf("} else if (%s) {", cond)
    out <- c(out, head, indent_lines(gen_row_stmt(w$body, cx), 1L))
    first <- FALSE
  }
  other <- if (s$has_otherwise) gen_row_stmt(s$otherwise, cx) else sprintf("sas_select_error(%dL)", as.integer(s$line))
  if (first) return(other)
  c(out, "} else {", indent_lines(other, 1L), "}")
}

# Translation driver ------------------------------------------------------

translate_step <- function(step, meta, level = "auto") {
  ct_reset()
  an <- sas_analyze(step, meta, level)
  title <- if (step$null) "_NULL_" else paste(toupper(vapply(step$outputs, `[[`, "", "name")), collapse = " ")
  header <- c(
    sprintf("# ---- DATA %s (SAS line %d) -- level %s", title, as.integer(step$line), an$level),
    paste0("# ", an$level, ": ", paste(an$reasons, collapse = "; "))
  )
  body <- if (an$level == "L3") gen_row_step(an) else gen_vec_step(an)
  if (!is.null(an$file)) {
    body <- c(".fput <- character()", body,
              sprintf("sas_file_write(%s, .fput, mod = %s)", r_str(an$file$path), if (isTRUE(an$file$mod)) "TRUE" else "FALSE"))
  }
  uninit <- uninit_vars(an)
  notes <- c(compile_notes(),
             if (length(uninit) > 0) sprintf("NOTE: Variable %s is uninitialized.",
                                             vapply(uninit, function(v) an$info[[v]]$disp %||% v, "")))
  list(
    code = paste(c(header, body), collapse = "\n"), level = an$level, reasons = an$reasons,
    line = step$line, outputs = vapply(an$outputs, `[[`, "", "name"), meta = out_meta(an),
    compile_notes = notes
  )
}
