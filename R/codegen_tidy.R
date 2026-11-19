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
  cx
}

with_mode <- function(cx, mode) {
  cx2 <- new_cx(mode, cx$an)
  cx2
}

var_lang <- function(name, cx) {
  switch(cx$mode,
    vec = rlang::sym(name),
    out = rlang::call2("$", rlang::sym(".d"), rlang::sym(name)),
    row = rlang::call2("$", rlang::sym("pdv"), rlang::sym(name))
  )
}

negate_if <- function(x, negate) if (isTRUE(negate)) rlang::call2("sas_not", x) else x

# The value of a numeric constant operand: a literal, a signed literal or an
# arithmetic operation on such operands (the operation itself valid); NULL
# for anything else.
const_num_value <- function(e) {
  switch(e$node,
    num = if (identical(e$kind, "num")) e$value,
    unary = if (e$op %in% c("-", "+")) {
      v <- const_num_value(e$arg)
      if (!is.null(v)) (if (e$op == "-") -v else v)
    },
    binary = if (e$op %in% c("+", "-", "*", "/", "**")) {
      a <- const_num_value(e$lhs)
      b <- const_num_value(e$rhs)
      if (is.null(a) || is.null(b)) return(NULL)
      if (e$op == "/" && b == 0) return(NULL)
      if (e$op == "**" && ((a == 0 && b <= 0) || (a < 0 && b != trunc(b)))) return(NULL)
      v <- switch(e$op, "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b, "**" = a^b)
      if (is.finite(v)) v
    },
    NULL
  )
}

# Expressions -------------------------------------------------------------

# Operations whose missing result is a "generated" missing value for the
# missing-value NOTE: +, -, *, / and the unary minus.  The result of ** is not
# (SAS records the next operation again: (a ** 2) + 1 has 2 places), nor is a
# function result (abs(a) + 1: 2 places).  Inside the arguments of a function
# call nothing is skipped: every operation records its place
# (int((a - 1) / 7): 3 places).
gen_missing_node <- function(x, cx) {
  if (isTRUE(cx$fn_arg)) return(FALSE)
  x$node == "binary" && x$op %in% c("+", "-", "*", "/") || x$node == "unary" && identical(x$op, "-")
}

# Missing value literal: `.` or a special missing value.
missing_lang <- function(code) if (code == ".") NA_real_ else rlang::call2("sas_missing", code)

gen_lang <- function(e, cx) {
  g <- function(x) gen_lang(x, cx)
  inexact <- function() if (isTRUE(cx$inexact)) list(.exact = FALSE)
  switch(e$node,
    num = cg_num(e$value),
    str = if (nzchar(e$value)) e$value else " ",
    missing = missing_lang(e$code),
    var = var_lang(e$name, cx),
    array_ref = gen_array_lang(e, cx),
    unary = switch(e$op,
      "-" = {
        # SAS records the place of a unary minus at the operand's position; the
        # minus itself is not recorded when the operand is a generated missing
        # value (gen_missing_node(): -(a + b) records the "+" only, -(a ** 2)
        # records the "**" and the minus)
        at <- if (!is.null(e$arg$col)) cg_at(e$arg$line, e$arg$col)
        if (gen_missing_node(e$arg, cx)) at <- c(at, list(.gen_a = TRUE))
        cg_call("sas_neg", c(list(g(e$arg)), at, inexact()))
      },
      "+" = g(e$arg),
      not = rlang::call2("sas_not", g(e$arg))
    ),
    binary = {
      ct <- ct_diag()
      # in a WHERE condition the compile-time NOTEs of constant operations
      # below are not supported
      where_ct <- function() {
        if (isTRUE(cx$where)) abort_unsupported("a constant operation with a compile-time NOTE in a WHERE condition", e$line)
      }
      # constant division by zero is folded at compile time (no _ERROR_ at run time)
      if (e$op == "/" && e$lhs$node == "num" && e$rhs$node == "num" && e$rhs$value == 0) {
        where_ct()
        log_place_check(c(e$line, e$col %||% NA))
        ct$notes <- c(ct$notes, sprintf("NOTE: Division by zero detected during the compilation phase, detected at line %d column %d.",
                                        as.integer(e$line), as.integer(e$col %||% 0L)))
        return(NA_real_)
      }
      arith <- e$op %in% c("+", "-", "*", "/", "**")
      # ** of two constant operands (literals, signed literals and operations
      # on them) is folded at compile time when it is invalid: 0 ** n with
      # n <= 0, a negative base with a non-integer exponent, or a result too
      # large: the NOTE without a place, a missing value, no _ERROR_
      if (identical(e$op, "**")) {
        a0 <- const_num_value(e$lhs)
        b0 <- const_num_value(e$rhs)
        if (!is.null(a0) && !is.null(b0) &&
            ((a0 == 0 && b0 <= 0) || (a0 < 0 && b0 != trunc(b0)) || is.infinite(a0^b0))) {
          where_ct()
          ct$notes <- c(ct$notes, "NOTE: Invalid argument(s) to the exponential operator \"**\".")
          return(NA_real_)
        }
      }
      if (arith && e$op != "**" && e$lhs$node == "num" && e$rhs$node == "num") {
        # compile-time diagnostics of constant operations
        a0 <- e$lhs$value
        b0 <- e$rhs$value
        v0 <- switch(e$op, "+" = a0 + b0, "-" = a0 - b0, "*" = a0 * b0, "/" = a0 / b0)
        if (is.infinite(v0)) {
          where_ct()
          log_place_check(c(e$line, e$col %||% NA))
          ct$notes <- c(ct$notes, sprintf("NOTE: A number has become too large during the compilation phase, detected at line %d column %d. ",
                                          as.integer(e$line), as.integer(e$col %||% 0L)))
        }
      }
      # an operation on constants with a missing operand is folded as well
      if (arith && e$lhs$node %in% c("num", "missing") && e$rhs$node %in% c("num", "missing") &&
          (e$lhs$node == "missing" || e$rhs$node == "missing")) {
        where_ct()
        log_place("mv.compile", at = c(e$line, e$col %||% NA_integer_))
        return(NA_real_)
      }
      a <- g(e$lhs)
      b <- g(e$rhs)
      fun <- switch(e$op,
        "/" = "sas_div", "**" = "sas_pow", "||" = "sas_concat", and = "sas_and",
        or = "sas_or", min = "sas_min_op", max = "sas_max_op", NA_character_
      )
      at <- if (arith && !is.null(e$col)) cg_at(e$line, e$col)
      # vectorised code evaluates guarded expressions for every row, so the
      # missing-value counts there are not exact (the engine re-runs as L3)
      if (arith && isTRUE(cx$inexact)) at <- c(at, list(.exact = FALSE))
      # an operand that is a generated missing value (gen_missing_node()): its place
      # was already recorded and +, -, *, / do not record another one (a + b + c with
      # b missing: 1 place; (a + b) / 3: 1 place).  ** always records its place,
      # also for a generated operand ((a + b) ** 2: 2 places).
      if ((is.na(fun) || fun == "sas_div") && length(at)) {
        if (gen_missing_node(e$lhs, cx)) at <- c(at, list(.gen_a = TRUE))
        if (gen_missing_node(e$rhs, cx)) at <- c(at, list(.gen_b = TRUE))
      }
      if (is.na(fun)) cg_call("sas_arith", c(list(a, b, e$op), at)) else cg_call(fun, c(list(a, b), at))
    },
    compare = if (isTRUE(e$colon)) {
      rlang::call2("sas_cmp_trunc", e$op, g(e$lhs), g(e$rhs))
    } else {
      rlang::call2(cmp_helper[[e$op]], g(e$lhs), g(e$rhs))
    },
    chain = rlang::call2("sas_chain", cg_call("list", lapply(e$args, g)), cg_val(e$ops)),
    "in" = negate_if(rlang::call2("sas_in", g(e$lhs), gen_in_values(e)), e$negate),
    between = negate_if(rlang::call2("sas_between", g(e$lhs), g(e$lo), g(e$hi)), e$negate),
    contains = negate_if(rlang::expr(as.numeric(sas_f_index(!!g(e$lhs), sas_rtrim(!!g(e$rhs))) > 0)), e$negate),
    is_missing = negate_if(rlang::call2("sas_f_missing", g(e$lhs)), e$negate),
    call = gen_call(e, cx),
    omitted = quote(NULL),
    abort_unsupported(paste("expression node", e$node), e$line)
  )
}

gen_in_values <- function(e) {
  vals <- unlist(lapply(e$items, function(it) {
    switch(it$node,
      num = list(cg_num(it$value)),
      str = list(it$value),
      missing = list(missing_lang(it$code)),
      range = {
        if (it$to - it$from > 10000) abort_unsupported("IN range wider than 10000 values", e$line)
        lapply(seq(it$from, it$to), function(v) cg_num(as.numeric(v)))
      }
    )
  }), recursive = FALSE)
  cg_call("c", vals)
}

at_code <- function(e) sprintf(", .at = c(%dL, %dL)", as.integer(e$line), as.integer(e$col %||% NA_integer_))

fmt_lang <- function(fmt) cg_val(fmt[c("name", "w", "d", "char", "text")])

# VNAME(), VLENGTH() ... : compile-time facts about a variable (or an array element).
gen_var_info <- function(e, cx) {
  an <- cx$an
  a <- e$args[[1]]
  info_of <- function(v) {
    inf <- an$info[[v]]
    fmt <- an$formats[[v]]
    char <- identical(inf$type, "char")
    # a numeric variable reports its storage length (LENGTH / ATTRIB or the input
    # data set), 8 by default
    len <- if (char) inf$len else vctrs::vec_cast(an$numlen[[v]] %||% 8L, integer())
    fmt_text <- if (!is.null(fmt)) toupper(fmt$text) else if (char) paste0("$", len, ".") else "BEST12."
    switch(e$name,
      vname = inf$disp %||% v,
      vlength = cg_num(as.numeric(len)),
      vtype = if (char) "C" else "N",
      vlabel = if (!is.na(an$labels[v] %|NA|% NA)) an$labels[[v]] else (inf$disp %||% v),
      vformat = fmt_text,
      vformatn = if (!is.null(fmt)) toupper(paste0(if (fmt$char) "$", fmt$name)) else if (char) "$" else "BEST",
      vformatw = cg_num(as.numeric(if (!is.null(fmt) && !is.na(fmt$w)) fmt$w else if (char) len else 12)),
      vformatd = cg_num(as.numeric(if (!is.null(fmt) && !is.na(fmt$d)) fmt$d else 0)),
      vinformat = if (char) paste0("$", len, ".") else "BEST32.",
      vvalue = rlang::call2("sas_rtrim", cg_call("sas_put_value", c(list(var_lang(v, cx)), if (!is.null(fmt)) list(fmt_lang(fmt)))))
    )
  }
  if (a$node == "var") return(info_of(a$name))
  arr <- an$arrays[[a$name]]
  if (arr$temporary) abort_unsupported(paste0(toupper(e$name), " of a temporary array element"), e$line)
  if (cx$mode != "row") abort_unsupported("array reference in vectorised code", e$line)
  idx <- rlang::call2("sas_idx", gen_lang(a$index[[1]], cx), as.integer(arr$lo), as.integer(arr$dim))
  if (e$name == "vvalue") return(rlang::expr(sas_rtrim(sas_put_value(!!gen_array_lang(a, cx)))))
  rlang::call2("[", cg_call("c", lapply(arr$elems, info_of)), idx)
}

gen_call <- function(e, cx) {
  # the arguments are generated in function-argument context (gen_missing_node())
  old_fn_arg <- cx$fn_arg
  cx$fn_arg <- TRUE
  on.exit(cx$fn_arg <- old_fn_arg, add = TRUE)
  g <- function(x) gen_lang(x, cx)
  inexact <- if (isTRUE(cx$inexact)) list(.exact = FALSE)
  if (e$name %in% c("dim", "lbound", "hbound")) {
    arr <- cx$an$arrays[[e$args[[1]]$name]]
    return(switch(e$name, dim = as.integer(arr$dim), lbound = as.integer(arr$lo),
                  hbound = as.integer(arr$lo + arr$dim - 1L)))
  }
  if (e$name %in% var_info_fns) return(gen_var_info(e, cx))
  if (e$name == "put") {
    return(cg_call("sas_f_put", c(list(g(e$args[[1]]), fmt_lang(e$fmt)), if (!is.null(e$align)) list(align = e$align))))
  }
  if (e$name == "input") {
    return(cg_call("sas_f_input", c(list(g(e$args[[1]]), fmt_lang(e$fmt)),
                                    if (nzchar(e$qq)) list(qq = e$qq),
                                    if (cx$mode == "row" && !is.null(e$col)) cg_at(e$line, e$col))))
  }
  if (e$name == "exist") {
    # EXIST(member <, type>): the generated DATA step environment carries the
    # WORK library (`work`, see engine_eval()); SASHELP from the store of R/sashelp.R
    a2 <- if (length(e$args) > 1) g(e$args[[2]]) else quote(NULL)
    return(rlang::call2("sas_f_exist", g(e$args[[1]]), a2, rlang::sym("work")))
  }
  args <- unlist(lapply(e$args, function(a) {
    if (a$node == "of") lapply(a$vars, var_lang, cx = cx) else list(g(a))
  }), recursive = FALSE)
  if (!is.null(e$lag)) {
    x <- args[[1]]
    n <- as.integer(e$lag$n)
    if (cx$mode == "row") {
      fun <- if (e$lag$kind == "lag") "sas_lag" else "sas_dif"
      return(rlang::call2(fun, rlang::sym(".lags"), paste0("site", e$site), x, n))
    }
    if (e$lag$kind == "lag") return(rlang::call2("sas_lag_vec", x, n))
    return(rlang::expr((!!x - sas_lag_vec(!!x, !!n))))
  }
  args <- chr_arg_gates(e, args, cx)
  if (e$name == "symget") args <- c(list(rlang::sym(".symtab")), args)
  in_sql <- identical(cx$an$context, "sql")
  if (e$name %in% names(fn_policy) && (!is.null(e$col) || in_sql)) {
    # missing / invalid arguments: log NOTEs and _ERROR_ (fn_policy.R).
    # In PROC SQL the expressions carry no line/column; the invalid-argument
    # NOTE of PROC SQL does not use them either (diag.R).
    return(cg_call("sas_fcall", c(list(e$name, rlang::sym(paste0("sas_f_", e$name))), args,
                                  if (!is.null(e$col)) cg_at(e$line, e$col), inexact)))
  }
  at <- if (cx$mode == "row" && e$name %in% err_fns && !is.null(e$col)) cg_at(e$line, e$col)
  if (e$name %in% c("catx", "cat", "cats", "catt")) at <- c(at, list(.len = as.integer(e$buf_len %||% 200L)))
  if (e$name %in% mv_note_fns && !is.null(e$col) && length(args) > 0) {
    # count missing arguments once, then call with the evaluated arguments
    mv <- cg_call("sas_mv_args", c(list(rlang::call2("c", as.integer(e$line), as.integer(e$col))), args, inexact))
    return(rlang::call2("do.call", rlang::sym(paste0("sas_f_", e$name)),
                if (length(at)) rlang::call2("c", mv, cg_call("list", at)) else rlang::call2("c", mv)))
  }
  out <- cg_call(paste0("sas_f_", e$name), c(args, at))
  if (e$name %in% mv_result_fns && !is.null(e$col)) {
    out <- cg_call("sas_mv_res", c(list(out, rlang::call2("c", as.integer(e$line), as.integer(e$col))), inexact))
  }
  out
}

# Character arguments with bytes outside ASCII (sas_chr_arg()): every function
# except the case and length functions.  Arguments known to be numeric are
# not checked.
chr_arg_gates <- function(e, args, cx) {
  if (e$name %in% chr_wide_ok_fns && !(e$name %in% chr_wide_data_fns && identical(cx$an$context, "sql"))) return(args)
  nodes <- unlist(lapply(e$args, function(a) {
    if (a$node == "of") lapply(a$vars, function(v) list(node = "var", name = v)) else list(a)
  }), recursive = FALSE)
  # no node per argument: every argument is checked
  if (length(nodes) != length(args)) nodes <- vector("list", length(args))
  line <- if (is.null(e$line)) NULL else as.integer(e$line)
  exact <- if (isTRUE(cx$inexact)) list(.exact = FALSE)
  for (k in seq_along(args)) {
    a <- args[[k]]
    if (is.null(a) || identical(a, quote(expr = )) || is.numeric(a) || (is.character(a) && !any(chr_wide(a)))) next
    ty <- if (is.null(nodes[[k]])) NA_character_ else
      tryCatch(expr_type(nodes[[k]], cx$an)$type, error = function(err) NA_character_)
    if (identical(ty, "num")) next
    args[k] <- list(cg_call("sas_chr_arg", c(list(a, e$name), if (length(line)) list(.line = line), exact)))
  }
  args
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

gen_array_lang <- function(e, cx) {
  if (cx$mode != "row") abort_unsupported("array reference in vectorised code", e$line)
  arr <- cx$an$arrays[[e$name]]
  idx <- rlang::call2("sas_idx", gen_lang(e$index[[1]], cx), as.integer(arr$lo), as.integer(arr$dim))
  if (arr$temporary) rlang::call2("[", rlang::sym(paste0(".ta_", arr$name)), idx)
  else rlang::call2("[[", rlang::sym("pdv"), rlang::call2("[", rlang::sym(paste0(".arr_", arr$name)), idx))
}

gen_array_ref <- function(e, cx) cg_text(gen_array_lang(e, cx))

proto_lang <- function(inf, use_init = TRUE) {
  init <- if (use_init) inf$init else NULL
  if (inf$type == "char") return(sas_char(if (is.null(init)) "" else init, inf$len))
  if (is.null(init)) return(NA_real_)
  code <- sas_miss_code(init)
  if (!is.na(code) && code != 1L) return(rlang::call2("sas_missing", paste0(".", miss_label(code))))
  cg_num(init)
}

# The value of a PUT item: sas_put_named(), sas_put_fvalue() (an explicit
# format) or sas_put_value().
put_item_lang <- function(it, cx) {
  fmt <- it$fmt %||% cx$an$formats[[it$name]]
  args <- c(list(var_lang(it$name, cx)), if (!is.null(fmt)) list(fmt_lang(fmt)))
  if (it$kind == "named") return(cg_call("sas_put_named", c(list(cx$an$info[[it$name]]$disp %||% it$name), args)))
  cg_call(if (!is.null(it$fmt)) "sas_put_fvalue" else "sas_put_value", args)
}

put_line_lang <- function(s, cx, guard = NULL) {
  if (any(vapply(s$items, function(it) it$kind == "ptr", logical(1)))) return(put_ptr_lang(s, cx, guard))
  items <- lapply(s$items, function(it) if (it$kind == "lit") it$text else put_item_lang(it, cx))
  # literals and formatted (unnamed, explicit format) items add no trailing blank
  lit <- vapply(s$items, function(it) it$kind == "lit" || (it$kind == "value" && !is.null(it$fmt)), logical(1))
  ln <- if (is.numeric(s$line) && length(s$line) == 1L) as.integer(s$line)
  file_args <- if (!is.null(cx$an$file)) list(.file = TRUE, .line = ln)
  cg_call("sas_put_line", c(items, if (any(lit)) list(.lit = cg_val(lit)), put_ls_arg(cx), file_args))
}

# PUT with pointer controls: every item becomes a positioned piece of text.
# `guard`: in a vectorised step, the rows where the PUT executes (only these
# are laid out, so a pointer of a row that does not execute cannot stop it).
put_ptr_lang <- function(s, cx, guard = NULL) {
  piece <- function(...) cg_call("list", list(...))
  pieces <- lapply(s$items, function(it) {
    if (it$kind == "ptr") return(if (it$k == "next") piece(k = "next") else piece(k = it$k, v = gen_lang(it$v, cx)))
    if (it$kind == "lit") return(piece(k = "text", s = it$text, blank = FALSE))
    piece(k = "text", s = put_item_lang(it, cx), blank = it$kind == "named" || is.null(it$fmt))
  })
  ln <- if (is.numeric(s$line) && length(s$line) == 1L) as.integer(s$line)
  cg_call("sas_put_layout", c(list(cg_call("list", pieces)), put_ls_arg(cx), if (!is.null(cx$an$file)) list(.file = TRUE),
                              list(.line = ln), if (!is.null(guard)) list(.guard = guard)))
}

# The line size argument of the PUT helpers: a FILE passes its LRECL, the
# log's LINESIZE is read at run time.
put_ls_arg <- function(cx) if (!is.null(cx$an$file)) list(.ls = as.integer(cx$an$file$lrecl))

# Input sources (shared by L1/L2 pipelines and the L3 loop) ---------------

# `where`: the analysed condition of the WHERE statement that applies (NULL
# when there is none).
ds_code <- function(d, an, obs_col = FALSE, where = NULL) {
  ds_opts_apply(ds_opts_record(d), an, obs_col = obs_col, where = where)
}

# One INPUT statement as a run-time spec for sas_input_stmt().
input_spec_lang <- function(rd, an) {
  rcx <- new_cx("row", an)
  ptr <- function(v) {
    if (v$node %in% c("num", "str")) return(if (v$node == "num") cg_num(v$value) else v$value)
    cg_fn(alist(pdv = ), gen_lang(v, rcx))
  }
  items <- lapply(rd$items, function(it) {
    if (it$k != "var") return(cg_call("list", c(list(k = it$k), if (!is.null(it$v)) list(v = ptr(it$v)))))
    inf <- an$info[[it$name]]
    char <- identical(inf$type, "char")
    cg_call("list", c(
      list(k = "var", name = it$name, disp = inf$disp %||% it$name, char = char,
           len = if (char) as.integer(inf$len) else NA_integer_, mode = it$mode),
      if (!is.null(it$fmt)) list(fmt = fmt_lang(it$fmt)),
      if (it$mode == "column") list(from = as.integer(it$from), to = as.integer(it$to)),
      if (!is.null(it$cd)) list(cd = as.integer(it$cd)),
      if (isTRUE(it$amp)) list(amp = TRUE), if (isTRUE(it$tilde)) list(tilde = TRUE),
      if (nzchar(it$qq %||% "")) list(qq = it$qq)
    ))
  })
  rlang::call2("list", items = cg_call("list", items), hold = rd$hold %||% "")
}

# Its display in a source pipeline: one item per line.
input_spec_code <- function(rd, an) {
  x <- input_spec_lang(rd, an)
  x <- as.list(x)
  items <- vapply(as.list(x$items)[-1L], cg_text, "")
  sprintf("list(items = list(\n    %s\n  ), hold = %s)", paste(items, collapse = ",\n    "), cg_text(x$hold))
}

# Code that opens the input stream of a step (INFILE or DATALINES).
infile_open_lang <- function(an) {
  inf <- an$infile
  opts <- if (is.null(inf)) list() else inf$opts
  n_lines <- max(c(1L, vctrs::list_unchop(lapply(an$readers, function(r) {
    if (r$stmt != "input") return(NULL)
    vctrs::list_unchop(lapply(r$items, function(it) if (it$k == "line" && it$v$node == "num") as.integer(it$v$value)), ptype = integer(), name_spec = rlang::zap())
  }), ptype = integer(), name_spec = rlang::zap())))
  ropts <- list(dlm = opts$dlm, dsd = opts$dsd, mode = opts$mode, firstobs = opts$firstobs, obs = opts$obs, ls = opts$ls,
                pad = opts$pad, lines = max(n_lines, opts$lines %||% 1L))
  ropts <- Filter(Negate(is.null), ropts)
  if (!is.null(inf) && identical(inf$source$kind, "file")) {
    return(rlang::expr(sas_infile_open(sas_read_file(!!inf$source$path), opts = !!cg_val(ropts), instream = FALSE)))
  }
  dl <- an$dl
  if (is.null(dl)) abort_unsupported("INFILE DATALINES without a DATALINES block", inf$line)
  ropts$end_text <- if (isTRUE(dl$four)) ";;;;" else ";"
  line_no <- if (length(dl$line_no) == 0) quote(integer()) else rlang::call2("c", !!!as.list(as.integer(dl$line_no)))
  rlang::call2("sas_infile_open", cg_val(dl$lines), line_no,
       opts = cg_val(ropts))
}

# The input source of a step: a [cgi()] item.
source_item <- function(an, rd = if (length(an$readers) > 0) an$readers[[1]] else NULL) {
  if (is.null(rd)) return(cgi(quote(tibble::tibble(.rows = 1L))))
  if (rd$stmt == "input") {
    end_var <- an$infile$opts$end
    inf <- infile_open_lang(an)
    spec <- input_spec_lang(rd, an)
    end <- if (!is.null(end_var)) list(end_var = end_var)
    return(cgi(cg_call("sas_read_input", c(list(inf, spec), end)),
               sprintf("sas_read_input(%s, %s%s)", cg_text(inf), input_spec_code(rd, an),
                       if (is.null(end)) "" else paste0(", end_var = ", cg_text(end_var)))))
  }
  srcs <- lapply(rd$datasets, ds_code, an = an, where = an$dsir$slots$where$effective)
  in_vars <- vapply(rd$datasets, function(d) d$options$in_var %||% NA_character_, "")
  if (rd$stmt == "merge") {
    # match-merge: the input table of the observation plan (FIRST./LAST. included)
    args <- list(cg_call("list", lapply(srcs, `[[`, "lang")), by = cg_val(an$by$vars),
                 descending = cg_val(an$by$descending), in_vars = cg_val(in_vars))
    return(cgi(cg_call("sas_merge_input", args),
               sprintf("sas_merge_input(\n  list(\n    %s\n  ),\n  %s\n)",
                       paste(vapply(srcs, `[[`, "", "text"), collapse = ",\n    "),
                       paste0(names(args)[-1], " = ", vapply(args[-1], cg_text, ""), collapse = ", "))))
  }
  if (length(srcs) == 1 && all(is.na(in_vars))) return(srcs[[1]])
  by <- if (!is.null(an$by) && length(srcs) > 1) list(by = cg_val(an$by$vars), descending = cg_val(an$by$descending))
  args <- c(list(cg_call("list", lapply(srcs, `[[`, "lang")), in_vars = cg_val(in_vars)), by)
  cgi(cg_call("sas_set_input", args),
      sprintf("sas_set_input(list(%s), %s)", paste(vapply(srcs, `[[`, "", "text"), collapse = ", "),
              paste0(names(args)[-1], " = ", vapply(args[-1], cg_text, ""), collapse = ", ")))
}

input_columns <- function(an) {
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  if (is.null(rd)) return(character())
  if (rd$stmt == "input") return(unique(vctrs::list_unchop(lapply(an$readers, function(r) if (r$stmt == "input") input_var_names(r)), ptype = character(), name_spec = rlang::zap())))
  c(vctrs::list_unchop(lapply(rd$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap()),
    vctrs::list_unchop(lapply(rd$datasets, function(d) d$options$in_var), ptype = character(), name_spec = rlang::zap()))
}

# Stages common to L1/L2/L3 input preparation: WHERE, _N_, END=, FIRST./LAST.
add_input_stages <- function(cx, include_n = TRUE) {
  an <- cx$an
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  if (include_n && "_n_" %in% an$order) add_stage(cx, "mutate", list(`_n_` = quote(as.numeric(dplyr::row_number()))))
  if (!is.null(rd) && !is.null(rd$end) && rd$stmt %in% c("set", "merge")) {
    add_stage(cx, "mutate", stats::setNames(list(quote(as.numeric(dplyr::row_number() == dplyr::n()))), rd$end))
  }
  if (!is.null(an$by) && !is.null(rd) && rd$stmt == "set") {
    add_stage(cx, "raw", rlang::call2("sas_check_sorted", cg_val(an$by$vars), cg_val(an$by$descending)))
    flags <- list()
    for (k in seq_along(an$by$vars)) {
      keys <- rlang::syms(an$by$vars[seq_len(k)])
      v <- an$by$vars[k]
      flags[[paste0("first.", v)]] <- cg_call("sas_first", keys)
      flags[[paste0("last.", v)]] <- cg_call("sas_last", keys)
    }
    add_stage(cx, "mutate", flags)
  }
}

# A stage of the vectorised pipeline, as language objects: `mutate` a named
# list of column values, `filter` a condition, `raw` a call without its first
# argument.
add_stage <- function(cx, kind, code) {
  cx$stages <- c(cx$stages, list(list(kind = kind, code = code)))
}

# A mutate value on one line; a multi-line one (case_when) continues at the
# indentation of its column.
mutate_value_text <- function(x) {
  if (is.call(x) && identical(x[[1]], quote(dplyr::case_when))) {
    parts <- vapply(seq_along(as.list(x))[-1L], function(i) {
      nm <- names(as.list(x))[i]
      paste0(if (!is.null(nm) && nzchar(nm)) paste0(nm, " = ") else "", cg_text(x[[i]]))
    }, "")
    return(paste0("dplyr::case_when(\n      ", paste(parts, collapse = ",\n      "), "\n    )"))
  }
  cg_text(x)
}

# `target <- src |> stages`: a [cgi()] item; `src` is one too. A mutate stage
# shows one column per line.
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
  x <- src
  for (s in merged) {
    st <- switch(s$kind,
      mutate = rlang::call2(quote(dplyr::mutate), !!!s$code),
      filter = rlang::call2(quote(dplyr::filter), s$code),
      raw = s$code
    )
    y <- cgi_pipe(x, st, sep = " |>\n  ")
    if (s$kind == "mutate") {
      y$text <- paste0(x$text, " |>\n  dplyr::mutate(\n",
                       paste0("    ", r_name(names(s$code)), " = ", vapply(s$code, mutate_value_text, ""), collapse = ",\n"), "\n  )")
    }
    x <- y
  }
  cgi_assign(target, x)
}

# L1 / L2: vectorised pipeline -------------------------------------------

new_guard <- function(cx) {
  cx$n_guard <- cx$n_guard + 1L
  paste0(".g", cx$n_guard)
}

and_guard <- function(guard, cond) if (is.null(guard)) cond else rlang::call2("&", rlang::sym(guard), cond)

gen_vec_step <- function(an) {
  cx <- new_cx("vec", an)
  add_input_stages(cx)
  # Variables created by the step start missing (or at their RETAIN value).
  inputs <- input_columns(an)
  init_vars <- Filter(function(v) {
    inf <- an$info[[v]]
    inf$kind == "data" && !v %in% inputs && !v %in% names(an$sum_plan)
  }, an$order)
  k0 <- NA_integer_
  if (length(init_vars) > 0) {
    add_stage(cx, "mutate", stats::setNames(lapply(init_vars, function(v) proto_lang(an$info[[v]])), init_vars))
    k0 <- length(cx$stages)
  }
  gen_vec_block(an$stmts, NULL, cx, top = TRUE)
  if (!is.na(k0)) cx$stages <- drop_dead_inits(cx$stages, k0)

  c(list(render_pipeline(".d", source_item(an), cx$stages)), vec_put_item(cx), vec_output_item(an))
}

# Drop the initial (missing) value of a variable that the mutate stages right
# after the initial values (stage `k0`) replace before anything reads it:
# `x = NA_real_, ..., x = 1` -> `x = 1`.  mutate() replaces the whole column,
# so the result is the same; the output select() sets the column order.  A
# value that names a column by a string or a selection helper reads every
# pending column (they keep their initial value).
drop_dead_inits <- function(stages, k0) {
  pending <- names(stages[[k0]]$code)
  dead <- character()
  all_cols <- c("pick", "across", "c_across", "cur_data", "cur_data_all", "everything", "all_of", "any_of",
                ".data", "starts_with", "ends_with", "matches", "contains", "num_range", "where", "get", "mget")
  strings <- function(e) {
    if (is.character(e)) return(e)
    if (is.call(e) || is.pairlist(e)) return(unlist(lapply(as.list(e), strings)))
    character()
  }
  k <- k0 + 1L
  while (k <= length(stages) && stages[[k]]$kind == "mutate" && length(pending)) {
    code <- stages[[k]]$code
    for (j in seq_along(code)) {
      e <- code[[j]]
      used <- c(all.names(e), strings(e))
      if (any(all_cols %in% used)) {
        pending <- character()
        break
      }
      v <- names(code)[j]
      if (v %in% pending && !v %in% used) dead <- c(dead, v)
      pending <- setdiff(pending, c(used, v))
    }
    k <- k + 1L
  }
  if (!length(dead)) return(stages)
  keep <- !names(stages[[k0]]$code) %in% dead
  if (!any(keep)) return(stages[-k0])
  stages[[k0]]$code <- stages[[k0]]$code[keep]
  stages
}

# The PUT statements of a vectorised step: one line per iteration of the step
# (a row of `.d`), also for a PUT whose items are literals only (its line is
# computed once).
vec_put_lang <- function(cx) {
  if (length(cx$puts) == 0) return(NULL)
  sink <- rlang::sym(cx$sink)
  lines <- cg_call("list", lapply(cx$puts, `[[`, "line"))
  guards <- cg_call("list", lapply(cx$puts, function(p) if (is.null(p$guard)) TRUE else rlang::call2("$", rlang::sym(".d"), rlang::sym(p$guard))))
  assign_lang(sink, rlang::call2("c", sink, cg_call("sas_put_rows", list(lines, guards, n = quote(nrow(.d))))))
}

# Its display: one argument of sas_put_rows() per line, one PUT per line.
vec_put_item <- function(cx) {
  x <- vec_put_lang(cx)
  if (is.null(x)) return(list())
  rows <- x[[3]][[3]]
  args <- function(l, sep) paste(vapply(as.list(l)[-1L], cg_text, ""), collapse = sep)
  list(cgi(x, sprintf("%s <- c(%s, sas_put_rows(\n  list(%s),\n  list(%s),\n  n = nrow(.d)\n))", cx$sink, cx$sink,
                      args(rows[[2]], ",\n       "), args(rows[[3]], ", "))))
}

select_lang <- function(o) {
  cols <- rlang::syms(o$vars)
  if (any(o$vars != o$out_names)) names(cols) <- ifelse(o$vars == o$out_names, "", o$out_names)
  cg_call("dplyr::select", cols)
}
select_code <- function(o) cg_text(select_lang(o))

vec_output_item <- function(an) {
  if (length(an$dsir$slots$outputs) == 0) return(list())
  o <- an$dsir$slots$outputs[[1]]
  x <- cgi(rlang::sym(".d"))
  if (!is.null(an$output_pattern$cond)) {
    x <- cgi_pipe(x, rlang::call2(quote(dplyr::filter), rlang::call2("sas_true", rlang::sym(an$output_pattern$cond))), sep = " |>\n  ")
  }
  x <- cgi_pipe(x, select_lang(o), sep = " |>\n  ")
  list(work_assign(o$name, x))
}

# `work$name <- x`
work_assign <- function(name, x) {
  target <- rlang::call2("$", rlang::sym("work"), rlang::sym(name))
  cgi(rlang::call2("<-", target, x$lang), paste0(cg_text(target), " <- ", x$text))
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
    cond <- if (is.null(set$cond)) TRUE else rlang::call2("sas_true", gen_lang(set$cond, cx))
    cx$inexact <- !is.null(set$cond)
    val <- coerce_var_lang(gen_lang(set$value, cx), v, an)
    cx$inexact <- FALSE
    add_stage(cx, "mutate", stats::setNames(list(val, cond), c(cv[k], cs[k])))
    if (i == plan$last) {
      carry <- rlang::call2("sas_carry", cg_call("list", rlang::syms(cv)), cg_call("list", rlang::syms(cs)),
                    proto_lang(an$info[[v]]))
      add_stage(cx, "mutate", stats::setNames(list(carry), v))
    }
  }
  hit
}

# `v = val` (or `v = if_else(guard, val, v)`) as a one-item mutate list.
# `guard`: the name of a guard column or a condition.
assign_item <- function(v, val, guard) {
  if (is.character(guard)) guard <- rlang::sym(guard)
  stats::setNames(list(if (is.null(guard)) val else cg_call("dplyr::if_else", list(guard, val, rlang::sym(v)))), v)
}

gen_vec_stmt <- function(s, guard, cx) {
  an <- cx$an
  old <- cx$inexact
  cx$inexact <- isTRUE(old) || !is.null(guard)
  on.exit(cx$inexact <- old)
  switch(s$stmt,
    assign = {
      v <- s$target$name
      add_stage(cx, "mutate", assign_item(v, coerce_var_lang(gen_lang(s$value, cx), v, an), guard))
    },
    sum = {
      v <- s$target$name
      plan <- an$sum_plan[[v]]
      e <- gen_lang(s$value, cx)
      cum <- function(init) stats::setNames(list(rlang::call2("sas_cumsum", e, cg_num(as.numeric(init)), quote(dplyr::n()))), v)
      if (is.null(plan$reset_var)) {
        add_stage(cx, "mutate", cum(plan$init))
      } else {
        add_stage(cx, "raw", rlang::expr(dplyr::group_by(.grp = cumsum(!!rlang::sym(plan$reset_var)))))
        add_stage(cx, "mutate", cum(plan$reset_value))
        add_stage(cx, "raw", quote(dplyr::ungroup()))
      }
    },
    substr_assign = {
      v <- s$var
      val <- cg_call("sas_substr_assign", c(list(rlang::sym(v), gen_lang(s$pos, cx), if (!is.null(s$len)) gen_lang(s$len, cx),
                                                 gen_lang(s$value, cx)), cg_at(s$line, s$col)))
      add_stage(cx, "mutate", assign_item(v, val, guard))
    },
    "if" = gen_vec_if(s, guard, cx),
    subset_if = {
      cond <- rlang::call2("sas_true", gen_lang(s$cond, cx))
      add_stage(cx, "filter", if (is.null(guard)) cond else rlang::call2("!", rlang::call2("(", rlang::call2("&", rlang::sym(guard), rlang::call2("!", cond)))))
    },
    delete = add_stage(cx, "filter", if (is.null(guard)) FALSE else rlang::call2("!", rlang::sym(guard))),
    do = gen_vec_block(s$body, guard, cx),
    select = gen_vec_select(s, guard, cx),
    put = {
      g <- if (is.null(guard)) quote(rep(TRUE, nrow(.d))) else rlang::call2("$", rlang::sym(".d"), rlang::sym(guard))
      cx$puts <- c(cx$puts, list(list(guard = guard, line = put_line_lang(s, with_mode(cx, "out"), g))))
    },
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

# `target = dplyr::case_when(...)` as a one-item mutate list.
case_when_item <- function(target, conds, vals, default, guard) {
  arms <- unname(Map(function(c, v) rlang::call2("~", and_guard(guard, c), v), conds, vals))
  tail <- if (!is.null(default) && is.null(guard)) list(.default = default) else {
    c(if (!is.null(default)) list(rlang::call2("~", rlang::sym(guard), default)), list(.default = rlang::sym(target)))
  }
  stats::setNames(list(cg_call("dplyr::case_when", c(arms, tail))), target)
}

gen_vec_if <- function(s, guard, cx) {
  an <- cx$an
  ch <- if_chain(s)
  targets <- unique(vapply(c(ch$vals, if (!is.null(ch$default)) list(ch$default)), function(a) a$target$name, ""))
  if (!is.null(ch) && length(targets) == 1) {
    v <- targets
    old <- cx$inexact
    on.exit(cx$inexact <- old)
    conds <- lapply(seq_along(ch$conds), function(k) {
      if (k > 1) cx$inexact <- TRUE
      rlang::call2("sas_true", gen_lang(ch$conds[[k]], cx))
    })
    cx$inexact <- TRUE
    vals <- lapply(ch$vals, function(a) coerce_var_lang(gen_lang(a$value, cx), v, an))
    default <- if (is.null(ch$default)) NULL else coerce_var_lang(gen_lang(ch$default$value, cx), v, an)
    if (length(conds) == 1 && is.null(default)) {
      add_stage(cx, "mutate", assign_item(v, vals[[1]], and_guard(guard, conds[[1]])))
    } else {
      add_stage(cx, "mutate", case_when_item(v, conds, vals, default, guard))
    }
    return(invisible())
  }
  g <- new_guard(cx)
  add_stage(cx, "mutate", stats::setNames(list(and_guard(guard, rlang::call2("sas_true", gen_lang(s$cond, cx)))), g))
  gen_vec_stmt(s$then, g, cx)
  if (!is.null(s[["else"]])) {
    g2 <- new_guard(cx)
    add_stage(cx, "mutate", stats::setNames(list(and_guard(guard, rlang::call2("!", rlang::sym(g)))), g2))
    gen_vec_stmt(s[["else"]], g2, cx)
  }
}

# The condition of a WHEN clause; `sel`: the SELECT expression (NULL when
# there is none).
when_cond_lang <- function(w, sel, cx) {
  vals <- lapply(w$values, gen_lang, cx = cx)
  if (!is.null(sel)) return(rlang::call2("sas_true", rlang::call2("sas_in", sel, cg_call("list", vals))))
  conds <- lapply(vals, function(v) rlang::call2("sas_true", v))
  if (length(conds) == 1) return(conds[[1]])
  rlang::call2("(", Reduce(function(a, b) rlang::call2("|", a, b), conds))
}

gen_vec_select <- function(s, guard, cx) {
  an <- cx$an
  bodies <- c(lapply(s$whens, `[[`, "body"), list(s$otherwise))
  all_assign <- all(vapply(bodies, function(b) b$stmt == "assign" && b$target$node == "var", logical(1)))
  sel <- if (is.null(s$expr)) NULL else gen_lang(s$expr, cx)
  old <- cx$inexact
  on.exit(cx$inexact <- old)
  cx$inexact <- TRUE
  if (all_assign && length(unique(vapply(bodies, function(b) b$target$name, ""))) == 1) {
    v <- bodies[[1]]$target$name
    conds <- lapply(s$whens, when_cond_lang, sel = sel, cx = cx)
    vals <- lapply(s$whens, function(w) coerce_var_lang(gen_lang(w$body$value, cx), v, an))
    default <- coerce_var_lang(gen_lang(s$otherwise$value, cx), v, an)
    add_stage(cx, "mutate", case_when_item(v, conds, vals, default, guard))
    return(invisible())
  }
  if (!is.null(sel)) {
    cx$n_tmp <- cx$n_tmp + 1L
    tmp <- paste0(".s", cx$n_tmp)
    add_stage(cx, "mutate", stats::setNames(list(sel), tmp))
    sel <- rlang::sym(tmp)
  }
  rem <- guard
  for (w in s$whens) {
    g <- new_guard(cx)
    add_stage(cx, "mutate", stats::setNames(list(and_guard(rem, when_cond_lang(w, sel, cx))), g))
    gen_vec_stmt(w$body, g, cx)
    r <- new_guard(cx)
    add_stage(cx, "mutate", stats::setNames(list(and_guard(rem, rlang::call2("!", rlang::sym(g)))), r))
    rem <- r
  }
  gen_vec_stmt(s$otherwise, rem, cx)
}

# L3: explicit PDV loop ---------------------------------------------------

has_stmt <- function(stmts, kind) {
  any(vapply(stmts, function(s) s$stmt %in% kind || has_stmt(stmt_children(s), kind), logical(1)))
}

# An iteration that executes no SET, MERGE or INPUT statement stops the step
# (sas_loop_unread()), whatever the condition that skipped the read; its
# explicit OUTPUT statements run and DELETE (or a subsetting IF) only drops
# its observation.  A STOP statement ends the step before (no NOTE).  Known
# shapes: every reading statement runs at the top level of the step or in the
# branches of IF statements and DO; END; blocks; no RETURN statement.
# Returns NULL for a known shape, otherwise what is not supported (failed
# loud only when such an iteration happens).
loop_read_why <- function(stmts) {
  readers <- c("set", "merge", "input")
  if (has_stmt(stmts, "return")) return("a RETURN statement in a DATA step with an iteration that executed no SET, MERGE or INPUT statement")
  known <- function(s) {
    if (s$stmt %in% readers || !has_stmt(list(s), readers)) return(TRUE)
    if (identical(s$stmt, "if") || (identical(s$stmt, "do") && identical(s$kind, "block"))) {
      return(all(vapply(stmt_children(s), known, logical(1))))
    }
    FALSE
  }
  if (!all(vapply(stmts, known, logical(1)))) {
    return("a DATA step iteration that executed no SET, MERGE or INPUT statement when a reading statement is in a DO loop or a SELECT group")
  }
  NULL
}

# The L3 step: a list of [cgi()] items.
gen_row_step <- function(an) {
  cx <- new_cx("row", an)
  rd <- if (length(an$readers) > 0) an$readers[[1]] else NULL
  L <- list()
  add <- function(...) L <<- c(L, lapply(list(...), function(x) if (is.language(x)) cgi(x) else x))

  has_input <- any(vapply(an$readers, function(r) r$stmt == "input", logical(1)))
  if (has_input) add(cgi_assign(".inf", cgi(infile_open_lang(an))))
  set_rd <- Find(function(r) r$stmt == "set", an$readers)
  if (!is.null(set_rd)) {
    pcx <- new_cx("vec", an)
    if (identical(rd, set_rd)) add_input_stages(pcx, include_n = FALSE)
    add(render_pipeline(".in", source_item(an, set_rd), pcx$stages),
        quote(.rows <- sas_rows(.in)), quote(.n_in <- length(.rows)), quote(.p <- 0L))
  }
  if (!is.null(rd) && rd$stmt == "merge") {
    srcs <- lapply(rd$datasets, ds_code, an = an, where = an$dsir$slots$where$effective)
    merged_vars <- unique(vctrs::list_unchop(lapply(rd$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap()))
    add(cgi(rlang::call2("<-", rlang::sym(".m_in"), cg_call("list", lapply(srcs, `[[`, "lang"))),
            paste0(".m_in <- list(\n  ", paste(vapply(srcs, `[[`, "", "text"), collapse = ",\n  "), "\n)")),
        quote(.m <- lapply(.m_in, sas_rows)),
        rlang::expr(.plan <- sas_merge_plan(.m_in, !!cg_val(an$by$vars), !!cg_val(an$by$descending))),
        quote(.n_in <- nrow(.plan)), quote(.p <- 0L))
    cx$merge_reset <- merged_vars
  }
  for (arr in an$arrays) {
    if (arr$temporary) {
      miss <- if (arr$type == "char") strrep(" ", arr$len) else NA_real_
      vals <- lapply(seq_len(arr$dim), function(k) {
        if (!is.null(arr$init) && k <= length(arr$init)) {
          if (arr$type == "char") sas_char(arr$init[[k]], arr$len) else cg_num(arr$init[[k]])
        } else miss
      })
      add(rlang::call2("<-", rlang::sym(paste0(".ta_", arr$name)), rlang::call2("c", !!!vals)))
    } else {
      add(rlang::call2("<-", rlang::sym(paste0(".arr_", arr$name)), cg_val(arr$elems)))
    }
  }

  pdv_order <- union(an$order, c("_error_", "_n_"))
  pdv_vals <- lapply(pdv_order, function(v) {
    inf <- an$info[[v]]
    if (is.null(inf) || inf$kind == "auto") return(if (v == "_error_") 0 else NA_real_)
    proto_lang(inf, use_init = isTRUE(inf$retained))
  })
  add(cgi(rlang::call2("<-", rlang::sym("pdv"), cg_call("list", stats::setNames(pdv_vals, pdv_order))),
          paste0("pdv <- list(\n", paste0("  ", r_name(pdv_order), " = ", vapply(pdv_vals, cg_text, ""), collapse = ",\n"), "\n)")))
  dump_vars <- setdiff(an$order, c("_error_", "_n_"))
  dump_names <- vapply(dump_vars, function(v) an$info[[v]]$disp %||% v, "", USE.NAMES = FALSE)
  is_flag <- startsWith(dump_vars, "first.") | startsWith(dump_vars, "last.")
  if (any(is_flag)) {
    base <- sub("^[^.]*[.]", "", dump_vars[is_flag])
    dump_names[is_flag] <- paste0(toupper(sub("[.].*$", "", dump_vars[is_flag])), ".",
                                  vapply(base, function(v) an$info[[v]]$disp %||% v, "", USE.NAMES = FALSE))
  }
  dump_fmts <- Filter(Negate(is.null), lapply(stats::setNames(dump_vars, dump_vars), function(v) an$formats[[v]]))
  add(quote(.proto <- pdv),
      rlang::call2("<-", rlang::sym(".dump"), rlang::call2("sas_dump_spec", cg_val(dump_vars), cg_val(dump_names),
                                     if (length(dump_fmts) == 0) quote(list())
                                     else cg_val(lapply(dump_fmts, function(f) f[c("name", "w", "d", "char", "text")])))))
  if (!is.null(rd) && !is.null(rd$nobs)) {
    nobs <- Reduce(function(a, b) rlang::call2("+", a, b), lapply(vapply(rd$datasets, `[[`, "", "name"), function(nm) {
      rlang::expr(nrow(work[[!!nm]]))
    }))
    add(rlang::call2("<-", pdv_ref(rd$nobs), nobs))
  }
  reset_vars <- Filter(function(v) an$info[[v]]$kind == "data" && !isTRUE(an$info[[v]]$retained), an$order)
  if (length(reset_vars) > 0) add(rlang::expr(.reset <- pdv[!!cg_val(reset_vars)]))
  if (!is.null(cx$merge_reset)) add(rlang::expr(.m_reset <- pdv[!!cg_val(cx$merge_reset)]))
  if (an$lag_sites > 0) add(quote(.lags <- new.env()))
  for (k in seq_along(an$dsir$slots$outputs)) {
    add(rlang::call2("<-", rlang::sym(paste0(".out", k)), quote(list())),
        rlang::call2("<-", rlang::sym(paste0(".vars", k)), cg_val(an$dsir$slots$outputs[[k]]$vars)))
  }

  body <- gen_row_block(an$stmts, cx)
  implicit_output <- length(an$dsir$slots$outputs) > 0 && !has_stmt(an$stmts, "output")
  # the loop body: one-line statements, the tryCatch() of the step's
  # statements and the implicit OUTPUT block
  handler <- function(v) cg_fn(alist(e = ), v)
  status <- rlang::call2("<-", rlang::sym(".status"), rlang::call2("tryCatch", brace(c(body, list("ok"))),
    sasonr_delete = handler("delete"), sasonr_stop = handler("stop"), sasonr_return = handler("ok")))
  status_text <- c("  .status <- tryCatch({", indent_lines(row_lines(body), 2L), "    \"ok\"",
                   "  }, sasonr_delete = function(e) \"delete\", sasonr_stop = function(e) \"stop\", sasonr_return = function(e) \"ok\")")
  dump <- rlang::call2("sas_iter_dump", rlang::sym("pdv"), rlang::sym(".dump"), !!!if (has_input) list(rlang::sym(".inf")))
  outs <- output_langs(seq_along(an$dsir$slots$outputs))
  out_if <- if (implicit_output) rlang::expr(if (.status == "ok") !!brace(outs))
  loop <- list(
    quote(.n_iter <- .n_iter + 1L),
    if (length(reset_vars) > 0) quote(pdv[names(.reset)] <- .reset),
    quote(pdv$`_n_` <- .n_iter),
    quote(pdv$`_error_` <- 0),
    if (!is.null(rd)) quote(.read <- FALSE),
    status,
    if (has_input) quote(if (.status == "stop" && !is.null(.inf$pdv)) pdv[names(.inf$pdv)] <- .inf$pdv),
    err_sync(),
    rlang::expr(.log <- c(.log, !!dump)),
    if (has_input) quote(sas_input_iter_end(.inf)),
    quote(if (.status == "stop") break),
    out_if,
    if (!is.null(rd)) rlang::expr(if (!.read) {
      sas_loop_unread(!!loop_read_why(an$stmts))
      break
    }) else quote(break)
  )
  loop <- Filter(Negate(is.null), loop)
  loop_text <- unlist(lapply(loop, function(x) {
    if (identical(x, status)) return(status_text)
    if (implicit_output && identical(x, out_if)) {
      return(c("  if (.status == \"ok\") {", indent_lines(vapply(outs, cg_text, ""), 2L), "  }"))
    }
    paste0("  ", cg_text(x))
  }))
  add(quote(.n_iter <- 0L), cgi(rlang::call2("repeat", brace(loop)), paste(c("repeat {", loop_text, "}"), collapse = "\n")))
  if (!is.null(set_rd)) add(quote(sas_reads_consumed(.p)))
  for (k in seq_along(an$dsir$slots$outputs)) {
    o <- an$dsir$slots$outputs[[k]]
    x <- cgi(rlang::call2("sas_rows_to_tibble", rlang::sym(paste0(".out", k)), rlang::call2("[", rlang::sym(".proto"), rlang::sym(paste0(".vars", k)))))
    if (any(o$vars != o$out_names)) x <- cgi_pipe(x, select_lang(o), sep = " |>\n  ")
    add(work_assign(o$name, x))
  }
  L
}

# The L3 statement emitters return lists of language objects.

pdv_ref <- function(v) rlang::call2("$", rlang::sym("pdv"), rlang::sym(v))
assign_lang <- function(target, value) rlang::call2("<-", target, value)
brace <- function(body) rlang::call2("{", !!!body)
err_sync <- function() quote(pdv <- sas_err_sync(pdv))

coerce_lang <- function(x, type, len) if (type == "char") rlang::call2("sas_char", x, as.integer(len)) else x
coerce_var_lang <- function(x, name, an) {
  inf <- an$info[[name]]
  coerce_lang(x, inf$type, inf$len)
}

gen_row_block <- function(stmts, cx) {
  unlist(lapply(stmts, function(s) {
    code <- gen_row_stmt(s, cx)
    if (length(code) > 0 && !s$stmt %in% c("if", "do", "select") && stmt_may_error(s)) code <- c(code, list(err_sync()))
    code
  }), recursive = FALSE) %||% list()
}

# The display lines of L3 statements (2-space indentation, as the rest of the
# generated text).
row_lines <- function(objs) unlist(lapply(objs, function(x) strsplit(cg_deparse(x), "\n", fixed = TRUE)[[1]]))

# A condition whose evaluation may raise a data error: evaluate it first so
# _ERROR_ is set before the branch runs.
row_cond <- function(e, cx, out_name) {
  if (!expr_may_error(e)) return(list(pre = list(), cond = rlang::call2("sas_true", gen_lang(e, cx))))
  cx$n_tmp <- cx$n_tmp + 1L
  v <- rlang::sym(paste0(".c", cx$n_tmp))
  list(pre = list(assign_lang(v, rlang::call2("sas_true", gen_lang(e, cx))), err_sync()), cond = v)
}

gen_row_stmt <- function(s, cx) {
  an <- cx$an
  g <- function(e) gen_lang(e, cx)
  switch(s$stmt,
    assign = {
      if (s$target$node == "var") {
        v <- s$target$name
        list(assign_lang(var_lang(v, cx), coerce_var_lang(g(s$value), v, an)))
      } else {
        arr <- an$arrays[[s$target$name]]
        list(assign_lang(gen_array_lang(s$target, cx), coerce_lang(g(s$value), arr$type, arr$len)))
      }
    },
    sum = {
      ref <- var_lang(s$target$name, cx)
      list(assign_lang(ref, rlang::call2("sas_f_sum", ref, g(s$value))))
    },
    substr_assign = {
      ref <- var_lang(s$var, cx)
      list(assign_lang(ref, cg_call("sas_substr_assign", c(list(ref, g(s$pos), if (is.null(s$len)) NULL else g(s$len), g(s$value)),
                                                           cg_at(s$line, s$col)))))
    },
    "if" = {
      rc <- row_cond(s$cond, cx)
      then <- brace(gen_row_block(list(s$then), cx))
      out <- if (is.null(s[["else"]])) rlang::call2("if", rc$cond, then) else rlang::call2("if", rc$cond, then, brace(gen_row_block(list(s[["else"]]), cx)))
      c(rc$pre, list(out))
    },
    subset_if = list(rlang::expr(if (!sas_true(!!g(s$cond))) sas_delete())),
    stop = list(quote(sas_stop_step(stmt = TRUE))),
    "return" = list(quote(sas_return())),
    delete = list(quote(sas_delete())),
    leave = list(quote(break)),
    continue = list(quote(next)),
    do = gen_row_do(s, cx),
    select = gen_row_select(s, cx),
    set = list(
      quote(.p <- .p + 1L),
      quote(if (.p > .n_in) sas_stop_step()),
      quote(pdv <- sas_read_row(pdv, .rows, .p)),
      quote(.read <- TRUE)
    ),
    input = c(
      list(assign_lang(rlang::sym("pdv"), rlang::expr(sas_input_stmt(.inf, pdv, !!input_spec_lang(s, an)))), quote(.read <- TRUE)),
      if (!is.null(an$infile$opts$end)) list(assign_lang(pdv_ref(an$infile$opts$end), quote(sas_input_at_end(.inf))))
    ),
    merge = c(
      list(quote(.p <- .p + 1L), quote(if (.p > .n_in) sas_stop_step()),
           assign_lang(rlang::sym("pdv"), rlang::call2("sas_merge_read", rlang::sym("pdv"), rlang::sym(".m"), rlang::sym(".plan"), rlang::sym(".p"), rlang::sym(".m_reset"),
                                         cg_val(vapply(s$datasets, function(d) d$options$in_var %||% NA_character_, "")),
                                         cg_val(an$by$vars)))),
      if (!is.null(s$end)) list(assign_lang(pdv_ref(s$end), quote(as.numeric(.p == .n_in)))),
      list(quote(.read <- TRUE))
    ),
    output = {
      targets <- if (length(s$targets) == 0) seq_along(an$dsir$slots$outputs) else {
        k <- match(s$targets, vapply(an$dsir$slots$outputs, `[[`, "", "name"))
        if (anyNA(k)) abort_syntax(paste0("OUTPUT to ", toupper(s$targets[is.na(k)][1]), " which is not on the DATA statement"), s$line)
        k
      }
      output_langs(targets)
    },
    put = list(assign_lang(rlang::sym(cx$sink), rlang::call2("c", rlang::sym(cx$sink), put_line_lang(s, cx)))),
    call = if (s$routine %in% c("missing", "sortn", "sortc")) gen_row_call_vars(s, cx) else {
      args <- lapply(s$args, function(a) cg_call("sas_chr_arg", list(g(a), s$routine, .line = as.integer(s$line))))
      list(cg_call("sas_symput", c(list(rlang::sym(".symtab"), args[[1]], args[[2]], trim = s$routine == "symputx"),
                                   if (length(args) >= 3) list(scope = args[[3]]))))
    },
    list()
  )
}

output_langs <- function(ks) {
  lapply(ks, function(k) {
    out <- rlang::sym(paste0(".out", k))
    assign_lang(rlang::call2("[[", out, rlang::call2("+", rlang::call2("length", out), 1L)), rlang::call2("[", rlang::sym("pdv"), rlang::sym(paste0(".vars", k))))
  })
}

# CALL MISSING / SORTN / SORTC on variables and array elements.
gen_row_call_vars <- function(s, cx) {
  an <- cx$an
  refs <- lapply(s$targets, function(t) if (t$node == "var") var_lang(t$name, cx) else gen_array_lang(t, cx))
  if (s$routine == "missing") {
    return(Map(function(ref, t) {
      ty <- expr_type(t, an)
      assign_lang(ref, if (ty$type == "char") strrep(" ", ty$len) else NA_real_)
    }, refs, s$targets))
  }
  cx$n_tmp <- cx$n_tmp + 1L
  tmp <- rlang::sym(paste0(".srt", cx$n_tmp))
  c(list(assign_lang(tmp, rlang::call2("sas_sort_values", cg_call("list", refs)))),
    Map(function(ref, k) assign_lang(ref, rlang::call2("[[", tmp, k)), refs, seq_along(refs)))
}

gen_row_do <- function(s, cx) {
  g <- function(e) gen_lang(e, cx)
  body <- gen_row_block(s$body, cx)
  if (s$kind == "block") return(body)
  cx$n_tmp <- cx$n_tmp + 1L
  k <- cx$n_tmp
  tmp <- function(stem) rlang::sym(paste0(".", stem, k))
  brk_unless <- function(cond) rlang::call2("if", rlang::call2("!", cond), quote(break))
  if (s$kind == "while") return(list(rlang::call2("repeat", brace(c(list(brk_unless(rlang::call2("sas_true", g(s$cond)))), body)))))
  if (s$kind == "until") {
    return(list(assign_lang(tmp("first"), TRUE), rlang::call2("repeat", brace(c(list(
      rlang::call2("if", rlang::call2("&&", rlang::call2("!", tmp("first")), rlang::call2("sas_true", g(s$cond))), quote(break)),
      assign_lang(tmp("first"), FALSE)), body)))))
  }
  ref <- var_lang(s$var, cx)
  if (s$kind == "list") {
    return(list(rlang::call2("for", tmp("v"), cg_call("list", lapply(s$values, g)),
                     brace(c(list(assign_lang(ref, coerce_var_lang(tmp("v"), s$var, cx$an))), body)))))
  }
  wu <- s$while_until
  # the start, TO and BY are evaluated before the index is assigned
  list(
    assign_lang(tmp("from"), g(s$from)),
    assign_lang(tmp("to"), g(s$to)),
    assign_lang(tmp("by"), if (is.null(s$by)) 1 else g(s$by)),
    assign_lang(ref, tmp("from")),
    assign_lang(tmp("first"), TRUE),
    rlang::call2("repeat", brace(c(
      list(rlang::call2("if", rlang::call2("!", tmp("first")), brace(c(
        if (identical(wu$kind, "until")) list(rlang::call2("if", rlang::call2("sas_true", g(wu$cond)), quote(break))),
        list(assign_lang(ref, rlang::expr(!!ref + !!tmp("by"))))
      )))),
      list(assign_lang(tmp("first"), FALSE), brk_unless(rlang::call2("sas_do_test", ref, tmp("to"), tmp("by")))),
      if (identical(wu$kind, "while")) list(brk_unless(rlang::call2("sas_true", g(wu$cond)))),
      body
    )))
  )
}

gen_row_select <- function(s, cx) {
  pre <- list()
  sel <- NULL
  if (!is.null(s$expr)) {
    cx$n_tmp <- cx$n_tmp + 1L
    sel <- paste0(".s", cx$n_tmp)
    pre <- list(assign_lang(rlang::sym(sel), gen_lang(s$expr, cx)))
  }
  other <- if (s$has_otherwise) gen_row_stmt(s$otherwise, cx) else list(rlang::call2("sas_select_error", as.integer(s$line)))
  # the WHEN chain from the last clause up: `if (c1) {..} else if (c2) {..} else {..}`
  conds <- lapply(s$whens, when_cond_lang, sel = if (!is.null(sel)) rlang::sym(sel), cx = cx)
  bodies <- lapply(s$whens, function(w) brace(gen_row_stmt(w$body, cx)))
  chain <- if (length(s$whens) == 0) other else {
    acc <- brace(other)
    for (k in rev(seq_along(s$whens))) acc <- rlang::call2("if", conds[[k]], bodies[[k]], acc)
    list(acc)
  }
  out <- c(pre, chain)
  # LEAVE leaves the SELECT group: the SELECT runs inside a one-pass loop
  # whose `break` is the LEAVE
  if (!select_ctl(s, "leave")) return(out)
  if (select_ctl(s, "continue", nested = TRUE)) {
    abort_unsupported("CONTINUE inside a SELECT group that holds a LEAVE", s$line)
  }
  list(rlang::call2("repeat", brace(c(out, list(quote(break))))))
}

# Does a WHEN or OTHERWISE body of SELECT `s` hold statement `what` that acts
# on this SELECT (outside an iterative DO loop; also inside nested SELECT
# groups when `nested`)?
select_ctl <- function(s, what, nested = FALSE) {
  has <- function(x) {
    if (is.null(x)) return(FALSE)
    if (x$stmt == what) return(TRUE)
    if (x$stmt == "do" && x$kind != "block") return(FALSE)
    if (x$stmt == "select" && !nested) return(FALSE)
    any(vapply(stmt_children(x), has, NA))
  }
  any(vapply(stmt_children(s), has, NA))
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
    body <- c(list(cgi(quote(.fput <- character()))), body,
              list(cgi(rlang::call2("sas_file_write", an$file$path, quote(.fput), mod = isTRUE(an$file$mod)))))
  }
  uninit <- uninit_vars(an)
  notes <- c(compile_notes(),
             if (length(uninit) > 0) sprintf("NOTE: Variable %s is uninitialized.",
                                             vapply(uninit, function(v) an$info[[v]]$disp %||% v, "")))
  list(
    # `exprs`: the language objects that run; `code`: their display
    code = paste(c(header, vapply(body, `[[`, "", "text")), collapse = "\n"), exprs = lapply(body, `[[`, "lang"),
    level = an$level, reasons = an$reasons,
    line = step$line, outputs = vapply(an$dsir$slots$outputs, `[[`, "", "name"), meta = out_meta(an),
    compile_notes = notes
  )
}

# Names that exist only in generated step code: the rlang::expr() templates
# above mention them, and R CMD check would read them as undefined globals.
utils::globalVariables(c(".inf", ".m_in", ".status", "pdv", "work"))
