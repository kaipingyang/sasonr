# Level classification (L1 / L2 / L3) for an analysed DATA step.
#
# A step is compiled to vectorised dplyr code only when the analysis can show
# the result equals row-by-row execution; otherwise it falls back to L3.

decl_stmts <- c("length", "retain", "array", "format", "label", "keep", "drop",
                "rename", "by", "where", "null", "datalines", "informat", "attrib", "infile", "file")

is_decl <- function(s) s$stmt %in% decl_stmts
is_inert <- function(s) is_decl(s) || s$stmt == "put"

expr_children <- function(e) {
  switch(e$node,
    unary = list(e$arg),
    binary = ,
    compare = list(e$lhs, e$rhs),
    chain = e$args,
    "in" = ,
    is_missing = list(e$lhs),
    between = list(e$lhs, e$lo, e$hi),
    contains = ,
    like = list(e$lhs, e$rhs),
    call = Filter(function(a) a$node != "of", e$args),
    array_ref = e$index,
    method = lapply(e$args, `[[`, "value"),
    list()
  )
}

walk_expr <- function(e, f) {
  if (is.null(e)) return(invisible())
  f(e)
  for (ch in expr_children(e)) walk_expr(ch, f)
  invisible()
}

expr_reads <- function(e) {
  out <- character()
  walk_expr(e, function(x) {
    if (x$node == "var") out <<- c(out, x$name)
    if (x$node == "call") for (a in x$args) if (a$node == "of") out <<- c(out, a$vars)
  })
  out
}

stmt_exprs <- function(s) {
  switch(s$stmt,
    assign = list(s$target, s$value),
    sum = list(s$target, s$value),
    substr_assign = list(s$pos, s$len, s$value),
    "if" = list(s$cond),
    subset_if = ,
    where = list(s$cond),
    do = c(list(s$from, s$to, s$by, s$cond, s$while_until$cond), s$values),
    select = c(list(s$expr), vctrs::list_unchop(lapply(s$whens, `[[`, "values"), ptype = list(), name_spec = rlang::zap())),
    put = c(lapply(Filter(function(it) it$kind %in% c("value", "named"), s$items), function(it) list(node = "var", name = it$name)),
            lapply(Filter(function(it) it$kind == "ptr" && !is.null(it$v), s$items), `[[`, "v")),
    call = s$args,
    declare = lapply(s$args, `[[`, "value"),
    method = list(s$call),
    list()
  )
}

stmt_children <- function(s) {
  switch(s$stmt,
    "if" = Filter(Negate(is.null), list(s$then, s[["else"]])),
    do = s$body,
    select = Filter(Negate(is.null), c(lapply(s$whens, `[[`, "body"), list(s$otherwise))),
    list()
  )
}

stmt_reads <- function(s) {
  out <- vctrs::list_unchop(lapply(Filter(Negate(is.null), stmt_exprs(s)), expr_reads), ptype = character(), name_spec = rlang::zap())
  c(out, vctrs::list_unchop(lapply(stmt_children(s), stmt_reads), ptype = character(), name_spec = rlang::zap()))
}

stmt_assigns <- function(s) {
  out <- switch(s$stmt,
    assign = if (s$target$node == "var") s$target$name,
    sum = s$target$name,
    do = s$var,
    call = vctrs::list_unchop(lapply(s$targets, function(t) if (t$node == "var") t$name), ptype = character(), name_spec = rlang::zap()),
    NULL
  )
  c(out, vctrs::list_unchop(lapply(stmt_children(s), stmt_assigns), ptype = character(), name_spec = rlang::zap()))
}

is_flag_output <- function(s) {
  s$stmt == "if" && is.null(s[["else"]]) && s$then$stmt == "output" &&
    length(s$then$targets) == 0 && s$cond$node == "var" &&
    (startsWith(s$cond$name, "first.") || startsWith(s$cond$name, "last."))
}

is_reset <- function(s, v) {
  s$stmt == "if" && is.null(s[["else"]]) && s$cond$node == "var" &&
    startsWith(s$cond$name, "first.") && s$then$stmt == "assign" &&
    s$then$target$node == "var" && s$then$target$name == v &&
    s$then$value$node %in% c("num", "missing")
}

classify_step <- function(an) {
  sc <- new.env(parent = emptyenv())
  sc$l3 <- character()
  sc$l2 <- character()
  sc$sums <- list()
  sc$outputs <- list()
  sc$filters <- integer()
  add3 <- function(r) sc$l3 <- union(sc$l3, r)
  add2 <- function(r) sc$l2 <- union(sc$l2, r)

  check_expr <- function(e, guarded) {
    walk_expr(e, function(x) {
      if (x$node == "call" && !is.null(x$lag)) {
        if (guarded) add3("conditional LAG/DIF (queue updated only when executed)") else add2("unconditional LAG/DIF")
      }
      if (x$node == "array_ref") add3("array references")
    })
  }

  scan_block <- function(stmts, guarded, top, tail_ok) {
    n <- length(stmts)
    for (i in seq_len(n)) {
      rest <- if (i < n) stmts[(i + 1L):n] else list()
      tail_i <- tail_ok && all(vapply(rest, is_inert, logical(1)))
      scan_stmt(stmts[[i]], guarded, top %||% i, is.null(top), tail_i)
    }
  }

  scan_stmt <- function(s, guarded, ti, direct, tail_i) {
    switch(s$stmt,
      assign = {
        if (s$target$node == "array_ref") add3("array references")
        check_expr(s$value, guarded)
      },
      sum = {
        if (guarded || !direct) add3("conditional or nested sum statement")
        add2("sum statement (RETAIN + accumulate)")
        check_expr(s$value, guarded)
        sc$sums <- c(sc$sums, list(list(var = s$target$name, idx = ti, stmt = s)))
      },
      "if" = {
        if (is_flag_output(s)) {
          sc$outputs <- c(sc$outputs, list(list(idx = ti, ok = direct && !guarded && tail_i, cond = s$cond$name)))
          return(invisible())
        }
        check_expr(s$cond, guarded)
        scan_stmt(s$then, TRUE, ti, FALSE, tail_i)
        if (!is.null(s[["else"]])) scan_stmt(s[["else"]], TRUE, ti, FALSE, tail_i)
      },
      subset_if = {
        check_expr(s$cond, guarded)
        sc$filters <- c(sc$filters, ti)
      },
      delete = sc$filters <- c(sc$filters, ti),
      do = {
        if (s$kind == "block") {
          scan_block(s$body, guarded, ti, tail_i)
        } else {
          add3("iterative DO loop")
          scan_block(s$body, TRUE, ti, FALSE)
        }
      },
      select = {
        if (!s$has_otherwise) add3("SELECT without OTHERWISE (runtime error semantics)")
        check_expr(s$expr, guarded)
        for (w in s$whens) {
          for (v in w$values) check_expr(v, TRUE)
          scan_stmt(w$body, TRUE, ti, FALSE, tail_i)
        }
        if (!is.null(s$otherwise)) scan_stmt(s$otherwise, TRUE, ti, FALSE, tail_i)
      },
      leave = ,
      continue = add3("LEAVE/CONTINUE"),
      output = {
        ok <- direct && !guarded && tail_i && length(s$targets) == 0
        sc$outputs <- c(sc$outputs, list(list(idx = ti, ok = ok, cond = NULL)))
      },
      put = if (!tail_i) add3("PUT followed by other executable statements"),
      call = add3("CALL routine"),
      stop = add3("STOP statement"),
      "return" = add3("RETURN statement"),
      array = add3("ARRAY statement"),
      NULL
    )
    invisible()
  }

  stmts <- an$stmts
  step <- an$step
  if (length(step$outputs) > 1) add3("multiple output data sets")
  readers <- an$readers
  if (length(readers) > 1) add3("more than one SET/MERGE/INPUT statement")
  if (length(an$objects)) add3("component objects (DECLARE HASH)")
  if (any(c("_error_") %in% an$uses)) add3("_ERROR_ is referenced")
  if (!is.null(an$infile) && !is.null(an$infile$opts$end)) add3("INFILE END= variable")
  n_ds <- sum(vapply(readers, function(r) r$stmt %in% c("set", "merge"), logical(1)))
  if (n_ds > 1) {
    # each SET or MERGE statement has its own reader (row by row)
    ds_rd <- Filter(function(r) r$stmt %in% c("set", "merge"), readers)
    merges <- Filter(function(r) r$stmt == "merge", ds_rd)
    if (length(merges) > 1) abort_unsupported("more than one MERGE statement in a step", merges[[2]]$line)
    if (!is.null(an$dsir$slots$where$effective)) abort_unsupported("a WHERE statement in a step with several SET or MERGE statements", ds_rd[[2]]$line)
  }
  for (r in readers) {
    if (r$stmt == "merge" && is.null(r$by_spec)) abort_unsupported("MERGE without BY (one-to-one merge)", r$line)
  }
  if (length(readers) >= 1) {
    rd <- readers[[1]]
    if (any(vapply(readers, function(r) r$stmt == "input", logical(1))) && is.null(an$datalines) && is.null(an$infile)) {
      abort_unsupported("INPUT without DATALINES or INFILE", rd$line)
    }
    if (rd$stmt == "input") {
      in_vars <- vapply(Filter(function(it) it$k == "var", rd$items), `[[`, "", "name")
      ptr_reads <- vctrs::list_unchop(lapply(Filter(function(it) !is.null(it$v), rd$items), function(it) expr_reads(it$v)), ptype = character(), name_spec = rlang::zap())
      if (length(setdiff(ptr_reads, in_vars)) > 0) add3("INPUT pointer depends on non-INPUT variables")
    }
    if (rd$stmt == "merge") {
      for (r in merge_row_reasons(an)) add3(r)
    }
    exec_idx <- which(!vapply(stmts, is_decl, logical(1)))
    rdr_idx <- which(vapply(stmts, function(s) s$stmt %in% c("set", "merge", "input"), logical(1)))
    if (length(rdr_idx) == 0) {
      add3("SET/INPUT inside a block")
    } else if (rdr_idx[1] != exec_idx[1]) {
      why <- before_read_move(an)$why
      add3(paste0("executable statements before SET/INPUT", if (!is.null(why)) paste0(" (", why, ")")))
    }
    if (!is.null(rd$end)) add2("END= variable")
    if (!is.null(rd$nobs)) add3("NOBS= variable")
    if (rd$stmt == "set" && length(rd$datasets) > 1 && !is.null(an$by)) add2("interleaving SET with BY")
  }
  if (!is.null(an$dsir$slots$where$effective) && (length(readers) == 0 || readers[[1]]$stmt == "input")) {
    abort_syntax("WHERE statement requires SET or MERGE", step$line)
  }
  if (!is.null(an$by)) add2("BY-group processing (FIRST./LAST.)")

  scan_block(stmts, FALSE, NULL, TRUE)

  skip_top <- integer()
  output_pattern <- NULL
  if (length(sc$outputs) > 0) {
    o <- sc$outputs[[1]]
    if (length(sc$outputs) == 1 && o$ok && length(step$outputs) <= 1) {
      add2("OUTPUT at the end of the step")
      skip_top <- c(skip_top, o$idx)
      output_pattern <- list(cond = o$cond)
    } else {
      add3("explicit OUTPUT")
    }
  }

  # Sum statements: vectorisable as (grouped) cumulative sums only when the
  # accumulator is not otherwise touched.
  sum_plan <- list()
  sum_vars <- vapply(sc$sums, `[[`, character(1), "var")
  for (v in unique(sum_vars)) {
    entries <- sc$sums[sum_vars == v]
    if (length(entries) > 1) {
      add3("several sum statements on one variable")
      next
    }
    k <- entries[[1]]$idx
    reset <- which(vapply(stmts, is_reset, logical(1), v = v))
    reset <- reset[reset < k]
    if (length(reset) > 1) {
      add3("several resets of a sum variable")
      next
    }
    before <- setdiff(seq_len(k - 1L), reset)
    assigned_elsewhere <- any(vapply(stmts[-c(k, reset)], function(s) v %in% stmt_assigns(s), logical(1)))
    read_before <- any(vapply(stmts[before], function(s) v %in% stmt_reads(s), logical(1))) ||
      v %in% expr_reads(entries[[1]]$stmt$value)
    from_input <- any(vapply(readers, function(r) v %in% vctrs::list_unchop(lapply(r$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap()), logical(1)))
    filtered_before <- any(sc$filters < k)
    if (assigned_elsewhere || read_before || from_input || filtered_before) {
      add3(paste0("sum variable ", v, " is used outside the accumulate pattern"))
      next
    }
    init <- an$info[[v]]$init
    if (is.null(init) || is.na(init)) init <- 0
    plan <- list(init = init, reset_var = NULL, reset_value = NULL)
    if (length(reset) == 1) {
      rs <- stmts[[reset]]
      if (is.null(an$by) || !sub("first.", "", rs$cond$name, fixed = TRUE) %in% an$by$vars) {
        add3("sum reset not tied to a BY variable")
        next
      }
      plan$reset_var <- rs$cond$name
      plan$reset_value <- const_value(rs$then$value)
      if (is.na(plan$reset_value)) {
        add3("sum reset to missing")
        next
      }
      skip_top <- c(skip_top, reset)
    }
    sum_plan[[v]] <- plan
  }

  # RETAIN on a variable that is assigned in the step carries state across rows.
  # The carry-forward (LOCF / baseline) shape is vectorisable; anything else is L3.
  assigns_var <- function(v) vapply(stmts, function(s) v %in% stmt_assigns(s), logical(1))
  retained_assigned <- Filter(function(v) any(assigns_var(v)), setdiff(an$retain_stmt_vars, names(sum_plan)))
  state_vars <- union(names(sum_plan), retained_assigned)
  carry_plan <- list()
  for (v in retained_assigned) {
    plan <- carry_plan_for(v, which(assigns_var(v)), stmts, an, sc$filters, state_vars)
    if (is.null(plan)) {
      add3(paste0("RETAIN of assigned variable ", v))
    } else {
      carry_plan[[v]] <- plan
      add2("retained value carried forward (baseline / LOCF)")
    }
  }

  level <- if (length(sc$l3) > 0) "L3" else if (length(sc$l2) > 0) "L2" else "L1"
  reasons <- switch(level, L3 = sc$l3, L2 = sc$l2, L1 = "no cross-row state")
  list(level = level, reasons = reasons, sum_plan = sum_plan, skip_top = skip_top,
       output_pattern = output_pattern, carry_plan = carry_plan)
}

# Why a match-merge step has no vectorised form (character(0): it has one).
# The vectorised input table holds the values the data sets write; a variable
# read by MERGE that the step assigns or retains keeps the changed value on
# the next row of its BY group where its data set has no row, which only the
# row-wise path does.
merge_row_reasons <- function(an) {
  rd <- an$readers[[1]]
  vars <- unique(vctrs::list_unchop(lapply(rd$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap()))
  assigned <- unique(vctrs::list_unchop(lapply(an$stmts, stmt_assigns_all), ptype = character(), name_spec = rlang::zap()))
  hit <- intersect(vars, union(assigned, an$retain_stmt_vars))
  if (length(hit)) {
    return(paste0("MERGE with an assignment to or RETAIN of a variable read by MERGE (", paste(hit, collapse = ", "), ")"))
  }
  character()
}

# Executable statements before the top-level read (SET, MERGE, INPUT). They
# run at the top of every iteration, also on the one where the read meets the
# end of the input and the step stops, and they see the previous row's input
# variables. A leading run of `v = <literal>;` gives the same result when it
# runs right after the read: it reads nothing, the literal has the type of v
# (no conversion, nothing logged on the extra run), and v is a variable the
# step creates that the read neither reads nor writes and that is not
# retained. Returns `list(stmts, why)`: `stmts` the statements with that run
# moved to just after the read (NULL when nothing is to be moved or it cannot
# be), `why` what blocks the move (NULL when nothing does).
before_read_move <- function(an) {
  stmts <- an$stmts
  rdr <- which(vapply(stmts, function(s) s$stmt %in% c("set", "merge", "input"), logical(1)))
  if (length(rdr) == 0) return(list(stmts = NULL, why = NULL))
  r <- rdr[1]
  pre <- which(!vapply(stmts[seq_len(r - 1L)], is_decl, logical(1)))
  if (length(pre) == 0) return(list(stmts = NULL, why = NULL))
  used_by_read <- unique(c(unlist(stmts[[r]], use.names = FALSE), unlist(an$infile, use.names = FALSE)))
  for (i in pre) {
    s <- stmts[[i]]
    if (s$stmt != "assign") {
      what <- switch(s$stmt, sum = "sum statement", subset_if = "subsetting IF", paste0(toupper(s$stmt), " statement"))
      return(list(stmts = NULL, why = what))
    }
    if (s$target$node != "var") return(list(stmts = NULL, why = "assignment to an array element"))
    v <- s$target$name
    inf <- an$info[[v]]
    if (is.null(inf) || v %in% c("_n_", "_error_") || inf$kind != "data") {
      return(list(stmts = NULL, why = "assignment to an automatic variable"))
    }
    if (isTRUE(inf$retained) || v %in% an$retain_stmt_vars) {
      return(list(stmts = NULL, why = paste0("assignment to retained variable ", v)))
    }
    if (v %in% used_by_read) {
      return(list(stmts = NULL, why = paste0("assignment to variable ", v, " used by the read")))
    }
    lit <- switch(s$value$node, num = "num", missing = "num", str = "char", NA_character_)
    if (is.na(lit)) return(list(stmts = NULL, why = "assignment of an expression that is not a literal"))
    if (!identical(lit, inf$type)) return(list(stmts = NULL, why = paste0("type conversion in the assignment to ", v)))
  }
  rest <- setdiff(seq_along(stmts), pre)
  at <- match(r, rest)
  list(stmts = stmts[c(rest[seq_len(at)], pre, rest[-seq_len(at)])], why = NULL)
}

has_lag_call <- function(e) {
  found <- FALSE
  walk_expr(e, function(x) if (x$node == "call" && !is.null(x$lag)) found <<- TRUE)
  found
}

# `v = e;` or `if c then v = e;` (top level, no ELSE) -> list(cond, value)
carry_assign <- function(s, v) {
  if (s$stmt == "assign" && s$target$node == "var" && s$target$name == v) {
    return(list(cond = NULL, value = s$value))
  }
  if (s$stmt == "if" && is.null(s[["else"]]) && s$then$stmt == "assign" &&
      s$then$target$node == "var" && s$then$target$name == v) {
    return(list(cond = s$cond, value = s$then$value))
  }
  NULL
}

# A retained variable whose value is carried forward from the last row on
# which one of its assignments fired:
#   retain base; if first.id then base = aval;
#   retain base; if first.id then base = .; if ablfl = 'Y' then base = aval;
# Row-by-row this is base[r] = value of the last assignment that fired on row r,
# otherwise base[r - 1]; it equals a vectorised forward fill when
#   * every assignment is a top-level `v = e;` / `if c then v = e;`,
#   * c and e depend on the current row only (no LAG/DIF, no retained or sum
#     variables, not v itself),
#   * v is not read before its first assignment or between its assignments,
#   * no subsetting IF / DELETE sits between the first and last assignment,
#   * v does not come from the input data set (SET would overwrite it).
# Returns NULL when the shape is not provably equivalent.
carry_plan_for <- function(v, idx, stmts, an, filters, state_vars) {
  input_vars <- vctrs::list_unchop(lapply(an$readers, function(r) {
    if (r$stmt == "input") input_var_names(r) else vctrs::list_unchop(lapply(r$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap())
  }), ptype = character(), name_spec = rlang::zap())
  if (v %in% input_vars) return(NULL)
  sets <- list()
  for (k in idx) {
    a <- carry_assign(stmts[[k]], v)
    if (is.null(a)) return(NULL)
    for (e in Filter(Negate(is.null), list(a$value, a$cond))) {
      if (has_lag_call(e) || any(expr_reads(e) %in% state_vars)) return(NULL)
    }
    sets <- c(sets, list(list(idx = k, cond = a$cond, value = a$value)))
  }
  first <- min(idx)
  last <- max(idx)
  reads_v <- function(ks) any(vapply(stmts[ks], function(s) v %in% stmt_reads(s), logical(1)))
  if (first > 1 && reads_v(seq_len(first - 1L))) return(NULL)
  mid <- setdiff(seq.int(first, last), idx)
  if (length(mid) > 0 && reads_v(mid)) return(NULL)
  if (any(filters > first & filters < last)) return(NULL)
  list(sets = sets, last = last)
}
