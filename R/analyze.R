# Compile-time analysis of a DATA step AST.
#
# Builds the PDV (variables in compile order with type/length/retain status),
# resolves OF lists and array declarations, validates functions and formats,
# and decides the compilation level (plan.md section 2.2):
#   L1  pure vectorisable: assignments, IF/ELSE, SELECT, WHERE, KEEP/DROP...
#   L2  recognised cross-row patterns: BY + FIRST./LAST., sum statements
#       (optionally reset at FIRST.x), unconditional LAG/DIF, END=,
#       `if last.x then output;`
#   L3  anything else: explicit row-by-row PDV loop.

# Function catalogue ------------------------------------------------------

fn_spec <- function(type, min, max = min, len = NA) list(type = type, min = min, max = max, len = len)

fn_table <- list(
  sum = fn_spec("num", 1, Inf), n = fn_spec("num", 1, Inf), nmiss = fn_spec("num", 1, Inf),
  mean = fn_spec("num", 1, Inf), min = fn_spec("num", 1, Inf), max = fn_spec("num", 1, Inf),
  round = fn_spec("num", 1, 2), mod = fn_spec("num", 2), int = fn_spec("num", 1),
  abs = fn_spec("num", 1), floor = fn_spec("num", 1), ceil = fn_spec("num", 1),
  sqrt = fn_spec("num", 1), exp = fn_spec("num", 1), log = fn_spec("num", 1),
  log10 = fn_spec("num", 1), sign = fn_spec("num", 1), missing = fn_spec("num", 1),
  coalesce = fn_spec("num", 1, Inf), mdy = fn_spec("num", 3), year = fn_spec("num", 1),
  month = fn_spec("num", 1), day = fn_spec("num", 1), qtr = fn_spec("num", 1),
  weekday = fn_spec("num", 1), today = fn_spec("num", 0), date = fn_spec("num", 0),
  datepart = fn_spec("num", 1), timepart = fn_spec("num", 1), hour = fn_spec("num", 1),
  minute = fn_spec("num", 1), second = fn_spec("num", 1), hms = fn_spec("num", 3),
  intck = fn_spec("num", 3), intnx = fn_spec("num", 3, 4), index = fn_spec("num", 2),
  indexc = fn_spec("num", 2, Inf), length = fn_spec("num", 1), lengthn = fn_spec("num", 1),
  cats = fn_spec("char", 1, Inf, 200L), catt = fn_spec("char", 1, Inf, 200L),
  cat = fn_spec("char", 1, Inf, 200L), catx = fn_spec("char", 2, Inf, 200L),
  trim = fn_spec("char", 1, 1, "arg"), trimn = fn_spec("char", 1, 1, "arg"),
  strip = fn_spec("char", 1, 1, "arg"), left = fn_spec("char", 1, 1, "arg"),
  right = fn_spec("char", 1, 1, "arg"), upcase = fn_spec("char", 1, 1, "arg"),
  lowcase = fn_spec("char", 1, 1, "arg"), propcase = fn_spec("char", 1, 1, "arg"),
  reverse = fn_spec("char", 1, 1, "arg"), substr = fn_spec("char", 2, 3, "arg"),
  scan = fn_spec("char", 2, 3, 200L), compress = fn_spec("char", 1, 2, "arg"),
  tranwrd = fn_spec("char", 3, 3, 200L), translate = fn_spec("char", 3, 3, "arg"),
  coalescec = fn_spec("char", 1, Inf, 200L)
)

# lag, lag2, dif3 ... -> list(kind, n) or NULL
lag_kind <- function(name) {
  sp <- split_trailing_digits(name)
  if (!sp$base %in% c("lag", "dif")) return(NULL)
  list(kind = sp$base, n = if (is.na(sp$width)) 1L else sp$width)
}

# Analysis state ----------------------------------------------------------

new_state <- function(meta) {
  st <- new.env(parent = emptyenv())
  st$order <- character()
  st$info <- list()
  st$arrays <- list()
  st$formats <- list()
  st$labels <- character()
  st$keep <- list()
  st$drop <- list()
  st$rename <- character()
  st$readers <- list()
  st$by <- NULL
  st$wheres <- list()
  st$meta <- meta
  st$lag_sites <- 0L
  st$loop_depth <- 0L
  st$datalines <- NULL
  st$uses <- character()
  st$retain_stmt_vars <- character()
  st$retain_only <- character()
  st$infile <- NULL
  st$informats <- list()
  st$numlen <- list()
  st$dl <- NULL
  st
}

reg_var <- function(st, name, type = NA_character_, len = NA_integer_, kind = "data",
                    disp = name, retained = FALSE, init = NULL) {
  inf <- st$info[[name]]
  if (is.null(inf)) {
    st$order <- c(st$order, name)
    inf <- list(type = type, len = len, kind = kind, disp = disp, retained = retained, init = init)
  } else {
    inf$label_only <- NULL
    if (is.na(inf$type) && !is.na(type)) {
      inf$type <- type
      inf$len <- len
    }
    if (retained) inf$retained <- TRUE
    if (!is.null(init)) inf$init <- init
  }
  st$info[[name]] <- inf
  invisible(inf)
}

var_type <- function(st, name) {
  inf <- st$info[[name]]
  if (is.null(inf) || is.na(inf$type)) "num" else inf$type
}

# Resolve variable-list items (names, ranges, prefixes, arrays) to names.
resolve_items <- function(items, st, line = NULL, register = TRUE) {
  out <- character()
  disp <- character()
  for (it in items) {
    names <- switch(it$kind,
      name = {
        if (!is.null(it$text)) disp[it$name] <- it$text
        it$name
      },
      range = {
        nms <- expand_numbered_range(it$from, it$to, line)
        if (!is.null(it$text)) {
          # the stored name keeps the spelling of the first name of the range
          base <- split_trailing_digits(it$text)$base
          disp[nms] <- paste0(base, substring(nms, nchar(base) + 1L))
        }
        nms
      },
      pos_range = {
        a <- match(it$from, st$order)
        b <- match(it$to, st$order)
        if (is.na(a) || is.na(b) || a > b) abort_syntax(paste0("invalid variable range ", it$from, "--", it$to), line)
        st$order[a:b]
      },
      prefix = st$order[startsWith(st$order, it$prefix) & vapply(st$order, function(v) st$info[[v]]$kind == "data", logical(1))],
      array = {
        arr <- st$arrays[[it$name]]
        if (is.null(arr)) abort_syntax(paste0("'", it$name, "' is not an array"), line)
        if (arr$temporary) abort_unsupported("temporary arrays in variable lists", line)
        arr$elems
      },
      special = {
        data_vars <- st$order[vapply(st$order, function(v) st$info[[v]]$kind == "data", logical(1))]
        switch(it$name,
          "_all_" = data_vars,
          "_numeric_" = data_vars[vapply(data_vars, function(v) var_type(st, v) == "num", logical(1))],
          "_character_" = data_vars[vapply(data_vars, function(v) var_type(st, v) == "char", logical(1))]
        )
      }
    )
    out <- c(out, names)
  }
  if (register) for (v in out) if (is.null(st$info[[v]])) reg_var(st, v, disp = disp[v] %|NA|% v)
  attr(out, "disp") <- disp
  out
}

# Expression typing -------------------------------------------------------

expr_type <- function(e, st) {
  num <- list(type = "num", len = 8L)
  if (!is.null(e$sql_type)) return(e$sql_type)
  switch(e$node,
    num = ,
    missing = num,
    str = list(type = "char", len = max(1L, nchar(e$value))),
    var = {
      inf <- st$info[[e$name]]
      if (!is.null(inf) && identical(inf$type, "char")) list(type = "char", len = inf$len) else num
    },
    array_ref = {
      arr <- st$arrays[[e$name]]
      if (arr$type == "char") list(type = "char", len = arr$len) else num
    },
    binary = {
      if (e$op == "||") {
        a <- expr_type(e$lhs, st)
        b <- expr_type(e$rhs, st)
        list(type = "char", len = (if (a$type == "char") a$len else 12L) + (if (b$type == "char") b$len else 12L))
      } else {
        num
      }
    },
    call = {
      lk <- lag_kind(e$name)
      if (!is.null(lk)) {
        if (lk$kind == "dif") return(num)
        return(expr_type(e$args[[1]], st))
      }
      if (e$name == "put") {
        a <- expr_type(e$args[[1]], st)
        w <- fmt_width(e$fmt, a$type == "char")
        if (is.na(w)) w <- if (a$type == "char") a$len + if (identical(e$fmt$name, "quote")) 2L else 0L else 12L
        return(list(type = "char", len = as.integer(w)))
      }
      if (e$name == "input") {
        uf <- user_fmt(e$fmt$name, e$fmt$char, "invalue")
        if (e$fmt$char || isTRUE(uf$char)) {
          w <- if (!is.null(uf) && is.na(e$fmt$w)) uf$default_w else e$fmt$w
          if (is.na(w) && is.null(uf)) w <- registry_defw("informat", e$fmt)
          if (is.na(w)) {
            a <- expr_type(e$args[[1]], st)
            w <- if (a$type == "char") a$len else 12L
          }
          # $HEXw. yields w/2 bytes, every other character informat keeps w
          len <- if (identical(e$fmt$name, "hex")) as.integer(w %/% 2L) else as.integer(w)
          return(list(type = "char", len = len))
        }
        return(num)
      }
      spec <- fn_table[[e$name]]
      if (is.null(spec) || spec$type == "num") return(num)
      if (identical(spec$len, "arg")) {
        a <- expr_type(e$args[[1]], st)
        # SUBSTRN of a number: length 8 (SAS 9.4); others: the BEST12. text
        return(list(type = "char", len = if (a$type == "char") a$len else if (e$name == "substrn") 8L else 12L))
      }
      list(type = "char", len = spec$len)
    },
    num
  )
}

# Expression annotation ---------------------------------------------------

an_expr <- function(e, st) {
  if (is.null(e)) return(e)
  switch(e$node,
    var = {
      if (e$name %in% c("_infile_", "_cmd_", "_msg_", "_iorc_")) abort_unsupported(paste0("automatic variable ", toupper(e$name)), e$line)
      if (e$name %in% c("_n_", "_error_")) {
        reg_var(st, e$name, "num", kind = "auto", disp = toupper(e$name))
      } else if (startsWith(e$name, "first.") || startsWith(e$name, "last.")) {
        if (is.null(st$info[[e$name]])) abort_unsupported(paste0(toupper(e$name), " without a matching top-level BY statement"), e$line)
      } else if (is.null(st$info[[e$name]])) {
        reg_var(st, e$name, disp = e$text %||% e$name)
      }
      st$uses <- c(st$uses, e$name)
    },
    array_ref = {
      if (is.null(st$arrays[[e$name]])) abort_syntax(paste0("'", e$name, "' is not an array"), e$line)
      e$index <- lapply(e$index, an_expr, st = st)
    },
    unary = e$arg <- an_expr(e$arg, st),
    binary = {
      e$lhs <- an_expr(e$lhs, st)
      e$rhs <- an_expr(e$rhs, st)
      if (e$op %in% c("+", "-", "*", "/", "**", "min", "max", "and", "or")) {
        if (expr_type(e$lhs, st)$type == "char" || expr_type(e$rhs, st)$type == "char") {
          abort_unsupported("automatic character-to-numeric conversion", e$line)
        }
      }
    },
    compare = {
      e$lhs <- an_expr(e$lhs, st)
      e$rhs <- an_expr(e$rhs, st)
      if (expr_type(e$lhs, st)$type != expr_type(e$rhs, st)$type) {
        abort_unsupported("comparison between character and numeric values", e$line)
      }
    },
    chain = e$args <- lapply(e$args, an_expr, st = st),
    "in" = e$lhs <- an_expr(e$lhs, st),
    between = {
      e$lhs <- an_expr(e$lhs, st)
      e$lo <- an_expr(e$lo, st)
      e$hi <- an_expr(e$hi, st)
    },
    contains = ,
    like = {
      e$lhs <- an_expr(e$lhs, st)
      e$rhs <- an_expr(e$rhs, st)
      if (e$node == "like") {
        if (!is.null(e$escape)) abort_unsupported("LIKE ... ESCAPE in a WHERE statement", e$line)
        call <- list(node = "call", name = "sqllike", args = list(e$lhs, e$rhs), line = e$line)
        e <- if (isTRUE(e$negate)) list(node = "unary", op = "not", arg = call, line = e$line) else call
      }
    },
    is_missing = e$lhs <- an_expr(e$lhs, st),
    call = e <- an_call(e, st),
    of = abort_syntax("OF variable list outside a function call", e$line),
    omitted = NULL
  )
  e
}

an_call <- function(e, st) {
  lk <- lag_kind(e$name)
  if (e$name %in% c("dim", "lbound", "hbound")) {
    if (length(e$args) < 1 || e$args[[1]]$node != "var" || is.null(st$arrays[[e$args[[1]]$name]])) {
      abort_syntax(paste(toupper(e$name), "requires an array name"), e$line)
    }
    if (length(e$args) > 1) abort_unsupported(paste0("multi-dimensional ", toupper(e$name)), e$line)
    return(e)
  }
  if (e$name %in% c("put", "input")) {
    f <- e$fmt
    known <- if (e$name == "put") fmt_known(f) else infmt_known(f)
    if (!known) {
      if (f$char || e$name == "input") abort_unsupported(paste0(if (e$name == "put") "format " else "informat ", f$text), e$line)
      abort_unsupported(paste0("format ", f$text), e$line)
    }
    if (e$name == "put") check_format_width(f, e$line)
    feature_check("function", e$name, e$line)
    if (is.null(if (e$name == "put") user_fmt(f$name, f$char) else user_fmt(f$name, f$char, "invalue"))) {
      feature_check(if (e$name == "put") "format" else "informat", fmt_registry_name(f), e$line)
    }
    e$args <- lapply(e$args, an_expr, st = st)
    a <- expr_type(e$args[[1]], st)
    if (e$name == "input" && a$type == "num") ct_conversion("n2c", e$args[[1]]$line %||% e$line, e$args[[1]]$col %||% e$col)
    # PUT(123, $5.) writes the number with 5.; PUT('abc', 8.) writes the text with $8.
    if (e$name == "put" && f$name == "" && !is.na(f$w) && is.na(f$d) && f$char != (a$type == "char") &&
        is.null(user_fmt("", TRUE))) {
      e$fmt <- fmt_spec("", f$w, NA_integer_, a$type == "char")
      return(e)
    }
    if (e$name == "put" && a$type == "num" && f$char && is.null(user_fmt(f$name, TRUE))) {
      abort_syntax(paste0("The format $", toupper(f$name), " was not found or could not be loaded"), e$line)
    }
    return(e)
  }
  if (e$name %in% var_info_fns) {
    if (length(e$args) != 1 || !e$args[[1]]$node %in% c("var", "array_ref")) {
      abort_syntax(paste(toupper(e$name), "requires a variable name"), e$line)
    }
    e$args <- lapply(e$args, an_expr, st = st)
    return(e)
  }
  if (!is.null(lk)) {
    if (length(e$args) != 1) abort_syntax(paste0(toupper(e$name), " takes one argument"), e$line)
    st$lag_sites <- st$lag_sites + 1L
    e$site <- st$lag_sites
    e$lag <- lk
  } else {
    spec <- fn_table[[e$name]]
    if (is.null(spec)) abort_unsupported(paste0("function ", toupper(e$name), "()"), e$line)
    feature_check("function", e$name, e$line)
    nargs <- sum(vapply(e$args, function(a) if (a$node == "of") 2L else 1L, integer(1)))
    # argument counts SAS 9.4 accepts (feature registry, from the SAS inventory): more or fewer is a compile-time ERROR
    sas_n <- registry_arg_counts(e$name)
    if (!is.null(sas_n)) {
      if (!is.na(sas_n[2]) && length(e$args) > sas_n[2]) {
        abort_syntax(paste0("The ", toupper(e$name), " function call has too many arguments (ERROR 72-185)"), e$line)
      }
      if (length(e$args) < sas_n[1] && nargs < sas_n[1]) {
        abort_syntax(paste0("The ", toupper(e$name), " function call does not have enough arguments (ERROR 71-185)"), e$line)
      }
    }
    if (length(e$args) > spec$max && e$name %in% fn_max_exact) {
      abort_syntax(paste0("The ", toupper(e$name), " function call has too many arguments (ERROR 72-185)"), e$line)
    }
    if (length(e$args) > spec$max) {
      # SAS may accept optional arguments beyond those implemented here
      abort_unsupported(paste0("more than ", spec$max, " arguments to ", toupper(e$name)), e$line)
    }
    if (length(e$args) < spec$min && nargs < spec$min) {
      abort_syntax(paste0("wrong number of arguments to ", toupper(e$name)), e$line)
    }
  }
  e$args <- lapply(e$args, function(a) {
    if (a$node == "of") {
      if (!is.null(lk) || !identical(fn_table[[e$name]]$max, Inf)) {
        abort_unsupported(paste0("OF list in ", toupper(e$name)), e$line)
      }
      a$vars <- resolve_items(a$items, st, e$line)
      st$uses <- c(st$uses, a$vars)
      return(a)
    }
    an_expr(a, st)
  })
  if (is.null(lk)) e$args <- convert_fn_args(e, st)
  e
}

# SAS converts a numeric argument passed where a function expects a character
# value with BEST12. (right-aligned), and a character argument passed where it
# expects a number as if read with the BEST32. informat (invalid data sets
# _ERROR_).  Only functions whose argument types are listed in fn_arg_types
# convert; for the others a mismatch keeps the previous behaviour.
convert_fn_args <- function(e, st) {
  types <- fn_arg_types[[e$name]] %||% registry_arg_types(e$name, length(e$args))
  if (is.null(types)) return(e$args)
  lapply(seq_along(e$args), function(k) {
    a <- e$args[[k]]
    want <- if (k <= length(types)) types[k] else NA_character_
    if (is.na(want) || a$node %in% c("of", "omitted")) return(a)
    have <- expr_type(a, st)$type
    if (have == want) return(a)
    ct_conversion(if (want == "char") "n2c" else "c2n", a$line %||% e$line, a$col %||% e$col)
    list(node = "call", name = if (want == "char") "cvt_n2c" else "cvt_c2n", args = list(a),
         line = a$line %||% e$line, col = a$col %||% e$col)
  })
}

# Statement annotation ----------------------------------------------------

check_format <- function(fmt, line) {
  if (is.null(fmt)) return(invisible())
  if (!fmt_known(fmt) || (fmt$name == "" && is.na(fmt$w) && !fmt$char)) abort_unsupported(paste0("format ", fmt$text), line)
  check_format_width(fmt, line)
  if (is.null(user_fmt(fmt$name, fmt$char))) feature_check("format", fmt_registry_name(fmt), line)
}

check_format_width <- function(fmt, line) {
  if (fmt$char || !is.null(user_fmt(fmt$name, fmt$char))) return(invisible())
  nm <- if (nzchar(fmt$name)) fmt$name else "w"
  # format widths outside the SAS range are an error (SAS: "Width specified for format X is invalid")
  rng <- num_fmt_width_range[[nm]]
  if (is.null(rng) && !is.null(fmt_base_name(nm)$sep)) rng <- c(2, 10)
  if (!is.null(rng) && !is.na(fmt$w) && (fmt$w < rng[1] || fmt$w > rng[2])) {
    abort_syntax(paste0("Width specified for format ", toupper(if (nzchar(fmt$name)) fmt$name else "F"), " is invalid"), line)
  }
  # PVALUE's default of 4 decimals needs a width of at least 5
  if (nm == "pvalue" && !is.na(fmt$w) && is.na(fmt$d) && fmt$w <= 4) {
    abort_syntax(sprintf("The decimal specification of 4 must be less than the width specification of %d (ERROR 156-185)", fmt$w), line)
  }
  if (!is.na(fmt$d) && !is.na(fmt$w) && fmt$d >= fmt$w && nm %in% c("w", "f", "z", "comma", "dollar", "commax", "dollarx")) {
    abort_syntax(paste0("Decimal specified for format ", fmt$text, " is invalid"), line)
  }
  invisible()
}


assign_target_type <- function(st, name, value, line) {
  vt <- expr_type(value, st)
  inf <- st$info[[name]]
  if (is.na(inf$type)) {
    reg_var(st, name, vt$type, if (vt$type == "char") vt$len else NA_integer_)
  } else if (inf$type != vt$type) {
    abort_unsupported(paste0("automatic type conversion when assigning to ", name), line)
  }
}

an_stmts <- function(stmts, st) lapply(stmts, an_stmt, st = st)

# The external file PUT writes to (FILE statement), or NULL for the log.
# Supported: FILE statements at the top level of the step that all name the
# same file, with no PUT executed before the first of them (so every PUT of
# the step goes to that file).  Anything else fails loud.
file_target <- function(stmts) {
  nested <- function(ss) any(vapply(ss, function(s) s$stmt == "file" || nested(stmt_children(s)), logical(1)))
  has_put <- function(ss) any(vapply(ss, function(s) s$stmt == "put" || has_put(stmt_children(s)), logical(1)))
  if (any(vapply(stmts, function(s) nested(stmt_children(s)), logical(1)))) {
    f <- Find(function(s) nested(stmt_children(s)), stmts)
    abort_unsupported("FILE statement inside a conditional or DO block", f$line)
  }
  k <- which(vapply(stmts, function(s) s$stmt == "file", logical(1)))
  if (length(k) == 0) return(NULL)
  dests <- lapply(stmts[k], function(s) {
    d <- s$dest
    if (d$kind == "print") abort_unsupported("FILE PRINT", s$line)
    if (d$kind == "fileref") {
      path <- rt$filerefs[[d$ref]]
      if (is.null(path)) abort_unsupported(paste0("FILE fileref ", toupper(d$ref), " (no FILENAME statement)"), s$line)
      d <- list(kind = "path", path = path)
    }
    c(d, s$opts)
  })
  keys <- unique(vapply(dests, function(d) paste(d$kind, d$path %||% "", d$mod, d$lrecl), ""))
  if (length(keys) > 1) abort_unsupported("several FILE destinations in one step", stmts[[k[2]]]$line)
  if (dests[[1]]$kind == "log") return(NULL)
  if (k[1] > 1 && has_put(stmts[seq_len(k[1] - 1L)])) {
    abort_unsupported("PUT before the FILE statement of the step", stmts[[k[1]]]$line)
  }
  dests[[1]]
}

an_stmt <- function(s, st) {
  line <- s$line
  switch(s$stmt,
    assign = {
      if (s$target$node == "var" && s$target$name %in% c("_n_", "_error_")) {
        reg_var(st, s$target$name, "num", kind = "auto", disp = toupper(s$target$name))
        st$uses <- c(st$uses, s$target$name)
        s$value <- an_expr(s$value, st)
      } else if (s$target$node == "var") {
        # the value is compiled first: in `s = s || w;` the first reference to s
        # is on the right-hand side, which makes s numeric (SAS)
        existed <- !is.null(st$info[[s$target$name]])
        s$value <- an_expr(s$value, st)
        if (!existed && !is.null(st$info[[s$target$name]]) && is.na(st$info[[s$target$name]]$type)) {
          reg_var(st, s$target$name, "num")
        }
        reg_var(st, s$target$name, disp = s$target$text %||% s$target$name)
        assign_target_type(st, s$target$name, s$value, line)
        # CATX writes into a buffer as long as the target variable
        if (s$value$node == "call" && s$value$name %in% c("catx", "cat", "cats", "catt")) s$value$buf_len <- st$info[[s$target$name]]$len
      } else {
        s$target <- an_expr(s$target, st)
        s$value <- an_expr(s$value, st)
        arr <- st$arrays[[s$target$name]]
        if (arr$type != expr_type(s$value, st)$type) abort_unsupported("automatic type conversion in array assignment", line)
      }
    },
    sum = {
      reg_var(st, s$target$name, "num", disp = s$target$text, retained = TRUE)
      if (is.null(st$info[[s$target$name]]$init)) st$info[[s$target$name]]$init <- 0
      s$value <- an_expr(s$value, st)
      if (var_type(st, s$target$name) != "num" || expr_type(s$value, st)$type != "num") {
        abort_unsupported("sum statement on character values", line)
      }
    },
    "if" = {
      s$cond <- an_expr(s$cond, st)
      s$then <- an_stmt(s$then, st)
      if (!is.null(s[["else"]])) s[["else"]] <- an_stmt(s[["else"]], st)
    },
    subset_if = s$cond <- an_expr(s$cond, st),
    where = {
      s$cond <- an_expr(s$cond, st)
      st$wheres <- list(s$cond) # a later WHERE statement replaces an earlier one
    },
    do = {
      if (s$kind == "iter") {
        reg_var(st, s$var, "num", disp = s$var_text)
        s$from <- an_expr(s$from, st)
        s$to <- an_expr(s$to, st)
        s$by <- an_expr(s$by, st)
        if (!is.null(s$while_until)) s$while_until$cond <- an_expr(s$while_until$cond, st)
      } else if (s$kind == "list") {
        reg_var(st, s$var, disp = s$var_text)
        s$values <- lapply(s$values, an_expr, st = st)
        types <- vapply(s$values, function(v) expr_type(v, st)$type, character(1))
        if (length(unique(types)) > 1) abort_syntax("DO value list mixes character and numeric values", line)
        if (is.na(st$info[[s$var]]$type)) {
          lens <- vapply(s$values, function(v) expr_type(v, st)$len, integer(1))
          reg_var(st, s$var, types[1], if (types[1] == "char") max(lens) else NA_integer_)
        } else if (st$info[[s$var]]$type != types[1]) {
          abort_unsupported("automatic type conversion in DO list", line)
        }
      } else if (s$kind %in% c("while", "until")) {
        s$cond <- an_expr(s$cond, st)
      }
      st$loop_depth <- st$loop_depth + (s$kind != "block")
      s$body <- an_stmts(s$body, st)
      st$loop_depth <- st$loop_depth - (s$kind != "block")
    },
    select = {
      s$expr <- an_expr(s$expr, st)
      s$whens <- lapply(s$whens, function(w) {
        w$values <- lapply(w$values, an_expr, st = st)
        w$body <- an_stmt(w$body, st)
        w
      })
      if (!is.null(s$otherwise)) s$otherwise <- an_stmt(s$otherwise, st)
    },
    leave = ,
    continue = if (st$loop_depth == 0) abort_syntax(paste(toupper(s$stmt), "outside an iterative DO loop"), line),
    set = ,
    merge = s <- an_reader(s, st),
    input = {
      s$items <- lapply(s$items, function(it) {
        if (it$k != "var") {
          if (!is.null(it$v)) it$v <- an_expr(it$v, st)
          return(it)
        }
        if (is.null(it$fmt) && it$mode == "list" && !is.null(st$informats[[it$name]])) {
          it$fmt <- st$informats[[it$name]]
          it$mode <- "modified"
          it$char <- it$char || it$fmt$char
        }
        if (!is.null(it$fmt)) {
          if (!infmt_known(it$fmt)) abort_unsupported(paste0("informat ", it$fmt$text), line)
          uf <- user_fmt(it$fmt$name, it$fmt$char, "invalue")
          if (is.null(uf)) feature_check("informat", fmt_registry_name(it$fmt), line)
          if (isTRUE(uf$char)) it$char <- TRUE
        }
        inf <- st$info[[it$name]]
        if (!is.null(inf) && !is.na(inf$type)) {
          it$char <- inf$type == "char"
        } else if (it$char) {
          w <- switch(it$mode,
            column = it$to - it$from + 1L,
            list = 8L,
            if (!is.null(it$fmt)) infmt_default_w(it$fmt) else 8L
          )
          if (is.na(w)) w <- 8L
          reg_var(st, it$name, "char", as.integer(w), disp = it$text)
        } else {
          reg_var(st, it$name, "num", disp = it$text)
        }
        if (is.null(st$info[[it$name]])) reg_var(st, it$name, disp = it$text)
        if (!it$char && !is.null(it$fmt) && it$fmt$char) abort_syntax(paste0("character informat ", it$fmt$text, " for numeric variable"), line)
        it
      })
      st$readers <- c(st$readers, list(s))
    },
    file = NULL,
    infile = {
      if (!is.null(st$infile)) abort_unsupported("more than one INFILE statement in a step", line)
      if (identical(s$source$kind, "fileref")) {
        path <- rt$filerefs[[s$source$ref]]
        if (is.null(path)) abort_unsupported(paste0("INFILE fileref ", toupper(s$source$ref), " (no FILENAME statement)"), line)
        s$source <- list(kind = "file", path = path)
      }
      st$infile <- s
      if (!is.null(s$opts$end)) {
        reg_var(st, s$opts$end, "num", kind = "auto", retained = TRUE)
        st$info[[s$opts$end]]$kind <- "auto"
      }
    },
    datalines = {
      st$datalines <- s$lines
      st$dl <- s
    },
    by = NULL,
    retain = {
      for (g in s$groups) {
        vars <- resolve_items(g$vars, st, line, register = FALSE)
        for (k in seq_along(vars)) {
          init <- NULL
          # `retain a b c (0)` binds the list positionally; a bare value seeds every variable
          if (isTRUE(g$paren)) {
            if (k <= length(g$init)) init <- g$init[[k]]
          } else if (length(g$init) == 1) {
            init <- g$init[[1]]
          } else if (length(g$init) > 1) {
            init <- if (k <= length(g$init)) g$init[[k]] else list(node = "missing", code = ".")
          }
          type <- if (is.null(init)) NA_character_ else if (init$node == "str") "char" else "num"
          len <- if (identical(type, "char")) max(1L, nchar(init$value)) else NA_integer_
          if (is.null(st$info[[vars[k]]]) && is.null(init)) st$retain_only <- c(st$retain_only, vars[k])
          reg_var(st, vars[k], type, len, retained = TRUE, disp = attr(vars, "disp")[vars[k]] %|NA|% vars[k])
          if (!is.null(init)) st$info[[vars[k]]]$init <- const_value(init)
          st$retain_stmt_vars <- c(st$retain_stmt_vars, vars[k])
        }
      }
    },
    length = {
      for (sp in s$specs) {
        vv <- resolve_items(sp$vars, st, line, register = FALSE)
        for (v in vv) {
          old_t <- st$info[[v]]$type
          if (!is.null(old_t) && !is.na(old_t) && old_t != sp$type) {
            abort_syntax(if (sp$type == "num") paste0("Numeric length cannot be used with character variable ", v, ".")
                         else paste0("Character length cannot be used with numeric variable ", v, "."), line)
          }
          reg_var(st, v, sp$type, if (sp$type == "char") sp$len else NA_integer_, disp = attr(vv, "disp")[v] %|NA|% v)
          if (sp$type == "num") st$numlen[[v]] <- sp$len
        }
      }
    },
    format = {
      for (sp in s$specs) {
        check_format(sp$fmt, line)
        vv <- resolve_items(sp$vars, st, line, register = FALSE)
        for (v in vv) {
          if (is.null(st$info[[v]])) {
            # first seen in FORMAT: a $w. format fixes the type and length
            f <- sp$fmt
            if (isTRUE(f$char) && !is.na(f$w)) reg_var(st, v, "char", f$w, disp = attr(vv, "disp")[v] %|NA|% v)
            else reg_var(st, v, disp = attr(vv, "disp")[v] %|NA|% v)
          }
          st$formats[[v]] <- sp$fmt
        }
      }
    },
    label = {
      st$labels[names(s$labels)] <- s$labels
      # a LABEL statement fixes the position of a variable it names first;
      # a variable named only in LABEL is not created (dropped in an_step)
      for (v in names(s$labels)) {
        if (is.null(st$info[[v]])) {
          reg_var(st, v, disp = s$texts[[v]] %||% v)
          st$info[[v]]$label_only <- TRUE
        }
      }
    },
    informat = {
      for (sp in s$specs) {
        if (is.null(sp$fmt)) next
        if (!infmt_known(sp$fmt)) abort_unsupported(paste0("informat ", sp$fmt$text), line)
        if (is.null(user_fmt(sp$fmt$name, sp$fmt$char, "invalue"))) feature_check("informat", fmt_registry_name(sp$fmt), line)
        vv <- resolve_items(sp$vars, st, line, register = FALSE)
        for (v in vv) {
          dv <- attr(vv, "disp")[v] %|NA|% v
          if (is.null(st$info[[v]])) {
            if (sp$fmt$char) reg_var(st, v, "char", if (is.na(sp$fmt$w)) 8L else sp$fmt$w, disp = dv) else reg_var(st, v, "num", disp = dv)
          }
          st$informats[[v]] <- sp$fmt
        }
      }
    },
    attrib = {
      for (g in s$groups) {
        vv <- resolve_items(g$vars, st, line, register = FALSE)
        for (v in vv) {
          dv <- attr(vv, "disp")[v] %|NA|% v
          if (!is.null(g$len)) reg_var(st, v, g$len$type, if (g$len$type == "char") g$len$len else NA_integer_, disp = dv)
          if (!is.null(g$len) && g$len$type == "num") st$numlen[[v]] <- g$len$len
          if (!is.null(g$format)) {
            check_format(g$format, line)
            if (is.null(st$info[[v]])) reg_var(st, v, if (g$format$char) "char" else "num", if (g$format$char) (if (is.na(g$format$w)) 8L else g$format$w) else NA_integer_, disp = dv)
            st$formats[[v]] <- g$format
          }
          if (!is.null(g$informat)) {
            if (!infmt_known(g$informat)) abort_unsupported(paste0("informat ", g$informat$text), line)
            if (is.null(user_fmt(g$informat$name, g$informat$char, "invalue"))) {
              feature_check("informat", fmt_registry_name(g$informat), line)
            }
            if (is.null(st$info[[v]]) && isTRUE(g$informat$char) && !is.na(g$informat$w)) reg_var(st, v, "char", g$informat$w, disp = dv)
            st$informats[[v]] <- g$informat
          }
          if (!is.null(g$label)) st$labels[v] <- g$label
          if (is.null(st$info[[v]])) reg_var(st, v, disp = dv)
        }
      }
    },
    keep = st$keep <- c(st$keep, s$items),
    drop = st$drop <- c(st$drop, s$items),
    rename = st$rename[names(s$pairs)] <- s$pairs,
    array = an_array(s, st),
    put = {
      s$items <- lapply(s$items, function(it) {
        if (it$kind == "lit") return(it)
        if (it$kind != "ptr" && is.null(st$info[[it$name]]) && isTRUE(it$fmt$char)) {
          reg_var(st, it$name, "char", if (is.na(it$fmt$w)) 8L else it$fmt$w, disp = it$text)
        }
        if (it$kind == "ptr") {
          if (!is.null(it$v)) it$v <- an_expr(it$v, st)
          return(it)
        }
        an_expr(list(node = "var", name = it$name, text = it$text, line = line), st)
        check_format(it$fmt, line)
        it
      })
    },
    call = {
      if (s$routine %in% c("missing", "sortn", "sortc")) {
        targets <- list()
        for (a in s$args) {
          if (a$node == "of") {
            for (v in resolve_items(a$items, st, line)) targets <- c(targets, list(list(node = "var", name = v, line = line)))
          } else if (a$node == "var") {
            if (is.null(st$info[[a$name]])) reg_var(st, a$name, disp = a$text %||% a$name)
            targets <- c(targets, list(a))
          } else if (a$node == "array_ref") {
            targets <- c(targets, list(an_expr(a, st)))
          } else {
            abort_syntax(paste0("CALL ", toupper(s$routine), " arguments must be variables"), line)
          }
        }
        s$targets <- targets
        s$args <- list()
        types <- vapply(targets, function(t) expr_type(t, st)$type, "")
        if (s$routine == "sortn" && any(types != "num")) abort_syntax("CALL SORTN requires numeric variables", line)
        if (s$routine == "sortc" && any(types != "char")) abort_syntax("CALL SORTC requires character variables", line)
      } else {
        s$args <- lapply(s$args, an_expr, st = st)
      }
    },
    output = ,
    delete = ,
    null = NULL
  )
  s
}

const_value <- function(e) {
  switch(e$node,
    num = e$value,
    str = e$value,
    missing = if (e$code == ".") NA_real_ else sas_missing(e$code)
  )
}

an_array <- function(s, st) {
  line <- s$line
  type <- if (s$char) "char" else "num"
  len <- if (s$char) (if (is.na(s$len)) 8L else s$len) else NA_integer_
  elems <- character()
  if (!s$temporary) {
    elems <- resolve_items(s$items, st, line, register = FALSE)
    if (length(elems) == 0) {
      if (is.na(s$dim)) abort_syntax("array with {*} needs elements", line)
      elems <- paste0(s$name, seq_len(s$dim))
    }
  } else if (length(s$items) > 0) {
    abort_syntax("_TEMPORARY_ arrays cannot list elements", line)
  }
  dim <- if (is.na(s$dim)) length(elems) else s$dim
  if (!s$temporary && length(elems) != dim) abort_syntax("array dimension does not match its elements", line)
  init <- if (is.null(s$init)) NULL else lapply(s$init, const_value)
  if (!is.null(init) && length(init) > dim) abort_syntax("too many initial values for array", line)
  for (k in seq_along(elems)) {
    iv <- if (!is.null(init) && k <= length(init)) init[[k]] else NULL
    reg_var(st, elems[k], type, len, retained = !is.null(iv), init = iv,
            disp = (attr(elems, "disp") %||% character())[elems[k]] %|NA|% elems[k])
    if (st$info[[elems[k]]]$type != type) abort_syntax(paste("array", s$name, "mixes types"), line)
  }
  st$arrays[[s$name]] <- list(
    name = s$name, elems = elems, temporary = s$temporary, dim = dim, lo = s$lo,
    type = type, len = len, init = init
  )
  invisible()
}

an_reader <- function(s, st) {
  line <- s$line
  for (k in seq_along(s$datasets)) {
    ds <- s$datasets[[k]]
    m <- st$meta[[ds$name]]
    if (is.null(m)) {
      sasonr_abort(paste0("data set WORK.", toupper(ds$name), " does not exist"), "sasonr_runtime_error", line)
    }
    vars <- m$vars
    o <- ds$options
    for (opt in c("keep", "drop")) {
      if (is.null(o[[opt]])) next
      miss <- setdiff(resolve_ds_items(o[[opt]], m$vars, line), m$vars)
      if (length(miss) > 0) abort_runtime(paste0("The variable ", toupper(miss[1]), " in the DROP, KEEP, or RENAME list has never been referenced"), line)
    }
    if (!is.null(o$keep)) vars <- intersect(vars, resolve_ds_items(o$keep, m$vars, line))
    if (!is.null(o$drop)) vars <- setdiff(vars, resolve_ds_items(o$drop, m$vars, line))
    out_names <- vars
    if (!is.null(o$rename)) out_names[match(names(o$rename), vars)] <- unname(o$rename)
    for (j in seq_along(vars)) {
      v <- vars[j]
      is_char <- identical(m$type[[v]], "char")
      if (!is_char && !is.null(m$len[[v]]) && !is.na(m$len[[v]]) && is.null(st$numlen[[out_names[j]]])) {
        st$numlen[[out_names[j]]] <- m$len[[v]]
      }
      if (!is.null(m$informats[[v]]) && is.null(st$informats[[out_names[j]]])) st$informats[[out_names[j]]] <- m$informats[[v]]
      reg_var(st, out_names[j], m$type[[v]], if (is_char) m$len[[v]] else NA_integer_, retained = TRUE,
              disp = if (out_names[j] == v) m$disp[[v]] %||% v else out_names[j])
      if (!is.null(m$formats[[v]]) && is.null(st$formats[[out_names[j]]])) st$formats[[out_names[j]]] <- m$formats[[v]]
      if (!is.null(m$labels[[v]]) && !out_names[j] %in% names(st$labels)) st$labels[out_names[j]] <- m$labels[[v]]
    }
    s$datasets[[k]]$vars <- vars
    s$datasets[[k]]$out_names <- out_names
    if (!is.null(o$in_var)) {
      reg_var(st, o$in_var, "num", kind = "auto", retained = TRUE)
      st$info[[o$in_var]]$kind <- "auto"
    }
  }
  # WHERE= is evaluated against the renamed input variables.
  for (k in seq_along(s$datasets)) {
    w <- s$datasets[[k]]$options$where
    if (!is.null(w)) s$datasets[[k]]$options$where <- an_expr(w, st)
  }
  for (a in c(s$end, s$nobs)) {
    reg_var(st, a, "num", kind = "auto", retained = TRUE)
    st$info[[a]]$kind <- "auto"
  }
  register_by_flags(st)
  st$readers <- c(st$readers, list(s))
  s
}

resolve_ds_items <- function(items, vars, line) {
  unlist(lapply(items, function(it) {
    switch(it$kind,
      name = it$name,
      range = expand_numbered_range(it$from, it$to, line),
      prefix = vars[startsWith(vars, it$prefix)],
      abort_unsupported("this variable list form in a data set option", line)
    )
  }))
}

# Main entry --------------------------------------------------------------

#' Analyse a parsed DATA step
#'
#' @param step One element of `sas_parse(code)$steps`.
#' @param meta Named list of input data set metadata (see [sas_translate()]).
#' @param level `"auto"` or a forced level (`"L3"`).
#' @return A list describing the PDV, annotated statements, and the level.
#' @examples
#' p <- sas_parse("data b; do i = 1 to 3; s + i; output; end; run;")
#' a <- sas_analyze(p$steps[[1]])
#' a$level
#' @export
sas_analyze <- function(step, meta = list(), level = "auto") {
  st <- new_state(meta)
  # BY first: FIRST./LAST. variables may be referenced anywhere in the step.
  by <- Filter(function(s) s$stmt == "by", step$stmts)
  if (length(by) > 1) abort_unsupported("more than one BY statement", by[[2]]$line)
  stmts <- step$stmts
  if (length(by) == 1) st$by <- by[[1]]
  st$file <- file_target(stmts)
  # Arrays are declared before use; declare them in order while annotating.
  stmts <- an_stmts(stmts, st)
  if (!is.null(st$by)) {
    readers <- st$readers
    if (length(readers) == 0 || !readers[[1]]$stmt %in% c("set", "merge")) {
      abort_syntax("BY statement requires SET or MERGE", st$by$line)
    }
  }
  for (v in st$order) {
    if (isTRUE(st$info[[v]]$label_only) && !v %in% st$uses) {
      st$order <- setdiff(st$order, v)
      st$info[[v]] <- NULL
    }
  }
  for (v in st$order) {
    inf <- st$info[[v]]
    if (is.na(inf$type)) st$info[[v]]$type <- "num"
    if (identical(st$info[[v]]$type, "char") && is.na(st$info[[v]]$len)) st$info[[v]]$len <- 8L
  }
  for (v in names(st$formats)) {
    f <- st$formats[[v]]
    t <- st$info[[v]]$type
    if (is.null(f) || is.null(t)) next
    if (t == "char" && !f$char) abort_syntax(paste0("The format $", toupper(f$name), " was not found or could not be loaded"), step$line)
    if (t == "num" && f$char) abort_syntax(paste0("The character format ", f$text, " cannot be used with numeric variable ", toupper(v)), step$line)
  }
  an <- as.list(st)
  an$stmts <- stmts
  an$step <- step
  cls <- classify_step(an)
  an$level <- if (level == "L3") "L3" else cls$level
  an$reasons <- if (level == "L3") "forced L3" else cls$reasons
  an$sum_plan <- cls$sum_plan
  an$skip_top <- cls$skip_top
  an$output_pattern <- cls$output_pattern
  an$carry_plan <- cls$carry_plan
  an$outputs <- output_specs(an)
  an
}

# Pre-register FIRST./LAST. vars right after the reader is annotated.
# (Called from an_stmt via the BY statement being present in st$by.)
register_by_flags <- function(st) {
  if (is.null(st$by)) return(invisible())
  for (v in st$by$vars) {
    reg_var(st, paste0("first.", v), "num", kind = "auto", retained = TRUE)
    reg_var(st, paste0("last.", v), "num", kind = "auto", retained = TRUE)
  }
}

# Output data sets --------------------------------------------------------

output_specs <- function(an) {
  if (an$step$null) return(list())
  data_vars <- an$order[vapply(an$order, function(v) an$info[[v]]$kind == "data", logical(1))]
  # A variable that appears only in RETAIN, without an initial value, is not
  # written to the output data set.
  assigned <- unique(unlist(lapply(an$stmts, stmt_assigns)))
  in_arrays <- unlist(lapply(an$arrays, `[[`, "elems"))
  inputs <- unlist(lapply(an$readers, function(r) {
    if (r$stmt == "input") input_var_names(r) else unlist(lapply(r$datasets, `[[`, "out_names"))
  }))
  retain_only <- setdiff(an$retain_only, c(an$uses, assigned, in_arrays, inputs))
  data_vars <- setdiff(data_vars, retain_only)
  tmp <- new_state(list())
  tmp$order <- data_vars
  tmp$info <- an$info[data_vars]
  base <- data_vars
  if (length(an$keep) > 0) base <- intersect(base, resolve_items(an$keep, tmp, register = FALSE))
  if (length(an$drop) > 0) base <- setdiff(base, resolve_items(an$drop, tmp, register = FALSE))
  # Order of application: DROP/KEEP statements (old names), RENAME statement,
  # then the output data set options DROP=/KEEP= (new names) and RENAME=.
  rename_by <- function(names, ren) {
    hit <- match(names(ren), names)
    names[hit[!is.na(hit)]] <- unname(ren[!is.na(hit)])
    names
  }
  base_names <- rename_by(base, an$rename)
  lapply(an$step$outputs, function(o) {
    keep <- rep(TRUE, length(base))
    if (!is.null(o$options$keep)) keep <- keep & base_names %in% resolve_ds_items(o$options$keep, base_names, o$line)
    if (!is.null(o$options$drop)) keep <- keep & !base_names %in% resolve_ds_items(o$options$drop, base_names, o$line)
    if (!is.null(o$options$where)) abort_unsupported("WHERE= on an output data set", o$line)
    vars <- base[keep]
    out_names <- rename_by(base_names[keep], o$options$rename)
    # RENAME= onto an existing name: the first variable of that name is kept
    dup <- duplicated(out_names)
    list(name = o$name, vars = vars[!dup], out_names = out_names[!dup])
  })
}

out_meta <- function(an) {
  res <- list()
  for (o in an$outputs) {
    type <- lapply(o$vars, function(v) an$info[[v]]$type)
    len <- lapply(o$vars, function(v) an$info[[v]]$len)
    fmts <- lapply(o$vars, function(v) an$formats[[v]])
    disp <- Map(function(v, on) if (on == v) an$info[[v]]$disp %||% v else on, o$vars, o$out_names)
    names(type) <- names(len) <- names(fmts) <- names(disp) <- o$out_names
    len <- Map(function(v, l) if (identical(an$info[[v]]$type, "char")) l else as.integer(an$numlen[[v]] %||% 8L), o$vars, len)
    infs <- lapply(o$vars, function(v) an$informats[[v]])
    labs <- lapply(o$vars, function(v) if (v %in% names(an$labels)) substr(an$labels[[v]], 1L, 256L) else NULL)
    names(labs) <- names(infs) <- names(len) <- o$out_names
    res[[o$name]] <- list(vars = o$out_names, type = type, len = len, formats = Filter(Negate(is.null), fmts), disp = disp,
                          labels = Filter(Negate(is.null), labs), informats = Filter(Negate(is.null), infs))
  }
  res
}
