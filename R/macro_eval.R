# %EVAL / %SYSEVALF / %IF expression evaluation.
#
# Operands are runs of text between operators (blanks inside a run are kept,
# quoted text is opaque, quoted/masked characters never act as operators).
# Precedence (high to low): **; prefix + - NOT; * /; + -; comparisons;
# AND &; OR |. An operand is numeric when it is an integer (%EVAL) or any
# number (%SYSEVALF); comparisons are numeric when both sides are numeric,
# otherwise character comparisons of the operand text.

mac_ops_word <- c(and = "and", or = "or", not = "not", eq = "=", ne = "^=", lt = "<",
                  le = "<=", gt = ">", ge = ">=")

mac_tokenize <- function(text) {
  cp <- utf8ToInt(text)
  n <- length(cp)
  toks <- list()
  add <- function(type, value, a, b) toks[[length(toks) + 1L]] <<- list(type = type, value = value, a = a, b = b)
  is_ws <- function(c) c %in% c(32L, 9L, 10L, 13L)
  op_chars <- c(43L, 45L, 42L, 47L, 40L, 41L, 60L, 62L, 61L, 94L, 126L, 172L, 38L, 124L)
  i <- 1L
  while (i <= n) {
    c <- cp[i]
    nx <- if (i < n) cp[i + 1L] else -1L
    if (is_ws(c)) {
      i <- i + 1L
      next
    }
    if (c %in% op_chars) {
      op <- switch(as.character(c),
        "43" = "+", "45" = "-", "47" = "/", "40" = "(", "41" = ")", "38" = "and", "124" = "or",
        "61" = "=",
        "42" = if (nx == 42L) "**" else "*",
        "60" = if (nx == 61L) "<=" else "<",
        "62" = if (nx == 61L) ">=" else ">",
        if (nx == 61L) "^=" else "not"
      )
      len <- if (op %in% c("**", "<=", ">=") || (op == "^=")) 2L else 1L
      add("op", op, i, i + len - 1L)
      i <- i + len
      next
    }
    # operand word (quoted sections are opaque)
    j <- i
    while (j <= n && !is_ws(cp[j]) && (!(cp[j] %in% op_chars) || num_exp_sign(cp, i, j))) {
      if (cp[j] %in% c(34L, 39L)) {
        q <- cp[j]
        j <- j + 1L
        while (j <= n && cp[j] != q) j <- j + 1L
      }
      j <- j + 1L
    }
    j <- min(j, n + 1L)
    word <- intToUtf8(cp[i:(j - 1L)])
    low <- tolower(word)
    if (low %in% names(mac_ops_word)) {
      add("op", mac_ops_word[[low]], i, j - 1L)
    } else {
      k <- length(toks)
      if (k > 0 && toks[[k]]$type == "atom") {
        # adjacent operand words form one operand, blanks included
        toks[[k]]$b <- j - 1L
        toks[[k]]$value <- intToUtf8(cp[toks[[k]]$a:(j - 1L)])
      } else {
        add("atom", word, i, j - 1L)
      }
    }
    i <- j
  }
  toks
}

#' Evaluate a macro expression
#'
#' @param text Resolved expression text.
#' @param float `FALSE` for %EVAL (integer arithmetic), `TRUE` for %SYSEVALF.
#' @return A list with `kind` ("num" or "str") and `value`.
#' @keywords internal
mac_eval <- function(text, float = FALSE, what = "%EVAL") {
  toks <- mac_tokenize(text)
  pos <- 1L
  peek <- function() if (pos <= length(toks)) toks[[pos]] else list(type = "eof", value = "")
  is_op <- function(ops) {
    t <- peek()
    t$type == "op" && t$value %in% ops
  }
  bad <- function(msg) sasonr_abort(paste0(msg, " in ", what, ": ", mq_unmask(text)), "sasonr_runtime_error")
  as_num <- function(v) {
    if (v$kind == "num") return(v$value)
    bad(paste0("A character operand was found where a numeric operand is required (", v$value, ")"))
  }
  num <- function(x) list(kind = "num", value = x)
  atom_value <- function(s) {
    s <- mq_unmask(s)
    if (!float) {
      cp <- utf8ToInt(s)
      if (length(cp) > 0 && all(cp >= 48L & cp <= 57L)) return(num(as.numeric(s)))
      return(list(kind = "str", value = s))
    }
    v <- if (s == ".") NA_real_ else suppressWarnings(as.numeric(s))
    if (!is.na(v) || s == ".") return(num(v))
    dt <- mac_dt_literal(s)
    if (!is.null(dt)) return(num(dt))
    list(kind = "str", value = s)
  }
  primary <- function() {
    t <- peek()
    if (t$type == "atom") {
      pos <<- pos + 1L
      return(atom_value(t$value))
    }
    if (t$type == "op" && t$value == "(") {
      pos <<- pos + 1L
      v <- or_e()
      if (!is_op(")")) bad("Missing closing parenthesis")
      pos <<- pos + 1L
      return(v)
    }
    list(kind = "str", value = "")
  }
  pow_e <- function() {
    l <- primary()
    if (is_op("**")) {
      pos <<- pos + 1L
      r <- unary()
      x <- as_num(l)^as_num(r)
      return(num(if (float) x else trunc(x)))
    }
    l
  }
  unary <- function() {
    if (is_op("-")) {
      pos <<- pos + 1L
      return(num(-as_num(unary())))
    }
    if (is_op("+")) {
      pos <<- pos + 1L
      return(num(as_num(unary())))
    }
    if (is_op("not")) {
      pos <<- pos + 1L
      return(num(as.numeric(!truthy(unary()))))
    }
    pow_e()
  }
  mul <- function() {
    l <- unary()
    while (is_op(c("*", "/"))) {
      op <- peek()$value
      pos <<- pos + 1L
      a <- as_num(l)
      b <- as_num(unary())
      if (op == "*") {
        l <- num(a * b)
        if (!float && !is.na(l$value) && abs(l$value) >= 2^63) bad("Overflow has occurred")
      } else {
        if (!is.na(b) && b == 0) bad("Division by zero")
        l <- num(if (float) a / b else trunc(a / b))
      }
    }
    l
  }
  add_e <- function() {
    l <- mul()
    while (is_op(c("+", "-"))) {
      op <- peek()$value
      pos <<- pos + 1L
      a <- as_num(l)
      b <- as_num(mul())
      l <- num(if (op == "+") a + b else a - b)
      if (!float && !is.na(l$value) && abs(l$value) >= 2^63) bad("Overflow has occurred")
    }
    l
  }
  cmp_e <- function() {
    l <- add_e()
    while (is_op(c("=", "^=", "<", "<=", ">", ">="))) {
      op <- peek()$value
      pos <<- pos + 1L
      r <- add_e()
      o <- if (l$kind == "num" && r$kind == "num") {
        num_cmp(l$value, r$value)
      } else {
        text_cmp(val_text(l), val_text(r))
      }
      l <- num(as.numeric(switch(op, "=" = o == 0, "^=" = o != 0, "<" = o < 0, "<=" = o <= 0,
                                 ">" = o > 0, ">=" = o >= 0)))
    }
    l
  }
  truthy <- function(v) {
    x <- as_num(v)
    !is.na(x) && x != 0
  }
  and_e <- function() {
    l <- cmp_e()
    while (is_op("and")) {
      pos <<- pos + 1L
      r <- cmp_e()
      l <- num(as.numeric(truthy(l) && truthy(r)))
    }
    l
  }
  or_e <- function() {
    l <- and_e()
    while (is_op("or")) {
      pos <<- pos + 1L
      r <- and_e()
      l <- num(as.numeric(truthy(l) || truthy(r)))
    }
    l
  }
  if (length(toks) == 0) bad("Missing expression")
  v <- or_e()
  if (pos <= length(toks)) bad(paste0("Unexpected '", peek()$value, "'"))
  v$single <- length(toks) == 1L && toks[[1]]$type == "atom"
  v
}

# a number compared with text: its BEST16. text (the display engine, as %SYSEVALF writes it)
val_text <- function(v) if (v$kind == "str") v$value else mac_float_text(v$value)

num_cmp <- function(a, b) {
  if (is.na(a) && is.na(b)) return(0L)
  if (is.na(a)) return(-1L)
  if (is.na(b)) return(1L)
  sign(a - b)
}

text_cmp <- function(a, b) {
  x <- utf8ToInt(a)
  y <- utf8ToInt(b)
  m <- min(length(x), length(y))
  d <- which(x[seq_len(m)] != y[seq_len(m)])
  if (length(d) > 0) return(sign(x[d[1]] - y[d[1]]))
  sign(length(x) - length(y))
}

# Integer macro values (%EVAL, %DO, %SYSEVALF INTEGER/CEIL/FLOOR) in full.
# Other values had glibc's 15-digit rounding (%.15g), which is not SAS's
# display rounding and is not verified against SAS: fail loud.
mac_num_text <- function(x) {
  if (is.na(x)) return(".")
  if (x == trunc(x) && abs(x) < 1e15) return(sprintf("%.0f", x))
  abort_unsupported(paste0("the macro text of the value ", format(x, digits = 17),
                           " (a non-integer or an integer of 1E15 or more is not verified against SAS)"))
}

mac_eval_int <- function(text, what = "%EVAL") {
  v <- mac_eval(text, float = FALSE, what = what)
  if (v$kind != "num") {
    sasonr_abort(paste0("A character operand was found in ", what, " where a numeric operand is required: ",
                        mq_unmask(text)), "sasonr_runtime_error")
  }
  v$value
}

mac_eval_bool <- function(text, what = "%IF condition") {
  x <- mac_eval_int(text, what)
  !is.na(x) && x != 0
}

mac_sysevalf <- function(text, conv = NULL) {
  v <- mac_eval(text, float = TRUE, what = "%SYSEVALF")
  if (v$single && is.null(conv) && is.null(mac_dt_literal(trimws(mq_unmask(mq_strip(text)))))) return(mq_unmask(mq_strip(text)))
  if (v$kind != "num") {
    sasonr_abort(paste0("A character operand was found in %SYSEVALF: ", mq_unmask(text)), "sasonr_runtime_error")
  }
  x <- v$value
  if (is.null(conv)) return(mac_float_text(x))
  switch(tolower(conv),
    boolean = if (is.na(x) || x == 0) "0" else "1",
    integer = mac_num_text(trunc(x)),
    ceil = mac_num_text(ceiling(x)),
    floor = mac_num_text(floor(x)),
    abort_unsupported(paste0("%SYSEVALF conversion type ", conv))
  )
}

# `1e-2`: a sign directly after the exponent marker of a number is part of it.
num_exp_sign <- function(cp, i, j) {
  if (!cp[j] %in% c(43L, 45L) || j <= i + 1L || j == length(cp) || !(cp[j + 1L] >= 48L && cp[j + 1L] <= 57L)) return(FALSE)
  pre <- cp[i:(j - 1L)]
  last <- pre[length(pre)]
  body <- pre[-length(pre)]
  last %in% c(69L, 101L) && length(body) > 0 && all((body >= 48L & body <= 57L) | body == 46L) && any(body >= 48L & body <= 57L)
}

# %SYSEVALF results: integers in full, otherwise BEST16.-style text.
mac_float_text <- function(x) {
  if (is.na(x)) return(".")
  if (x == trunc(x) && abs(x) < 1e15) return(sprintf("%.0f", x))
  trimws(sas_fmt_best(x, 16L))
}

# A date, time or datetime literal ('15JAN2020'd, '12:00't, '...'dt) as an
# operand of %SYSEVALF or a numeric %SYSFUNC argument: its SAS value, or NULL.
mac_dt_literal <- function(s) {
  m <- regmatches(s, regexec("^(['\"])(.*)\\1(dt|d|t)$", s, ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  kind <- c(d = "date", t = "time", dt = "datetime")[[tolower(m[4])]]
  v <- sas_parse_dt_literal(m[3], kind)
  if (is.na(v)) NULL else v
}
