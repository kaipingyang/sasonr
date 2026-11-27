# Runtime library: SAS value semantics used by generated R code.
#
# Every helper is vectorised so the same expression code works for L1/L2
# (columns inside dplyr verbs) and L3 (length-1 values in the PDV loop).
# Character values are stored blank-padded to their SAS length.

# Missing values ---------------------------------------------------------

#' SAS missing value
#'
#' `.` is `NA_real_`; special missing values `.A`-`.Z` and `._` are encoded as
#' NaN payloads so that they keep the SAS sort order `._ < . < .A < ... < .Z`.
#' @param code `"."`, `"._"`, or `".A"`-`".Z"` (the leading dot is optional).
#' @examples
#' sas_missing()      # .  (NA_real_)
#' a <- sas_missing(".A")
#' is.na(a)
#' sas_f_missing(a)   # SAS MISSING(.A) is 1
#' sas_f_max(a, 1)    # missing values are ignored by MAX
#' @export
sas_missing <- function(code = ".") {
  code <- toupper(sub(".", "", code, fixed = TRUE))
  if (!nzchar(code)) return(NA_real_)
  idx <- if (code == "_") 0L else utf8ToInt(code) - 64L
  bytes <- as.raw(c(idx, 0x5A, 0x5A, 0, 0, 0, 0xF8, 0x7F))
  if (.Platform$endian == "big") bytes <- rev(bytes)
  readBin(bytes, "double", n = 1L)
}

#' Rank of SAS missing values
#'
#' @return Integer vector: `NA` for non-missing, 0 for `._`, 1 for `.`,
#'   2-27 for `.A`-`.Z`.
#' @keywords internal
sas_miss_code <- function(x) {
  out <- rep(NA_integer_, length(x))
  if (!is.numeric(x)) return(out)
  na <- is.na(x)
  out[na] <- 1L
  nan <- which(is.nan(x))
  if (length(nan) > 0) {
    raw <- matrix(writeBin(x[nan], raw()), nrow = 8L)
    if (.Platform$endian == "big") raw <- raw[8:1, , drop = FALSE]
    special <- raw[2, ] == as.raw(0x5A) & raw[3, ] == as.raw(0x5A)
    idx <- as.integer(raw[1, ])
    out[nan[special]] <- ifelse(idx[special] == 0L, 0L, idx[special] + 1L)
  }
  out
}

sas_is_missing <- function(x) {
  if (is.character(x)) !nzchar(sas_rtrim(x)) else is.na(x)
}

miss_label <- function(code) {
  ifelse(code == 0L, "_", ifelse(code == 1L, ".", LETTERS[pmax(code - 1L, 1L)]))
}

# Character helpers -----------------------------------------------------

#' @export
sas_rtrim <- function(x) sub(" +$", "", x)

#' Store a value in a character variable of a given length (truncate / pad)
#' @export
#' @keywords internal
sas_char <- function(x, len) {
  if (is.numeric(x)) x <- sas_fmt_best(x, 12L)
  if (anyNA(x)) x[is.na(x)] <- ""
  wide <- chr_wide(x)
  if (any(wide)) return(sas_char_bytes(x, len, wide))
  if (length(len) != 1L) {
    x <- substr(x, 1L, len)
    return(paste0(x, strrep(" ", pmax(0L, len - nchar(x)))))
  }
  # ASCII values: cut only when a value is longer than `len`
  n <- nchar(x)
  if (any(n > len)) {
    x <- substr(x, 1L, len)
    n <- pmin(n, len)
  }
  k <- pmax(0L, len - n)
  # short lengths: the blanks are looked up (strrep() per value is slower)
  if (len <= 256L) return(paste0(x, strrep(" ", seq.int(0L, max(0L, len)))[k + 1L]))
  paste0(x, strrep(" ", k))
}

# Values with a byte outside ASCII.
chr_wide <- function(x) grepl("[^\\x01-\\x7f]", x, perl = TRUE, useBytes = TRUE)

# The UTF-8 text of values with bytes outside ASCII: not supported in an R
# session that is not UTF-8, nor for bytes that are not valid UTF-8.
chr_utf8 <- function(x) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    abort_unsupported("a character value with bytes outside ASCII in an R session that is not UTF-8")
  }
  x <- enc2utf8(x)
  if (!all(validUTF8(x))) abort_unsupported("a character value whose bytes are not valid UTF-8")
  x
}

# Number of bytes of each value (the storage length of a character value).
chr_bytes <- function(x) {
  wide <- chr_wide(x) & !is.na(x)
  if (!any(wide)) return(nchar(x))
  n <- nchar(x, type = "bytes")
  n[is.na(x)] <- NA_integer_
  n[wide] <- nchar(chr_utf8(x[wide]), type = "bytes")
  n
}

# Byte semantics of the character functions (UTF-8 session) -----------------
#
# Positions, lengths, counts and character lists count bytes.  A value with
# bytes outside ASCII is mapped to "byte text" (one character U+0000 to U+00FF
# per byte), so that the character-level code of a function works on bytes;
# a character result is mapped back and must be valid UTF-8 (a result that
# leaves part of a multi-byte character fails loud).  Case folding (the I
# modifier) and the class modifiers recognise ASCII letters only.

# Does a call need byte text?  Not inside a call that already uses it.
chr_any_wide <- function(...) {
  if (isTRUE(.active$chr_bytes)) return(FALSE)
  for (a in list(...)) if (is.character(a) && any(chr_wide(a), na.rm = TRUE)) return(TRUE)
  FALSE
}

chr_to_bytes <- function(x) {
  if (!is.character(x)) return(x)
  w <- chr_wide(x) & !is.na(x)
  if (!any(w)) return(x)
  y <- chr_utf8(x[w])
  # one code point per byte (raw values: no code page mapping of 0x80-0x9F)
  x[w] <- vapply(y, function(v) intToUtf8(as.integer(charToRaw(v))), "", USE.NAMES = FALSE)
  x
}

chr_from_bytes <- function(x, name) {
  if (!is.character(x)) return(x)
  w <- chr_wide(x) & !is.na(x)
  if (!any(w)) return(x)
  y <- vapply(x[w], function(v) {
    cp <- utf8ToInt(v)
    if (anyNA(cp) || any(cp > 255L)) return(NA_character_)
    r <- rawToChar(as.raw(cp))
    Encoding(r) <- "UTF-8"
    r
  }, "", USE.NAMES = FALSE)
  if (anyNA(y) || !all(validUTF8(y))) {
    abort_unsupported(sprintf("%s with a result that cuts a multi-byte character", toupper(name)))
  }
  x[w] <- y
  x
}

# Call `f` with its character arguments as byte text; `chr` maps a character
# result back.
chr_bytes_call <- function(name, f, args, chr = FALSE) {
  args <- lapply(args, chr_to_bytes)
  old <- .active$chr_bytes
  .active$chr_bytes <- TRUE
  on.exit(.active$chr_bytes <- old, add = TRUE)
  out <- do.call(f, args)
  .active$chr_bytes <- old
  if (chr) chr_from_bytes(out, name) else out
}

# Case folding of ASCII letters only (the I modifier).
ascii_lower <- function(x) chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
ascii_upper <- function(x) chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)

# sas_char() for values with bytes outside ASCII: the stored value is the
# first `len` bytes, padded with blanks to `len` bytes.  A cut inside a
# multi-byte character leaves bytes that are not a character: not supported.
sas_char_bytes <- function(x, len, wide) {
  len <- rep_len(len, length(x))
  out <- character(length(x))
  if (any(!wide)) out[!wide] <- sas_char(x[!wide], len[!wide])
  w <- which(wide)
  s <- chr_utf8(x[w])
  for (k in seq_along(w)) {
    r <- charToRaw(s[k])
    n <- len[w[k]]
    if (length(r) > n) {
      r <- r[seq_len(n)]
      t <- rawToChar(r)
      if (!validUTF8(t)) {
        abort_unsupported(sprintf("a character value cut to %d bytes inside a multi-byte character", as.integer(n)))
      }
    }
    v <- rawToChar(c(r, rep(as.raw(0x20), max(0L, n - length(r)))))
    Encoding(v) <- "UTF-8"
    out[w[k]] <- v
  }
  out
}

as_sas_chr <- function(x) if (is.numeric(x)) sas_fmt_best(x, 12L) else x

#' @export
sas_concat <- function(a, b) paste0(as_sas_chr(a), as_sas_chr(b))

# Comparisons -------------------------------------------------------------

recycle2 <- function(a, b) {
  n <- if (length(a) == 0 || length(b) == 0) 0L else max(length(a), length(b))
  list(rep_len(a, n), rep_len(b, n))
}

chr_cmp <- function(a, b) {
  ab <- recycle2(sas_rtrim(a), sas_rtrim(b))
  a <- ab[[1]]
  b <- ab[[2]]
  n <- length(a)
  out <- integer(n)
  neq <- which(a != b)
  if (length(neq) > 0) {
    x <- c(a[neq], b[neq])
    r <- integer(length(x))
    r[order(x, method = "radix")] <- seq_along(x)
    k <- length(neq)
    out[neq] <- ifelse(r[seq_len(k)] < r[k + seq_len(k)], -1L, 1L)
  }
  out
}

#' Three-way SAS comparison (missing sorts lowest)
#' @keywords internal
sas_cmp <- function(a, b) {
  if (is.character(a) || is.character(b)) {
    if (!(is.character(a) && is.character(b))) {
      abort_unsupported_construct("comparison between character and numeric values")
    }
    return(chr_cmp(a, b))
  }
  ab <- recycle2(as.numeric(a), as.numeric(b))
  a <- ab[[1]]
  b <- ab[[2]]
  out <- as.integer(sign(a - b))
  ma <- is.na(a)
  mb <- is.na(b)
  if (any(ma | mb)) {
    ca <- sas_miss_code(a)
    cb <- sas_miss_code(b)
    out[ma & !mb] <- -1L
    out[!ma & mb] <- 1L
    both <- ma & mb
    out[both] <- as.integer(sign(ca[both] - cb[both]))
  }
  out
}

#' @export
sas_eq <- function(a, b) as.numeric(sas_cmp(a, b) == 0L)
#' @export
sas_ne <- function(a, b) as.numeric(sas_cmp(a, b) != 0L)
#' @export
sas_lt <- function(a, b) as.numeric(sas_cmp(a, b) < 0L)
#' @export
sas_le <- function(a, b) as.numeric(sas_cmp(a, b) <= 0L)
#' @export
sas_gt <- function(a, b) as.numeric(sas_cmp(a, b) > 0L)
#' @export
sas_ge <- function(a, b) as.numeric(sas_cmp(a, b) >= 0L)

cmp_fun <- function(op) {
  switch(op, "=" = sas_eq, "^=" = sas_ne, "<" = sas_lt, "<=" = sas_le, ">" = sas_gt, ">=" = sas_ge)
}

#' Truncated (colon-modified) character comparison, e.g. `s =: 'AB'`
#' @export
#' @keywords internal
sas_cmp_trunc <- function(op, a, b) {
  ab <- recycle2(a, b)
  # the prefix length counts bytes (byte text; the prefix may cut a character)
  if (is.character(ab[[1]]) && is.character(ab[[2]]) && any(chr_wide(ab[[1]]) | chr_wide(ab[[2]]), na.rm = TRUE)) {
    ab <- lapply(ab, chr_to_bytes)
  }
  len <- pmin(nchar(ab[[1]]), nchar(ab[[2]]))
  cmp_fun(op)(substr(ab[[1]], 1L, len), substr(ab[[2]], 1L, len))
}

#' Comparison chain `a < b < c`: every operand is evaluated once
#'
#' An op ending in `:` is a colon (truncated) comparison.
#' @export
#' @keywords internal
sas_chain <- function(args, ops) {
  res <- TRUE
  for (k in seq_along(ops)) {
    op <- ops[k]
    v <- if (endsWith(op, ":")) {
      sas_cmp_trunc(sub(":", "", op, fixed = TRUE), args[[k]], args[[k + 1L]])
    } else {
      cmp_fun(op)(args[[k]], args[[k + 1L]])
    }
    res <- res & sas_true(v)
  }
  as.numeric(res)
}

#' BETWEEN-AND `x between lo and hi`
#'
#' The range runs from the smaller bound to the larger one, so the bounds may
#' come in either order (missing values sort lowest); every operand is
#' evaluated once.
#' @export
#' @keywords internal
sas_between <- function(x, lo, hi) {
  n <- if (length(x) == 0 || length(lo) == 0 || length(hi) == 0) 0L else max(length(x), length(lo), length(hi))
  x <- rep_len(x, n)
  lo <- rep_len(lo, n)
  hi <- rep_len(hi, n)
  sw <- sas_cmp(lo, hi) > 0L
  l <- lo
  h <- hi
  l[sw] <- hi[sw]
  h[sw] <- lo[sw]
  as.numeric(sas_cmp(x, l) >= 0L & sas_cmp(x, h) <= 0L)
}

#' @export
sas_in <- function(x, values) {
  res <- rep(FALSE, length(x))
  for (v in values) res <- res | sas_cmp(x, v) == 0L
  as.numeric(res)
}

# Logic and arithmetic --------------------------------------------------

#' Truth value of a SAS expression (missing and 0 are false)
#' @export
#' @keywords internal
sas_true <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.character(x)) abort_unsupported_construct("character value used as a condition")
  !is.na(x) & x != 0
}
#' @export
sas_not <- function(x) as.numeric(!sas_true(x))
#' @export
sas_and <- function(a, b) as.numeric(sas_true(a) & sas_true(b))
#' @export
sas_or <- function(a, b) as.numeric(sas_true(a) | sas_true(b))

#' @export
sas_div <- function(a, b, .at = NULL, .exact = TRUE, .gen_a = FALSE, .gen_b = FALSE) {
  if (anyNA(a) || anyNA(b)) {
    n <- max(length(a), length(b))
    ma <- rep_len(is.na(a), n)
    mb <- rep_len(is.na(b), n)
    # rows where a missing operand is the missing result of an earlier operation
    # are not counted (the place of that operation was already recorded)
    cnt <- (ma | mb) & !(.gen_a & ma) & !(.gen_b & mb)
    if (any(cnt)) sas_mv_count(.at, ifelse(cnt, NA_real_, 0), .exact = .exact)
  }
  zero <- !is.na(b) & b == 0
  if (any(zero & !is.na(a))) rt_error(paste0("NOTE: Division by zero detected", at_text(.at), "."))
  bb <- rep_len(b, max(length(a), length(b)))
  bb[rep_len(zero, length(bb))] <- NA_real_
  out <- sas_num(a / bb, .at)
  out
}

#' @export
sas_pow <- function(a, b, .at = NULL, .exact = TRUE) {
  # ** records its place also when an operand is a generated missing value
  # ((a + b) ** 2)
  if (anyNA(a) || anyNA(b)) sas_mv_count(.at, a, b, .exact = .exact)
  # 0**0, 0**-n and a negative base with a non-integer exponent: an invalid
  # argument (NOTE, _ERROR_) and a mathematical operation that could not be
  # performed at the place of the operator
  bad <- !is.na(a) & !is.na(b) & ((a == 0 & b <= 0) | (a < 0 & b != trunc(b)))
  if (any(bad)) {
    rt_error(paste0("NOTE: Invalid argument(s) to the exponential operator \"**\"", at_text(.at), "."))
    note_math(.at, sum(bad))
  }
  x <- a^b
  x[rep_len(bad, length(x))] <- NA_real_
  out <- sas_num(x, .at)
  out[is.na(a) | is.na(b)] <- NA_real_
  out
}

# Unary minus: SAS records the place at the operand's position, and only when the
# operand is missing but not the missing result of an earlier operation
# (-(a + b) records the "+", not the minus).
#' @export
#' @keywords internal
sas_neg <- function(x, .at = NULL, .exact = TRUE, .gen_a = FALSE) {
  if (anyNA(x)) {
    n <- length(x)
    ma <- rep_len(is.na(x), n)
    cnt <- ma & !(.gen_a & ma)
    if (any(cnt)) {
      # the place is the operand's position; without one it cannot be written
      if (is.null(.at)) abort_unsupported("missing-value NOTE of a unary minus whose operand has no source position")
      sas_mv_count(.at, ifelse(cnt, NA_real_, 0), .exact = .exact)
    }
  }
  sas_num(-x, .at)
}

#' MIN (><) and MAX (<>) operators
#' @export
#' @keywords internal
sas_min_op <- function(a, b) {
  ab <- recycle2(a, b)
  ifelse(sas_cmp(ab[[1]], ab[[2]]) <= 0L, ab[[1]], ab[[2]])
}
#' @export
sas_max_op <- function(a, b) {
  ab <- recycle2(a, b)
  ifelse(sas_cmp(ab[[1]], ab[[2]]) >= 0L, ab[[1]], ab[[2]])
}

# Numeric functions -------------------------------------------------------

args_matrix <- function(args) {
  args <- lapply(args, as.numeric)
  n <- max(lengths(args))
  matrix(unlist(lapply(args, rep_len, n)), nrow = n)
}

#' @export
sas_f_sum <- function(...) {
  m <- args_matrix(list(...))
  # sequential double sum in argument order; a missing argument counts as +0
  # for the sign of a zero result (SUM(., -0) is +0)
  out <- as.numeric(apply(m, 1, function(r) {
    if (all(is.na(r))) return(0)
    r[is.na(r)] <- 0
    Reduce(`+`, r)
  }))
  none <- rowSums(!is.na(m)) == 0
  out[none] <- NA_real_
  # all arguments missing: SUM(.A) is .A; mixed missing kinds are not supported
  for (i in which(none)) {
    codes <- unique(sas_miss_code(m[i, ]))
    if (length(codes) == 1L && codes != 1L) out[i] <- m[i, 1]
    if (length(codes) > 1L) abort_unsupported("SUM of different special missing values")
  }
  out
}
#' @export
sas_f_n <- function(...) as.numeric(rowSums(!is.na(args_matrix(list(...)))))
#' @export
sas_f_nmiss <- function(...) as.numeric(rowSums(is.na(args_matrix(list(...)))))
#' @export
sas_f_mean <- function(...) {
  m <- args_matrix(list(...))
  # sequential double sum (rowMeans accumulates in long double; MEAN(-0) is -0)
  out <- apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    if (length(v) == 0) return(NA_real_)
    # all values are zero: the result has the first non-missing argument's sign
    # (MEAN(-0, 0) is -0, MEAN(0, -0) is +0)
    if (all(v == 0)) return(v[1])
    Reduce(`+`, v) / length(v)
  })
  as.numeric(out)
}
#' @export
sas_f_min <- function(...) {
  m <- args_matrix(list(...))
  # ties keep the first non-missing argument (its zero sign)
  out <- apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    if (length(v) == 0) return(NA_real_)
    k <- min(v)
    v[which(v == k)[1L]]
  })
  as.numeric(out)
}
#' @export
sas_f_max <- function(...) {
  m <- args_matrix(list(...))
  out <- apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    if (length(v) == 0) return(NA_real_)
    k <- max(v)
    v[which(v == k)[1L]]
  })
  as.numeric(out)
}

fuzz_int <- function(x) {
  r <- round(x)
  ifelse(!is.na(x) & abs(x - r) < 1e-12 * pmax(1, abs(x)), r, x)
}

#' @export
sas_f_abs <- function(x) {
  x <- abs(x)
  x[is.na(x)] <- NA_real_ # special missing values give an ordinary missing
  x
}
#' @export
sas_f_sign <- function(x) {
  x <- sign(x)
  x[is.na(x)] <- NA_real_
  x
}
#' @export
sas_f_sqrt <- function(x, .at = NULL) {
  # invalid arguments: see fn_policy (log NOTE, _ERROR_)
  ifelse(!is.na(x) & x < 0, NA_real_, sqrt(pmax(x, 0)))
}
#' @export
sas_f_exp <- function(x, .at = NULL) {
  out <- exp(x)
  out[is.infinite(out)] <- NA_real_
  out
}
log_base <- function(x, f, name, .at) {
  bad <- !is.na(x) & x <= 0
  ifelse(bad, NA_real_, f(pmax(x, .Machine$double.xmin)))
}
#' @export
sas_f_log <- function(x, .at = NULL) log_base(x, log, "log", .at)
#' @export
sas_f_log10 <- function(x, .at = NULL) log_base(x, log10, "log10", .at)
#' @export
sas_f_log2 <- function(x, .at = NULL) {
  # powers of 2 give the exact exponent; other values log(x) * (1/log(2))
  log_base(x, function(v) {
    e <- log2(v)
    ifelse(e == round(e) & 2^round(e) == v, round(e), v * 0 + log(v) * (1 / log(2)))
  }, "log2", .at)
}
#' @export
sas_f_mod <- function(a, b, .at = NULL) {
  n <- max(length(a), length(b))
  a <- rep_len(as.numeric(a), n)
  b <- rep_len(as.numeric(b), n)
  out <- mod_exact(a, b)
  zero <- !is.na(b) & b == 0
  if (any(zero & !is.na(a))) note_invalid_arg("mod", NULL, .at, list(a, b), zero & !is.na(a))
  out[zero] <- NA_real_
  # fuzz: a remainder within 1e-12 (relative) of 0 or of the divisor is 0
  tol <- 1e-12 * pmax(abs(a), abs(b), 1, na.rm = TRUE)
  near <- !is.na(out) & (abs(out) < tol | abs(abs(out) - abs(b)) < tol)
  # the fuzz also moves other remainders by a few ulps toward the decimal
  # result; only exact remainders are in the supported domain
  mod_domain_check("MOD", a, b, exact_only = TRUE, fuzz_zero = near)
  out[near] <- 0
  out <- fuzz_int(out)
  # a zero remainder of integer arguments has the sign of a (MOD(-1, 1) is -0),
  # otherwise it is +0
  z <- !is.na(out) & out == 0
  out[z] <- ifelse(a[z] < 0 & a[z] == trunc(a[z]) & b[z] == trunc(b[z]), -0, 0)
  out
}
#' @export
sas_f_missing <- function(x) as.numeric(sas_is_missing(x))
#' @export
sas_f_coalesce <- function(...) {
  m <- args_matrix(list(...))
  apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    if (length(v) == 0) NA_real_ else v[1]
  })
}

# Character functions -----------------------------------------------------

strip_both <- function(x) sub("^ +", "", sas_rtrim(x))

# STRIP: remove the blanks matched by `pattern` with stringr (ICU regex; `\z` is the
# absolute end, since ICU's `$` also matches before a final line feed).  The
# result keeps the attributes of `x`, as sub() does.  A value that is not
# valid UTF-8 (a latin1 value, or invalid bytes, which must stay an error)
# takes the base function `base`.
remove_blanks <- function(x, pattern, base) {
  if (!is.character(x) || !all(validUTF8(x))) return(base(x))
  out <- stringr::str_remove_all(x, pattern)
  attributes(out) <- attributes(x)
  out
}

#' @export
sas_f_trim <- function(x) {
  if (chr_any_wide(x)) return(chr_bytes_call("trim", sys.function(), list(x), chr = TRUE))
  out <- sas_rtrim(as_sas_chr(x))
  out[!nzchar(out)] <- " "
  out
}
#' @export
sas_f_trimn <- function(x) sas_rtrim(as_sas_chr(x))
#' @export
sas_f_strip <- function(x) {
  if (chr_any_wide(x)) return(chr_bytes_call("strip", sys.function(), list(x), chr = TRUE))
  remove_blanks(as_sas_chr(x), "\\A +| +\\z", strip_both)
}
#' @export
sas_f_left <- function(x) {
  if (chr_any_wide(x)) return(chr_bytes_call("left", sys.function(), list(x), chr = TRUE))
  x <- as_sas_chr(x)
  s <- sub("^ +", "", x)
  paste0(s, strrep(" ", nchar(x) - nchar(s)))
}
#' @export
sas_f_right <- function(x) {
  if (chr_any_wide(x)) return(chr_bytes_call("right", sys.function(), list(x), chr = TRUE))
  x <- as_sas_chr(x)
  s <- sas_rtrim(x)
  paste0(strrep(" ", nchar(x) - nchar(s)), s)
}
#' @export
sas_f_upcase <- function(x) toupper(as_sas_chr(x))
#' @export
sas_f_lowcase <- function(x) tolower(as_sas_chr(x))
# CAT, CATS, CATT and CATX write a numeric item with BEST32. (whole numbers)
# or BEST12. without blanks; missing values are written ('.', 'A').
cat_num_text <- function(a) {
  a[!is.na(a) & a == 0] <- 0
  whole <- !is.na(a) & a == trunc(a)
  strip_both(ifelse(whole, sas_fmt_best(a, 32L), sas_fmt_best(a, 12L)))
}
cat_item <- function(a, trim) if (is.numeric(a)) cat_num_text(a) else trim(a)

# A result longer than the target variable (`.len`, 200 for a new variable):
# blank result and _ERROR_ = 1, no NOTE (CATX also writes a NOTE).
cat_fit <- function(out, .len, fname = NULL, args = NULL, .at = NULL, trim = identity) {
  if (is.null(.len)) return(out)
  # the CAT functions build their value in a 200-byte buffer: a longer value is
  # blank (a shorter target variable just truncates, as in t = cats(t, c))
  if (.len > 200L && any(nchar(out) > 200L)) abort_unsupported("CAT, CATS or CATT result longer than 200 characters")
  long <- nchar(out) > 200L
  if (any(long)) {
    rt_error()
    out[long] <- ""
  }
  if (isTRUE(.active$chr_bytes) && !is.null(fname)) out <- cat_fit_bytes(out, .len, fname, args, .at, trim)
  out
}

# A target variable shorter than the result of CAT, CATS or CATT, for values
# with bytes outside ASCII (byte text, `out`): when the cut falls inside a
# multi-byte character the result is blank, with the buffer WARNING, the NOTE
# on the left-most argument that does not fit and _ERROR_ = 1.  A cut between
# characters is not supported.
cat_fit_bytes <- function(out, .len, fname, args, .at, trim) {
  short <- nchar(out) > .len
  if (!any(short)) return(out)
  n <- max(lengths(args))
  for (i in which(short)) {
    nxt <- utf8ToInt(substr(out[i], .len + 1L, .len + 1L))
    # (vectorised code has no call position for the NOTE: not supported)
    if (!(nxt >= 0x80 && nxt < 0xC0) || is.null(.at)) {
      abort_unsupported(sprintf("%s with a result longer than its target variable (bytes outside ASCII)", toupper(fname)))
    }
    v <- vapply(args, function(a) cat_item(a[min(i, length(a))], trim), "")
    argn <- which(cumsum(nchar(v)) > .len)[1]
    rt <- rt_diag()
    rt$notes <- c(rt$notes, paste0(
      "WARNING: In a call to the ", toupper(fname), " function, the buffer allocated for the result was not long enough to contain ",
      "the concatenation of all the arguments. The correct result would contain ", nchar(out[i]),
      " characters, but the actual result might either be truncated to ", .len,
      " character(s) or be completely blank, depending on the calling environment. ",
      "The following note indicates the left-most argument that caused truncation."))
    note_invalid_arg(fname, argn, .at, lapply(args, function(a) chr_from_bytes(a[min(i, length(a))], fname)), NULL,
                     numbered = TRUE, math = FALSE)
    out[i] <- ""
  }
  out
}
#' @export
sas_f_cats <- function(..., .at = NULL, .len = NULL) {
  if (chr_any_wide(...)) return(chr_bytes_call("cats", sys.function(), c(list(...), list(.at = .at, .len = .len)), chr = TRUE))
  cat_fit(rlang::exec(paste0, !!!lapply(list(...), cat_item, trim = strip_both)), .len, "cats", list(...), .at, strip_both)
}
#' @export
sas_f_catt <- function(..., .at = NULL, .len = NULL) {
  if (chr_any_wide(...)) return(chr_bytes_call("catt", sys.function(), c(list(...), list(.at = .at, .len = .len)), chr = TRUE))
  cat_fit(rlang::exec(paste0, !!!lapply(list(...), cat_item, trim = sas_rtrim)), .len, "catt", list(...), .at, sas_rtrim)
}
#' @export
sas_f_cat <- function(..., .at = NULL, .len = NULL) {
  if (chr_any_wide(...)) return(chr_bytes_call("cat", sys.function(), c(list(...), list(.at = .at, .len = .len)), chr = TRUE))
  cat_fit(rlang::exec(paste0, !!!lapply(list(...), cat_item, trim = identity)), .len, "cat", list(...), .at, identity)
}
#' @export
sas_f_catx <- function(sep, ..., .at = NULL, .len = 200L) {
  if (chr_any_wide(sep, ...)) return(chr_bytes_call("catx", sys.function(), c(list(sep), list(...), list(.at = .at, .len = .len)), chr = TRUE))
  rt <- rt_diag()
  args <- list(...)
  # numeric items: BEST32. without blanks (missing values are written, not
  # skipped: '.', 'A'); only blank character items are skipped
  parts <- lapply(args, cat_item, trim = strip_both)
  n <- max(lengths(parts), length(sep))
  parts <- lapply(parts, rep_len, n)
  sep <- rep_len(sep, n)
  vapply(seq_len(n), function(i) {
    v <- vapply(parts, `[`, character(1), i)
    keep <- nzchar(v)
    out <- paste(v[keep], collapse = sep[i])
    if (nchar(out) > .len) {
      # the buffer is too short: blank result, WARNING and _ERROR_
      cum <- cumsum(ifelse(keep, nchar(v), 0L) + ifelse(keep & cumsum(keep) > 1, nchar(sep[i]), 0L))
      argn <- which(cum > .len)[1] + 1L
      rt$notes <- c(rt$notes, paste0(
        "WARNING: In a call to the CATX function, the buffer allocated for the result was not long enough to contain ",
        "the concatenation of all the arguments. The correct result would contain ", nchar(out),
        " characters, but the actual result might either be truncated to ", .len,
        " character(s) or be completely blank, depending on the calling environment. ",
        "The following note indicates the left-most argument that caused truncation."))
      note_invalid_arg("catx", argn, .at, lapply(c(list(sep[i]), lapply(args, `[`, min(i, max(lengths(args))))), chr_from_bytes, "catx"), NULL,
                       numbered = TRUE, math = FALSE)
      return("")
    }
    out
  }, character(1))
}
#' @export
sas_f_substr <- function(x, pos, len = NULL, .at = NULL) substr_impl(x, pos, len, .at, "substr")
# KSUBSTR(x, pos <, len>): positions and length in characters; n is the length
# of x (trailing blanks included); the result has the length of x.
#   - pos or len of magnitude 1E300 or more: "Invalid argument to function
#     KSUBSTR(args)", _ERROR_ = 1, blank result.
#   - pos (truncated) missing (any kind), < 1 or > n: "Argument 2 to function
#     KSUBSTR(args) ... is invalid.", _ERROR_ = 1, blank result.
#   - len missing (any kind): "Argument 3 ... is invalid.", _ERROR_ = 1, blank.
#   - len <= 0: "ERROR: Argument three to KSUBSTR is invalid. The value must be
#     greater than zero." then the Argument 3 NOTE, _ERROR_ = 1, blank.
#   - len (truncated) past the end of x: the rest of x, no NOTE.
# Not supported (fail loud): an argument of magnitude between 32767 and 1E300;
# len between 0 and 1; an invalid pos together with an invalid or huge len (or
# a huge pos with an invalid len); a value with bytes outside ASCII whose pos
# or len is invalid or past the end.
#' @export
sas_f_ksubstr <- function(x, pos, len = NULL, .at = NULL) {
  x <- as_sas_chr(x)
  m <- max(length(x), length(pos), length(len))
  x <- rep_len(x, m)
  pos <- rep_len(as.numeric(pos), m)
  has_len <- !is.null(len)
  len <- if (has_len) rep_len(as.numeric(len), m) else rep(NA_real_, m)
  wide <- chr_wide(x) & !is.na(x)
  if (any(wide)) x[wide] <- chr_utf8(x[wide])
  n <- nchar(x, type = "chars")
  huge_p <- !is.na(pos) & abs(pos) >= 1e300
  huge_l <- has_len & !is.na(len) & abs(len) >= 1e300
  mid <- (!is.na(pos) & abs(pos) > 32767 & !huge_p) | (has_len & !is.na(len) & abs(len) > 32767 & !huge_l)
  p <- trunc(pos)
  bad_p <- !huge_p & (is.na(p) | p < 1 | p > n)
  miss_l <- has_len & is.na(len)
  neg_l <- has_len & !is.na(len) & !huge_l & len <= 0
  bad_l <- miss_l | neg_l
  frac_l <- has_len & !is.na(len) & len > 0 & len < 1
  over <- !bad_p & !huge_p & has_len & !bad_l & !huge_l & p + trunc(len) - 1 > n
  if (any(mid | frac_l | (bad_p & (bad_l | huge_l)) | (huge_p & bad_l))) {
    abort_unsupported("KSUBSTR with these position and length arguments")
  }
  if (any(wide & (bad_p | bad_l | huge_p | huge_l | over))) {
    abort_unsupported("KSUBSTR with an invalid position or length for a value with bytes outside ASCII")
  }
  args <- if (has_len) list(x, pos, len) else list(x, pos)
  huge <- huge_p | huge_l
  if (any(huge)) note_invalid_arg("ksubstr", NULL, .at, args, huge, math = FALSE)
  if (any(bad_p)) note_invalid_arg("ksubstr", 2L, .at, args, bad_p, numbered = TRUE, math = FALSE)
  bad_l <- bad_l & !bad_p & !huge_p
  if (any(neg_l & bad_l)) rt_error("ERROR: Argument three to KSUBSTR is invalid. The value must be greater than zero.")
  if (any(bad_l)) note_invalid_arg("ksubstr", 3L, .at, args, bad_l, numbered = TRUE, math = FALSE)
  ok <- !(huge | bad_p | bad_l)
  end <- if (has_len) pmin(p + trunc(len) - 1, n) else n
  out <- character(m)
  out[ok] <- substr(x[ok], p[ok], end[ok])
  out
}

# SUBSTR counts positions and lengths in bytes (a UTF-8 session).  ASCII
# values: one byte per character.
substr_impl <- function(x, pos, len, .at, fname) {
  x <- as_sas_chr(x)
  m <- max(length(x), length(pos), length(len))
  x <- rep_len(x, m)
  wide <- chr_wide(x) & !is.na(x)
  if (any(wide)) x[wide] <- chr_utf8(x[wide])
  n <- nchar(x, type = if (fname == "substr") "bytes" else "chars")
  pos <- rep_len(trunc(pos), m)
  bad_pos <- is.na(pos) | pos < 1 | pos > n
  bad_len <- rep(FALSE, m)
  if (is.null(len)) {
    end <- n
  } else {
    len <- rep_len(trunc(len), m)
    # a nonpositive or missing length returns the remainder (a NOTE: invalid argument)
    bad_len <- !bad_pos & (is.na(len) | len <= 0 | pos + len - 1 > n)
    end <- ifelse(is.na(len) | len <= 0, n, pmin(pos + len - 1, n))
  }
  if (any(bad_len)) note_invalid_arg(fname, 3L, .at)
  if (any(bad_pos)) note_invalid_arg(fname, 2L, .at)
  out <- character(m)
  byte_cut <- fname == "substr" & wide & !bad_pos
  chr_cut <- !bad_pos & !byte_cut
  out[chr_cut] <- substr(x[chr_cut], pos[chr_cut], end[chr_cut])
  for (i in which(byte_cut)) {
    v <- rawToChar(charToRaw(x[i])[pos[i]:end[i]])
    # a start or end inside a multi-byte character leaves bytes that are not a
    # character: not supported
    if (!validUTF8(v)) {
      abort_unsupported(sprintf("SUBSTR from byte %d to byte %d inside a multi-byte character",
                                as.integer(pos[i]), as.integer(end[i])))
    }
    Encoding(v) <- "UTF-8"
    out[i] <- v
  }
  # an invalid position returns blank (a NOTE: invalid argument)
  out
}
#' @export
# Left-hand SUBSTR: replace part of a character variable in place.  An invalid
# position is a SAS data error (the variable keeps its value); a replacement
# shorter than the target window is padded with blanks, a longer one is cut to
# the room that remains.
#' @export
sas_substr_assign <- function(x, pos, len = NULL, value, .at = NULL) {
  x <- as_sas_chr(x)
  value <- as_sas_chr(value)
  # values with bytes outside ASCII: not supported
  if (any(chr_wide(x) | chr_wide(value), na.rm = TRUE)) {
    abort_unsupported("SUBSTR on the left of = with a value that has bytes outside ASCII")
  }
  m <- max(length(x), length(pos), length(value), if (is.null(len)) 1L else length(len))
  x <- rep_len(x, m)
  value <- rep_len(value, m)
  pos <- rep_len(trunc(pos), m)
  n <- nchar(x)
  bad <- is.na(pos) | pos < 1 | pos > n
  if (any(bad)) note_invalid_arg("substr", 2L, .at)
  if (is.null(len)) {
    fill <- n - pos + 1L
  } else {
    len <- rep_len(trunc(len), m)
    bad_len <- !bad & (is.na(len) | len < 0)
    if (any(bad_len)) note_invalid_arg("substr", 3L, .at)
    fill <- ifelse(is.na(len), n - pos + 1L, pmin(len, n - pos + 1L))
  }
  fill[bad] <- 0L
  fill <- pmax(0L, fill)
  out <- x
  for (i in seq_len(m)) {
    if (is.na(fill[i]) || fill[i] <= 0L) next
    p <- pos[i]
    rep_txt <- substr(value[i], 1L, fill[i])
    pad <- fill[i] - nchar(rep_txt)
    if (pad > 0L) rep_txt <- paste0(rep_txt, strrep(" ", pad))
    out[i] <- paste0(substr(x[i], 1L, p - 1L), rep_txt, substr(x[i], p + fill[i], n[i]))
  }
  out
}

#' @export
# EXIST(member <, type>): 1 when the member exists, 0 otherwise.  WORK and
# SASHELP are the libraries sasonr knows (R/sashelp.R).  The type argument is a
# member type: 'DATA' is supported, an explicit 'ANY' is not a valid member type
# and gives 0, and any other type fails loud because it is not supported.
#' @export
sas_f_exist <- function(name, type = NULL, .work = NULL, .at = NULL) {
  n <- max(length(name), if (is.null(type)) 1L else length(type))
  nm <- rep_len(as_sas_chr(name), n)
  ty <- if (is.null(type)) rep_len(NA_character_, n) else rep_len(toupper(trimws(as_sas_chr(type))), n)
  vapply(seq_len(n), function(i) sas_exist_one(nm[i], ty[i], .work, .at), 0)
}

sas_exist_one <- function(nm, type, .work, .at) {
  nm <- trimws(nm)
  if (is.na(nm) || !nzchar(nm)) {
    abort_unsupported("EXIST() of a missing or blank member name (not supported)")
  }
  # an explicit TYPE='ANY' is not a valid member type: EXIST returns 0
  if (!is.na(type) && identical(type, "ANY")) return(0)
  if (!is.na(type) && !identical(type, "DATA")) {
    abort_unsupported(paste0("EXIST() with TYPE='", type, "' (only DATA is supported)"))
  }
  dot <- regexpr("\\.", nm)
  if (dot > 0) {
    lib <- tolower(substr(nm, 1L, dot - 1L))
    mem <- substr(nm, dot + 1L, nchar(nm))
  } else {
    lib <- "work"
    mem <- nm
  }
  mem <- tolower(trimws(mem))
  if (!nzchar(mem)) abort_unsupported("EXIST() with an empty member name (not supported)")
  if (lib %in% c("work", "w")) {
    if (is.null(.work)) abort_unsupported("EXIST() outside a DATA step environment")
    return(as.numeric(mem %in% tolower(ls(.work, all.names = TRUE))))
  }
  if (lib == "sashelp") {
    # a member missing from the SASHELP directory may still be a SASHELP
    # member: only a member found there answers (1)
    if (sashelp_has(mem)) return(1)
    abort_unsupported(paste0("EXIST('SASHELP.", toupper(mem), "'): the member is not in the SASHELP directory ",
                             "(EXIST cannot tell whether it is a SASHELP member)"))
  }
  abort_unsupported(paste0("EXIST('", toupper(lib), ".", toupper(mem), "') for a library other than WORK or SASHELP (not supported)"))
}

#' @export
sas_f_index <- function(x, s) {
  if (chr_any_wide(x, s)) return(chr_bytes_call("index", sys.function(), list(x, s)))
  ab <- recycle2(as_sas_chr(x), s)
  out <- mapply(function(a, b) regexpr(b, a, fixed = TRUE)[1], ab[[1]], ab[[2]], USE.NAMES = FALSE)
  as.numeric(pmax(out, 0L))
}
#' @export
sas_f_indexc <- function(x, ...) {
  if (chr_any_wide(x, ...)) return(chr_bytes_call("indexc", sys.function(), list(x, ...)))
  # the characters of all excerpts form one set (INDEXC(source, excerpt-1, ..., excerpt-n))
  ab <- rlang::exec(recycle_n, as_sas_chr(x), !!!lapply(list(...), as_sas_chr))
  vapply(seq_along(ab[[1]]), function(i) {
    set <- unlist(lapply(ab[-1], function(e) strsplit(e[i], "")[[1]]))
    w <- which(strsplit(ab[[1]][i], "")[[1]] %in% set)
    if (length(w) == 0) 0 else as.numeric(w[1])
  }, numeric(1))
}
#' @export
sas_f_length <- function(x) {
  n <- chr_bytes(sas_rtrim(as_sas_chr(x)))
  as.numeric(pmax(n, 1L))
}
#' @export
sas_f_lengthn <- function(x) as.numeric(chr_bytes(sas_rtrim(as_sas_chr(x))))
# Number of characters without trailing blanks; 0 for a blank value.
#' @export
sas_f_klength <- function(x) {
  s <- sas_rtrim(as_sas_chr(x))
  wide <- chr_wide(s)
  if (any(wide)) s[wide] <- chr_utf8(s[wide])
  as.numeric(nchar(s, type = "chars"))
}
#' @export
sas_f_tranwrd <- function(x, from, to) {
  if (chr_any_wide(x, from, to)) return(chr_bytes_call("tranwrd", sys.function(), list(x, from, to), chr = TRUE))
  x <- as_sas_chr(x)
  # one target and one replacement: a single vectorised gsub(); its result
  # carries no attributes, like the element-wise path
  if (length(x) > 0L && length(from) == 1L && length(to) == 1L) {
    out <- gsub(from, to, x, fixed = TRUE)
    attributes(out) <- NULL
    return(out)
  }
  vapply(x, function(s) gsub(from, to, s, fixed = TRUE), character(1), USE.NAMES = FALSE)
}
#' @export
sas_f_coalescec <- function(...) {
  parts <- lapply(list(...), as_sas_chr)
  n <- max(lengths(parts))
  parts <- lapply(parts, rep_len, n)
  vapply(seq_len(n), function(i) {
    v <- vapply(parts, `[`, character(1), i)
    v <- v[nzchar(sas_rtrim(v))]
    if (length(v) == 0) "" else v[1]
  }, character(1))
}
#' @export
sas_f_reverse <- function(x) {
  if (chr_any_wide(x)) return(chr_bytes_call("reverse", sys.function(), list(x), chr = TRUE))
  vapply(as_sas_chr(x), function(s) intToUtf8(rev(utf8ToInt(s))), character(1), USE.NAMES = FALSE)
}

# Dates and times (SAS dates count days from 1960-01-01) ------------------

sas_epoch <- as.Date("1960-01-01")
month_abb <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

#' Convert a date/time/datetime literal body to its SAS numeric value
#' @keywords internal
sas_parse_dt_literal <- function(text, kind) {
  text <- trimws(text)
  parse_date <- function(s) {
    m <- regmatches(s, regexec("^([0-9]{1,2})([A-Za-z]{3})([0-9]{2}|[0-9]{4})$", s))[[1]]
    if (length(m) == 0) return(NA_real_)
    mon <- match(toupper(m[3]), month_abb)
    yr <- as.integer(m[4])
    if (nchar(m[4]) == 2) yr <- if (yr < 40) 2000L + yr else 1900L + yr
    d <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", yr, mon, as.integer(m[2]))), error = function(e) NA)
    as.numeric(d - sas_epoch)
  }
  parse_time <- function(s) {
    parts <- as.numeric(strsplit(s, ":", fixed = TRUE)[[1]])
    if (length(parts) < 2 || length(parts) > 3 || anyNA(parts)) return(NA_real_)
    if (length(parts) == 2) parts <- c(parts, 0)
    sum(parts * c(3600, 60, 1))
  }
  switch(kind,
    date = parse_date(text),
    time = parse_time(text),
    datetime = {
      k <- regexpr(":", text, fixed = TRUE)
      if (k < 0) return(NA_real_)
      parse_date(substr(text, 1L, k - 1L)) * 86400 + parse_time(substring(text, k + 1L))
    },
    NA_real_
  )
}

to_date <- function(x) sas_epoch + floor(x)

# SAS calendar ------------------------------------------------------------------
#
# Proleptic Gregorian calendar, except that the years divisible by 4000 have
# no 29 February (DAY('31DEC20000'd) is 31; R's Date would be 5 days
# off).  `g` counts Gregorian days from 1960-01-01; SAS day = g minus the
# skipped 29 Februaries before it.
greg_days <- function(y, m, d) {
  y <- y - (m <= 2)
  era <- floor(y / 400)
  yoe <- y - era * 400
  mp <- (m + 9) %% 12
  doy <- floor((153 * mp + 2) / 5) + d - 1
  doe <- yoe * 365 + floor(yoe / 4) - floor(yoe / 100) + doy
  era * 146097 + doe - 719468 + 3653
}
greg_civil <- function(g) {
  z <- g - 3653 + 719468
  era <- floor(z / 146097)
  doe <- z - era * 146097
  yoe <- floor((doe - floor(doe / 1460) + floor(doe / 36524) - floor(doe / 146096)) / 365)
  doy <- doe - (365 * yoe + floor(yoe / 4) - floor(yoe / 100))
  mp <- floor((5 * doy + 2) / 153)
  d <- doy - floor((153 * mp + 2) / 5) + 1
  m <- ifelse(mp < 10, mp + 3, mp - 9)
  list(y = yoe + era * 400 + (m <= 2), m = m, d = d)
}
skip4000_g <- greg_days(c(4000, 8000, 12000, 16000, 20000), 3, 1)
skip4000_sas <- skip4000_g - seq_along(skip4000_g)
sas_day_to_greg <- function(d) d + findInterval(d, skip4000_sas)
greg_to_sas_day <- function(g) g - findInterval(g, skip4000_g)

# One field of civil(); each computes only what it needs (the weekday needs
# no calendar).
date_part <- function(x, what) {
  out <- if (what == "wday") {
    (floor(as.numeric(x)) + 5) %% 7 + 1 # 1 = Sunday
  } else {
    civil_ymd(x)[[switch(what, year = "y", month = "m", day = "d")]]
  }
  out <- as.numeric(out)
  out[is.na(x)] <- NA_real_
  out
}
from_ymd <- function(y, m, d) {
  n <- max(length(y), length(m), length(d))
  y <- rep_len(as.numeric(y), n)
  m <- rep_len(as.numeric(m), n)
  d <- rep_len(as.numeric(d), n)
  ok <- !is.na(y) & !is.na(m) & !is.na(d) & abs(y) < 1e6 & m >= 1 & m <= 12 & d >= 1
  ok[ok] <- y[ok] == trunc(y[ok]) & m[ok] == trunc(m[ok]) & d[ok] == trunc(d[ok])
  out <- rep(NA_real_, n)
  if (!any(ok)) return(out)
  leap <- (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0 & y %% 4000 != 0)
  dim <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[pmin(pmax(m, 1), 12)] + (m == 2 & leap)
  ok <- ok & d <= dim
  out[ok] <- greg_to_sas_day(greg_days(y[ok], m[ok], d[ok]))
  out
}
days_in_month <- function(y, m) {
  nm <- ifelse(m == 12, 1, m + 1)
  ny <- ifelse(m == 12, y + 1, y)
  from_ymd(ny, nm, 1) - from_ymd(y, m, 1)
}

#' @export
sas_f_mdy <- function(m, d, y, .at = NULL) {
  # arguments are truncated to integers; years 0-99 use the YEARCUTOFF window
  # (1940-2039); valid years 1582-20000 (invalid arguments: fn_policy)
  y <- floor(y)
  y <- ifelse(!is.na(y) & y >= 0 & y < 100, ifelse(y < 40, 2000 + y, 1900 + y), y)
  out <- from_ymd(y, floor(m), floor(d))
  out[!is.na(y) & (y < 1582 | y > 20000)] <- NA_real_
  out
}
#' @export
sas_f_year <- function(x) date_part(x, "year")
#' @export
sas_f_month <- function(x) date_part(x, "month")
#' @export
sas_f_day <- function(x) date_part(x, "day")
#' @export
sas_f_weekday <- function(x) date_part(x, "wday")
#' @export
sas_f_qtr <- function(x) floor((date_part(x, "month") - 1) / 3) + 1
#' @export
sas_f_today <- function() as.numeric(Sys.Date() - sas_epoch)
#' @export
sas_f_date <- function() sas_f_today()
#' @export
sas_f_datepart <- function(x) floor(x / 86400) + 0 # DATEPART(-0) is 0
#' @export
sas_f_timepart <- function(x) x - floor(x / 86400) * 86400
#' @export
sas_f_hour <- function(x) floor((x %% 86400) / 3600)
#' @export
sas_f_minute <- function(x) ifelse(!is.na(x) & x == 0, x, floor((x %% 3600) / 60)) # MINUTE(-0) is -0
#' @export
sas_f_second <- function(x) ifelse(!is.na(x) & x == 0, x, x %% 60) # SECOND(-0) is -0
#' @export
sas_f_hms <- function(h, m, s) h * 3600 + m * 60 + s

# LAG ---------------------------------------------------------------------

#' LAG queue for row-wise (L3) code; one queue per call site
#' @export
#' @keywords internal
sas_lag <- function(state, key, value, n = 1L) {
  q <- state[[key]]
  if (is.null(q)) q <- rep(list(if (is.character(value)) "" else NA_real_), n)
  state[[key]] <- c(q[-1L], list(value))
  q[[1L]]
}

#' Vectorised LAG for unconditional calls (L2)
#' @export
#' @keywords internal
sas_lag_vec <- function(x, n = 1L) {
  dplyr::lag(x, n, default = if (is.character(x)) "" else NA_real_)
}

# Formats -----------------------------------------------------------------

strip_zeros <- function(s) ifelse(grepl(".", s, fixed = TRUE), sub("\\.?0+$", "", s), s)

# 15 digits of v (a leading "0" counts as a digit): the first 15 digits of
# the display value fl(|v| * (1 + 2^-52)), truncated (just below the next
# 15-digit decimal, the digits round up exactly when the display value reaches
# it).
best_digits15 <- function(v, w) {
  a <- abs(v)
  intd <- if (a < 1) 1L else nchar(sprintf("%.0f", floor(a)))
  dd <- max(15L - intd, 0L)
  s <- sprintf("%.*f", dd + 25L, display_value(a))
  dot <- regexpr(".", s, fixed = TRUE)
  keep <- paste0(substr(s, 1L, dot - 1L), substr(s, dot + 1L, dot + dd))
  r <- as.numeric(paste0(substr(keep, 1L, nchar(keep) - dd), ".", substring(keep, nchar(keep) - dd + 1L)))
  if (v < 0) -r else r
}

#' BESTw. format (right-aligned to width w)
#' @param x Numeric vector.
#' @param w Width.
#' @keywords internal
sas_fmt_best <- function(x, w = 12L) {
  w <- vctrs::vec_cast(w %||% 12L, integer())
  code <- sas_miss_code(x)
  out <- character(length(x))
  sig <- function(s) {
    d <- gsub("[^0-9]", "", sub("E.*$", "", s))
    nchar(sub("0+$", "", sub("^0+", "", d)))
  }
  for (i in seq_along(x)) {
    if (!is.na(code[i])) {
      out[i] <- miss_label(code[i])
      next
    }
    v <- x[i]
    if (v == 0) {
      # -0 is written as 0
      out[i] <- "0"
      next
    }
    if ((w <= 5L && abs(v) < 1) || (w <= 7L && abs(v) < 1e-3)) {
      abort_unsupported(paste0("BEST", w, ". for a value below 1 in magnitude (", format(v, digits = 17),
                               "): the narrow-width rules are not supported"))
    }
    if (w > 16 && v != trunc(v) && abs(v) >= 1e-5) {
      # at most 15 digits are written (a leading 0 counts), truncating unless
      # the value lies within the display fuzz below the next digit
      v <- best_digits15(v, w)
    }
    s <- NA_character_
    if (v == trunc(v) && abs(v) < 10^w) {
      s <- sprintf("%.0f", v)
      if (nchar(s) > w) s <- NA_character_
    }
    # at most 15 significant digits are shown
    dmax <- if (v == 0) w - 1L else max(0L, 15L - as.integer(floor(log10(abs(v)))) - 1L)
    if (is.na(s)) {
      for (d in seq(min(w - 1L, dmax), 0L)) {
        # skip widths that cannot fit before rounding (rounding adds at most one digit)
        if (nchar(sprintf("%.0f", trunc(abs(v)))) + (d > 0) + d + (v < 0) > w) next
        # above 15 columns a value below 1E-5 is written in E notation (below)
        if (w > 15L && abs(v) < 1e-5) break
        if (w > 16L) {
          # above 16 columns v already holds the 15 digits (best_digits15):
          # written as they are; dropping some of them is not supported
          cand <- display_fixed(v, d, exact = TRUE)
          if (as.numeric(cand) != v) {
            abort_unsupported(paste0("BEST", w, ". of ", format(v, digits = 17), ": fewer than its 15 digits in fixed ",
                                     "notation is not supported"))
          }
          cand <- strip_zeros(cand)
        } else {
          # the one display engine (half away from zero on the display value)
          cand <- strip_zeros(display_fixed(v, d))
        }
        if (nchar(cand) <= w) {
          if (cand %in% c("0", "-0") && v != 0) break
          s <- cand
          break
        }
      }
    }
    sci <- NA_character_
    if (is.na(s) && abs(v) >= 1 && w <= 15L) {
      # too large for fixed notation: the E form of w.d (integer mantissa allowed)
      sci <- sci_fit(v, w)
      # w.d writes an integer mantissa ("25E5"); for BEST6. of 2500000 the
      # choice between that and "2.5E6" is not supported
      if (!is.na(sci) && grepl("^-?[0-9]{2,}E", sci)) {
        abort_unsupported(paste0("BEST", w, ". of ", format(v, digits = 17), " in E notation with an integer mantissa (",
                                 sci, "): not supported"))
      }
    } else if ((is.na(s) || abs(v) < 1e-5) && w > 15L) {
      # wide widths: as many mantissa decimals k as fit in w (at most 14
      # significant digits): k >= 13 the 14-digit rule (e14_digits, k passed
      # on), k <= 12 sci_dec_text; a text that still does not fit fails loud
      ex0 <- as.integer(sub("^.*e", "", sprintf("%.13e", abs(v))))
      k <- w - (v < 0) - 3L - (ex0 < 0) - nchar(as.character(abs(ex0)))
      # BEST16.-BEST19. and k = 13: the choice of k and the 14th-digit carry
      # are not supported (fail loud), except for mantissas of at most 4
      # significant digits (BEST16. of -1E15, 1E20, 1.234E-7, 1E-10), where
      # every k >= 3 and every carry rule give the same text
      if ((w <= 19L || k == 13L) && nchar(sub("0+$", "", gsub("[^0-9]", "", sub("e.*$", "", sprintf("%.14e", abs(v)))))) > 4L) {
        abort_unsupported(paste0("BEST", w, ". of ", format(v, digits = 17), " in E notation",
                                 if (w <= 19L) " at widths 16-19" else " with 13 mantissa decimals",
                                 ": not supported"))
      }
      if (k >= 13L) {
        g <- e14_digits(NULL, v, k)
        m <- g$keep
        m <- sub("0+$", "", m)
        m <- if (nchar(m) > 1L) paste0(substr(m, 1L, 1L), ".", substring(m, 2L)) else m
        sci <- paste0(if (v < 0) "-", m, "E", if (g$ex < 0) "-", abs(g$ex))
      } else {
        sci <- sci_dec_text(v, k)$text
      }
      if (nchar(sci) > w) {
        abort_unsupported(paste0("BEST", w, ". of ", format(v, digits = 17), ": the E form ", sci,
                                 " does not fit (not supported)"))
      }
    } else if (is.na(s) || (v != 0 && abs(v) < 1e-5)) {
      # the E form of w.d (sci_fit): as many mantissa decimals as fit, or an
      # integer mantissa when fewer than three digits fit and it shows more
      sci <- sci_fit(v, w)
      if (!is.na(sci) && abs(v) < 1) {
        # below 1: when the decimal mantissa has room for fewer than three
        # digits (trailing zeros counted), an integer mantissa with a negative
        # exponent that shows more digits, or as many in fewer columns, is
        # written instead (-102E-14, -16E-13); the rule holds for
        # BEST8.-BEST12. and is applied at every width that reaches here
        kfit <- -1L
        for (k in seq(min(w - 2L, 12L), 0L)) {
          e <- sci_dec_text(v, k)
          if ((v < 0) + (k + 1L) + (k > 0) + 1L + nchar(as.character(e$ex)) <= w) {
            kfit <- k
            break
          }
        }
        alt <- NA_character_
        if (kfit >= 0L && kfit + 1L < 3L) {
          for (nd in 2:min(13L, w)) {
            e <- sci_dec_text(v, nd - 1L)
            digits <- gsub(".", "", e$mant, fixed = TRUE)
            d2 <- sub("0+$", "", digits)
            t <- paste0(if (v < 0) "-", d2, "E", e$ex - (nchar(d2) - 1L))
            if (nchar(t) > w || nchar(d2) < sig(sci)) next
            if (nchar(d2) > sig(sci) || nchar(t) < nchar(sci)) {
              if (is.na(alt) || sig(t) > sig(alt) || (sig(t) == sig(alt) && nchar(t) < nchar(alt))) alt <- t
            }
          }
        }
        if (!is.na(alt)) sci <- alt
      }
    }
    # tiny values: E notation when it shows at least as many significant digits
    # (BEST12. of 1E-6 is 1E-6; below 1E-5 the E form wins a tie at every
    # width; at widths above 15 a value below 1E-5 is written in E notation)
    if (!is.na(sci) && (is.na(s) || sig(sci) > sig(s) || (abs(v) < 1e-5 && sig(sci) >= sig(s)) ||
                        (w > 15L && abs(v) < 1e-5))) s <- sci
    out[i] <- if (is.na(s)) strrep("*", w) else s
  }
  formatC(out, width = w)
}

# PUT ---------------------------------------------------------------------

#' Text written by PUT for one value (list or named output)
#' @export
#' @keywords internal
sas_put_value <- function(value, fmt = NULL) {
  # list and named output write character values without leading blanks
  if (is.character(value)) return(strip_both(sas_format(value, fmt)))
  trimws(sas_format(value, fmt))
}
#' @export
sas_put_named <- function(name, value, fmt = NULL) {
  if (length(value) == 0) return(character())
  out <- sas_put_value(value, fmt)
  # a blank character value is written as one blank ("r=  next=...")
  if (is.character(value)) out[!nzchar(out)] <- " "
  paste0(name, "=", out)
}

#' Assemble one PUT line per row
#' @export
#' @keywords internal
sas_put_line <- function(..., .lit = NULL, .ls = rt_linesize(), .file = FALSE, .line = NULL) {
  parts <- list(...)
  if (length(parts) == 0) return("")
  if (any(lengths(parts) == 0)) return(character())
  # List output writes a blank after each value; literals add none.
  lit <- .lit %||% rep(FALSE, length(parts))
  parts <- Map(function(p, l) if (l) p else paste0(p, " "), parts, lit)
  line <- rlang::exec(paste0, !!!unname(parts))
  if (any(nchar(line) > .ls)) {
    # the log line size is 132 (a FILE: its LRECL): an item that does not fit starts a new line
    line <- vapply(seq_along(line), function(i) {
      cur <- ""
      out <- character()
      for (p in parts) {
        t <- rep_len(p, length(line))[i]
        if (nchar(cur) > 0 && nchar(cur) + nchar(sas_rtrim(t)) > .ls) {
          out <- c(out, sas_rtrim(cur))
          cur <- ""
        }
        cur <- paste0(cur, t)
      }
      paste(c(out, sas_rtrim(cur)), collapse = "\n")
    }, "")
    # a FILE record still longer than LRECL (one item wider than the record) is
    # split across records by SAS; that is not implemented yet
    if (.file && any(nchar(sas_rtrim(unlist(strsplit(line, "\n", fixed = TRUE)))) > .ls)) {
      abort_unsupported_construct("PUT item longer than the FILE LRECL", .line)
    }
    # the log: a PUT of one item wider than LINESIZE is cut every LINESIZE
    # characters (measured for one item; after other items it is not)
    if (!.file && length(parts) == 1L) {
      line <- vapply(line, function(l) {
        l <- sas_rtrim(l)
        if (nchar(l) <= .ls) return(l)
        paste(substring(l, seq(1L, nchar(l), by = .ls), pmin(seq(1L, nchar(l), by = .ls) + .ls - 1L, nchar(l))), collapse = "\n")
      }, "", USE.NAMES = FALSE)
    }
  }
  sas_rtrim(line)
}

#' Write the PUT output of a step to an external file (FILE statement)
#'
#' Paths are relative to the working directory; `mod = TRUE` appends.
#' @export
#' @keywords internal
sas_file_write <- function(path, lines, mod = FALSE) {
  # every PUT line is a record, an empty one too (log_split_lines())
  lines <- log_split_lines(lines %||% character(), put = rep(TRUE, length(lines)))$lines
  # a file in a directory that does not exist cannot be opened (an ERROR)
  if (!dir.exists(dirname(path))) abort_runtime(paste0("Physical file does not exist, ", path))
  con <- file(path, open = if (mod) "ab" else "wb")
  on.exit(close(con))
  if (length(lines)) writeLines(lines, con, useBytes = TRUE)
  invisible()
}

#' Interleave several guarded PUT statements row by row (L1/L2)
#' @export
#' @keywords internal
sas_put_rows <- function(lines, guards, n = NULL) {
  n <- n %||% max(lengths(lines), lengths(guards), 0L)
  if (n == 0) return(character())
  # a line is computed once (literals only) or per row
  if (!all(lengths(lines) %in% c(1L, n)) || !all(lengths(guards) %in% c(1L, n))) {
    rlang::abort("sas_put_rows(): a PUT line or guard does not have one value per row.", .internal = TRUE)
  }
  lines <- lapply(lines, rep_len, n)
  guards <- lapply(guards, function(g) rep_len(sas_true(g), n))
  txt <- vctrs::vec_interleave(!!!unname(lines), .ptype = character())
  keep <- vctrs::vec_interleave(!!!unname(guards), .ptype = logical())
  out <- as.vector(txt)[as.vector(keep)]
  # NA: a PUT with pointer controls that writes no line (sas_put_layout())
  out[!is.na(out)]
}

# Data sets ---------------------------------------------------------------

#' A member of a read-only library (LIBNAME) as a data frame
#'
#' The member was read when the step was analysed (library.R).
#' @export
#' @keywords internal
sas_lib_member <- function(name) {
  d <- .active$session$lib_members[[name]]$data
  if (is.null(d)) abort_runtime(paste0("data set ", toupper(name), " is not read"))
  d
}

#' Fetch a WORK data set and apply KEEP=/DROP=/RENAME=/FIRSTOBS=/OBS=
#'
#' `work` is the WORK library (a list) or the data set itself (a data frame,
#' as in the tidy form of a script; `name` is then not used).
#' @export
#' @keywords internal
sas_ds <- function(work, name, keep = NULL, drop = NULL, rename = NULL, firstobs = NULL, obs = NULL) {
  d <- if (is.data.frame(work)) work else work[[name]]
  # a member of a read-only library (library.R)
  if (is.null(d) && grepl(".", name, fixed = TRUE)) d <- sas_lib_member(name)
  if (is.null(d)) abort_runtime(paste0("data set WORK.", toupper(name), " does not exist"))
  if (!is.null(keep)) d <- d[intersect(keep, names(d))]
  if (!is.null(drop)) d <- d[setdiff(names(d), drop)]
  if (!is.null(rename)) names(d)[match(names(rename), names(d))] <- unname(rename)
  if (!is.null(firstobs) || !is.null(obs)) {
    from <- firstobs %||% 1
    to <- min(obs %||% nrow(d), nrow(d))
    d <- d[seq_len(max(0, to - from + 1)) + from - 1, , drop = FALSE]
  }
  tibble::as_tibble(d)
}

sort_keys <- function(d, by) {
  keys <- list()
  for (v in by) {
    x <- d[[v]]
    if (is.character(x)) {
      keys <- c(keys, list(sas_rtrim(x)))
    } else {
      code <- sas_miss_code(x)
      keys <- c(keys, list(ifelse(is.na(code), 99L, code), ifelse(is.na(x), 0, x)))
    }
  }
  keys
}

sas_order <- function(d, by, descending = rep(FALSE, length(by))) {
  if (nrow(d) == 0) return(integer())
  keys <- sort_keys(d, by)
  dec <- vctrs::list_unchop(lapply(seq_along(by), function(k) rep(descending[k], if (is.character(d[[by[k]]])) 1L else 2L)), ptype = logical(), name_spec = rlang::zap())
  rlang::exec(order, !!!unname(keys), decreasing = dec, method = "radix")
}

#' Stop with a SAS-style error when a data set is not sorted by the BY variables
#' @export
#' @keywords internal
sas_check_sorted <- function(d, by, descending = rep(FALSE, length(by))) {
  missing_vars <- setdiff(by, names(d))
  if (length(missing_vars) > 0) abort_runtime(paste0("BY variable ", missing_vars[1], " not found"))
  if (nrow(d) > 1) {
    res <- integer(nrow(d) - 1L)
    for (k in seq_along(by)) {
      x <- d[[by[k]]]
      ck <- sas_cmp(x[-nrow(d)], x[-1L])
      if (descending[k]) ck <- -ck
      res <- ifelse(res == 0L, ck, res)
    }
    if (any(res > 0L)) abort_runtime(paste("data set is not sorted by", paste(by, collapse = " ")))
  }
  d
}

same_value <- function(a, b) {
  if (is.character(a)) return(sas_rtrim(a) == sas_rtrim(b))
  sas_cmp(a, b) == 0L
}

key_change <- function(...) {
  keys <- list(...)
  n <- length(keys[[1]])
  chg <- rep(FALSE, n)
  if (n == 0) return(chg)
  chg[1] <- TRUE
  if (n > 1) for (v in keys) chg[-1] <- chg[-1] | !same_value(v[-1], v[-n])
  chg
}

#' FIRST.x / LAST.x flags from BY variables (pass the BY variables up to x)
#' @export
#' @keywords internal
sas_first <- function(...) as.numeric(key_change(...))
#' @export
sas_last <- function(...) {
  chg <- key_change(...)
  as.numeric(c(chg[-1L], length(chg) > 0))[seq_along(chg)]
}

#' Combine the data sets of a SET statement (concatenate, or interleave with BY)
#' @export
#' @keywords internal
sas_set_input <- function(dfs, in_vars = NULL, by = NULL, descending = NULL) {
  for (j in seq_along(dfs)) dfs[[j]][[".src"]] <- rep(j, nrow(dfs[[j]]))
  d <- dplyr::bind_rows(dfs)
  if (!is.null(in_vars)) {
    for (j in seq_along(in_vars)) {
      if (!is.na(in_vars[j])) d[[in_vars[j]]] <- as.numeric(d$.src == j)
    }
  }
  if (!is.null(by) && length(dfs) > 1) {
    for (df in dfs) sas_check_sorted(df, by, descending %||% rep(FALSE, length(by)))
    d <- d[sas_order(d, c(by, ".src"), c(descending %||% rep(FALSE, length(by)), FALSE)), ]
  }
  d$.src <- NULL
  pad_char_columns(d)
}

pad_char_columns <- function(d) {
  for (v in names(d)) {
    if (is.character(d[[v]]) && nrow(d) > 0) {
      len <- max(1L, chr_bytes(d[[v]]), na.rm = TRUE)
      d[[v]] <- sas_char(d[[v]], len)
    }
  }
  d
}

#' Rows of a data frame as a list of named lists (for the L3 PDV loop)
#' @export
#' @keywords internal
sas_rows <- function(d) {
  cols <- as.list(d)
  lapply(seq_len(nrow(d)), function(i) lapply(cols, `[[`, i))
}

#' Copy one input row into the PDV
#' @export
#' @keywords internal
sas_read_row <- function(pdv, rows, i) {
  r <- rows[[i]]
  pdv[names(r)] <- r
  pdv
}

#' Build the observation plan of a match-merge (MERGE + BY)
#'
#' A BY group in which more than one data set has several rows queues the
#' log NOTE about repeats of BY values.  Repeats in several data sets that
#' never share a BY group write no NOTE.
#' @export
#' @keywords internal
sas_merge_plan <- function(dfs, by, descending = rep(FALSE, length(by)), in_vars = NULL) {
  for (df in dfs) sas_check_sorted(df, by, descending)
  keys <- dplyr::bind_rows(lapply(seq_along(dfs), function(j) {
    k <- dfs[[j]][by]
    k$.src <- rep(j, nrow(k))
    k$.row <- seq_len(nrow(k))
    k
  }))
  keys <- keys[sas_order(keys, c(by, ".src", ".row"), c(descending, FALSE, FALSE)), ]
  grp <- cumsum(rlang::exec(key_change, !!!unname(as.list(keys[by]))))
  plan <- list()
  rep_same_group <- FALSE
  for (g in unique(grp)) {
    sub <- keys[grp == g, ]
    rows_by_src <- lapply(seq_along(dfs), function(j) sub$.row[sub$.src == j])
    if (sum(lengths(rows_by_src) > 1L) > 1L) rep_same_group <- TRUE
    m <- max(lengths(rows_by_src))
    for (r in seq_len(m)) {
      idx <- vapply(rows_by_src, function(x) if (r <= length(x)) x[r] else NA_integer_, integer(1))
      plan[[length(plan) + 1L]] <- c(
        list(.new = r == 1L),
        stats::setNames(as.list(idx), paste0(".r", seq_along(dfs))),
        stats::setNames(lapply(rows_by_src, function(x) as.numeric(length(x) > 0)), paste0(".in", seq_along(dfs))),
        as.list(sub[1, by])
      )
    }
  }
  if (rep_same_group) {
    rt <- rt_diag()
    rt$merge_repeats <- TRUE
  }
  plan <- dplyr::bind_rows(plan)
  for (k in seq_along(by)) {
    keyset <- unname(as.list(plan[by[seq_len(k)]]))
    plan[[paste0("first.", by[k])]] <- rlang::exec(sas_first, !!!keyset)
    plan[[paste0("last.", by[k])]] <- rlang::exec(sas_last, !!!keyset)
  }
  plan
}

#' Execute one MERGE read against the PDV
#' @export
#' @keywords internal
sas_merge_read <- function(pdv, rows, plan, p, reset, in_vars, by) {
  if (plan$.new[p]) pdv[names(reset)] <- reset
  for (j in seq_along(rows)) {
    r <- plan[[paste0(".r", j)]][p]
    if (!is.na(r)) {
      row <- rows[[j]][[r]]
      pdv[names(row)] <- row
    }
    if (!is.na(in_vars[j])) pdv[[in_vars[j]]] <- plan[[paste0(".in", j)]][p]
  }
  for (v in by) {
    pdv[[paste0("first.", v)]] <- plan[[paste0("first.", v)]][p]
    pdv[[paste0("last.", v)]] <- plan[[paste0("last.", v)]][p]
  }
  pdv
}

#' The input table of a match-merge (MERGE + BY): one row per row of the plan
#'
#' One row per row of [sas_merge_plan()], with the values the program data
#' vector holds after the MERGE read of that row: at the first row of a BY
#' group every variable starts missing; each data set that has a row writes
#' its variables in MERGE order (a later data set wins); a data set that has
#' run out of rows in the group writes nothing, so its variables keep their
#' value from the previous row of the group.  IN= variables and FIRST./LAST.
#' flags come from the plan.  Only valid when the step does not change a
#' variable read by MERGE (the row-wise path carries the changed value).
#' @export
#' @keywords internal
sas_merge_input <- function(dfs, by, descending = rep(FALSE, length(by)), in_vars = NULL) {
  plan <- sas_merge_plan(dfs, by, descending)
  vars <- unique(vctrs::list_unchop(lapply(dfs, names), ptype = character(), name_spec = rlang::zap()))
  n <- nrow(plan)
  pos <- seq_len(n)
  # the plan row that starts the BY group of each row
  gstart <- if (n) cummax(ifelse(plan$.new, pos, 0L)) else integer()
  out <- list()
  for (v in vars) {
    js <- which(vapply(dfs, function(d) v %in% names(d), NA))
    cols <- lapply(dfs[js], `[[`, v)
    vals <- vctrs::vec_c(!!!lapply(cols, function(x) { attributes(x) <- NULL; x }))
    offs <- cumsum(c(0L, lengths(cols)))[seq_along(js)]
    # the row (in `vals`) a data set writes on each plan row; later data sets win
    src <- rep(NA_integer_, n)
    for (k in seq_along(js)) {
      r <- plan[[paste0(".r", js[k])]]
      hit <- !is.na(r)
      src[hit] <- offs[k] + r[hit]
    }
    # the last row of the group so far on which some data set wrote `v`
    w <- if (n) cummax(ifelse(is.na(src), 0L, pos)) else integer()
    idx <- rep(NA_integer_, n)
    take <- w > 0L & w >= gstart
    idx[take] <- src[w[take]]
    col <- vals[idx]
    a <- attributes(cols[[1]])
    if (length(a)) attributes(col) <- a[setdiff(names(a), "names")]
    out[[v]] <- col
  }
  d <- tibble::as_tibble(out, .rows = n)
  for (j in seq_along(in_vars)) {
    if (!is.na(in_vars[j])) d[[in_vars[j]]] <- if (n) plan[[paste0(".in", j)]] else numeric()
  }
  for (v in by) {
    for (f in paste0(c("first.", "last."), v)) d[[f]] <- if (n) plan[[f]] else numeric()
  }
  pad_char_columns(d)
}

#' Collect PDV rows written by OUTPUT into a tibble
#' @export
#' @keywords internal
sas_rows_to_tibble <- function(rows, proto) {
  cols <- lapply(names(proto), function(v) {
    vapply(rows, function(r) r[[v]], FUN.VALUE = proto[[v]], USE.NAMES = FALSE)
  })
  names(cols) <- names(proto)
  tibble::as_tibble(cols)
}

# INPUT / DATALINES -------------------------------------------------------

#' Read in-stream data lines according to an INPUT specification
#' @keywords internal
sas_read_datalines <- function(lines, spec) {
  if (any(chr_wide(lines))) abort_unsupported("INPUT from records with bytes outside ASCII")
  cols <- lapply(spec, function(s) if (s$char) character() else numeric())
  convert <- function(txt, s) {
    if (s$char) {
      if (s$mode != "formatted" && txt == ".") txt <- ""
      if (s$mode == "formatted" && s$fmt$name != "char") txt <- sub("^ +", "", txt)
      return(sas_char(txt, s$len))
    }
    txt <- trimws(txt)
    if (txt %in% c("", ".")) return(NA_real_)
    v <- suppressWarnings(as.numeric(txt))
    d <- if (is.null(s$fmt)) NA_integer_ else s$fmt$d
    if (!is.na(v) && !is.na(d) && d > 0 && !grepl(".", txt, fixed = TRUE)) v <- v / 10^d
    v
  }
  li <- 1L
  nl <- length(lines)
  while (li <= nl) {
    line <- lines[li]
    pos <- 1L
    rec <- vector("list", length(spec))
    ok <- TRUE
    for (k in seq_along(spec)) {
      s <- spec[[k]]
      if (s$mode == "formatted") {
        w <- s$fmt$w
        padded <- paste0(line, strrep(" ", max(0L, pos + w - 1L - nchar(line))))
        rec[[k]] <- convert(substr(padded, pos, pos + w - 1L), s)
        pos <- pos + w
        next
      }
      repeat {
        while (pos <= nchar(line) && substr(line, pos, pos) %in% c(" ", "\t")) pos <- pos + 1L
        if (pos <= nchar(line)) break
        li <- li + 1L # FLOWOVER: continue on the next line
        if (li > nl) {
          ok <- FALSE
          break
        }
        line <- lines[li]
        pos <- 1L
      }
      if (!ok) break
      end <- pos
      while (end <= nchar(line) && !substr(line, end, end) %in% c(" ", "\t")) end <- end + 1L
      rec[[k]] <- convert(substr(line, pos, end - 1L), s)
      pos <- end
    }
    if (!ok) break
    for (k in seq_along(spec)) cols[[k]] <- c(cols[[k]], rec[[k]])
    li <- li + 1L
  }
  names(cols) <- vapply(spec, `[[`, character(1), "name")
  tibble::as_tibble(cols)
}

# Macro variables written by the DATA step -------------------------------

#' CALL SYMPUT / SYMPUTX
#' @export
#' @keywords internal
sas_symput <- function(symtab, name, value, trim = TRUE, scope = NULL) {
  name <- strip_both(name)
  if (is.numeric(value)) {
    # SYMPUTX: integers keep every digit, other values BEST12.; SYMPUT: BEST12.
    value <- if (trim) {
      ifelse(!is.na(value) & value == trunc(value) & abs(value) < 1e15, sprintf("%.0f", value), sas_fmt_best(value, 12L))
    } else {
      sas_fmt_best(value, 12L)
    }
  }
  if (trim) value <- strip_both(value)
  if (!is.null(scope)) scope <- strip_both(scope)
  for (i in seq_along(name)) {
    if (inherits(symtab, "sasonr_macro_session")) {
      mv_symput(symtab, name[i], value[i], scope[i])
    } else {
      assign(tolower(name[i]), value[i], envir = symtab)
    }
  }
  invisible(NULL)
}

#' CALL EXECUTE
#'
#' Runs the macro code of its argument now (`%LET`, `%PUT`, macro calls) and
#' queues the text that results; the engine runs the queue after the DATA
#' step ends (engine_call_execute_drain()).  Returns the `%PUT` lines, which
#' go to the step's log buffer between its PUT lines.
#' @export
#' @keywords internal
sas_call_execute <- function(symtab, value, .line = NULL) {
  if (!length(.line) || is.na(.line[1])) .line <- NULL
  if (!inherits(symtab, "sasonr_macro_session")) abort_unsupported("CALL EXECUTE outside sas_run()", .line)
  if (!is.character(value)) abort_unsupported("CALL EXECUTE with a numeric argument (not supported)", .line)
  value <- sub(" +$", "", value)
  puts <- character()
  macro <- grepl("[%&]", value)
  if (macro) {
    if (length(symtab$frames)) abort_unsupported("CALL EXECUTE with macro code in a step that a macro generated (not supported)", .line)
    rt <- rt_diag()
    old <- list(put = symtab$on_put, warn = rt$on_warn, error = rt$on_error, note = rt$on_note)
    on.exit({
      symtab$on_put <- old$put
      rt$on_warn <- old$warn
      rt$on_error <- old$error
      rt$on_note <- old$note
    }, add = TRUE)
    symtab$on_put <- function(line) puts <<- c(puts, line)
    msg <- function(m, ...) abort_unsupported(paste0("a macro message from the CALL EXECUTE argument (", m, ")"), .line)
    rt$on_warn <- msg
    rt$on_error <- msg
    rt$on_note <- msg
    value <- withCallingHandlers(mp_scan_text(mp_src(value), symtab, mode = "code")$text,
                                 sasonr_macro_warning = function(w) msg(conditionMessage(w)))
    # the text the macro code generates starts at its first non-blank
    value <- sub("^[ \n\t]+", "", sub("[ \n\t]+$", "", value))
    if (grepl("\n", value)) abort_unsupported("CALL EXECUTE with macro code that generates more than one line (not supported)", .line)
  } else if (!nzchar(value)) {
    abort_unsupported("CALL EXECUTE with a blank argument (not supported)", .line)
  }
  symtab$ce_queue <- c(symtab$ce_queue, value)
  puts
}

# Control flow signals for L3 code ----------------------------------------

#' @export
sas_delete <- function() {
  rlang::signal(message = "delete", class = "sasonr_delete")
  abort_runtime("DELETE signalled outside a DATA step loop")
}
#' @export
sas_return <- function() {
  rlang::signal(message = "return", class = "sasonr_return")
  abort_runtime("RETURN signalled outside a DATA step loop")
}
# An iteration of a step with a reading statement ended (normally or by
# DELETE) without executing one.  After its observation, if any, is written
# the step stops with the looping NOTE.  `why`: the step has a shape whose
# behaviour here is not known (it fails loud).
#' @export
sas_loop_unread <- function(why = NULL) {
  if (!is.null(why)) abort_unsupported(why)
  rt <- rt_diag()
  rt$looping <- TRUE
  invisible()
}

#' @export
sas_stop_step <- function(stmt = FALSE) {
  # `stmt`: a STOP statement of the program (not the end of the input)
  if (stmt) {
    rt <- rt_diag()
    rt$stop_stmt <- TRUE
  }
  rlang::signal(message = "stop", class = "sasonr_stop")
  abort_runtime("STOP signalled outside a DATA step loop")
}
#' @export
sas_select_error <- function(line) abort_runtime("no WHEN clause matched and there is no OTHERWISE", line)

#' DO loop test for an iterative DO
#' @export
#' @keywords internal
sas_do_test <- function(i, to, by) {
  if (is.na(by) || by == 0) abort_runtime("invalid DO loop BY value")
  if (is.na(i) || is.na(to)) abort_runtime("invalid DO loop control value")
  if (by > 0) i <= to else i >= to
}

#' Running total for a vectorised sum statement (`n + x;`)
#' @export
#' @keywords internal
sas_cumsum <- function(x, init, n) {
  x <- rep_len(dplyr::coalesce(as.numeric(x), 0), n)
  cumsum(c(init, x))[-1L]
}

#' Forward fill for a retained variable (vectorised carry-forward)
#'
#' Row r takes the value of the last assignment k whose flag `sets[[k]]` is
#' true on that row, otherwise the result of row r - 1 (or `init`).
#' @export
#' @keywords internal
sas_carry <- function(values, sets, init) {
  n <- length(sets[[1]])
  src <- integer(n)
  for (k in seq_along(sets)) src[sets[[k]] %in% TRUE] <- k
  cur <- rep(init, length.out = n)
  for (k in seq_along(values)) {
    on <- src == k
    cur[on] <- rep_len(values[[k]], n)[on]
  }
  pos <- cummax(ifelse(src > 0L, seq_len(n), 0L))
  out <- rep(init, length.out = n)
  out[pos > 0L] <- cur[pos[pos > 0L]]
  out
}

#' DIF for row-wise code
#' @export
#' @keywords internal
sas_dif <- function(state, key, value, n = 1L) value - sas_lag(state, key, value, n)

#' The element of a multi-dimensional array: subscripts `i` of dimensions
#' with lower bounds `lo` and sizes `dims` (row-major)
#' @export
#' @keywords internal
sas_idx_md <- function(i, lo, dims) {
  k <- 0
  for (j in seq_along(dims)) {
    ij <- sas_idx(i[j], lo[j], dims[j]) - 1L
    k <- k * dims[j] + ij
  }
  vctrs::vec_cast(k + 1, integer())
}

#' Validate an array subscript and convert it to a 1-based position
#' @export
#' @keywords internal
sas_idx <- function(i, lo, dim) {
  k <- i - lo + 1
  if (length(k) != 1 || is.na(k) || k != trunc(k) || k < 1 || k > dim) {
    abort_runtime("array subscript out of range")
  }
  vctrs::vec_cast(k, integer())
}

#' Normalise an arithmetic result: special missing values and overflow
#' (infinite results) become the ordinary missing value.
#' @export
#' @keywords internal
sas_num <- function(x, .at = NULL) {
  inf <- is.infinite(x)
  if (any(inf)) note_math(.at, sum(inf))
  x[is.na(x) | inf] <- NA_real_
  x
}

#' Truncate character columns to PDV lengths (later data sets of SET/MERGE)
#' @export
#' @keywords internal
sas_trunc_chars <- function(d, lens) {
  for (v in names(lens)) if (v %in% names(d)) d[[v]] <- sas_char(d[[v]], lens[[v]])
  d
}

#' Formatted PUT output (`put x 8.2;`): the field keeps its alignment blanks
#' @export
#' @keywords internal
sas_put_fvalue <- function(value, fmt) sas_format(value, fmt)

#' PUT with column/line pointer controls (`@n`, `+n`, `/`)
#'
#' Pieces are written into a line buffer at the current column (overwriting);
#' list and named output leave one blank after the value. `/` starts a new line.
#' `.ls`: the line size (the log's LINESIZE, a FILE's LRECL; `.file`); `.guard`:
#' the rows where the PUT executes (vectorised steps): the result then has one
#' element per row, NA where no line is written.
#' @export
#' @keywords internal
sas_put_layout <- function(pieces, .ls = rt_linesize(), .line = NULL, .file = FALSE, .guard = NULL) {
  n <- max(1L, unlist(lapply(pieces, function(p) if (p$k == "text") length(p$s) else if (!is.null(p$v)) length(p$v) else 1L)), length(.guard))
  if (any(vapply(pieces, function(p) p$k == "text" && length(p$s) == 0, logical(1)))) return(character())
  run <- if (is.null(.guard)) rep(TRUE, n) else rep_len(sas_true(.guard), n)
  has_next <- any(vapply(pieces, function(p) p$k == "next", logical(1)))
  row <- function(i) {
    lines <- character()
    buf <- character()
    col <- 1L
    for (j in seq_along(pieces)) {
      p <- pieces[[j]]
      if (p$k == "next") {
        lines <- c(lines, sas_rtrim(paste(buf, collapse = "")))
        buf <- character()
        col <- 1L
        next
      }
      if (p$k %in% c("col", "rel")) {
        v <- rep_len(p$v, n)[i]
        if (is.na(v)) abort_runtime("invalid PUT pointer value")
        nc <- if (p$k == "col") trunc(v) else col + trunc(v)
        if (nc > 32767) {
          # a PUT to the log that starts with a pointer beyond 32767 writes no
          # line; other forms (a FILE, text before it, a `/`) are not implemented yet
          if (!.file && j == 1L && p$k == "col" && !has_next) return(NA_character_)
          abort_unsupported_construct("PUT pointer beyond the line size", .line)
        }
        # what a pointer beyond the line size writes is not implemented yet
        if (nc > .ls) abort_unsupported_construct("PUT pointer beyond the line size", .line)
        col <- max(1L, as.integer(nc))
        next
      }
      t <- rep_len(p$s, n)[i]
      ch <- strsplit(t, "")[[1]]
      if (length(buf) < col - 1L) buf <- c(buf, rep(" ", col - 1L - length(buf)))
      if (length(ch) > 0) buf[col:(col + length(ch) - 1L)] <- ch
      col <- col + length(ch)
      if (isTRUE(p$blank)) {
        if (length(buf) < col) buf[col] <- " "
        col <- col + 1L
      }
    }
    buf[is.na(buf)] <- " "
    out <- c(lines, sas_rtrim(paste(buf, collapse = "")))
    # a pointer line longer than the line size is not implemented yet (sas_put_line() wraps list output)
    if (any(nchar(out) > .ls)) abort_unsupported_construct("PUT with pointer controls writing past the line size", .line)
    paste(out, collapse = "\n")
  }
  out <- rep(NA_character_, n)
  for (i in which(run)) out[i] <- row(i)
  if (is.null(.guard)) out[!is.na(out)] else out
}

# The state of an L3 DATA step (row by row): the PDV prototype, the values
# that each iteration resets, the observations written to each output data
# set and the error-dump settings. The generated loop calls the helpers
# below, so it shows only the step's own statements.

#' Start the state of a row-by-row DATA step
#'
#' `pdv`: the initial program data vector; `reset`: the variables set to
#' their initial value at the start of each iteration; `outputs`: the
#' variables written to each output data set; `dump`: the error dump spec
#' ([sas_dump_spec()]); `readers`: the reader objects of its SET, MERGE and
#' INPUT statements.
#' @export
#' @keywords internal
sas_step_state <- function(pdv, reset = character(), outputs = list(), dump = NULL, readers = list()) {
  st <- new.env(parent = emptyenv())
  st$readers <- readers
  st$proto <- pdv
  st$reset <- pdv[reset]
  st$vars <- outputs
  for (k in seq_along(outputs)) st[[paste0("out", k)]] <- list()
  st$n <- 0L
  st$dump <- dump
  st
}

#' Begin an iteration: reset values, `_N_` and `_ERROR_`
#' @export
#' @keywords internal
sas_iter_begin <- function(st, pdv) {
  st$n <- st$n + 1L
  for (r in st$readers) r$read <- FALSE
  if (length(st$reset)) pdv[names(st$reset)] <- st$reset
  pdv$`_n_` <- st$n
  pdv$`_error_` <- 0
  pdv
}

#' Run the statements of an iteration
#'
#' `expr` is evaluated in the caller's environment (where the PDV is). The
#' result is how the iteration ended: "ok" (also after RETURN), "delete" or
#' "stop".
#' @export
#' @keywords internal
sas_iter_run <- function(expr) {
  tryCatch({
    expr
    "ok"
  }, sasonr_delete = function(e) "delete", sasonr_stop = function(e) "stop", sasonr_return = function(e) "ok")
}

#' Write the PDV to output data sets `k` (OUTPUT)
#' @export
#' @keywords internal
sas_write_pdv <- function(st, pdv, k = seq_along(st$vars)) {
  for (j in k) {
    nm <- paste0("out", j)
    st[[nm]][[length(st[[nm]]) + 1L]] <- pdv[st$vars[[j]]]
  }
  invisible()
}

#' The observations written to output data set `k`, as a tibble
#' @export
#' @keywords internal
sas_step_output <- function(st, k) {
  sas_rows_to_tibble(st[[paste0("out", k)]], st$proto[st$vars[[k]]])
}

# Readers of a row-by-row step (spec 01 §4.3): one per SET or MERGE
# statement, an environment with the observations, the position and whether
# the reader ran in the current iteration (`read`; the INFILE stream of INPUT
# has the same flag).

#' A SET statement's reader over the observations of `d`
#'
#' `notes`: the read NOTE entries of its data sets (all of them by default);
#' `ahead`: the reader reads one observation ahead (SET with BY).
#' @export
#' @keywords internal
sas_set_reader <- function(d, notes = NULL, ahead = FALSE) {
  r <- new.env(parent = emptyenv())
  r$kind <- "set"
  r$rows <- sas_rows(d)
  r$n <- length(r$rows)
  r$p <- 0L
  r$notes <- notes
  r$ahead <- ahead
  r$read <- FALSE
  r
}

#' Execute a SET statement: the next observation into the PDV (the step
#' stops after the last one)
#' @export
#' @keywords internal
sas_set_next <- function(r, pdv) {
  r$p <- r$p + 1L
  if (r$p > r$n) sas_stop_step()
  pdv <- sas_read_row(pdv, r$rows, r$p)
  r$read <- TRUE
  pdv
}

#' Execute a SET statement with POINT=: observation `k` into the PDV
#'
#' An invalid observation number (missing, not an integer, out of range)
#' sets `_ERROR_` and leaves the PDV [measured]. A POINT= reader writes no
#' NOTE about the observations it read.
#' @export
#' @keywords internal
sas_set_point <- function(r, pdv, k) {
  r$point <- TRUE
  r$read <- TRUE
  if (length(k) != 1L || is.na(k) || k != trunc(k) || k < 1 || k > r$n) {
    rt_error()
    return(pdv)
  }
  sas_read_row(pdv, r$rows, as.integer(k))
}

#' A MERGE statement's reader: the match-merge plan of the data sets `dfs`
#'
#' `reset`: the values its variables take at the start of a BY group;
#' `check`: the line of a MERGE that may stop before its last observation
#' (its read counts are known only when it read all of them).
#' @export
#' @keywords internal
sas_merge_reader <- function(dfs, by, descending, reset, in_vars = rep(NA_character_, length(dfs)), check = NULL, notes = NULL) {
  r <- new.env(parent = emptyenv())
  r$kind <- "merge"
  r$rows <- lapply(dfs, sas_rows)
  # without BY: one-to-one, the n-th observation of each data set
  r$noby <- !length(by)
  r$vars <- lapply(dfs, function(d) setdiff(names(d), ".obs"))
  r$notes <- notes
  r$plan <- if (r$noby) merge_plan_noby(dfs) else sas_merge_plan(dfs, by, descending)
  r$n <- nrow(r$plan)
  r$p <- 0L
  r$by <- by
  r$reset <- reset
  r$in_vars <- in_vars
  r$check <- check
  r$read <- FALSE
  r
}

# The observation plan of a one-to-one MERGE (no BY): row p takes the p-th
# observation of each data set that has one.
merge_plan_noby <- function(dfs) {
  n <- vapply(dfs, nrow, 1L)
  m <- max(c(0L, n))
  p <- seq_len(m)
  plan <- list(.new = p == 1L)
  for (j in seq_along(dfs)) {
    plan[[paste0(".r", j)]] <- ifelse(p <= n[j], p, NA_integer_)
    plan[[paste0(".in", j)]] <- as.numeric(p <= n[j])
  }
  tibble::as_tibble(plan)
}

#' Execute a MERGE statement (`end`: its END= variable)
#' @export
#' @keywords internal
sas_merge_next <- function(r, pdv, end = NULL) {
  r$p <- r$p + 1L
  if (r$p > r$n) sas_stop_step()
  pdv <- sas_merge_read(pdv, r$rows, r$plan, r$p, r$reset, r$in_vars, r$by)
  if (r$noby) {
    # a data set whose observations are all read: its variables are missing
    gone <- which(is.na(unlist(r$plan[r$p, paste0(".r", seq_along(r$rows))])))
    for (j in gone) {
      shared <- intersect(r$vars[[j]], unlist(r$vars[-gone]))
      if (length(shared)) abort_unsupported(paste0("a MERGE without BY where the variable ", toupper(shared[1]), " of a data set whose observations are all read is also in another one"), r$check)
      pdv[r$vars[[j]]] <- r$reset[r$vars[[j]]]
    }
  }
  if (!is.null(end)) pdv[[end]] <- as.numeric(r$p == r$n)
  r$read <- TRUE
  pdv
}

#' Did a reader of the step run in this iteration?
#' @export
#' @keywords internal
sas_iter_read <- function(st) {
  any(vapply(st$readers, function(r) isTRUE(r$read), NA))
}

#' End a row-by-row step: the read NOTE counts of its readers
#' @export
#' @keywords internal
sas_step_end <- function(st) {
  for (r in st$readers) {
    if (identical(r$kind, "set")) sas_reads_consumed(if (isTRUE(r$point)) 0 else r$p, r$notes, r$ahead)
    # an UPDATE reads all its observations when the step reaches its end
    if (identical(r$kind, "update") && r$p < r$n) abort_unsupported("an UPDATE step that stops before its last observation (the counts of its read NOTEs)", r$line)
    if (identical(r$kind, "merge") && !is.null(r$check)) {
      # a one-to-one MERGE of one data set that stopped early has read one
      # observation ahead [measured]
      if (isTRUE(r$noby) && length(r$rows) == 1L && r$p < r$n) sas_reads_consumed(r$p, r$notes, ahead = TRUE)
      else sas_merge_consumed(r$p, r$n, r$check)
    }
  }
  invisible()
}

#' Does the step end after this iteration?
#'
#' After a STOP (or the end of a reader's input), after the one iteration of
#' a step without readers, and after an iteration that ran no reader (the
#' looping NOTE; `why`: the reason this case is not supported, if it is not).
#' @export
#' @keywords internal
sas_iter_done <- function(st, status, why = NULL) {
  if (identical(status, "stop") || !length(st$readers)) return(TRUE)
  if (sas_iter_read(st)) return(FALSE)
  sas_loop_unread(why)
  TRUE
}

#' An UPDATE statement's reader: the observations of the step
#'
#' Per BY group, the first master observation with the transaction
#' observations of the group applied in order, then the group's other master
#' observations unchanged; a BY group only in the transaction data set starts
#' from missing values [measured]. A missing value in a transaction does not
#' replace a value (`missing = TRUE`: UPDATEMODE=NOMISSINGCHECK, it does);
#' the special missing values .A-.Z do; `._` is not supported.
#' @export
#' @keywords internal
sas_update_reader <- function(dfs, by, descending, missing = FALSE, line = NULL) {
  plan <- sas_merge_plan(dfs, by, descending)
  rows_m <- sas_rows(dfs[[1]])
  rows_t <- sas_rows(dfs[[2]])
  vars <- setdiff(union(names(dfs[[1]]), names(dfs[[2]])), ".obs")
  proto <- lapply(stats::setNames(vars, vars), function(v) {
    x <- if (v %in% names(dfs[[1]])) dfs[[1]][[v]] else dfs[[2]][[v]]
    if (is.character(x)) strrep(" ", max(1L, nchar(x[1] %||% " ", "bytes"))) else NA_real_
  })
  apply_trans <- function(cur, t) {
    for (v in names(t)) {
      x <- t[[v]]
      if (!missing) {
        if (is.character(x) && !nzchar(trimws(x))) next
        if (is.numeric(x) && is.na(x)) {
          code <- sas_miss_code(x)
          if (identical(code, 1L)) next
          if (identical(code, 0L)) abort_unsupported("UPDATE with the special missing value ._ in a transaction", line)
        }
      }
      cur[[v]] <- x
    }
    cur
  }
  grp <- cumsum(plan$.new)
  out <- list()
  for (g in unique(grp)) {
    sub <- plan[grp == g, ]
    mi <- stats::na.omit(sub$.r1)
    ti <- stats::na.omit(sub$.r2)
    first <- if (length(mi)) utils::modifyList(proto, rows_m[[mi[1]]]) else proto
    for (j in ti) first <- apply_trans(first, rows_t[[j]])
    out[[length(out) + 1L]] <- first
    for (j in mi[-1]) out[[length(out) + 1L]] <- utils::modifyList(proto, rows_m[[j]])
  }
  r <- new.env(parent = emptyenv())
  r$kind <- "update"
  r$rows <- out
  r$n <- length(out)
  r$p <- 0L
  r$read <- FALSE
  r$line <- line
  r
}

#' The END= value of an UPDATE reader: its last observation was read
#' @export
#' @keywords internal
sas_update_at_end <- function(r) as.numeric(r$p == r$n)
