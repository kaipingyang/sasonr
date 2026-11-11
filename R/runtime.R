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
#' @export
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

#' @export
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
sas_char <- function(x, len) {
  if (is.numeric(x)) x <- sas_fmt_best(x, 12L)
  x[is.na(x)] <- ""
  x <- substr(x, 1L, len)
  paste0(x, strrep(" ", pmax(0L, len - nchar(x))))
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
#' @export
sas_cmp <- function(a, b) {
  if (is.character(a) || is.character(b)) {
    if (!(is.character(a) && is.character(b))) {
      abort_unsupported("comparison between character and numeric values")
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
sas_cmp_trunc <- function(op, a, b) {
  ab <- recycle2(a, b)
  len <- pmin(nchar(ab[[1]]), nchar(ab[[2]]))
  cmp_fun(op)(substr(ab[[1]], 1L, len), substr(ab[[2]], 1L, len))
}

#' Comparison chain `a < b < c`: every operand is evaluated once
#' @export
sas_chain <- function(args, ops) {
  res <- TRUE
  for (k in seq_along(ops)) {
    res <- res & sas_true(cmp_fun(ops[k])(args[[k]], args[[k + 1L]]))
  }
  as.numeric(res)
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
sas_true <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.character(x)) abort_unsupported("character value used as a condition")
  !is.na(x) & x != 0
}
#' @export
sas_not <- function(x) as.numeric(!sas_true(x))
#' @export
sas_and <- function(a, b) as.numeric(sas_true(a) & sas_true(b))
#' @export
sas_or <- function(a, b) as.numeric(sas_true(a) | sas_true(b))

#' @export
sas_div <- function(a, b, .at = NULL, .exact = TRUE) {
  if (anyNA(a) || anyNA(b)) sas_mv_count(.at, a, b, .exact = .exact)
  zero <- !is.na(b) & b == 0
  if (any(zero & !is.na(a))) rt_error(paste0("NOTE: Division by zero detected", at_text(.at), "."))
  bb <- rep_len(b, max(length(a), length(b)))
  bb[rep_len(zero, length(bb))] <- NA_real_
  out <- sas_num(a / bb, .at)
  out
}

#' @export
sas_pow <- function(a, b, .at = NULL, .exact = TRUE) {
  if (anyNA(a) || anyNA(b)) sas_mv_count(.at, a, b, .exact = .exact)
  out <- sas_num(a^b, .at)
  out[is.na(a) | is.na(b) | (a == 0 & b <= 0)] <- NA_real_ # 0**0 and 0**-n are missing in SAS
  out
}

#' MIN (><) and MAX (<>) operators
#' @export
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
  out <- rowSums(m, na.rm = TRUE)
  out[rowSums(!is.na(m)) == 0] <- NA_real_
  out
}
#' @export
sas_f_n <- function(...) as.numeric(rowSums(!is.na(args_matrix(list(...)))))
#' @export
sas_f_nmiss <- function(...) as.numeric(rowSums(is.na(args_matrix(list(...)))))
#' @export
sas_f_mean <- function(...) {
  m <- args_matrix(list(...))
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}
#' @export
sas_f_min <- function(...) {
  m <- args_matrix(list(...))
  out <- suppressWarnings(apply(m, 1, min, na.rm = TRUE))
  out[is.infinite(out)] <- NA_real_
  out
}
#' @export
sas_f_max <- function(...) {
  m <- args_matrix(list(...))
  out <- suppressWarnings(apply(m, 1, max, na.rm = TRUE))
  out[is.infinite(out)] <- NA_real_
  out
}

fuzz_int <- function(x) {
  r <- round(x)
  ifelse(!is.na(x) & abs(x - r) < 1e-12 * pmax(1, abs(x)), r, x)
}

#' SAS ROUND: half away from zero, with fuzz
#' @export
sas_f_round <- function(x, unit = 1, .at = NULL) {
  n <- max(length(x), length(unit))
  if (n == 0) return(numeric())
  x <- rep_len(as.numeric(x), n)
  unit <- rep_len(as.numeric(unit), n)
  # missing arguments (special missing values included) give an ordinary missing
  out <- rep(NA_real_, n)
  bad <- !is.na(unit) & unit <= 0
  if (any(bad)) note_invalid_arg("round", 2L, .at, list(x, unit), bad, numbered = TRUE)
  ok <- !is.na(x) & !is.na(unit) & !bad
  q <- x / unit
  # values that are already whole multiples at this magnitude come back unchanged
  big <- ok & (!is.finite(q) | abs(q) >= 2^52)
  out[big] <- x[big]
  k <- ok & !big
  if (any(k)) {
    a <- abs(q[k])
    r <- floor(a)
    # a halfway point within the fuzz (relative 1e-12, at most 1e-6) rounds away from zero
    r <- r + (a - r >= 0.5 - pmin(1e-12 * pmax(1, a), 1e-6))
    r <- sign(q[k]) * r
    inv <- 1 / unit[k]
    exact <- abs(inv - round(inv)) < 1e-9 & inv >= 1
    out[k] <- ifelse(exact, r / round(inv), r * unit[k])
  }
  out
}
#' @export
sas_f_int <- function(x) trunc(fuzz_int(x))
#' @export
sas_f_floor <- function(x) floor(fuzz_int(x))
#' @export
sas_f_ceil <- function(x) ceiling(fuzz_int(x))
#' @export
sas_f_abs <- function(x) abs(x)
#' @export
sas_f_sign <- function(x) sign(x)
#' @export
sas_f_sqrt <- function(x, .at = NULL) {
  bad <- !is.na(x) & x < 0
  if (any(bad)) note_invalid_arg("sqrt", NULL, .at, list(x), bad)
  ifelse(bad, NA_real_, sqrt(pmax(x, 0)))
}
#' @export
sas_f_exp <- function(x, .at = NULL) {
  out <- exp(x)
  if (any(is.infinite(out) & !is.na(x))) note_invalid_arg("exp", NULL, .at, list(x), is.infinite(out) & !is.na(x))
  out[is.infinite(out)] <- NA_real_
  sas_num(out)
}
log_base <- function(x, f, name, .at) {
  bad <- !is.na(x) & x <= 0
  if (any(bad)) note_invalid_arg(name, NULL, .at, list(x), bad)
  ifelse(bad, NA_real_, f(pmax(x, .Machine$double.xmin)))
}
#' @export
sas_f_log <- function(x, .at = NULL) log_base(x, log, "log", .at)
#' @export
sas_f_log10 <- function(x, .at = NULL) log_base(x, log10, "log10", .at)
#' @export
sas_f_log2 <- function(x, .at = NULL) log_base(x, log2, "log2", .at)
#' SAS MOD: the result has the sign of the dividend
#' @export
sas_f_mod <- function(a, b, .at = NULL) {
  out <- a - b * trunc(a / b)
  zero <- !is.na(b) & b == 0
  if (any(zero & !is.na(a))) note_invalid_arg("mod", NULL, .at, list(a, b), rep_len(zero, length(a)) & !is.na(a))
  out[zero] <- NA_real_
  # fuzz: a remainder within 1e-12 (relative) of 0 or of the divisor is 0
  tol <- 1e-12 * pmax(abs(a), abs(b), 1, na.rm = TRUE)
  near <- !is.na(out) & (abs(out) < tol | abs(abs(out) - abs(b)) < tol)
  out[near] <- 0
  fuzz_int(out)
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

#' @export
sas_f_trim <- function(x) {
  out <- sas_rtrim(as_sas_chr(x))
  out[!nzchar(out)] <- " "
  out
}
#' @export
sas_f_trimn <- function(x) sas_rtrim(as_sas_chr(x))
#' @export
sas_f_strip <- function(x) strip_both(as_sas_chr(x))
#' @export
sas_f_left <- function(x) {
  x <- as_sas_chr(x)
  s <- sub("^ +", "", x)
  paste0(s, strrep(" ", nchar(x) - nchar(s)))
}
#' @export
sas_f_right <- function(x) {
  x <- as_sas_chr(x)
  s <- sas_rtrim(x)
  paste0(strrep(" ", nchar(x) - nchar(s)), s)
}
#' @export
sas_f_upcase <- function(x) toupper(as_sas_chr(x))
#' @export
sas_f_lowcase <- function(x) tolower(as_sas_chr(x))
#' @export
sas_f_cats <- function(...) {
  parts <- lapply(list(...), function(a) strip_both(as_sas_chr(a)))
  do.call(paste0, parts)
}
#' @export
sas_f_catt <- function(...) {
  parts <- lapply(list(...), function(a) sas_rtrim(as_sas_chr(a)))
  do.call(paste0, parts)
}
#' @export
sas_f_cat <- function(...) do.call(paste0, lapply(list(...), as_sas_chr))
#' @export
sas_f_catx <- function(sep, ..., .at = NULL, .len = 200L) {
  args <- list(...)
  # numeric items: BEST32. without blanks (missing values are written, not
  # skipped: '.', 'A'); only blank character items are skipped
  parts <- lapply(args, function(a) {
    if (is.numeric(a)) {
      a[!is.na(a) & a == 0] <- 0
      # whole numbers in full (BEST32.), other values with BEST12.
      whole <- !is.na(a) & a == trunc(a)
      return(strip_both(ifelse(whole, sas_fmt_best(a, 32L), sas_fmt_best(a, 12L))))
    }
    strip_both(a)
  })
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
      note_invalid_arg("catx", argn, .at, c(list(sep[i]), lapply(args, `[`, min(i, max(lengths(args))))), NULL,
                       numbered = TRUE, math = FALSE)
      return("")
    }
    out
  }, character(1))
}
#' @export
sas_f_substr <- function(x, pos, len = NULL, .at = NULL) {
  x <- as_sas_chr(x)
  m <- max(length(x), length(pos), length(len))
  x <- rep_len(x, m)
  n <- nchar(x)
  pos <- rep_len(trunc(pos), m)
  bad_pos <- is.na(pos) | pos < 1 | pos > n
  if (is.null(len)) {
    end <- n
  } else {
    len <- rep_len(trunc(len), m)
    # a nonpositive or missing length returns the remainder (SAS NOTE: invalid argument)
    bad_len <- !bad_pos & (is.na(len) | len <= 0 | pos + len - 1 > n)
    if (any(bad_len)) note_invalid_arg("substr", 3L, .at)
    end <- ifelse(is.na(len) | len <= 0, n, pmin(pos + len - 1, n))
  }
  if (any(bad_pos)) note_invalid_arg("substr", 2L, .at)
  out <- substr(x, pos, end)
  # an invalid position returns blank (SAS NOTE: invalid argument)
  out[bad_pos] <- ""
  out
}
#' @export
sas_f_index <- function(x, s) {
  ab <- recycle2(as_sas_chr(x), s)
  out <- mapply(function(a, b) regexpr(b, a, fixed = TRUE)[1], ab[[1]], ab[[2]], USE.NAMES = FALSE)
  as.numeric(pmax(out, 0L))
}
#' @export
sas_f_indexc <- function(x, chars) {
  x <- as_sas_chr(x)
  set <- strsplit(chars, "")[[1]]
  vapply(x, function(s) {
    w <- which(strsplit(s, "")[[1]] %in% set)
    if (length(w) == 0) 0 else as.numeric(w[1])
  }, numeric(1), USE.NAMES = FALSE)
}
#' @export
sas_f_length <- function(x) {
  n <- nchar(sas_rtrim(as_sas_chr(x)))
  as.numeric(pmax(n, 1L))
}
#' @export
sas_f_lengthn <- function(x) as.numeric(nchar(sas_rtrim(as_sas_chr(x))))
#' @export
sas_f_tranwrd <- function(x, from, to) {
  x <- as_sas_chr(x)
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
  vapply(as_sas_chr(x), function(s) intToUtf8(rev(utf8ToInt(s))), character(1), USE.NAMES = FALSE)
}

# Dates and times (SAS dates count days from 1960-01-01) ------------------

sas_epoch <- as.Date("1960-01-01")
month_abb <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

#' Convert a date/time/datetime literal body to its SAS numeric value
#' @export
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
date_part <- function(x, what) {
  lt <- as.POSIXlt(to_date(x))
  out <- switch(what,
    year = lt$year + 1900,
    month = lt$mon + 1,
    day = lt$mday,
    wday = lt$wday + 1
  )
  as.numeric(ifelse(is.na(x), NA, out))
}
from_ymd <- function(y, m, d) {
  ok <- !is.na(y) & !is.na(m) & !is.na(d) & abs(y) < 1e5 & abs(m) < 1e5 & abs(d) < 1e5
  txt <- rep(NA_character_, length(ok))
  txt[ok] <- sprintf("%04d-%02d-%02d", as.integer(y[ok]), as.integer(m[ok]), as.integer(d[ok]))
  out <- suppressWarnings(as.Date(txt, format = "%Y-%m-%d"))
  as.numeric(out - sas_epoch)
}
days_in_month <- function(y, m) {
  nm <- ifelse(m == 12, 1, m + 1)
  ny <- ifelse(m == 12, y + 1, y)
  from_ymd(ny, nm, 1) - from_ymd(y, m, 1)
}

#' @export
sas_f_mdy <- function(m, d, y, .at = NULL) {
  out <- from_ymd(y, m, d)
  bad <- is.na(out) & !is.na(m) & !is.na(d) & !is.na(y)
  if (any(bad)) note_invalid_arg("mdy", NULL, .at, list(m, d, y), bad)
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
sas_f_datepart <- function(x) floor(x / 86400)
#' @export
sas_f_timepart <- function(x) x - floor(x / 86400) * 86400
#' @export
sas_f_hour <- function(x) floor((x %% 86400) / 3600)
#' @export
sas_f_minute <- function(x) floor((x %% 3600) / 60)
#' @export
sas_f_second <- function(x) x %% 60
#' @export
sas_f_hms <- function(h, m, s) h * 3600 + m * 60 + s

# LAG ---------------------------------------------------------------------

#' LAG queue for row-wise (L3) code; one queue per call site
#' @export
sas_lag <- function(state, key, value, n = 1L) {
  q <- state[[key]]
  if (is.null(q)) q <- rep(list(if (is.character(value)) "" else NA_real_), n)
  state[[key]] <- c(q[-1L], list(value))
  q[[1L]]
}

#' Vectorised LAG for unconditional calls (L2)
#' @export
sas_lag_vec <- function(x, n = 1L) {
  dplyr::lag(x, n, default = if (is.character(x)) "" else NA_real_)
}

# Formats -----------------------------------------------------------------

strip_zeros <- function(s) ifelse(grepl(".", s, fixed = TRUE), sub("\\.?0+$", "", s), s)

#' BESTw. format (right-aligned to width w)
#' @export
sas_fmt_best <- function(x, w = 12L) {
  w <- as.integer(w %||% 12L)
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
    if (v != 0 && w > 15) {
      # SAS shows at most 15 significant digits (sprintf rounds correctly
      # where signif() loses the last digit for very small magnitudes)
      v15 <- as.numeric(sprintf("%.15g", v))
      if (is.finite(v15)) v <- v15
    }
    s <- NA_character_
    if (v == trunc(v) && abs(v) < 10^w) {
      s <- sprintf("%.0f", v)
      if (nchar(s) > w) s <- NA_character_
    }
    # SAS shows at most 15 significant digits
    dmax <- if (v == 0) w - 1L else max(0L, 15L - as.integer(floor(log10(abs(v)))) - 1L)
    if (is.na(s)) {
      for (d in seq(min(w - 1L, dmax), 0L)) {
        # SAS rounds half away from zero (sprintf rounds half to even)
        cand <- strip_zeros(sprintf("%.*f", d, if (d <= 10L) sas_f_round(v, 10^-d) else v))
        if (nchar(cand) <= w) {
          if (cand %in% c("0", "-0") && v != 0) break
          s <- cand
          break
        }
      }
    }
    sci <- NA_character_
    if (is.na(s) || (v != 0 && abs(v) < 1e-5)) {
      for (d in seq(min(max(w - 2L, 0L), 14L), 0L)) {
        cand <- sprintf("%.*E", d, v)
        mant <- strip_zeros(sub("E.*$", "", cand))
        ex <- as.integer(sub("^.*E", "", cand))
        cand <- paste0(mant, "E", ifelse(ex < 0, "-", ""), abs(ex))
        if (nchar(cand) <= w) {
          sci <- cand
          break
        }
      }
    }
    # tiny values: E notation when it shows at least as many significant digits
    if (!is.na(sci) && (is.na(s) || sig(sci) > sig(s) || (abs(v) < 1e-6 && sig(sci) >= sig(s)))) s <- sci
    out[i] <- if (is.na(s)) strrep("*", w) else s
  }
  formatC(out, width = w)
}

# PUT ---------------------------------------------------------------------

#' Text written by PUT for one value (list or named output)
#' @export
sas_put_value <- function(value, fmt = NULL) {
  # list and named output write character values without leading blanks
  if (is.character(value)) return(strip_both(sas_format(value, fmt)))
  trimws(sas_format(value, fmt))
}
#' @export
sas_put_named <- function(name, value, fmt = NULL) {
  if (length(value) == 0) return(character())
  paste0(name, "=", sas_put_value(value, fmt))
}

#' Assemble one PUT line per row
#' @export
sas_put_line <- function(..., .lit = NULL) {
  parts <- list(...)
  if (length(parts) == 0) return("")
  if (any(lengths(parts) == 0)) return(character())
  # List output writes a blank after each value; literals add none.
  lit <- .lit %||% rep(FALSE, length(parts))
  parts <- Map(function(p, l) if (l) p else paste0(p, " "), parts, lit)
  line <- do.call(paste0, parts)
  if (any(nchar(line) > 132L)) {
    # the log line size is 132: an item that does not fit starts a new line
    line <- vapply(seq_along(line), function(i) {
      cur <- ""
      out <- character()
      for (p in parts) {
        t <- rep_len(p, length(line))[i]
        if (nchar(cur) > 0 && nchar(cur) + nchar(sas_rtrim(t)) > 132L) {
          out <- c(out, sas_rtrim(cur))
          cur <- ""
        }
        cur <- paste0(cur, t)
      }
      paste(c(out, sas_rtrim(cur)), collapse = "\n")
    }, "")
  }
  sas_rtrim(line)
}

#' Interleave several guarded PUT statements row by row (L1/L2)
#' @export
sas_put_rows <- function(lines, guards) {
  n <- max(lengths(lines), lengths(guards), 0L)
  if (n == 0) return(character())
  lines <- lapply(lines, rep_len, n)
  guards <- lapply(guards, function(g) rep_len(sas_true(g), n))
  txt <- do.call(rbind, lines)
  keep <- do.call(rbind, guards)
  as.vector(txt)[as.vector(keep)]
}

# Data sets ---------------------------------------------------------------

#' Fetch a WORK data set and apply KEEP=/DROP=/RENAME=/FIRSTOBS=/OBS=
#' @export
sas_ds <- function(work, name, keep = NULL, drop = NULL, rename = NULL, firstobs = NULL, obs = NULL) {
  d <- work[[name]]
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
  dec <- unlist(lapply(seq_along(by), function(k) rep(descending[k], if (is.character(d[[by[k]]])) 1L else 2L)))
  do.call(order, c(unname(keys), list(decreasing = dec, method = "radix")))
}

#' Stop with a SAS-style error when a data set is not sorted by the BY variables
#' @export
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
sas_first <- function(...) as.numeric(key_change(...))
#' @export
sas_last <- function(...) {
  chg <- key_change(...)
  as.numeric(c(chg[-1L], length(chg) > 0))[seq_along(chg)]
}

#' Combine the data sets of a SET statement (concatenate, or interleave with BY)
#' @export
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
      len <- max(1L, nchar(d[[v]]), na.rm = TRUE)
      d[[v]] <- sas_char(d[[v]], len)
    }
  }
  d
}

#' Rows of a data frame as a list of named lists (for the L3 PDV loop)
#' @export
sas_rows <- function(d) {
  cols <- as.list(d)
  lapply(seq_len(nrow(d)), function(i) lapply(cols, `[[`, i))
}

#' Copy one input row into the PDV
#' @export
sas_read_row <- function(pdv, rows, i) {
  r <- rows[[i]]
  pdv[names(r)] <- r
  pdv
}

#' Build the observation plan of a match-merge (MERGE + BY)
#' @export
sas_merge_plan <- function(dfs, by, descending = rep(FALSE, length(by)), in_vars = NULL) {
  for (df in dfs) sas_check_sorted(df, by, descending)
  keys <- dplyr::bind_rows(lapply(seq_along(dfs), function(j) {
    k <- dfs[[j]][by]
    k$.src <- rep(j, nrow(k))
    k$.row <- seq_len(nrow(k))
    k
  }))
  keys <- keys[sas_order(keys, c(by, ".src", ".row"), c(descending, FALSE, FALSE)), ]
  grp <- cumsum(do.call(key_change, unname(as.list(keys[by]))))
  plan <- list()
  for (g in unique(grp)) {
    sub <- keys[grp == g, ]
    rows_by_src <- lapply(seq_along(dfs), function(j) sub$.row[sub$.src == j])
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
  plan <- dplyr::bind_rows(plan)
  for (k in seq_along(by)) {
    keyset <- unname(as.list(plan[by[seq_len(k)]]))
    plan[[paste0("first.", by[k])]] <- do.call(sas_first, keyset)
    plan[[paste0("last.", by[k])]] <- do.call(sas_last, keyset)
  }
  plan
}

#' Execute one MERGE read against the PDV
#' @export
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

#' Collect PDV rows written by OUTPUT into a tibble
#' @export
sas_rows_to_tibble <- function(rows, proto) {
  cols <- lapply(names(proto), function(v) {
    vapply(rows, function(r) r[[v]], FUN.VALUE = proto[[v]], USE.NAMES = FALSE)
  })
  names(cols) <- names(proto)
  tibble::as_tibble(cols)
}

# INPUT / DATALINES -------------------------------------------------------

#' Read in-stream data lines according to an INPUT specification
#' @export
sas_read_datalines <- function(lines, spec) {
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
#' @export
sas_stop_step <- function() {
  rlang::signal(message = "stop", class = "sasonr_stop")
  abort_runtime("STOP signalled outside a DATA step loop")
}
#' @export
sas_select_error <- function(line) abort_runtime("no WHEN clause matched and there is no OTHERWISE", line)

#' DO loop test for an iterative DO
#' @export
sas_do_test <- function(i, to, by) {
  if (is.na(by) || by == 0) abort_runtime("invalid DO loop BY value")
  if (is.na(i) || is.na(to)) abort_runtime("invalid DO loop control value")
  if (by > 0) i <= to else i >= to
}

#' Running total for a vectorised sum statement (`n + x;`)
#' @export
sas_cumsum <- function(x, init, n) {
  x <- rep_len(dplyr::coalesce(as.numeric(x), 0), n)
  cumsum(c(init, x))[-1L]
}

#' Forward fill for a retained variable (vectorised carry-forward)
#'
#' Row r takes the value of the last assignment k whose flag `sets[[k]]` is
#' true on that row, otherwise the result of row r - 1 (or `init`).
#' @export
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
sas_dif <- function(state, key, value, n = 1L) value - sas_lag(state, key, value, n)

#' Validate an array subscript and convert it to a 1-based position
#' @export
sas_idx <- function(i, lo, dim) {
  k <- i - lo + 1
  if (length(k) != 1 || is.na(k) || k != trunc(k) || k < 1 || k > dim) {
    abort_runtime("array subscript out of range")
  }
  as.integer(k)
}

#' Normalise an arithmetic result: special missing values and overflow
#' (infinite results) become the ordinary missing value, as in SAS.
#' @export
sas_num <- function(x, .at = NULL) {
  inf <- is.infinite(x)
  if (any(inf)) note_math(.at, sum(inf))
  x[is.na(x) | inf] <- NA_real_
  x
}

#' Truncate character columns to PDV lengths (later data sets of SET/MERGE)
#' @export
sas_trunc_chars <- function(d, lens) {
  for (v in names(lens)) if (v %in% names(d)) d[[v]] <- sas_char(substr(d[[v]], 1L, lens[[v]]), lens[[v]])
  d
}

#' Formatted PUT output (`put x 8.2;`): the field keeps its alignment blanks
#' @export
sas_put_fvalue <- function(value, fmt) sas_format(value, fmt)

#' PUT with column/line pointer controls (`@n`, `+n`, `/`)
#'
#' Pieces are written into a line buffer at the current column (overwriting);
#' list and named output leave one blank after the value. `/` starts a new line.
#' @export
sas_put_layout <- function(pieces) {
  n <- max(1L, unlist(lapply(pieces, function(p) if (p$k == "text") length(p$s) else if (!is.null(p$v)) length(p$v) else 1L)))
  if (any(vapply(pieces, function(p) p$k == "text" && length(p$s) == 0, logical(1)))) return(character())
  vapply(seq_len(n), function(i) {
    lines <- character()
    buf <- character()
    col <- 1L
    for (p in pieces) {
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
        if (nc > 32767) return("") # beyond the maximum line size: nothing is written
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
    paste(c(lines, sas_rtrim(paste(buf, collapse = ""))), collapse = "\n")
  }, "")
}
