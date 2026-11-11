# SAS rounding and truncation functions: ROUND, ROUNDE, ROUNDZ, INT, INTZ,
# FLOOR, FLOORZ, CEIL, CEILZ.
#
# Rules reverse-engineered from the SAS 9.4 gold standard (21,052 points; see
# sasonr-conformance/docs/conformance/rounding_gold.md):
#  * subnormal arguments count as zero (SAS runs with denormals-are-zero);
#  * q = x / unit (double); the multiple k = |q| rounded, with a fuzz window
#    W = min(1.1e-12 * |q|, 2e-6) below a half-integer (ROUND: away from
#    zero; ROUNDE: an exact or fuzzed tie goes to the even multiple; ROUNDZ:
#    no fuzz, ties to even);
#  * the result is the double nearest to k times the decimal value of the
#    unit when the unit has at most 15 significant digits (ROUND(x, 0.1) gives
#    the literal 0.3, not 3 * 0.1), k / n when the unit is 1/n, and the
#    sign is that of x (x < 0 gives -0 for a zero result);
#  * |q| >= 2^52: x is returned unchanged;
#  * FLOOR / CEIL / INT have an absolute fuzz of 1.5e-12 towards the next
#    integer; FLOOR returns +0 for a zero result, CEIL(x) = -FLOOR(-x),
#    INT(x) = FLOOR(x) for x > 0 and -FLOOR(-x) otherwise; FLOORZ / CEILZ
#    have no fuzz.
# Points the gold standard leaves ambiguous fail loud (`sasonr_unsupported`):
# a quotient within 0.8-1.25 times the fuzz width below a half-integer, a
# fractional unit that is not a power of ten with a non-integer
# 1e15 <= |x| < 2^53, and a unit that is neither a short decimal nor 1/n.

sas_tiny <- 2.2250738585072014e-308

daz <- function(x) ifelse(!is.na(x) & abs(x) < sas_tiny, x * 0, x)

# Shortest decimal form of a unit: list(digits, mant, exp) with
# unit == mant * 10^exp (mant an integer below 1e15), or NULL when the unit
# needs more than 15 significant digits.
unit_decimal <- function(u) {
  for (p in 1:15) {
    s <- sprintf("%.*e", p - 1L, u)
    if (as.numeric(s) == u) {
      m <- sub("e.*$", "", s)
      e <- as.integer(sub("^.*e", "", s))
      digits <- gsub(".", "", m, fixed = TRUE)
      digits <- sub("0+$", "", digits)
      if (!nzchar(digits)) digits <- "0"
      return(list(mant = digits, exp = e - (nchar(digits) - 1L)))
    }
  }
  NULL
}

# Decimal digits of a nonnegative integer-valued double below 2^53.
int_digits <- function(k) sprintf("%.0f", k)

# Product of two nonnegative decimal integer strings.
mul_decimal <- function(a, b) {
  x <- rev(as.integer(strsplit(a, "")[[1]]))
  y <- rev(as.integer(strsplit(b, "")[[1]]))
  out <- integer(length(x) + length(y))
  for (i in seq_along(x)) {
    carry <- 0L
    for (j in seq_along(y)) {
      t <- out[i + j - 1L] + x[i] * y[j] + carry
      out[i + j - 1L] <- t %% 10L
      carry <- t %/% 10L
    }
    k <- i + length(y)
    while (carry > 0L) {
      t <- out[k] + carry
      out[k] <- t %% 10L
      carry <- t %/% 10L
      k <- k + 1L
    }
  }
  s <- paste(rev(out), collapse = "")
  s <- sub("^0+", "", s)
  if (nzchar(s)) s else "0"
}

# The double nearest to k * unit for an integer-valued k >= 0.
unit_multiple <- function(k, unit, dec) {
  if (k == 0) return(0)
  if (!is.null(dec)) {
    digits <- if (dec$mant == "1") int_digits(k) else mul_decimal(int_digits(k), dec$mant)
    return(as.numeric(paste0(digits, "e", dec$exp)))
  }
  n <- round(1 / unit)
  if (n >= 2 && abs(1 / unit - n) <= 1e-9 * n) return(k / n)
  abort_unsupported(paste0("ROUND with rounding unit ", format(unit, digits = 17),
                           " (neither a short decimal nor 1/n): not verified against SAS"))
}

# One rounding: mode "round" (half away from zero, fuzz), "even" (ROUNDE),
# "zero" (ROUNDZ, no fuzz).
round_one <- function(x, unit, mode, fname) {
  x <- daz(x)
  dec <- unit_decimal(unit)
  p10 <- !is.null(dec) && dec$mant == "1"
  if (unit != floor(unit) && !p10 && abs(x) >= 1e15 && abs(x) < 2^53 && x != floor(x)) {
    abort_unsupported(paste0(toupper(fname), "(", format(x, digits = 17), ", ", format(unit, digits = 17),
                             "): a non-integer value of 1E15 or more with a fractional unit is not verified against SAS"))
  }
  q <- x / unit
  if (!is.finite(q) || abs(q) >= 2^52) return(x)
  a <- abs(q)
  k <- floor(a)
  d <- (a - k) - 0.5
  w <- if (mode == "zero") 0 else min(1.1e-12 * a, 2e-6)
  if (w > 0 && d <= -0.8 * w && d >= -1.25 * w) {
    abort_unsupported(paste0(toupper(fname), "(", format(x, digits = 17), ", ", format(unit, digits = 17),
                             "): the value lies at the edge of the SAS fuzz window (not determined by the gold standard)"))
  }
  if (mode == "round") {
    if (d >= -w) k <- k + 1
  } else {
    if (abs(d) <= w) {
      if (k %% 2 != 0) k <- k + 1
    } else if (d > 0) {
      k <- k + 1
    }
  }
  v <- unit_multiple(k, unit, dec)
  if (x < 0) -v else v
}

round_family <- function(x, unit, mode, fname, .at = NULL, note = TRUE) {
  n <- max(length(x), length(unit))
  if (n == 0) return(numeric())
  x <- rep_len(as.numeric(x), n)
  unit <- rep_len(as.numeric(unit), n)
  # missing arguments (special missing values included) give an ordinary missing
  out <- rep(NA_real_, n)
  bad <- !is.na(unit) & unit <= 0
  if (any(bad) && note) note_invalid_arg(fname, 2L, .at, list(x, unit), bad, numbered = TRUE)
  ok <- !is.na(x) & !is.na(unit) & !bad
  for (i in which(ok)) out[i] <- round_one(x[i], unit[i], mode, fname)
  out
}

#' @export
sas_f_round <- function(x, unit = 1, .at = NULL) round_family(x, unit, "round", "round", .at)

#' @export
sas_f_rounde <- function(x, unit = 1, .at = NULL) round_family(x, unit, "even", "rounde", .at)

#' @export
sas_f_roundz <- function(x, unit = 1, .at = NULL) round_family(x, unit, "zero", "roundz", .at)

# FLOOR with the absolute fuzz; a zero result is +0
floor_fuzz <- function(x) {
  x[is.na(x)] <- NA_real_
  x <- daz(x)
  k <- floor(x)
  k <- ifelse(!is.na(x) & (x - k) >= 1 - 1.5e-12, k + 1, k)
  k + 0
}

#' @export
sas_f_floor <- function(x) floor_fuzz(as.numeric(x))

#' @export
sas_f_ceil <- function(x) -floor_fuzz(-as.numeric(x))

#' @export
sas_f_int <- function(x) {
  x <- as.numeric(x)
  y <- daz(x)
  ifelse(!is.na(y) & y > 0, floor_fuzz(x), -floor_fuzz(-x))
}

#' @export
sas_f_floorz <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- NA_real_ # special missing values give an ordinary missing
  floor(daz(x))
}

#' @export
sas_f_ceilz <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- NA_real_ # special missing values give an ordinary missing
  ceiling(daz(x))
}

#' @export
sas_f_intz <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- NA_real_ # special missing values give an ordinary missing
  trunc(daz(x))
}

# Display rounding of w.d-type formats (w.d, Z, COMMA, DOLLAR, PERCENT, ...):
# half away from zero on the exact binary value with a narrow fuzz.  From
# the gold standard: a value up to 2.25e-16 (relative) below the half rounds
# up, 2.6e-16 or more below rounds down; in between SAS's choice is not
# determined by the data, so it fails loud.
#
# Returns the absolute value with exactly `d` decimals (digits, ".", no
# sign), or NA when |v| >= 1e17.
display_round_text <- function(v, d, edge = "abort", down_at = -2.6e-16, exact = FALSE) {
  a <- abs(v)
  if (!is.finite(a) || a >= 1e17) return(NA_character_)
  if (a == 0) return(formatC(0, format = "f", digits = d))
  extra <- 30L
  s <- sprintf("%.*f", d + extra, a) # glibc prints the exact binary value
  dot <- regexpr(".", s, fixed = TRUE)
  ip <- substr(s, 1L, dot - 1L)
  fr <- substr(s, dot + 1L, nchar(s))
  keep <- paste0(ip, substr(fr, 1L, d))
  tail <- substr(fr, d + 1L, nchar(fr))
  # distance of the tail from one half, in units of 10^-d, then relative to a
  f <- as.numeric(paste0("0.", tail))
  rel <- (f - 0.5) * 10^-d / a
  up <- if (f >= 0.5) TRUE else if (exact) FALSE else if (rel >= -2.25e-16) TRUE else if (rel <= down_at) FALSE else if (edge == "up") TRUE else if (edge == "down") FALSE else {
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " to ", d,
                             " decimals: the value lies at the edge of the SAS fuzz (not determined by the gold standard)"))
  }
  if (up) keep <- incr_decimal(keep)
  if (d > 0) paste0(substr(keep, 1L, nchar(keep) - d), ".", substr(keep, nchar(keep) - d + 1L, nchar(keep))) else keep
}

# Add one to a string of decimal digits (keeps at least d+1 digits).
incr_decimal <- function(s) {
  x <- as.integer(strsplit(s, "")[[1]])
  i <- length(x)
  repeat {
    if (i == 0) {
      x <- c(1L, x)
      break
    }
    if (x[i] < 9L) {
      x[i] <- x[i] + 1L
      break
    }
    x[i] <- 0L
    i <- i - 1L
  }
  s <- paste(x, collapse = "")
  s
}

# Display rounding as a number (for PROC listings that format with sprintf).
display_round <- function(v, d, exact = FALSE) {
  vapply(v, function(x) {
    if (is.na(x)) return(NA_real_)
    t <- display_round_text(x, d, exact = exact)
    if (is.na(t)) return(x)
    r <- as.numeric(t)
    if (x < 0) -r else r
  }, 0)
}

# Scientific forms used when a number does not fit its width (w.d, Z,
# COMMA, DOLLAR fall back to these; BESTw. chooses among them):
#   sci_dec_text(v, k)  "-1.23E6", "5E-4"   (k mantissa decimals, exact digits,
#                                             half away from zero with the fuzz)
sci_dec_text <- function(v, k, edge = "abort", down_at = -2.6e-16) {
  a <- abs(v)
  s <- sprintf("%.*e", k + 30L, a)
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  keep <- substr(dg, 1L, k + 1L)
  f <- as.numeric(paste0("0.", substring(dg, k + 2L)))
  rel <- (f - 0.5) * 10^(ex - k) / a
  up <- if (f >= 0.5) TRUE else if (rel >= -2.25e-16) TRUE else if (rel <= down_at) FALSE else if (edge == "up") TRUE else if (edge == "down") FALSE else {
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " in E notation: the value lies at the edge of the SAS fuzz"))
  }
  if (up) keep <- incr_decimal(keep)
  if (nchar(keep) > k + 1L) {
    keep <- substr(keep, 1L, k + 1L)
    ex <- ex + 1L
  }
  m <- if (k > 0) paste0(substr(keep, 1L, 1L), ".", substring(keep, 2L)) else keep
  ms <- if (k > 0) sub("\\.$", "", sub("0+$", "", m)) else m
  list(text = paste0(if (v < 0) "-", ms, "E", if (ex < 0) "-", abs(ex)), sig = nchar(gsub(".", "", ms, fixed = TRUE)),
       mant = m, ex = ex)
}

# The E form SAS writes for a value that does not fit in w columns in fixed
# notation: the decimal-mantissa form with as many digits as fit, unless it
# shows fewer than three significant digits and an integer mantissa
# ("123E9") shows more.  NA when nothing fits.
sci_fit <- function(v, w) {
  for (k in seq(max(w - 3L, 0L), 0L)) {
    if (nchar(sci_dec_text(v, k, edge = "up")$text) > w) next
    dec <- sci_dec_text(v, k)
    if (dec$sig >= 3L) return(dec$text)
    # an integer mantissa with more digits ("1.23E11" -> "123E9"); when two
    # lengths fit, SAS's choice is not determined by the gold standard
    fit_int <- character()
    for (nd in seq(3L, dec$sig + 1L)) {
      if (nd <= dec$sig) break
      alt <- sci_dec_text(v, nd - 1L, edge = "up")
      digits <- gsub(".", "", alt$mant, fixed = TRUE)
      d2 <- sub("0+$", "", digits)
      ex2 <- alt$ex - (nchar(digits) - 1L) + (nchar(digits) - nchar(d2))
      t0 <- paste0(if (v < 0) "-", d2, if (ex2 > 0) paste0("E", ex2))
      if (nzchar(d2) && ex2 >= 0L && (nchar(d2) > dec$sig || (nchar(d2) == dec$sig && nchar(t0) < nchar(dec$text))) &&
          nchar(t0) <= w) fit_int <- c(fit_int, d2)
    }
    if (length(unique(fit_int)) > 1L) {
      abort_unsupported(paste0("E notation of ", format(v, digits = 17), " in ", w,
                               " columns: the integer-mantissa length is not determined by the gold standard"))
    }
    for (nd in seq(3L, dec$sig + 1L)) {
      if (nd <= dec$sig) break
      alt <- sci_dec_text(v, nd - 1L, edge = "up")
      digits <- gsub(".", "", alt$mant, fixed = TRUE)
      d2 <- sub("0+$", "", digits)
      ex2 <- alt$ex - (nchar(digits) - 1L) + (nchar(digits) - nchar(d2))
      t <- paste0(if (v < 0) "-", d2, if (ex2 > 0) paste0("E", ex2))
      # more digits, or as many in fewer columns ("75E4" rather than "7.5E5")
      if (!nzchar(d2) || ex2 < 0L || nchar(d2) < dec$sig || (nchar(d2) == dec$sig && nchar(t) >= nchar(dec$text))) next
      if (nchar(t) <= w) {
        sci_dec_text(v, nd - 1L) # fail loud at the fuzz edge
        return(t)
      }
    }
    return(dec$text)
  }
  NA_character_
}

# 15 significant digits of |v| (truncated unless within the display fuzz of
# the next digit) as list(digits = "ddd...", ex = decimal exponent)
sig15 <- function(a) {
  s <- sprintf("%.*e", 40L, a)
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  keep <- substr(dg, 1L, 15L)
  f <- as.numeric(paste0("0.", substring(dg, 16L)))
  if ((1 - f) * 10^(ex - 14L) / a <= 4.5e-16) keep <- incr_decimal(keep)
  if (nchar(keep) > 15L) {
    keep <- substr(keep, 1L, 15L)
    ex <- ex + 1L
  }
  list(digits = keep, ex = ex)
}
