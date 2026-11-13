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
  # SAS also rejects a unit that is not a normal double: ROUND(1, 2.3e-308) is
  # 1 while ROUND(1, 2.2e-308) is ". " with "Argument 2 ... is invalid",
  # because 2.2e-308 is subnormal (ROUNDE/ROUNDZ behave the same).
  sub <- !is.na(unit) & unit > 0 & unit < .Machine$double.xmin
  if (any(sub) && sas_target_version() != "TS1M2") {
    # TS1M2 gold only (ROUND_047); TS1M7 rejects subnormal literals, a computed one is not verified
    abort_unsupported(paste0(toupper(fname), " with a subnormal rounding unit (not verified for SAS 9.4 ", sas_target_version(), ")"))
  }
  bad <- !is.na(unit) & (unit <= 0 | sub)
  if (any(bad) && note) {
    # ROUND writes "Argument 2 ... is invalid"; ROUNDE/ROUNDZ only count the place (SAS 9.4)
    if (fname == "round") note_invalid_arg(fname, 2L, .at, list(x, unit), bad, numbered = TRUE) else note_math(.at, sum(bad))
  }
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

# Display rounding of w.d-type formats (w.d, Z, COMMA, DOLLAR, PERCENT, ...)
# and BESTw., from the round-13 ulp-by-ulp SAS 9.4 scans
# (sasonr-conformance/docs/conformance/rounding_100.md):
#  * SAS works with the display value a' = fl(|x| * (1 + 2^-52)): BESTw. at
#    widths above 16 and w.d at more than 15 significant digits show the
#    first 15 digits of a' truncated;
#  * 0 decimals: a' rounded half away from zero (exact on a');
#  * d >= 1 decimals: |x| at or above the decimal half point rounds up, and so
#    does the double nearest to the half point; at least 3 ulps of |x| below
#    the half point (5 ulps when the result is 0) rounds down; in between
#    SAS's choice is not a function of the distance alone (it depends on the
#    binary exponent: the scans show both directions at equal distances), so
#    the value fails loud (edge = "abort");
#  * 16 significant digits: rounding at the 16th digit and truncating to 15
#    gives the SAS text unless the two readings below disagree (fail loud);
#    17 or more: the rounded value of a' truncated to 15 digits.  Integers
#    are written with their exact digits where verified (below 1E18 without
#    decimals, below 1E15 with decimals); other integers of 1E15 or more with
#    decimals, non-integers with 16 or more integer digits and values below 1
#    with 15 or more decimals fail loud.
#
# Returns the absolute value with exactly `d` decimals (digits, ".", no
# sign), or NA when |v| >= 1e17 (the callers write the E form).  `exact =
# TRUE`: half away from zero on the exact binary value without the display
# adjustment (PROC MEANS MAXDEC=).  `down_rel` (PERCENTw.d): a value below a
# half point by less than `down_rel` relative that the rules above round down
# fails loud instead (gold: PERCENT rounds 1.0049999999999997 up to 101%,
# where w.d rounds down; PERCENT has not been scanned ulp by ulp).
display_round_text <- function(v, d, edge = "abort", exact = FALSE, down_rel = 0) {
  a <- abs(v)
  if (!is.finite(a)) return(NA_character_)
  if (a == 0) return(paste0("0", if (d > 0L) paste0(".", strrep("0", d))))
  if (a == floor(a) && a >= 1 && !exact) {
    # integers are written with their exact digits (SAS 9.4: 32. of
    # 9.39586291501115E17 is 939586291501115008); verified without decimals
    # below 1E18, and with decimals while the integer has at most 15 digits
    if (d == 0L && a < 1e18) return(sprintf("%.0f", a))
    if (d > 0L && a < 1e15) return(paste0(sprintf("%.0f", a), ".", strrep("0", d)))
    if (a < 1e17) {
      # every reading has the same width: a width probe (edge "up"/"down") gets the exact digits
      if (!identical(edge, "abort")) return(paste0(sprintf("%.0f", a), ".", strrep("0", d)))
      abort_unsupported(paste0("format rounding of the integer ", sprintf("%.0f", v), " to ", d,
                               " decimals: an integer of 16 or more digits with decimals is not verified against SAS"))
    }
  }
  if (a >= 1e17) return(NA_character_)
  fmt_d <- function(keep) {
    if (d > 0) paste0(substr(keep, 1L, nchar(keep) - d), ".", substr(keep, nchar(keep) - d + 1L, nchar(keep))) else keep
  }
  split <- function(x) {
    s <- sprintf("%.*f", d + 30L, x) # glibc prints the exact binary value
    dot <- regexpr(".", s, fixed = TRUE)
    fr <- substr(s, dot + 1L, nchar(s))
    list(keep = paste0(substr(s, 1L, dot - 1L), substr(fr, 1L, d)), f = as.numeric(paste0("0.", substr(fr, d + 1L, nchar(fr)))))
  }
  edge_fail <- function(why) {
    if (identical(edge, "up")) return(TRUE)
    if (identical(edge, "down")) return(FALSE)
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " to ", d, " decimals: ", why))
  }
  # PERCENT: rounding down within down_rel below a half point is not verified
  down_ok <- function(f) {
    if (down_rel <= 0 || (0.5 - f) * 10^-d / a > down_rel) return(FALSE)
    edge_fail("PERCENT rounding just below a half point is not determined by the gold standard")
  }
  if (exact) {
    p <- split(a)
    return(fmt_d(if (p$f >= 0.5) incr_decimal(p$keep) else p$keep))
  }
  ab <- display_value(a)
  ni <- if (a >= 1) nchar(sprintf("%.0f", floor(a))) else 0L
  if (ni >= 16L) {
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " to ", d,
                             " decimals: a non-integer with 16 or more integer digits is not verified against SAS"))
  }
  if (a < 1 && d >= 15L) {
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " to ", d,
                             " decimals: the 15-digit limit for values below 1 is not verified against SAS"))
  }
  if (ni + d >= 16L) {
    if (down_rel > 0) {
      abort_unsupported(paste0("PERCENT rounding of ", format(v, digits = 17), " to ", d,
                               " decimals: more than 15 significant digits are not verified against SAS"))
    }
    # SAS shows 15 significant digits: from 17 digits on, the first 15 digits
    # of the display value (rounding at the last decimal does not carry into
    # them: 2922024225.7520895 in 32.7 is 2922024225.7520800)
    p <- split(ab)
    if (ni + d >= 17L) return(fmt_d(digits15(p$keep)))
    r1 <- digits15(if (p$f >= 0.5) incr_decimal(p$keep) else p$keep)
    if (ni + d == 16L) {
      # SAS either truncates or rounds the 16th digit (both seen in the scans)
      r0 <- digits15(p$keep)
      k15 <- substr(p$keep, 1L, nchar(p$keep) - 1L)
      if (substr(p$keep, nchar(p$keep), nchar(p$keep)) >= "5") k15 <- incr_decimal(k15)
      r2 <- digits15(paste0(k15, "0"))
      if (r1 != r0 || r2 != r0) {
        r1 <- if (edge_fail("rounding at the 16th significant digit is not determined by the gold standard")) r2 else r0
      }
    }
    return(fmt_d(r1))
  }
  p <- split(a)
  up <- if (p$f >= 0.5) {
    TRUE
  } else if (d == 0L) {
    split(ab)$f >= 0.5 || down_ok(p$f)
  } else {
    hdec <- paste0(substr(p$keep, 1L, nchar(p$keep) - d), ".", substring(p$keep, nchar(p$keep) - d + 1L), "5")
    if (as.numeric(hdec) == a) {
      TRUE # the double nearest to the half point
    } else {
      ulp <- 2^(floor(log2(a)) - 52)
      dist <- (0.5 - p$f) * 10^-d / ulp
      lim <- if (grepl("^0*$", p$keep)) 5 else 3
      if (dist >= lim) down_ok(p$f) else edge_fail("the value lies within a few ulps below the half point, where SAS's choice is not determined by the gold standard")
    }
  }
  fmt_d(if (up) incr_decimal(p$keep) else p$keep)
}

# The value SAS 9.4 formats: |x| * (1 + 2^-52) rounded to a double (the
# round-13 scans: BEST32. and 32.30 show its first 15 digits truncated, and
# rounding to 0 decimals is exact on it).
display_value <- function(a) {
  b <- a * (1 + .Machine$double.eps)
  ifelse(is.finite(b), b, a) # the largest double: the product overflows
}

# A digit string truncated to 15 significant digits (the rest zeros).
digits15 <- function(keep) {
  lead <- regexpr("[1-9]", keep)
  if (lead < 0) return(keep)
  last <- lead + 14L
  if (nchar(keep) <= last) return(keep)
  paste0(substr(keep, 1L, last), strrep("0", nchar(keep) - last))
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

# Signed fixed-point display text of v with d decimals (the one display
# engine, display_round_text): "-" for a negative value, also when it rounds
# to zero ("-0.00", as SAS writes it).  Values of 1E17 or more have no
# verified fixed-point text: fail loud.
display_fixed <- function(v, d, edge = "abort", exact = FALSE) {
  t <- display_round_text(v, d, edge = edge, exact = exact)
  if (is.na(t)) {
    abort_unsupported(paste0("fixed-point display of ", format(v, digits = 17), " with ", d,
                             " decimals: values of 1E17 or more are not verified against SAS"))
  }
  paste0(if (v < 0) "-", t)
}

# Scientific forms used when a number does not fit its width (w.d, Z,
# COMMA, DOLLAR fall back to these; BESTw. chooses among them):
#   sci_dec_text(v, k)  "-1.23E6", "5E-4"   (k mantissa decimals, exact digits,
#                                             half away from zero with the fuzz)
sci_dec_text <- function(v, k, edge = "abort") {
  a <- abs(v)
  if (k >= 13L) {
    # at most 14 significant digits (SAS 9.4: 32. of the largest double is 1.7976931348623E308)
    g <- sig15(a)
    keep <- e14_digits(g, v)
    ex <- g$ex
    keep <- paste0(keep, strrep("0", k - 13L))
    m <- paste0(substr(keep, 1L, 1L), ".", substring(keep, 2L))
    ms <- sub("\\.$", "", sub("0+$", "", m))
    return(list(text = paste0(if (v < 0) "-", ms, "E", if (ex < 0) "-", abs(ex)), sig = nchar(gsub(".", "", ms, fixed = TRUE)),
                mant = m, ex = ex))
  }
  s <- sprintf("%.*e", k + 30L, a)
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  keep <- substr(dg, 1L, k + 1L)
  f <- as.numeric(paste0("0.", substring(dg, k + 2L)))
  # as in fixed notation (display_round_text): at or above the half point up,
  # 3 ulps or more below it down, in between not determined (fail loud)
  dist <- (0.5 - f) * 10^(ex - k) / 2^(floor(log2(a)) - 52)
  up <- if (f >= 0.5) TRUE else if (dist >= 3) FALSE else if (edge == "up") TRUE else if (edge == "down") FALSE else {
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " in E notation: the value lies within a few ulps ",
                             "below the half point, where SAS's choice is not determined by the gold standard"))
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

# 15 significant digits of |v| as list(digits = "ddd...", ex = decimal
# exponent, rest = the digits after the 14th as a fraction of the 14th)
sig15 <- function(a) {
  # the first 15 digits of the display value fl(a * (1 + 2^-52)), truncated
  # (round-13 scans: BEST32. and 32.30 of 172 + 58 values)
  s <- sprintf("%.*e", 40L, display_value(a))
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  list(digits = substr(dg, 1L, 15L), ex = ex, rest = as.numeric(paste0("0.", substring(dg, 15L))))
}
