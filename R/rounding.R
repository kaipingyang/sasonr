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
# a quotient within 0.8-1.25 times the fuzz width below a half-integer
# (ROUNDE: also anything up to 2E-6 below it once 1.1E-12 |q| > 1E-6), a
# fractional unit that is not a power of ten with a non-integer
# 1e15 <= |x| < 2^53, a fractional unit with a non-integer |x/unit| >= 2^49
# (ROUND, ROUNDE, ROUNDZ), and a unit that is neither a short decimal nor 1/n.

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
  # From |q| = 2^49 on, a fractional unit gives results that are not the
  # nearest multiple (exact half points rounded down at random: ROUND(
  # 507561523905906.125, 0.25) is 507561523905906, ROUND(399931701399944.05,
  # 0.1) rounds down; ROUND(269284231226460.16, 1/3) is not k/3): fail loud
  # for ROUND, ROUNDE and ROUNDZ (rounding_100.md section 3), as for |x| >=
  # 1E15 above, unless x/unit is an integer (x is a multiple: ROUND(
  # 123456789012.5, 1E-4), rounding/values/round_b01).
  if (unit != floor(unit) && a >= 2^49 && a != k) {
    abort_unsupported(paste0(toupper(fname), "(", format(x, digits = 17), ", ", format(unit, digits = 17),
                             "): x/unit of 2^49 or more with a fractional unit is not verified against SAS"))
  }
  if (mode == "round") {
    # SAS 9.4 (round-15 scan: 26,990 points, q = x/unit at every binary
    # exponent from 2^-2 to 2^52, units 1 10 0.5 0.25 0.1 0.01 0.001 1E-5 1/3):
    # up exactly when floor(fl(fl(|q| + 0.5) + W)) > floor(|q|), W =
    # min(1E-12 |q|, 1E-6) - the fuzz is added to |q| + 0.5 in double precision.
    if (floor((a + 0.5) + min(1e-12 * a, 1e-6)) > k) k <- k + 1
  } else {
    d <- (a - k) - 0.5
    w <- if (mode == "zero") 0 else min(1.1e-12 * a, 2e-6)
    # ROUNDE: the round-7 window W = min(1.1E-12 |q|, 2E-6) was refuted for
    # ROUND by the round-15 scan (cap 1E-6); until ROUNDE is rescanned, every
    # value up to 2E-6 below a half point fails loud once 1.1E-12 |q| > 1E-6
    if (w > 0 && ((d <= -0.8 * w && d >= -1.25 * w) || (1.1e-12 * a > 1e-6 && d >= -2e-6 && d < 0))) {
      abort_unsupported(paste0(toupper(fname), "(", format(x, digits = 17), ", ", format(unit, digits = 17),
                               "): the value lies at the edge of the SAS fuzz window (not determined by the gold standard)"))
    }
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
# and BESTw.: SAS 9.4's decimal conversion, derived from the round-13 and
# round-15 ulp scans (sasonr-conformance/docs/conformance/rounding_100.md):
#
#   t = fl( fl(|x| + h) * (1 + 2^-52) ),  h = the double nearest 0.5 * 10^-d
#
# (two double operations, in this order), then the exact binary value of t
# truncated to d decimals - but to at most 15 significant digits for
# |x| >= 1 (the integer digits always) and at most 14 - E decimals for
# |x| < 1 (E = the decimal exponent; 14 for 0.1 <= |x| < 1), the rest
# written as zeros.  This one closed form reproduces every scanned point:
# 41,605 points of the round-13 scans (d = 0..30) and 20,284 of round 15
# (every binary exponent from 2^-61 to 2^53, d = 0..17, 1 in 4 negative),
# PERCENT (fl(100 |x|), 2,799 points).  Integers are written with their exact
# digits (32. of 9.39586291501115E17 is 939586291501115008).
#
# Verified domain (outside it: fail loud, or NA for the E form):
#  * |x| >= 1, non-integer, below 1E15: every d; 1E15 or more: d <= 3;
#  * integers: no decimals below 1E18, d <= 3 below 2^53, any d below 1E15;
#  * |x| < 1: d <= 14, and d <= 16 when |x| < 0.01 (decimal exponent <= -3).
#    At 0.01 <= |x| < 0.1 with d >= 15 and 0.1 <= |x| < 1 with d >= 16 SAS
#    drops the last digits in a way that depends on the binary exponent
#    (0.0711077156197605 in 32.15 keeps 14 decimals, 0.0703075513494065 in
#    32.16 keeps 13), and values with more decimals are only sparsely scanned.
#
# Returns the absolute value with exactly `d` decimals (digits, ".", no
# sign), or NA when |v| >= 1e17 (the callers write the E form).  `exact =
# TRUE`: half away from zero on the exact binary value without the display
# adjustment (PROC MEANS MAXDEC=).  `edge` ("abort", "up", "down"): what an
# unverified rounding does - fail loud, or (width probes) give a text of the
# same length.  `down_rel` is accepted for compatibility and ignored (PERCENT
# follows the same rule on fl(100 |x|)).
display_round_text <- function(v, d, edge = "abort", exact = FALSE, down_rel = 0) {
  a <- abs(v)
  if (!is.finite(a)) return(NA_character_)
  if (a == 0) return(paste0("0", if (d > 0L) paste0(".", strrep("0", d))))
  unverified <- function(why) {
    if (!identical(edge, "abort")) return(TRUE)
    abort_unsupported(paste0("format rounding of ", format(v, digits = 17), " to ", d, " decimals: ", why))
  }
  fmt_d <- function(keep) {
    if (d > 0) paste0(substr(keep, 1L, nchar(keep) - d), ".", substr(keep, nchar(keep) - d + 1L, nchar(keep))) else keep
  }
  # exact decimal digits of a double: integer part and fraction digits
  digits_of <- function(x, nd) {
    s <- sprintf("%.*f", nd, x) # glibc prints the exact binary value
    dot <- regexpr(".", s, fixed = TRUE)
    if (dot < 0) return(list(ip = s, fr = ""))
    list(ip = substr(s, 1L, dot - 1L), fr = substr(s, dot + 1L, nchar(s)))
  }
  if (a == floor(a) && a >= 1 && !exact) {
    # integers are written with their exact digits (SAS 9.4: 32. of
    # 9.39586291501115E17 is 939586291501115008)
    # (no decimals: verified below 1E18; from 1E18 to 1E21 only the gold
    # point 32. of 2.6745296926266266E19 = 26745296926266265600 - owner
    # decision 2026-11-13: fail loud elsewhere; a width probe gets the digits)
    if ((d == 0L && a < 1e18) || a < 1e15 || (d <= 3L && a < 2^53) || (d == 0L && a == 26745296926266265600)) {
      return(paste0(sprintf("%.0f", a), if (d > 0L) paste0(".", strrep("0", d))))
    }
    if (d == 0L && a < 1e21) {
      unverified("an integer from 1E18 to 1E21 without decimals is not verified against SAS")
      return(sprintf("%.0f", a))
    }
    if (a < 1e17) {
      unverified("an integer of 16 or more digits with more than 3 decimals is not verified against SAS")
      return(paste0(sprintf("%.0f", a), ".", strrep("0", d)))
    }
  }
  if (a >= 1e17) return(NA_character_)
  if (exact) {
    p <- digits_of(a, d + 30L)
    keep <- paste0(p$ip, substr(p$fr, 1L, d))
    f <- as.numeric(paste0("0.", substring(p$fr, d + 1L)))
    return(fmt_d(if (f >= 0.5) incr_decimal(keep) else keep))
  }
  if (a >= 1e15 && d > 3L) {
    unverified("a non-integer of 1E15 or more with more than 3 decimals is not verified against SAS")
  }
  if (a < 1) {
    ea <- floor(log10(a))
    if (10^(ea + 1) <= a) ea <- ea + 1 # log10 rounding at a power of ten
    if (d >= 15L && !(d <= 16L && ea <= -3L)) {
      unverified("15 or more decimals of a value below 0.01 in magnitude (16 or more below 1) are not verified against SAS")
    }
  }
  h <- as.numeric(paste0("5e-", d + 1L))
  t <- (a + h) * (1 + .Machine$double.eps)
  if (!is.finite(t)) t <- a + h # the largest doubles (never with decimals)
  p <- digits_of(t, d + 30L)
  ip <- p$ip
  if (a >= 1 || t >= 1) {
    keep_dec <- max(0L, 15L - nchar(ip))
  } else {
    et <- -attr(regexpr("^0*", p$fr), "match.length") - 1L
    keep_dec <- if (et == -1L) 14L else 14L - et
    if (et <= -2L && d >= 15L && et > -3L) unverified("the decimal exponent changes when the value is rounded")
  }
  k <- min(d, keep_dec)
  fr <- paste0(substr(p$fr, 1L, k), strrep("0", d - k))
  fmt_d(paste0(ip, fr))
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
#   sci_dec_text(v, k)  "-1.23E6", "5E-4"   (k mantissa decimals: 14 significant
#                                             digits, then half up to k decimals)
sci_dec_text <- function(v, k, edge = "abort") {
  a <- abs(v)
  if (k >= 13L) {
    # at most 14 significant digits (SAS 9.4: 32. of the largest double is 1.7976931348623E308)
    g <- e14_digits(NULL, v, k)
    ex <- g$ex
    keep <- paste0(g$keep, strrep("0", k - 13L))
    m <- paste0(substr(keep, 1L, 1L), ".", substring(keep, 2L))
    ms <- sub("\\.$", "", sub("0+$", "", m))
    return(list(text = paste0(if (v < 0) "-", ms, "E", if (ex < 0) "-", abs(ex)), sig = nchar(gsub(".", "", ms, fixed = TRUE)),
                mant = m, ex = ex))
  }
  # SAS 9.4 (round-15 scan, Ew. w = 7..19 at every binary exponent from 2^-61
  # to 2^60, 19,308 points): the value rounded to 14 significant digits
  # (correctly, from its exact binary value), then those digits rounded half
  # up to k mantissa decimals - a double rounding: 6.6464999999999970E-19 is
  # 6.6465000000000E-19 at 14 digits and 6.647E-19 in E10.
  s <- sprintf("%.13e", a)
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  round_k <- function(dg, ex) {
    keep <- substr(dg, 1L, k + 1L)
    if (substr(dg, k + 2L, k + 2L) >= "5") keep <- incr_decimal(keep)
    if (nchar(keep) > k + 1L) {
      keep <- substr(keep, 1L, k + 1L)
      ex <- ex + 1L
    }
    list(keep = keep, ex = ex)
  }
  r <- round_k(dg, ex)
  # an exact tie at the 15th digit (2^-21 = 4.76837158203125E-7): sprintf
  # breaks it to even, SAS's direction has no gold - fail loud when the two
  # directions give different digits
  s30 <- sprintf("%.30e", a)
  d30 <- gsub(".", "", sub("e.*$", "", s30), fixed = TRUE)
  if (substr(d30, 15L, 15L) == "5" && !grepl("[1-9]", substring(d30, 16L)) &&
      !grepl("[1-9]", sub("^[0-9]\\.[0-9]{14}", "", sub("e.*$", "", sprintf("%.800e", a))))) {
    ex30 <- as.integer(sub("^.*e", "", s30))
    lo <- substr(d30, 1L, 14L)
    hi <- incr_decimal(lo)
    ehi <- ex30
    if (nchar(hi) > 14L) {
      hi <- substr(hi, 1L, 14L)
      ehi <- ehi + 1L
    }
    r_lo <- round_k(lo, ex30)
    r_hi <- round_k(hi, ehi)
    if (!identical(r_lo, r_hi)) {
      if (identical(edge, "abort")) {
        abort_unsupported(paste0("E notation of ", format(v, digits = 17), " with ", k, " mantissa decimals: an exact tie ",
                                 "at the 15th significant digit is not determined by the gold standard"))
      }
      r <- r_hi
    }
  }
  keep <- r$keep
  ex <- r$ex
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
