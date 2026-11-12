# Moment statistics computed the way SAS 9.4 computes them (bit for bit).
#
# Fitted on SAS gold probes (tests/conformance/proc_stats/: ~1,700 groups of
# random size and magnitude, presented sorted, reversed and shuffled):
#
# * Sums are plain sequential double additions in observation order (PROC
#   MEANS, PROC SQL).  R's sum()/cumsum() accumulate in long double and differ.
# * PROC SQL keeps, per group, the first non-missing value c0 and the
#   sequential sums Sd = sum(x - c0) and Sdd = sum((x - c0)^2); SAS 9.4 is built
#   with fused multiply-add, so
#     MEAN = fma(n, c0, Sd) / n
#     CSS  = fma(-Sd, Sd / n, Sdd),  VAR = CSS / (n - 1),  STD = sqrt(VAR)
#     USS  = fma(c0, fma(n, c0, 2 * Sd), Sdd)
# * PROC MEANS: MEAN = SUM / n; the corrected sum of squares is accumulated as
#   the shifted sums sd = sum(x - c0), sdd = sum((x - c0)^2) about the first
#   value c0 and finalised with the reported (rounded) mean;
#   USS = CSS + SUM * MEAN; VAR = CSS / (n - 1); STDERR = sqrt(VAR / n);
#   CV = 100 * STD / MEAN.  The exact finalisation and the constant-data fuzz
#   of STD are reproduced in stats_means_css() / stats_means_std() below.
# The fused multiply-add is emulated exactly in double arithmetic (Dekker's
# product and Boldo-Melquiond rounding to odd); it is exact when no partial
# product overflows or underflows, which the callers guarantee by the range
# check stats_fma_range_ok().

eft_two_sum <- function(a, b) {
  s <- a + b
  bb <- s - a
  list(s = s, e = (a - (s - bb)) + (b - bb))
}

eft_split <- function(a) {
  t <- 134217729 * a # 2^27 + 1 (Veltkamp)
  hi <- t - (t - a)
  list(hi = hi, lo = a - hi)
}

eft_two_prod <- function(a, b) {
  p <- a * b
  A <- eft_split(a)
  B <- eft_split(b)
  list(p = p, e = ((A$hi * B$hi - p) + A$hi * B$lo + A$lo * B$hi) + A$lo * B$lo)
}

dbl_words <- function(x) matrix(readBin(writeBin(as.double(x), raw()), "integer", n = 2L * length(x)), nrow = 2L)
dbl_from_words <- function(w, n) readBin(writeBin(as.vector(w), raw()), "double", n = n)

# next double away from zero / toward zero (finite, non-zero x)
dbl_step <- function(x, away) {
  w <- dbl_words(x)
  lo <- w[1L, ]
  if (away) {
    carry <- lo == -1L
    w[1L, ] <- ifelse(carry, 0L, lo + 1L)
    w[2L, ] <- w[2L, ] + carry
  } else {
    borrow <- lo == 0L
    w[1L, ] <- ifelse(borrow, -1L, lo - 1L)
    w[2L, ] <- w[2L, ] - borrow
  }
  dbl_from_words(w, length(x))
}

# a + b rounded to odd
ro_sum <- function(a, b) {
  t <- eft_two_sum(a, b)
  s <- t$s
  fix <- which(!is.na(t$e) & t$e != 0 & is.finite(s))
  if (length(fix)) {
    even <- bitwAnd(dbl_words(s[fix])[1L, ], 1L) == 0L
    idx <- fix[even]
    if (length(idx)) {
      away <- (t$e[idx] > 0) == (s[idx] > 0)
      if (any(away)) s[idx[away]] <- dbl_step(s[idx[away]], TRUE)
      if (any(!away)) s[idx[!away]] <- dbl_step(s[idx[!away]], FALSE)
    }
  }
  s
}

# Exactly rounded a * b + c (vectorised).
stats_fma <- function(a, b, c) {
  n <- max(length(a), length(b), length(c))
  a <- rep_len(as.double(a), n)
  b <- rep_len(as.double(b), n)
  c <- rep_len(as.double(c), n)
  u <- eft_two_prod(a, b)
  t <- eft_two_sum(c, u$p)
  z <- t$s + ro_sum(t$e, u$e)
  bad <- is.na(z) | !is.finite(z)
  z[bad] <- (a * b + c)[bad]
  z
}

# Values for which the emulated FMA is exact in every use above.
stats_fma_range_ok <- function(x) {
  ax <- abs(x[!is.na(x)])
  all(ax == 0 | (ax >= 1e-140 & ax <= 1e140))
}

# Sequential double sum in observation order (no long double accumulator).
stats_seq_sum <- function(x) {
  n <- length(x)
  if (n == 0L) return(0)
  if (n == 1L) return(x[1])
  as.numeric(stats::filter(x, 1, method = "recursive"))[n]
}

# Shifted sums of the non-missing values `x` (in observation order).
stats_shift_sums <- function(x) {
  n <- length(x)
  c0 <- x[1]
  d <- x - c0
  list(n = n, c0 = c0, sum = stats_seq_sum(x), sd = stats_seq_sum(d), sdd = stats_seq_sum(d * d))
}

# PROC MEANS corrected sum of squares of one cell, as SAS 9.4 computes it.
#
# SAS accumulates, in observation order and about the first value c0, the
# shifted sums sd = sum(x - c0) and sdd = sum((x - c0)^2); at the end it has the
# mean = sum / n and
#
#   md  = mean - c0
#   CSS = sdd                            if |md| <= 1E-14 * |mean|
#       = sdd - md * (2 * sd - n * md)   otherwise
#
# The first form is not a rounding detail: for data that are constant to within
# 1E-14 relative SAS returns the *uncorrected* sum of squares (the mean is taken
# to be the first value).  The second form is sum((x - mean)^2) written in the
# shifted sums with the reported, rounded mean; its rounding differs from both
# sum(x - c0)^2 - sd^2/n and sum((x - mean)^2) in the last bits.
#
# Verified bit for bit on ~3,300 SAS 9.4 groups (tests/conformance/proc_stats/:
# the means/sql moment probes plus probes of dyadic deviations, translations by
# powers of two, magnitude ladders 1E-8..1E8, constants, outliers and missing
# values; see the proc_stats registry row for CSS).
stats_means_css <- function(x) {
  n <- length(x)
  if (n == 0L) return(NA_real_)
  c0 <- x[1L]
  mean <- stats_seq_sum(x) / n
  d <- x - c0
  sd <- stats_seq_sum(d)
  sdd <- stats_seq_sum(d * d)
  md <- mean - c0
  if (abs(md) <= 1e-14 * abs(mean)) sdd else sdd - md * (2 * sd - n * md)
}

# PROC MEANS standard deviation: sqrt(VAR) except for constant data, where SAS
# fuzzes the result to 0.  The fuzz is on the deviation from the mean: SAS
# returns 0 when VAR <= 2^-86 * MEAN^2, i.e. when STD <= 2^-43 * |MEAN|.
stats_means_std <- function(var, mean) {
  if (is.na(var)) return(NA_real_)
  std <- sqrt(var)
  if (!is.na(mean) && is.finite(mean) && std <= 2^-43 * abs(mean)) 0 else std
}

# PROC SQL moment statistics of one group (non-missing values in order).
stats_sql_moment <- function(fn, x) {
  n <- length(x)
  if (n == 0L) return(NA_real_)
  if (fn == "sum") return(stats_seq_sum(x))
  s <- stats_shift_sums(x)
  css <- stats_fma(-s$sd, s$sd / n, s$sdd)
  mean <- stats_fma(n, s$c0, s$sd) / n
  var <- if (n > 1) css / (n - 1) else NA_real_
  switch(fn,
    avg = , mean = mean,
    css = css,
    uss = stats_fma(s$c0, stats_fma(n, s$c0, 2 * s$sd), s$sdd),
    var = var,
    std = sqrt(var),
    # not reproduced exactly (SAS uses a different, cancellation-prone formula):
    # `approximate` in the PROC statistics registry
    stderr = sqrt(var) / sqrt(n),
    cv = if (n > 1 && mean != 0) 100 * sqrt(var) / mean else NA_real_,
    abort_unsupported(paste0("summary function ", toupper(fn), "()"))
  )
}

# Can an order-dependent statistic be computed without knowing the order in
# which SAS reads the rows?  Integer values whose sums (of values, squares) are
# exact give the same SUM, MEAN and USS in every order.
stats_order_free <- function(fn, x) {
  if (!fn %in% c("sum", "avg", "mean", "uss")) return(FALSE)
  if (!all(x == trunc(x))) return(FALSE)
  if (sum(abs(x)) >= 2^53) return(FALSE)
  if (fn == "uss" && sum(x * x) >= 2^53) return(FALSE)
  TRUE
}
