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
# * PROC MEANS: MEAN = SUM / n; USS = fma(SUM, MEAN, CSS); VAR, STD, STDERR and
#   CV follow from CSS.  The CSS accumulation itself is not reproduced exactly
#   (about 88 % of random groups), so the CSS family is `approximate` in the
#   PROC statistics registry.
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

# PROC SQL moment statistics of one group (non-missing values in order):
# the moment kernel with the PROC SQL derivation.
stats_sql_moment <- function(fn, x) {
  if (length(x) == 0L) return(NA_real_)
  if (fn == "sum") return(stats_seq_sum(x))
  k <- stats_moments(x, "sql")
  switch(fn,
    avg = , mean = k$mean,
    css = k$css,
    uss = k$uss,
    var = k$var,
    std = k$std,
    # not reproduced exactly (SAS uses a different, cancellation-prone formula):
    # `approximate` in the PROC statistics registry
    stderr = k$stderr,
    cv = k$cv,
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

# ---- the moment kernel -------------------------------------------------------------
#
# One kernel for N, SUM, MEAN, CSS, USS, VAR, STD, STDERR, CV, SKEWNESS and
# KURTOSIS, shared by the DATA step functions, PROC MEANS (no WEIGHT / FREQ)
# and the PROC SQL summary functions.  `m` is a matrix with one group per row
# (the values in observation / argument order, NA = missing); every
# accumulation is a sequential double operation over the non-missing values
# of a row, vectorised over the rows.  The three callers differ only in how
# SAS 9.4 derives the moments (`method`), each fitted on SAS gold probes:
#   datastep  MEAN = SUM / N; CSS = sum of fma(d, d, .) over d = x - MEAN;
#             USS = sequential sum of x * x; STDERR = sqrt(CSS / (N (N - 1)));
#             CV = STD / MEAN * 100 (fn_probe/STD: 750 of 750 rows with at most
#             10 non-missing arguments; with 11 or 12 some CSS differ, so the
#             DATA step functions run only up to 10 arguments)
#   sql       shifted sums: MEAN = fma(N, c0, Sd) / N, CSS = fma(-Sd, Sd / N, Sdd),
#             USS = fma(c0, fma(N, c0, 2 Sd), Sdd) (proc_stats/ probes)
#   means     MEAN = SUM / N, CSS from the shifted sums as for sql,
#             USS = fma(SUM, MEAN, CSS) (proc_stats/ probes; CSS approximate)
# VAR = CSS / div (div = N - 1 unless the caller passes its VARDEF divisor),
# STD = sqrt(VAR).  SKEWNESS and KURTOSIS (VARDEF=DF form) are approximate in
# every caller (the registries say so).  The emulated fma is exact only for
# magnitudes 0 or 1E-140..1E140 (stats_fma_range_ok()); callers check it.

# Sequential double sum of the non-missing values of every row (NA: none).
stats_row_seq_sum <- function(m) {
  if (nrow(m) == 1L) {
    v <- m[1L, !is.na(m[1L, ])]
    return(if (length(v)) stats_seq_sum(v) else NA_real_)
  }
  s <- rep(NA_real_, nrow(m))
  for (j in seq_len(ncol(m))) {
    x <- m[, j]
    ok <- !is.na(x)
    s[ok] <- ifelse(is.na(s[ok]), x[ok], s[ok] + x[ok])
  }
  s
}

# Sum of fma(d, d, .) over the non-missing values of every row (from 0).
stats_row_fma_sq <- function(d) {
  acc <- rep(0, nrow(d))
  for (j in seq_len(ncol(d))) {
    x <- d[, j]
    ok <- !is.na(x)
    if (any(ok)) acc[ok] <- stats_fma(x[ok], x[ok], acc[ok])
  }
  acc
}

stats_moments <- function(m, method = c("datastep", "sql", "means"), div = NULL) {
  method <- match.arg(method)
  if (!is.matrix(m)) m <- matrix(as.numeric(m), nrow = 1L)
  n <- rowSums(!is.na(m))
  none <- n == 0
  sum <- stats_row_seq_sum(m)
  if (method == "datastep") {
    mean <- sum / n
    d <- m - mean
    css <- stats_row_fma_sq(d)
    uss <- stats_row_seq_sum(m * m)
  } else {
    c0 <- apply(m, 1L, function(r) r[!is.na(r)][1L])
    d <- m - c0
    sd <- stats_row_seq_sum(d)
    sdd <- stats_row_seq_sum(d * d)
    css <- stats_fma(-sd, sd / n, sdd)
    if (method == "sql") {
      mean <- stats_fma(n, c0, sd) / n
      uss <- stats_fma(c0, stats_fma(n, c0, 2 * sd), sdd)
    } else {
      mean <- sum / n
      uss <- stats_fma(sum, mean, css)
    }
  }
  div <- div %||% (n - 1)
  var <- ifelse(div > 0, css / div, NA_real_)
  std <- sqrt(var)
  stderr <- if (method == "datastep") ifelse(n > 1, sqrt(css / (n * (n - 1))), NA_real_) else ifelse(n > 1, std / sqrt(n), NA_real_)
  cv <- ifelse(!is.na(std) & mean != 0, if (method == "datastep") std / mean * 100 else 100 * std / mean, NA_real_)
  # a zero CV of negative values is +0 in the DATA step (fn_probe/STD row 603)
  if (method == "datastep") cv[!is.na(cv) & cv == 0] <- 0
  # skewness and kurtosis (VARDEF=DF) from the standardised deviations
  z <- (m - mean) / std
  sz3 <- rowSums(z^3, na.rm = TRUE)
  sz4 <- rowSums(z^4, na.rm = TRUE)
  ok <- !is.na(std) & std > 0
  skew <- ifelse(ok & n > 2, n / ((n - 1) * (n - 2)) * sz3, NA_real_)
  kurt <- ifelse(ok & n > 3, n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sz4 - 3 * (n - 1)^2 / ((n - 2) * (n - 3)), NA_real_)
  out <- list(n = n, sum = sum, mean = mean, css = css, uss = uss, var = var, std = std, stderr = stderr, cv = cv,
              skew = skew, kurt = kurt)
  for (k in setdiff(names(out), "n")) out[[k]][none] <- NA_real_
  lapply(out, stats_overflow_missing)
}

# An overflow (non-finite result) is a missing value in SAS: the one place
# that maps it (moment kernel, PROC SQL summary functions).
stats_overflow_missing <- function(v) {
  v[!is.finite(v)] <- NA_real_
  v
}
