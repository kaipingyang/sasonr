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
# * PROC MEANS / SUMMARY: see stats_means_moments() (round 12: every moment
#   statistic except SKEWNESS bit-identical on the proc_stats/means_* probes).
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
# KURTOSIS, shared by the DATA step functions and the PROC SQL summary
# functions (PROC MEANS has its own weighted kernel, stats_means_moments()).  `m` is a matrix with one group per row
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
# VAR = CSS / div (div = N - 1 unless the caller passes its VARDEF divisor),
# STD = sqrt(VAR).  DATA step SKEWNESS is exact (up to 10 arguments), KURTOSIS
# approximate (the registry says so).  The emulated fma is exact only for
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

stats_moments <- function(m, method = c("datastep", "sql"), div = NULL) {
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
    mean <- stats_fma(n, c0, sd) / n
    uss <- stats_fma(c0, stats_fma(n, c0, 2 * sd), sdd)
  }
  div <- div %||% (n - 1)
  var <- ifelse(div > 0, css / div, NA_real_)
  std <- sqrt(var)
  stderr <- if (method == "datastep") ifelse(n > 1, sqrt(css / (n * (n - 1))), NA_real_) else ifelse(n > 1, std / sqrt(n), NA_real_)
  cv <- ifelse(!is.na(std) & mean != 0, if (method == "datastep") std / mean * 100 else 100 * std / mean, NA_real_)
  # a zero CV of negative values is +0 in the DATA step (fn_probe/STD row 603)
  if (method == "datastep") cv[!is.na(cv) & cv == 0] <- 0
  # skewness and kurtosis (VARDEF=DF), DATA step (fn_probe/STD, at most 10
  # arguments): r = (N - 1) / CSS; SKEWNESS = N A3 / ((N - 1)(N - 2)) with A3
  # the sum of fma(z z, z, .) over z = d sqrt(r) (749 of 749 rows);
  # KURTOSIS = fma(N (N + 1) / ((N - 1)(N - 2)(N - 3)), A4, -3 (N - 1)**2 /
  # ((N - 2)(N - 3))) with A4 the sum of fma(t, t, .) over t = d (d r)
  # (487 of 523 rows: approximate)
  ok <- !is.na(std) & std > 0
  r <- (n - 1) / css
  dd <- if (method == "datastep") d else m - mean
  z <- dd * sqrt(r)
  a3 <- rep(0, nrow(m))
  a4 <- rep(0, nrow(m))
  for (j in seq_len(ncol(m))) {
    zj <- z[, j]
    tj <- dd[, j] * (dd[, j] * r)
    use <- !is.na(zj) & ok
    if (any(use)) {
      a3[use] <- stats_fma(zj[use] * zj[use], zj[use], a3[use])
      a4[use] <- stats_fma(tj[use], tj[use], a4[use])
    }
  }
  skew <- ifelse(ok & n > 2, n * a3 / ((n - 1) * (n - 2)), NA_real_)
  kurt <- ifelse(ok & n > 3, stats_fma(n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)), a4, -3 * (n - 1)^2 / ((n - 2) * (n - 3))),
                 NA_real_)
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

# ---- PROC MEANS / SUMMARY --------------------------------------------------------
#
# The moment sums of one cell, the way SAS 9.4 PROC MEANS accumulates them
# (fitted on the real-SAS probes tests/conformance/proc_stats/means_* : integer
# data with exact sums, the same patterns at 14 offsets, decimals with missing
# values, FREQ, WEIGHT, VARDEF=, CLASS x BY, n up to 100,000).
# `x` are the values of the observations that take part (non-missing value,
# non-missing weight, frequency >= 1) in observation order and `wt` their
# weights (frequency times the weight, negative weights as 0).  With
# c0 = x[1] (the first such observation, also when its weight is 0),
# d = x - c0 and per observation wd = wt * d, t2 = wd * d, t3 = t2 * d:
#   W  = sequential sum of wt          S  = fma(wt, x, S)  (SUM)
#   S1 = sequential sum of wd          S2 = sequential sum of t2
#   S3 = sequential sum of t3          S4 = fma(t3, d, S4)
# MEAN = S / W and q = MEAN - c0 (the shift of the provisional to the final
# mean; the central sums are a polynomial in q evaluated by Horner's rule
# with SAS's fused multiply-adds):
#   CSS = fma(q, W q - 2 S1, S2)       (0 when S2 = 0; missing when negative)
#   M3  = fma(q, fma(q, 3 S1 - W q, -3 S2), S3)
#   M4  = fma(q, q fma(q, W q - 4 S1, 6 S2) - 4 S3, S4)
#   USS = fma(S, MEAN, CSS)
# The emulated fma is exact for magnitudes 0 or 1E-140..1E140
# (stats_fma_range_ok()); the caller checks it.
# Sequential acc = fma(a[i], b[i], acc) from acc = 0, exactly rounded at every
# step: the products are split exactly (Dekker) once, vectorised; the loop
# adds p[i] + e[i] to the accumulator with a two-sum and the rounding-to-odd
# correction of stats_fma() (scalar arithmetic only).
stats_fma_accumulate <- function(a, b) {
  n <- length(a)
  if (n == 0L) return(0)
  u <- eft_two_prod(as.double(a), as.double(b))
  p <- u$p
  e <- u$e
  if (any(!is.finite(p) | is.na(e))) {
    acc <- 0
    for (i in seq_len(n)) acc <- stats_fma(a[i], b[i], acc)
    return(acc)
  }
  acc <- 0
  for (i in seq_len(n)) {
    pi <- p[i]
    s <- acc + pi
    bb <- s - acc
    err <- (acc - (s - bb)) + (pi - bb)
    ei <- e[i]
    v <- err + ei
    if (v != 0) {
      vb <- v - err
      r <- (err - (v - vb)) + (ei - vb)
      if (r != 0) v <- ro_sum(err, ei)
    }
    acc <- s + v
  }
  acc
}

stats_means_moments <- function(x, wt, higher = TRUE) {
  W <- stats_seq_sum(wt)
  S <- if (all(wt == 1)) stats_seq_sum(x) else stats_fma_accumulate(wt, x)
  out <- list(W = W, sum = S)
  if (!(W > 0)) return(out)
  c0 <- x[1]
  d <- x - c0
  wd <- wt * d
  t2 <- wd * d
  t3 <- t2 * d
  s1 <- stats_seq_sum(wd)
  s2 <- stats_seq_sum(t2)
  mean <- S / W
  q <- mean - c0
  css <- if (s2 == 0) 0 else stats_fma(q, W * q - 2 * s1, s2)
  if (!is.na(css) && css < 0) css <- NA_real_
  out <- c(out, list(mean = mean, css = css, uss = stats_fma(S, mean, css)))
  if (higher) {
    s3 <- stats_seq_sum(t3)
    s4 <- stats_fma_accumulate(t3, d)
    out$m3 <- stats_fma(q, stats_fma(q, 3 * s1 - W * q, -3 * s2), s3)
    out$m4 <- stats_fma(q, q * stats_fma(q, W * q - 4 * s1, 6 * s2) - 4 * s3, s4)
  }
  out
}
