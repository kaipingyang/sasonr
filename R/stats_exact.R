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
# * PROC MEANS / SUMMARY: see stats_means_moments() (every moment statistic
#   except SKEWNESS bit-identical on the proc_stats/means_* probes).
# The fused multiply-add is emulated exactly in double arithmetic (Dekker's
# product and Boldo-Melquiond rounding to odd); it is exact when no partial
# product overflows or underflows: stats_fma() checks its arguments and fails
# loud outside that range (stats_fma_check()).

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

# Range of the emulated fused multiply-add: Dekker's product a * b = p + e is
# exact when both factors are below 2^995 in magnitude (the Veltkamp split
# does not overflow) and a non-zero product is at least 2^-960 (the error
# term and the products of the split halves stay normal).
fma_max_factor <- 2^995
fma_min_product <- 2^-960

# Fail loud: an intermediate value of the moment arithmetic left the range in
# which sasonr reproduces SAS 9.4 exactly (SAS's result is not verified there).
stats_range_fail <- function(what) {
  abort_unsupported(paste0("moment statistics: ", what,
                           " (outside the range in which SAS 9.4's arithmetic is reproduced; not verified)"))
}

# Missing values (NA, and the special missing values .A-.Z ._, which are NaN
# payloads) are SAS missing values.  An infinite value is an arithmetic
# accident (overflow) and must never become one silently; where the inputs
# cannot be missing (PROC MEANS) a NaN is one too (`nan = TRUE`).
stats_finite_check <- function(x, what, nan = FALSE) {
  bad <- is.infinite(x) | (nan & is.nan(x))
  if (any(bad)) stats_range_fail(paste0(what, " overflowed or is undefined (NaN)"))
  invisible(x)
}

# a * b where neither factor is missing; fails loud when a product of non-zero
# factors overflows or is not a normal double (underflow).
stats_mul <- function(a, b) {
  p <- a * b
  stats_finite_check(c(a, b, p), "a product")
  nz <- !is.na(p) & !is.na(a) & !is.na(b) & a != 0 & b != 0
  if (any(nz & abs(p) < 2^-1022)) stats_range_fail("a product underflowed")
  p
}

# Exactly rounded c + (p + e), where p + e is the exact product split by
# eft_two_prod(): the shared core of stats_fma() and stats_fma_accumulate().
fma_add_split <- function(p, e, c) {
  t <- eft_two_sum(c, p)
  t$s + ro_sum(t$e, e)
}

# The arguments of an emulated fma are inside its exact range (else fail loud).
stats_fma_check <- function(a, b, c) {
  stats_finite_check(c(a, b, c), "an argument of a fused multiply-add")
  nz <- a != 0 & b != 0
  if (any(nz)) {
    aa <- abs(a[nz])
    bb <- abs(b[nz])
    p <- aa * bb
    if (any(aa >= fma_max_factor | bb >= fma_max_factor | !is.finite(p) | p < fma_min_product)) {
      stats_range_fail("a product inside a fused multiply-add overflowed or underflowed")
    }
  }
  invisible()
}

# Exactly rounded a * b + c (vectorised).  A missing argument gives a missing
# result; arguments or products outside the exact range, or a non-finite
# result, fail loud.
stats_fma <- function(a, b, c) {
  n <- max(length(a), length(b), length(c))
  a <- rep_len(as.double(a), n)
  b <- rep_len(as.double(b), n)
  c <- rep_len(as.double(c), n)
  out <- rep(NA_real_, n)
  ok <- !(is.na(a) | is.na(b) | is.na(c))
  if (!any(ok)) return(out)
  a <- a[ok]
  b <- b[ok]
  c <- c[ok]
  stats_fma_check(a, b, c)
  u <- eft_two_prod(a, b)
  out[ok] <- stats_finite_check(fma_add_split(u$p, u$e, c), "a fused multiply-add")
  out
}

# Sequential acc = fma(a[i], b[i], acc) from acc = 0 (PROC MEANS sums): the
# same arithmetic as stats_fma() step by step (the products are split once,
# vectorised; tests/testthat/test-stats-exact.R checks the equality).
stats_fma_accumulate <- function(a, b) {
  n <- length(a)
  if (n == 0L) return(0)
  a <- as.double(a)
  b <- as.double(b)
  stats_fma_check(a, b, 0)
  u <- eft_two_prod(a, b)
  p <- u$p
  e <- u$e
  acc <- 0
  for (i in seq_len(n)) acc <- fma_add_split(p[i], e[i], acc)
  stats_finite_check(acc, "a fused multiply-add sum")
}

# Values for which the moment statistics are verified (magnitudes 0 or
# 1E-140..1E140; the guards above catch what still leaves the exact range).
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
# KURTOSIS, shared by the DATA step functions, the PROC SQL summary functions
# and PROC MEANS / SUMMARY.  The callers differ only in how SAS 9.4 derives the
# moments (`method`), each fitted on SAS gold probes:
#   datastep  `m` is a matrix with one group per row (the arguments in order,
#             NA = missing), vectorised over the rows.  MEAN = SUM / N;
#             CSS = sum of fma(d, d, .) over d = x - MEAN; USS = sequential
#             sum of x * x; STDERR = sqrt(CSS / (N (N - 1))); CV = STD / MEAN * 100
#             (fn_probe/STD and fn_probe/SKEWNESS: every row with at most 9
#             non-missing arguments; with 10 to 12 some CSS differ in the last
#             bit, so the CSS-based DATA step functions run up to 9 values).
#   sql       `m` as for datastep.  Shifted sums: MEAN = fma(N, c0, Sd) / N,
#             CSS = fma(-Sd, Sd / N, Sdd), USS = fma(c0, fma(N, c0, 2 Sd), Sdd)
#             (proc_stats/ probes).
#   means     `m` the values of one PROC MEANS cell, see stats_means_moments().
# VAR = CSS / div (div = N - 1 unless the caller passes its divisor),
# STD = sqrt(VAR).  The emulated fma is exact only inside its range
# (stats_fma_check()); every intermediate product and sum is checked, and a
# non-finite value fails loud instead of becoming a missing value.

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

stats_moments <- function(m, method = c("datastep", "sql", "means"), div = NULL, ...) {
  method <- match.arg(method)
  if (method == "means") return(stats_means_moments(m, ...))
  if (!is.matrix(m)) m <- matrix(as.numeric(m), nrow = 1L)
  stats_finite_check(m, "an argument")
  n <- rowSums(!is.na(m))
  none <- n == 0
  sum <- stats_row_seq_sum(m)
  if (method == "datastep") {
    mean <- sum / n
    d <- m - mean
    css <- stats_row_fma_sq(d)
    # plain squares: an underflowed square adds 0 as in SAS (fn_matrix/USS uss(1e-300, 2, 3))
    uss <- stats_row_seq_sum(m * m)
  } else {
    c0 <- apply(m, 1L, function(r) r[!is.na(r)][1L])
    d <- m - c0
    sd <- stats_row_seq_sum(d)
    sdd <- stats_row_seq_sum(stats_mul(d, d))
    css <- stats_fma(-sd, sd / n, sdd)
    # a negative CSS (cancellation) has no SAS gold for PROC SQL
    if (any(!is.na(css) & css < 0)) stats_range_fail("PROC SQL CSS of nearly constant values came out negative")
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
  # skewness and kurtosis (VARDEF=DF), DATA step (fn_probe/STD, at most 9
  # arguments): r = (N - 1) / CSS; SKEWNESS = N A3 / ((N - 1)(N - 2)) with A3
  # the sum of fma(z z, z, .) over z = d sqrt(r) (fn_probe/STD 749 of 749 rows;
  # fn_probe/SKEWNESS see sas_f_skewness());
  # KURTOSIS = fma(N (N + 1) / ((N - 1)(N - 2)(N - 3)), A4, -3 (N - 1)**2 /
  # ((N - 2)(N - 3))) with A4 the sum of fma(t, t, .) over t = d (d r)
  # (487 of 523 rows: approximate).  Only the DATA step functions use them.
  skew <- kurt <- rep(NA_real_, nrow(m))
  ok <- method == "datastep" & !is.na(std) & std > 0
  if (any(ok)) {
    r <- (n[ok] - 1) / css[ok]
    dd <- d[ok, , drop = FALSE]
    z <- dd * sqrt(r)
    a3 <- a4 <- rep(0, sum(ok))
    for (j in seq_len(ncol(m))) {
      zj <- z[, j]
      use <- !is.na(zj)
      if (any(use)) {
        tj <- stats_mul(dd[use, j], stats_mul(dd[use, j], r[use]))
        a3[use] <- stats_fma(stats_mul(zj[use], zj[use]), zj[use], a3[use])
        a4[use] <- stats_fma(tj, tj, a4[use])
      }
    }
    nn <- n[ok]
    skew[ok] <- ifelse(nn > 2, nn * a3 / ((nn - 1) * (nn - 2)), NA_real_)
    kurt[ok] <- ifelse(nn > 3, stats_fma(nn * (nn + 1) / ((nn - 1) * (nn - 2) * (nn - 3)), a4,
                                         -3 * (nn - 1)^2 / ((nn - 2) * (nn - 3))), NA_real_)
  }
  out <- list(n = n, sum = sum, mean = mean, css = css, uss = uss, var = var, std = std, stderr = stderr, cv = cv,
              skew = skew, kurt = kurt)
  for (k in setdiff(names(out), "n")) out[[k]][none] <- NA_real_
  for (k in names(out)) stats_finite_check(out[[k]], toupper(k))
  out
}

# ---- PROC MEANS / SUMMARY --------------------------------------------------------
#
# The moment statistics of one cell, the way SAS 9.4 PROC MEANS accumulates
# them (fitted on the real-SAS probes tests/conformance/proc_stats/means_* :
# integer data with exact sums, the same patterns at 14 offsets, decimals with
# missing values, FREQ, WEIGHT, VARDEF=, CLASS x BY, n up to 100,000, and
# means_css_neg_probe: zero-weight first observations, near-constant data).
# `x` are the values of the observations that take part (non-missing value,
# non-missing weight, frequency >= 1) in observation order, `freq` their
# frequencies (NULL: no FREQ statement) and `weight` their weights (NULL: no
# WEIGHT statement; negative weights as 0); wt = freq * weight.  With
# c0 = x[1] (the first such observation, also when its weight is 0),
# d = x - c0 and per observation wd = wt * d, t2 = wd * d, t3 = t2 * d:
#   W  = sequential sum of wt          S  = fma(wt, x, S)  (SUM)
#   S1 = sequential sum of wd          S2 = sequential sum of t2
#   S3 = sequential sum of t3          S4 = fma(t3, d, S4)
# MEAN = S / W and q = MEAN - c0 (the shift of the provisional to the final
# mean), set to 0 when |q| <= 9.9751031807342009E-15 |c0| (SAS keeps it from
# 1.0005986172898007E-14 on; in between fails loud): means_css_neg_probe, 112
# of 260 near-constant groups.  The central sums are a polynomial in q
# evaluated by Horner's rule with SAS's fused multiply-adds:
#   CSS = fma(q, W q - 2 S1, S2)       (0 when S2 = 0; missing when negative:
#                                       means_css_neg_probe 86 of 86 groups,
#                                       with USS, VAR, STD, STDERR, CV missing)
#   M3  = fma(q, fma(q, 3 S1 - W q, -3 S2), S3)
#   M4  = fma(q, q fma(q, W q - 4 S1, 6 S2) - 4 S3, S4)
#   USS = fma(S, MEAN, CSS)
# `need`: the statistics the step computes (proc_means.R names).
stats_means_moments <- function(x, freq = NULL, weight = NULL, vardef = "df", need = character()) {
  if (sas_target_version() == "TS1M2") return(stats_means_moments_ts1m2(x, freq, weight, vardef, need))
  f <- freq %||% rep(1, length(x))
  w <- weight %||% rep(1, length(x))
  wt <- if (is.null(weight)) f else stats_mul(w, f)
  css_family <- c("css", "uss", "var", "std", "stderr", "cv", "skew", "kurt", "t", "prt", "lclm", "uclm")
  if ((!stats_fma_range_ok(x) || !stats_fma_range_ok(wt)) && any(need %in% c("sum", "mean", "sumwgt", css_family))) {
    abort_unsupported("PROC MEANS moment statistics of values or weights beyond 1E140 or below 1E-140 in magnitude (not verified)")
  }
  n <- sum(f)
  W <- stats_seq_sum(wt)
  S <- if (all(wt == 1)) stats_seq_sum(x) else stats_fma_accumulate(wt, x)
  out <- list(n = n, W = W, sum = S, mean = NA_real_, css = NA_real_, uss = NA_real_, var = NA_real_, std = NA_real_,
              stderr = NA_real_, cv = NA_real_, skew = NA_real_, kurt = NA_real_)
  if (!(W > 0)) return(stats_means_checked(out))
  mean <- S / W
  out$mean <- mean
  if (!any(need %in% css_family)) return(stats_means_checked(out))
  c0 <- x[1]
  d <- x - c0
  wd <- stats_mul(wt, d)
  t2 <- stats_mul(wd, d)
  s1 <- stats_seq_sum(wd)
  s2 <- stats_seq_sum(t2)
  q <- mean - c0
  # (with S2 = 0 the CSS is 0 and q does not matter)
  if (s2 != 0 && q != 0 && c0 != 0) {
    rq <- abs(q) / abs(c0)
    if (rq <= 9.9751031807342009e-15) {
      if (any(wt != 1)) {
        abort_unsupported("PROC MEANS moment statistics of nearly constant weighted data (SAS's rounding of the mean shift verified only without weights)")
      }
      q <- 0
    } else if (rq < 1.0005986172898007e-14) {
      abort_unsupported("PROC MEANS moment statistics of nearly constant data (SAS's rounding of the mean shift between 9.98E-15 and 1.0006E-14 relative not determined)")
    }
  }
  css <- if (s2 == 0) 0 else stats_fma(q, stats_mul(W, q) - 2 * s1, s2)
  if (css < 0) css <- NA_real_
  out$css <- css
  out$uss <- stats_fma(S, mean, css)
  div <- switch(vardef, df = n - 1, n = n, wdf = W - 1, weight = W)
  var <- if (div > 0 && !is.na(css)) css / div else NA_real_
  std <- sqrt(var)
  cut <- FALSE
  if (!is.na(var) && var > 0 && mean != 0) {
    ratio <- var / stats_mul(mean, mean)
    if (ratio < STD_CUT_MIN) {
      if (any(need %in% c("std", "cv", "stderr", "t", "prt", "lclm", "uclm", "skew", "kurt"))) {
        abort_unsupported(sprintf("PROC MEANS STD of a variance below %s times the squared mean (smaller than any SAS probe; not verified)",
                                  format(STD_CUT_MIN, digits = 4)))
      }
    }
    if (ratio <= STD_CUT_ZERO) {
      std <- 0
      cut <- TRUE
    } else if (ratio < STD_CUT_KEEP) {
      if (any(need %in% c("std", "cv", "skew", "kurt"))) {
        abort_unsupported(sprintf("PROC MEANS STD of a variance between %s and %s times the squared mean (SAS's cut-off of STD to 0 not determined)",
                                  format(STD_CUT_ZERO, digits = 4), format(STD_CUT_KEEP, digits = 4)))
      }
    }
  }
  if (cut) stats_std_cut_check(need, freq, weight, vardef)
  out$var <- var
  out$std <- std
  out$cv <- if (!is.na(std) && mean != 0) 100 * std / mean else NA_real_
  # a zero CV of a negative mean is +0 in SAS
  if (!is.na(out$cv) && out$cv == 0) out$cv <- 0
  if (vardef == "df" && n > 1 && !is.na(var)) out$stderr <- sqrt(var / W)
  if (any(need %in% c("skew", "kurt")) && !is.na(std) && std > 0) {
    if (is.null(weight) && vardef %in% c("df", "n")) {
      t3 <- stats_mul(t2, d)
      s3 <- stats_seq_sum(t3)
      s4 <- stats_fma_accumulate(t3, d)
      Wq <- stats_mul(W, q)
      m3 <- stats_fma(q, stats_fma(q, 3 * s1 - Wq, -3 * s2), s3)
      m4 <- stats_fma(q, stats_mul(q, stats_fma(q, Wq - 4 * s1, 6 * s2)) - 4 * s3, s4)
      stats_finite_check(c(s3, s4, m3, m4), "a central moment sum")
      vs <- stats_mul(var, std)
      vv <- stats_mul(var, var)
      if (vardef == "df") {
        # SKEWNESS approximate (registry); KURTOSIS bit-identical
        if (n > 2) out$skew <- n * m3 / ((n - 1) * (n - 2)) / vs
        if (n > 3) out$kurt <- stats_fma(n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)), m4 / vv, -3 * (n - 1)^2 / ((n - 2) * (n - 3)))
      } else {
        # VARDEF=N (proc_stats/means_css_int_probe m_vn: 600 of 600)
        out$skew <- m3 / stats_mul(n, vs)
        out$kurt <- m4 / vv / n - 3
      }
    } else if (vardef %in% c("df", "n")) {
      # WEIGHT: SAS's documented formulas, not reproduced exactly (the
      # registry has no verified SKEWNESS / KURTOSIS with WEIGHT: gated)
      z <- (x - mean) / std
      s3w <- sum(f * w^1.5 * z^3)
      s4w <- sum(f * w^2 * z^4)
      if (vardef == "df") {
        if (n > 2) out$skew <- n / ((n - 1) * (n - 2)) * s3w
        if (n > 3) out$kurt <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * s4w - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
      } else {
        out$skew <- s3w / n
        out$kurt <- s4w / n - 3
      }
    } else {
      abort_unsupported(paste0("SKEWNESS/KURTOSIS with VARDEF=", toupper(vardef)))
    }
  }
  stats_means_checked(out)
}

# PROC MEANS moments of SAS 9.4 TS1M2: the same shifted sums about c0 = x[1],
# closed without fused multiply-adds (TS1M7 above uses them):
#   MEAN = SUM / n,  q = MEAN - c0,  CSS = S2 - q (2 S1 - n q),
#   USS = CSS + SUM * MEAN,  VAR = CSS / (n - 1 | n),  STDERR = sqrt(VAR / n)
# Gold: cases/ts1m2/rounding/means_accum (exact doubles in two orders: SUM,
# MEAN, CSS, VAR, STD, USS bit-identical; the TS1M7 fma form differs in USS)
# and cases/ts1m2/proc/zero_sign.  Verified only without WEIGHT / FREQ, with
# VARDEF=DF or N, outside the nearly-constant region (|q| < 1.01E-14 |c0|)
# and the STD cut-off region (VAR <= 1.4312245245430491E-26 MEAN**2):
# everything else fails loud, as do SKEWNESS and KURTOSIS (no TS1M2 gold).
stats_means_moments_ts1m2 <- function(x, freq, weight, vardef, need) {
  css_family <- c("css", "uss", "var", "std", "stderr", "cv", "skew", "kurt", "t", "prt", "lclm", "uclm")
  moments <- c("sum", "mean", "sumwgt", css_family)
  why <- NULL
  if (!is.null(weight) || !is.null(freq)) why <- "a WEIGHT or FREQ statement"
  else if (!vardef %in% c("df", "n")) why <- paste0("VARDEF=", toupper(vardef))
  else if (any(need %in% c("skew", "kurt"))) why <- "SKEWNESS / KURTOSIS"
  if (!is.null(why) && any(need %in% moments)) {
    abort_unsupported(paste0("PROC MEANS moment statistics with ", why, " for SAS 9.4 TS1M2 (no TS1M2 gold)"))
  }
  if (!stats_fma_range_ok(x) && any(need %in% moments)) {
    abort_unsupported("PROC MEANS moment statistics of values beyond 1E140 or below 1E-140 in magnitude (not verified)")
  }
  f <- freq %||% rep(1, length(x))
  wt <- if (is.null(weight)) f else weight * f
  n <- sum(f)
  W <- stats_seq_sum(wt)
  S <- stats_seq_sum(x)
  out <- list(n = n, W = W, sum = S, mean = NA_real_, css = NA_real_, uss = NA_real_, var = NA_real_, std = NA_real_,
              stderr = NA_real_, cv = NA_real_, skew = NA_real_, kurt = NA_real_)
  if (!(W > 0)) return(stats_means_checked(out))
  mean <- S / W
  out$mean <- mean
  if (!any(need %in% css_family)) return(stats_means_checked(out))
  c0 <- x[1]
  d <- x - c0
  s1 <- stats_seq_sum(d)
  s2 <- stats_seq_sum(stats_mul(d, d))
  q <- mean - c0
  if (s2 != 0 && q != 0 && abs(q) < 1.01e-14 * abs(c0)) {
    abort_unsupported("PROC MEANS moment statistics of nearly constant data for SAS 9.4 TS1M2 (no TS1M2 gold)")
  }
  css <- if (s2 == 0) 0 else s2 - stats_mul(q, 2 * s1 - stats_mul(n, q))
  if (css < 0) abort_unsupported("PROC MEANS CSS of nearly constant data came out negative (SAS 9.4 TS1M2 not verified)")
  out$css <- css
  out$uss <- css + stats_mul(S, mean)
  div <- if (vardef == "df") n - 1 else n
  var <- if (div > 0) css / div else NA_real_
  std <- sqrt(var)
  if (!is.na(var) && var > 0 && mean != 0 && var / stats_mul(mean, mean) <= STD_CUT_KEEP &&
      any(need %in% c("std", "cv", "stderr", "t", "prt", "lclm", "uclm"))) {
    abort_unsupported("PROC MEANS STD of a variance negligible against the squared mean for SAS 9.4 TS1M2 (cut-off to 0 not in the TS1M2 gold)")
  }
  out$var <- var
  out$std <- std
  out$cv <- if (!is.na(std) && mean != 0) 100 * std / mean else NA_real_
  if (!is.na(out$cv) && out$cv == 0) out$cv <- 0
  if (vardef == "df" && n > 1 && !is.na(var)) out$stderr <- sqrt(var / W)
  stats_means_checked(out)
}

stats_means_checked <- function(out) {
  for (k in names(out)) stats_finite_check(out[[k]], toupper(k), nan = TRUE)
  out
}

# SAS 9.4 reports STD (and CV) 0 when VAR (with the VARDEF= divisor) is
# negligible against the squared mean: proc_stats/means_css_neg_probe and
# means_css_dec_probe: STD 0 up to VAR/MEAN**2 =
# 1.1965683145884955E-26 and STD kept from 1.4312245245430491E-26 on (the
# smallest ratio seen with STD 0 is 2.2713562904522125E-33). Below it the
# statistics that depend on STD fail loud, as they do between the two limits.
STD_CUT_MIN <- 2.2713562904522125e-33
STD_CUT_ZERO <- 1.1965683145884955e-26
STD_CUT_KEEP <- 1.4312245245430491e-26

# In the cut-off region SAS reports STD 0 but keeps VAR; the statistics that
# follow from them are verified there only where a probe has such groups
# (condition: FREQ / WEIGHT statement and VARDEF=).  Others fail loud.
STD_CUT_GOLD <- list(
  "df" = c("std", "cv", "stderr", "t", "skew", "kurt"),            # means_css_neg_probe m_plain
  "n" = c("std", "cv", "stderr", "skew", "kurt"),                  # means_css_neg_probe m_vn
  "weight/df" = c("std", "cv", "stderr", "t", "prt", "lclm", "uclm"), # means_css_neg_probe, means_css_dec_probe m_wgt
  "freq/weight/df" = c("std", "cv", "stderr"),                     # means_css_dec_probe m_fw
  "weight/wdf" = c("std", "cv"),                                   # means_css_dec_probe m_vwdf
  "weight/weight" = c("std", "cv")                                 # means_css_dec_probe m_vwgt
)

stats_std_cut_check <- function(need, freq, weight, vardef) {
  key <- paste(c(if (!is.null(freq)) "freq", if (!is.null(weight)) "weight", vardef), collapse = "/")
  dependent <- c("std", "cv", "stderr", "t", "prt", "lclm", "uclm", "skew", "kurt")
  open <- setdiff(intersect(need, dependent), STD_CUT_GOLD[[key]])
  if (length(open)) {
    abort_unsupported(paste0("PROC MEANS ", paste(toupper(open), collapse = ", "),
                             " of data whose variance SAS cuts to STD 0 (not verified under ",
                             gsub("/", ", ", toupper(key), fixed = TRUE), ")"))
  }
  invisible()
}
