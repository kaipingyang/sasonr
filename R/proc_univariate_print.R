# PROC UNIVARIATE printed output (block U2): the tables Moments, Basic
# Statistical Measures, Tests for Location (Mu0=0), Quantiles (Definition 5)
# and Extreme Observations of each analysis variable, as the listing writes
# them [measured, cases univariate_print and univariate_fmt]:
# - Moments: BEST10. values; labels in 15 and 16 positions, 4 blanks apart;
# - Basic Statistical Measures: the Location column in D8.6 and the
#   Variability column in D10.4, each column with the decimals it shares
#   (fmt_d_column());
# - Tests for Location: the statistics in BEST8., the p values in PVALUE6.4;
# - Quantiles and Extreme Observations: per column, the decimals the values
#   need once rounded to 6 significant digits (univ_r6_column()), the column
#   one position wider than its widest value;
# - a mode among several is the smallest, with a Note under the measures.
# The E form of a quantile column, more
# than 20 nonzero differences in the signed rank test are not measured (fail
# loud).

univ_r6_dec <- function(x) {
  if (is.na(x) || x == 0) return(0L)
  k <- floor(log10(abs(x)))
  d <- max(0L, 5L - k)
  s <- display_round_text(abs(x), d)
  if (is.na(s)) abort_unsupported("a PROC UNIVARIATE value at the edge of its display rounding")
  if (!grepl(".", s, fixed = TRUE)) return(0L)
  nchar(sub("0+$", "", sub("^[^.]*\\.", "", s)))
}

univ_r6_column <- function(v) {
  ok <- !is.na(v)
  d <- max(c(0L, vapply(v[ok], univ_r6_dec, 1L)))
  if (d >= 10L) abort_unsupported("a PROC UNIVARIATE quantile column in the E form")
  out <- rep(".", length(v))
  out[ok] <- vapply(v[ok], function(x) {
    b <- fixed_body(x, d)
    if (identical(b, NA_character_)) abort_unsupported("a PROC UNIVARIATE value at the edge of its display rounding")
    paste0(if (b$neg) "-", b$s)
  }, "")
  out
}

univ_best <- function(v, w) if (is.na(v)) just_right(".", w) else sas_fmt_best(v, w)

# A table of lines: its title centred over it (rounding down).
univ_titled <- function(title, lines) {
  w <- max(nchar(lines))
  c(paste0(strrep(" ", max(0L, (w - nchar(title)) %/% 2L)), title), "", lines)
}

univ_print_var <- function(x0, disp, label = NULL) {
  # the statistics of the nonmissing values; the observation numbers of the
  # extreme values count every observation
  keep <- which(!is.na(x0))
  x <- x0[keep]
  n <- length(x)
  if (n == 0L) abort_unsupported("PROC UNIVARIATE printed output of a variable without values")
  sp <- list(vardef = "df", qntldef = 5L, alpha = 0.05, exclnpwgt = FALSE, freq = NULL)
  st <- as.list(means_cell_stats(x, rep(1, n), NULL, c("n", "mean", "sum", "std", "var", "skew", "kurt", "uss", "css",
                                                       "cv", "stderr", "median", "q1", "q3", "range"), sp, TRUE))
  # skewness and kurtosis whose shown digits change with the order of the
  # sums (values near zero, or large values with small spread) fail loud:
  # the text of a second, one-pass computation must agree
  alt <- univ_moments_onepass(x)
  if (!identical(univ_best(alt$skew, 10L), univ_best(st$skew, 10L)) || !identical(univ_best(alt$kurt, 10L), univ_best(st$kurt, 10L))) {
    abort_unsupported("PROC UNIVARIATE skewness or kurtosis whose last digits depend on the order of the sums (not supported)")
  }
  m <- function(lab1, v1, lab2, v2) paste0(formatC(lab1, width = -15L), "    ", univ_best(v1, 10L), "    ",
                                           formatC(lab2, width = -16L), "    ", univ_best(v2, 10L))
  moments <- c(m("N", n, "Sum Weights", n), m("Mean", st$mean, "Sum Observations", st$sum),
               m("Std Deviation", st$std, "Variance", st$var), m("Skewness", st$skew, "Kurtosis", st$kurt),
               m("Uncorrected SS", st$uss, "Corrected SS", st$css), m("Coeff Variation", st$cv, "Std Error Mean", st$stderr))
  # the mode: the most frequent value, the smallest of several
  tb <- table(x)
  cnt <- max(tb)
  modes <- sort(as.numeric(names(tb)[tb == cnt]))
  mode <- if (cnt > 1L || n == 1L) modes[1] else NA_real_
  loc <- fmt_d_column(c(st$mean, st$median, mode), 8L, 6L)
  var <- fmt_d_column(c(st$std, st$var, st$range, st$q3 - st$q1), 10L, 4L)
  bline <- function(l1, v1, l2, v2) sas_rt_rtrim(paste0(formatC(l1, width = -6L), "  ", formatC(v1, width = 9L), "     ",
                                                     formatC(l2, width = -19L), "  ", formatC(v2, width = 11L)))
  basic <- c("    Location                    Variability", "",
             bline("Mean", loc[1], "Std Deviation", var[1]), bline("Median", loc[2], "Variance", var[2]),
             bline("Mode", loc[3], "Range", var[3]), sas_rt_rtrim(paste0(strrep(" ", 22L), formatC("Interquartile Range", width = -19L), "  ",
                                                                         formatC(var[4], width = 11L))))
  basic <- univ_titled("Basic Statistical Measures", basic)
  note <- if (cnt > 1L && length(modes) > 1L) {
    c("", paste0("Note: The mode displayed is the smallest of ", length(modes), " modes with a count of ", cnt, "."))
  }
  lt <- univ_location_tests(x)
  # PVALUE6.4 (a missing value in the units column)
  pv <- function(p) if (is.na(p)) paste0(just_right(".", 2L), strrep(" ", 4L)) else fmt_pvalue(p, 6L, 4L)
  tl <- function(l, sym, v, pl, p) paste0(formatC(l, width = -11L), "    ", sym, "  ", univ_best(v, 8L), "    ",
                                          formatC(pl, width = -9L), "   ", pv(p))
  tests <- univ_titled("Tests for Location: Mu0=0",
                       c("Test           -Statistic-    -----p Value------", "",
                         tl("Student's t", "t", lt$t, "Pr > |t|", lt$p_t), tl("Sign", "M", lt$m, "Pr >= |M|", lt$p_m),
                         tl("Signed Rank", "S", lt$s, "Pr >= |S|", lt$p_s)))
  xs <- sort(x)
  pcts <- c(1, .99, .95, .90, .75, .5, .25, .10, .05, .01, 0)
  qv <- vapply(pcts, function(p) means_quantile(xs, p, 5L), 1)
  qt <- univ_r6_column(qv)
  qw <- max(8L, max(nchar(qt)) + 1L)
  lev <- c("100% Max", "99%", "95%", "90%", "75% Q3", "50% Median", "25% Q1", "10%", "5%", "1%", "0% Min")
  quant <- c("Quantiles (Definition 5)", "", paste0("Level     ", "    ", formatC("Quantile", width = qw)), "",
             paste0(formatC(lev, width = -10L), "    ", formatC(qt, width = qw)))
  # extreme observations: the lowest values (ties: the last observation
  # first), the highest (ties: the first observation first)
  k <- min(5L, n)
  obs <- keep
  lo <- order(x, -obs)[seq_len(k)]
  hi <- utils::tail(order(x, obs), k)
  ev <- univ_r6_column(c(x[lo], x[hi]))
  lo <- obs[lo]
  hi <- obs[hi]
  vw <- max(5L, max(nchar(ev)) + 1L)
  ow <- max(3L, nchar(as.character(length(x0))))
  grp <- vw + 6L + ow
  dash <- function(t) {
    l <- (grp - nchar(t) + 1L) %/% 2L
    paste0(strrep("-", l), t, strrep("-", grp - nchar(t) - l))
  }
  ext <- c(paste0(dash("Lowest"), "        ", dash("Highest")), "",
           paste0(formatC("Value", width = vw), "      ", formatC("Obs", width = ow), "        ",
                  formatC("Value", width = vw), "      ", formatC("Obs", width = ow)), "",
           paste0(formatC(ev[seq_len(k)], width = vw), "      ", formatC(as.character(lo), width = ow), "        ",
                  formatC(ev[k + seq_len(k)], width = vw), "      ", formatC(as.character(hi), width = ow)))
  ext <- univ_titled("Extreme Observations", ext)
  # the Missing Values table: one row per missing value (_, ., A-Z, in this
  # order), its count and percents of all observations and of the missing
  # ones; a Total row under two or more kinds [measured, univariate_print p11,
  # univariate_fmt q04]
  miss <- NULL
  if (length(x) < length(x0)) {
    code <- c("_", ".", LETTERS)[sas_miss_code(x0[is.na(x0)]) + 1L]
    lev <- c("_", ".", LETTERS)
    cnts <- table(factor(code, levels = lev))
    cnts <- cnts[cnts > 0]
    tot <- length(x0)
    nm <- sum(cnts)
    row <- function(lab, c) paste0(formatC(lab, width = 7L), formatC(as.character(c), width = 12L),
                                   formatC(display_fixed(c / tot * 100, 2L), width = 12L), formatC(display_fixed(c / nm * 100, 2L), width = 12L))
    mt <- c(paste0(strrep(" ", 23L), "-----Percent Of-----"), "Missing                             Missing",
            "  Value       Count     All Obs         Obs", "",
            vapply(names(cnts), function(l) row(l, cnts[[l]]), "", USE.NAMES = FALSE),
            # a Total row under two or more kinds of missing values
            if (length(cnts) > 1L) row("Total", nm))
    miss <- c(paste0(strrep(" ", 15L), "Missing Values"), "", mt)
  }
  # the quantile title wider than its table is a unit of its own
  qunits <- if (nchar(quant[1]) > max(nchar(quant[-1]))) lst_c(lst_lines(quant[1:2]), lst_unit(quant[-(1:2)])) else lst_unit(quant)
  # centring units (listing_center.R): the procedure and variable lines and
  # the Note on their own, each table with its title [measured, center_listing]
  # a label follows the name in parentheses, on the same line [measured,
  # univariate_fmt q05]; a line wider than LINESIZE is not measured
  # (an empty label written '' shows as (') in the listing: not supported)
  if (length(label) && !nzchar(label)) abort_unsupported("PROC UNIVARIATE printed output of a variable with an empty label")
  vline <- paste0("Variable:  ", disp, if (length(label)) paste0("  (", label, ")"))
  if (nchar(vline) > rt_linesize()) abort_unsupported("a PROC UNIVARIATE variable line wider than LINESIZE")
  lst_c(lst_lines(c("The UNIVARIATE Procedure", vline, "")), lst_unit(univ_titled("Moments", moments)),
        lst_lines(c("", "")), lst_unit(basic), if (length(note)) lst_lines(note), lst_lines(c("", "")), lst_unit(tests),
        lst_lines(c("", "")), qunits, lst_lines(c("", "")), lst_unit(ext), if (length(miss)) lst_c(lst_lines(c("", "")), lst_unit(miss)))
}

# Skewness and kurtosis from one-pass (updating) central moments, a check of
# the digits of the two-pass values only.
univ_moments_onepass <- function(x) {
  n <- 0; m1 <- 0; m2 <- 0; m3 <- 0; m4 <- 0
  for (v in x) {
    n1 <- n
    n <- n + 1
    delta <- v - m1
    dn <- delta / n
    dn2 <- dn * dn
    t1 <- delta * dn * n1
    m1 <- m1 + dn
    m4 <- m4 + t1 * dn2 * (n * n - 3 * n + 3) + 6 * dn2 * m2 - 4 * dn * m3
    m3 <- m3 + t1 * dn * (n - 2) - 3 * dn * m2
    m2 <- m2 + t1
  }
  if (n < 3 || m2 == 0) return(list(skew = NA_real_, kurt = NA_real_))
  v <- m2 / (n - 1)
  skew <- n / ((n - 1) * (n - 2)) * m3 / v^1.5
  kurt <- if (n < 4) NA_real_ else n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * m4 / v^2 - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  list(skew = skew, kurt = kurt)
}
