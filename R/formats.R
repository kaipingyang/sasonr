# Formats and informats.
#
# A format spec is a list(name, w, d, char, text) as produced by
# parse_format() (or fmt_from_text() for format names held in strings).
# `name` is lower case without the `$` and the width; "" is the w.d format
# (and `$w.` for character formats).
#
# sas_format() writes values (right-justified numeric fields, left-justified
# character fields, always `w` characters wide); sas_informat_read() reads
# text. User-defined formats (PROC FORMAT) live in the run-time catalog
# `rt$catalog` (see diag.R) and take precedence over built-in names.

# Format text --------------------------------------------------------------

fmt_spec <- function(name = "", w = NA_integer_, d = NA_integer_, char = FALSE) {
  text <- paste0(if (char) "$", name, if (!is.na(w)) w, ".", if (!is.na(d)) d)
  list(name = name, w = as.integer(w), d = as.integer(d), char = char, text = text)
}

#' Parse a format or informat name held in a string (`"date9."`, `"$char10."`)
#' @export
#' @keywords internal
sas_fmt_text <- function(text) {
  s <- tolower(strip_both(text))
  char <- startsWith(s, "$")
  if (char) s <- substring(s, 2L)
  if (!grepl(".", s, fixed = TRUE)) s <- paste0(s, ".")
  k <- regexpr(".", s, fixed = TRUE)
  head <- substr(s, 1L, k - 1L)
  tail <- substring(s, k + 1L)
  sp <- split_trailing_digits(head)
  d <- if (nzchar(tail)) suppressWarnings(as.integer(tail)) else NA_integer_
  fmt_spec(sp$base, sp$width, d, char)
}

# Catalogue of built-in formats ---------------------------------------------

num_fmt_defaults <- c(
  best = 12, "f" = 12, "w" = 12, z = 1, comma = 6, commax = 6, dollar = 6, dollarx = 6,
  percent = 6, negparen = 6, e = 12, pvalue = 6, date = 7, yymmdd = 8, mmddyy = 8,
  ddmmyy = 8, e8601da = 10, is8601da = 10, b8601da = 8, e8601dt = 19, is8601dt = 19,
  e8601tm = 8, is8601tm = 8, e8601dn = 10, datetime = 16, dtdate = 7, dtmonyy = 5, dtyear = 4,
  time = 8, tod = 8, hhmm = 5, hour = 2, mmss = 5, monyy = 5, monname = 9, downame = 9,
  worddate = 18, worddatx = 18, weekdate = 29, weekdatx = 29, year = 4, qtr = 1, month = 2,
  day = 2, weekday = 1, yymm = 7, mmyy = 7, yyq = 6, julian = 5, yymmn = 6, timeampm = 11,
  hex = 8
)
# date formats with separator variants: yymmddb/c/d/n/p/s, mmddyy..., ddmmyy..., yymm..., mmyy...
sep_variants <- c(b = " ", c = ":", d = "-", n = "", p = ".", s = "/")
sep_fmt_bases <- c("yymmdd", "mmddyy", "ddmmyy", "yymm", "mmyy")

fmt_base_name <- function(name) {
  if (name %in% names(num_fmt_defaults)) return(list(base = name, sep = NULL))
  n <- nchar(name)
  if (n > 1) {
    base <- substr(name, 1L, n - 1L)
    suf <- substr(name, n, n)
    if (base %in% sep_fmt_bases && suf %in% names(sep_variants)) {
      return(list(base = base, sep = sep_variants[[suf]]))
    }
  }
  NULL
}

char_fmt_names <- c("", "char", "upcase", "quote", "hex")

num_fmt_width_range <- list(
  best = c(1, 32), w = c(1, 32), f = c(1, 32), z = c(1, 32), comma = c(1, 32), commax = c(1, 32),
  dollar = c(2, 32), dollarx = c(2, 32), percent = c(4, 32), negparen = c(1, 32), e = c(7, 32),
  date = c(5, 11), yymmdd = c(2, 10), mmddyy = c(2, 10), ddmmyy = c(2, 10), e8601da = c(10, 10),
  is8601da = c(10, 10), e8601dt = c(16, 26), is8601dt = c(16, 26), time = c(2, 20),
  datetime = c(7, 40), monyy = c(5, 7), worddate = c(3, 32), weekdate = c(3, 37), year = c(2, 32),
  tod = c(2, 20), hex = c(1, 16)
)

#' Is a (numeric or character) format known (built-in or user-defined)?
#' @noRd
fmt_known <- function(fmt) {
  if (!is.null(user_fmt(fmt$name, fmt$char))) return(TRUE)
  if (fmt$char) return(fmt$name %in% char_fmt_names)
  !is.null(fmt_base_name(if (fmt$name == "") "w" else fmt$name))
}

user_fmt <- function(name, char, kind = "value") {
  if (!nzchar(name)) return(NULL)
  cat <- rt$catalog
  key <- paste0(if (char) "$", name)
  f <- cat[[paste0(kind, ":", key)]]
  f
}

# Small helpers --------------------------------------------------------------

just_right <- function(s, w) if (is.na(w)) s else formatC(s, width = w)
just_left <- function(s, w) {
  if (is.na(w)) return(s)
  s <- substr(s, 1L, w)
  paste0(s, strrep(" ", pmax(0L, w - nchar(s))))
}
stars <- function(w) strrep("*", w)

month_full <- c("January", "February", "March", "April", "May", "June", "July", "August",
                "September", "October", "November", "December")
day_full <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

civil <- function(day) {
  day <- floor(as.numeric(day))
  p <- greg_civil(sas_day_to_greg(day))
  # weekday: 1 = Sunday (1960-01-01, day 0, was a Friday)
  p$wday <- (day + 5) %% 7 + 1
  p$yday <- day - from_ymd(p$y, 1, 1) + 1
  p
}

# Round |x| half away from zero to d decimals (display fuzz) and write it.
fixed_body <- function(v, d, commas = FALSE, edge = "abort", down_rel = 0) {
  s <- display_round_text(v, d, edge, down_rel = down_rel)
  if (is.na(s)) return(NA_character_)
  if (commas) s <- add_commas(s)
  # SAS writes a minus sign for a negative value that rounds to zero ("-0.00")
  list(s = s, neg = v < 0)
}

add_commas <- function(s) {
  ip <- sub("\\..*$", "", s)
  rest <- substring(s, nchar(ip) + 1L)
  n <- nchar(ip)
  if (n > 3) {
    starts <- rev(seq(n - 2L, 1L - 2L, by = -3L))
    ip <- paste(substring(ip, pmax(starts, 1L), starts + 2L), collapse = ",")
  }
  paste0(ip, rest)
}

# Fit ladder shared by w.d / Z / COMMA / DOLLAR (from the SAS gold standard):
# for each number of decimals from d down to 0 try the full form, then
# without the leading zero, then without thousands separators, then without
# the dollar sign; then the E form (sci_fit); then asterisks.  A rounding at
# the edge of the display fuzz only fails loud when the text would fit.
fit_number <- function(v, w, d, commas = FALSE, dollar = FALSE, zero = FALSE, swap = FALSE, down_rel = 0) {
  # an integer beyond 2^53 / 10^d: the decimals are zeros (SAS 9.4: COMMA32.4 of 1E15)
  # (and fewer zeros when they do not fit: SAS 9.4, 32.15 of 939586291501115008
  # is 939586291501115008.0000000000000)
  if (d > 0 && is.finite(v) && v == trunc(v) && abs(v) * 10^d >= 2^53 && abs(v) < 1e21) {
    ip <- sprintf("%.0f", abs(v))
    for (dd in seq(d, 1L)) {
      forms <- c(if (commas && dd == d) paste0(add_commas(ip), ".", strrep("0", dd)), paste0(ip, ".", strrep("0", dd)))
      for (s in forms) {
        if (swap) s <- chartr(",.", ".,", s)
        s <- paste0(if (dollar && dd == d) "$", if (v < 0) "-", s)
        if (nchar(s) <= w) return(just_right(s, w))
      }
    }
  }
  build <- function(dd, cm, dl, lead0 = TRUE, edge = "abort") {
    b <- fixed_body(v, dd, cm, edge, down_rel)
    if (identical(b, NA_character_)) return(NA_character_)
    s <- b$s
    if (!lead0 && startsWith(s, "0.")) s <- substring(s, 2L)
    if (swap) s <- chartr(",.", ".,", s)
    paste0(if (dl) "$", if (b$neg) "-", s)
  }
  fits <- function(...) {
    a <- build(..., edge = "up")
    b <- build(..., edge = "down")
    !is.na(a) && !is.na(b) && (nchar(a) <= w || nchar(b) <= w)
  }
  pad <- function(s) {
    if (!zero) return(just_right(s, w))
    neg <- startsWith(s, "-")
    body <- if (neg) substring(s, 2L) else s
    paste0(if (neg) "-", strrep("0", max(0L, w - nchar(s))), body)
  }
  forms <- list(list(cm = commas, dl = dollar))
  if (commas) forms <- c(forms, list(list(cm = FALSE, dl = dollar)))
  if (dollar) forms <- c(forms, list(list(cm = FALSE, dl = FALSE)))
  for (dd in seq(d, 0L)) {
    # with fewer decimals than d SAS drops the separators and the dollar sign
    for (f in if (dd == d) forms else list(list(cm = FALSE, dl = FALSE))) {
      for (lead0 in c(TRUE, FALSE)) {
        if (!fits(dd, f$cm, f$dl, lead0)) next
        s <- build(dd, f$cm, f$dl, lead0)
        if (!is.na(s) && nchar(s) <= w) return(pad(s))
      }
    }
  }
  if (is.finite(v) && v != 0) {
    # a negative value that rounds to zero and has no room for its sign is "0"
    if (v < 0 && w >= 1L && identical(display_round_text(v, 0L, edge = "up"), "0")) return(pad("0"))
    s <- sci_fit(v, w)
    # COMMAX/DOLLARX write the E form with a decimal comma (SAS 9.4: 1,23E6)
    if (!is.na(s) && swap) s <- chartr(".", ",", s)
    if (!is.na(s)) return(pad(s))
  }
  stars(w)
}

# Ew.d: [-]m.ddddE+nn.  Without d the mantissa gets w - 7 decimals (sign,
# digit, point and a two-digit exponent take 7 columns); a positive value
# with no room for decimals uses the sign column for one ("5.0E-01" in E7.,
# "-5.E-01" for the negative).  More than 14 decimals are padded with zeros.
fmt_e <- function(v, w, d) {
  neg <- v < 0
  k <- if (is.na(d)) w - 7L else d
  if (!neg && is.na(d) && k == 0L) k <- 1L
  repeat {
    # the mantissa comes from 15 significant digits (sig15); at 14 or more
    # decimals SAS shows 14 significant digits and pads with zeros
    kk <- max(min(k, 13L), 0L)
    if (v == 0) {
      m <- if (kk > 0) paste0("0.", strrep("0", kk)) else "0"
      ex <- 0L
    } else {
      if (k >= 13L) {
        g <- e14_digits(NULL, v, k)
        ex <- g$ex
        keep <- g$keep
      } else {
        g <- sci_dec_text(v, kk)
        ex <- g$ex
        keep <- gsub(".", "", g$mant, fixed = TRUE)
      }
      m <- if (nchar(keep) > 1L) paste0(substr(keep, 1L, 1L), ".", substring(keep, 2L)) else keep
    }
    if (k > 13L) m <- paste0(m, strrep("0", k - 13L))
    if (k == 0L) m <- paste0(m, ".")
    edig <- max(2L, nchar(as.character(abs(ex))))
    body <- paste0(if (neg) "-", m, "E", if (ex < 0) "-" else "+", formatC(abs(ex), width = edig, flag = "0"))
    if (nchar(body) <= w) return(just_right(body, w))
    if (k <= 0L) return(stars(w))
    k <- k - 1L
  }
}

# 14 significant digits of an E form with 13 or more mantissa decimals
# (Ew. from E20., BESTw. above 16 columns, w.d written in E notation), from
# the exact binary value: list(keep = 14 digits, ex = decimal exponent).
# Round-15 scan (E20.-E32., 19,700 points at every binary exponent from 2^-61
# to 2^60): SAS's 14th digit carries when the rest r (the digits after the
# 14th, as a fraction of it) plus a fuzz of about 1E-15 relative reaches the
# carry point - at 13 mantissa decimals (E20.) the half point, at 14 or more
# the next digit (truncation).  The fuzz grows with the mantissa (a rest of
# 0.97 carries for 1.x, 0.84 for 9.x) and is not a function of r and the
# mantissa alone, so: 13 decimals - r < 0.30 truncated, r >= 0.55 rounded up;
# 14 or more - truncated below the smallest rest that carried for the same
# leading digit (0.965 for 1.x down to 0.838 for 9.x); the band above fails
# loud (SAS truncated 9.9999999999999998E149, rest 0.998, in BEST32.).
e14_digits <- function(g, v, k = 14L) {
  a <- abs(v)
  s <- sprintf("%.*e", 40L, a)
  ex <- as.integer(sub("^.*e", "", s))
  dg <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  keep <- substr(dg, 1L, 14L)
  r <- as.numeric(paste0("0.", substring(dg, 15L)))
  up <- if (k == 13L) {
    if (r < 0.30) FALSE else if (r >= 0.55) TRUE else NA
  } else {
    # the smallest rest that carried in the scan, per leading digit (less
    # 0.001): every rest below it was truncated
    lim <- c(0.965, 0.954, 0.927, 0.920, 0.895, 0.880, 0.849, 0.846, 0.838)[as.integer(substr(dg, 1L, 1L))]
    if (r < lim) FALSE else NA
  }
  if (is.na(up)) {
    abort_unsupported(paste0("E notation of ", format(v, digits = 17), " with 14 significant digits: whether SAS 9.4 ",
                             "carries into the 14th digit (rest ", format(r, digits = 3), ") is not determined by the gold standard"))
  }
  if (up) {
    keep <- incr_decimal(keep)
    if (nchar(keep) > 14L) {
      keep <- substr(keep, 1L, 14L)
      ex <- ex + 1L
    }
  }
  list(keep = keep, ex = ex)
}

# PERCENTw.d: the number (x * 100, written as w.d would write it in w - 3
# columns, E notation included) followed by "% ", or "(" number "%)" for a
# negative value.
fmt_percent <- function(v, w, d) {
  if (w < 4L) return(stars(w))
  # the w.d rule on fl(100 |x|) (round-15 scan: 2,799 points, PERCENTw.d
  # d = 0..4, values from 2^-31 to 2^29)
  num <- trimws(fit_number(abs(v) * 100, w - 3L, d))
  if (v < 0) return(paste0("(", formatC(num, width = w - 3L), "%)"))
  paste0(" ", formatC(num, width = w - 3L), "% ")
}

fmt_negparen <- function(v, w, d) {
  for (dd in seq(d, 0L)) {
    for (cm in c(TRUE, FALSE)) {
      b <- fixed_body(v, dd, cm)
      if (identical(b, NA_character_)) return(stars(w))
      # a positive value keeps a column for ")" only when the negative form would fit
      s <- if (b$neg) paste0("(", b$s, ")") else paste0(b$s, " ")
      if (nchar(b$s) + 2L <= w) return(just_right(s, w))
      # no room for the parentheses (or the trailing column): a minus sign (SAS 9.4: NEGPAREN6. of -1234 is -1,234)
      s <- if (b$neg) paste0("-", b$s) else b$s
      if (nchar(s) <= w) return(just_right(s, w))
    }
  }
  stars(w)
}

fmt_pvalue <- function(v, w, d) {
  if (v < 10^-d) return(just_right(paste0("<", substring(display_fixed(10^-d, d), 2L)), w))
  fit_number(v, w, d)
}

# Date writers -----------------------------------------------------------------

two <- function(x) formatC(x, width = 2, flag = "0")
four <- function(x) formatC(x, width = 4, flag = "0")

fmt_date_value <- function(v, base, w, sep = NULL, d = NA_integer_) {
  dt <- base %in% c("datetime", "e8601dt", "is8601dt", "e8601dn", "dtdate", "dtmonyy", "dtyear")
  # datetime values are rounded to the decimals shown (SAS 9.4: 86399.5 is 02JAN60:00:00:00 with DATETIME16.)
  if (base %in% c("datetime", "e8601dt", "is8601dt")) v <- round_half_away(v, if (is.na(d)) 0L else d)
  dd <- if (dt) floor(v / 86400) else floor(v)
  if (base %in% c("time", "tod", "hhmm", "hour", "mmss", "e8601tm", "is8601tm", "timeampm")) return(fmt_time_value(v, base, w, d))
  if (abs(dd) > 7e6) return(stars(w))
  p <- civil(dd)
  if (p$y > 9999 && base %in% c("date", "yymmdd", "mmddyy", "ddmmyy", "datetime", "e8601da", "is8601da", "b8601da",
                                 "e8601dt", "is8601dt", "e8601dn")) {
    abort_unsupported(sprintf("format %s for a year beyond 9999 (SAS 9.4 writes **** for the year)", toupper(base)))
  }
  if (base == "month" && w < 2 && p$m >= 10) abort_unsupported("MONTH1. for October to December")
  if ((base %in% c("weekdate", "weekdatx") && w < 29) || (base == "worddatx" && w < 12)) {
    abort_unsupported(sprintf("%s%d. (narrow widths not verified against SAS 9.4)", toupper(base), w))
  }
  y2 <- two(p$y %% 100)
  body <- switch(base,
    date = , dtdate = {
      if (w < 5) return(stars(w))
      if (w >= 11) paste0(two(p$d), "-", month_abb[p$m], "-", four(p$y))
      else if (w >= 9) paste0(two(p$d), month_abb[p$m], four(p$y))
      else if (w >= 7) paste0(two(p$d), month_abb[p$m], y2)
      else paste0(two(p$d), month_abb[p$m])
    },
    yymmdd = , mmddyy = , ddmmyy = {
      parts <- switch(base, yymmdd = c(NA, p$m, p$d), mmddyy = c(p$m, p$d, NA), ddmmyy = c(p$d, p$m, NA))
      s <- if (is.null(sep)) (if (base == "yymmdd") "-" else "/") else sep
      date3_text(base, p, w, s, explicit = !is.null(sep))
    },
    e8601da = , is8601da = , e8601dn = paste0(four(p$y), "-", two(p$m), "-", two(p$d)),
    b8601da = paste0(four(p$y), two(p$m), two(p$d)),
    e8601dt = , is8601dt = {
      sec <- v - floor(v / 86400) * 86400
      h <- floor(sec / 3600)
      mi <- floor((sec - h * 3600) / 60)
      s <- sec - h * 3600 - mi * 60
      out <- paste0(four(p$y), "-", two(p$m), "-", two(p$d), "T", two(h), ":", two(mi))
      if (w >= 19) {
        nd <- if (is.na(d)) 0L else min(d, max(0L, w - 20L))
        out <- paste0(out, ":", sec_text(s, nd))
      }
      out
    },
    datetime = {
      sec <- v - floor(v / 86400) * 86400
      h <- floor(sec / 3600)
      mi <- floor((sec - h * 3600) / 60)
      s <- sec - h * 3600 - mi * 60
      # four-digit year only when the full value with a 4-digit year fits with one column to spare
      nd0 <- if (is.na(d)) 0L else d
      fy <- w >= 19L + if (nd0 > 0) nd0 + 1L else 0L
      if (w < 16 && w > 7 && w != 10 && w != 13) abort_unsupported(sprintf("DATETIME%d.", w))
      out <- paste0(two(p$d), month_abb[p$m], if (fy) four(p$y) else y2)
      ww <- if (fy) w - 2L else w
      if (ww >= 10) out <- paste0(out, ":", two(h))
      if (ww >= 13) out <- paste0(out, ":", two(mi))
      if (ww >= 16) {
        nd <- if (is.na(d)) 0L else d
        if (nd > 0 && nchar(out) + 4L + nd > w) nd <- max(0L, w - nchar(out) - 4L)
        out <- paste0(out, ":", sec_text(s, nd))
      }
      out
    },
    monyy = , dtmonyy = paste0(month_abb[p$m], if (w >= 7) four(p$y) else y2),
    monname = substr(month_full[p$m], 1L, w),
    downame = substr(day_full[p$wday], 1L, w),
    worddate = {
      full <- paste0(month_full[p$m], " ", p$d, ", ", p$y)
      if (w >= nchar(full)) full
      else if (w >= 12) paste0(substr(month_full[p$m], 1L, 3L), " ", p$d, ", ", p$y)
      else substr(month_full[p$m], 1L, w)
    },
    worddatx = {
      full <- paste0(p$d, " ", month_full[p$m], " ", p$y)
      if (w >= nchar(full)) full else paste0(p$d, " ", substr(month_full[p$m], 1L, 3L), " ", p$y)
    },
    weekdate = {
      full <- paste0(day_full[p$wday], ", ", month_full[p$m], " ", p$d, ", ", p$y)
      if (w >= nchar(full)) full
      else if (w >= 17) paste0(substr(day_full[p$wday], 1L, 3L), ", ", substr(month_full[p$m], 1L, 3L), " ", p$d, ", ", p$y)
      else substr(day_full[p$wday], 1L, min(w, 3L))
    },
    weekdatx = {
      full <- paste0(day_full[p$wday], ", ", p$d, " ", month_full[p$m], " ", p$y)
      if (w >= nchar(full)) full else paste0(substr(day_full[p$wday], 1L, 3L), ", ", p$d, " ", substr(month_full[p$m], 1L, 3L), " ", p$y)
    },
    year = , dtyear = if (w >= 4) four(p$y) else y2,
    qtr = as.character(floor((p$m - 1) / 3) + 1),
    month = as.character(p$m),
    day = as.character(p$d),
    weekday = as.character(p$wday),
    yymm = paste0(if (w >= 7 || (!is.null(sep) && !nzchar(sep) && w >= 6)) four(p$y) else y2, sep %||% "M", two(p$m)),
    yymmn = paste0(if (w >= 6) four(p$y) else y2, two(p$m)),
    mmyy = paste0(two(p$m), sep %||% "M", if (w >= 7 || (!is.null(sep) && !nzchar(sep) && w >= 6)) four(p$y) else y2),
    yyq = paste0(if (w >= 6) four(p$y) else y2, "Q", floor((p$m - 1) / 3) + 1),
    julian = paste0(if (w >= 7) four(p$y) else y2, formatC(p$yday, width = 3, flag = "0")),
    NULL
  )
  if (nchar(body) > w) return(stars(w))
  just_right(body, w)
}

sec_text <- function(s, nd) {
  if (nd <= 0) return(two(floor(s + 1e-9)))
  txt <- formatC(round_half_away(s, nd), format = "f", digits = nd)
  if (floor(s) < 10) txt <- paste0("0", txt)
  txt
}

# round half away from zero at nd decimals (on the decimal value of the binary
# number) for the time and datetime formats.  The 1E-9 fuzz is checked against
# the one display engine (display_round_text): where the two disagree (a value
# just below a half point) the rounding is not verified, so it fails loud.
round_half_away <- function(v, nd) {
  if (is.na(v)) return(v)
  m <- 10^nd
  r <- sign(v) * floor(abs(v) * m + 0.5 + 1e-9) / m
  if (abs(v) < 1e15) {
    e <- display_round_text(v, nd, edge = "up")
    if (!identical(e, NA_character_) && as.numeric(e) != abs(r)) {
      abort_unsupported(paste0("time format rounding of ", format(v, digits = 17), " to ", nd,
                               " decimals: the value lies just below a half point (not verified against SAS)"))
    }
  }
  r
}

# YYMMDD / MMDDYY / DDMMYY width ladder.
date3_text <- function(base, p, w, sep, explicit) {
  four_year <- if (explicit && !nzchar(sep)) w >= 8 else w >= 10
  yr <- if (four_year) four(p$y) else two(p$y %% 100)
  parts <- switch(base,
    yymmdd = c(yr, two(p$m), two(p$d)),
    mmddyy = c(two(p$m), two(p$d), yr),
    ddmmyy = c(two(p$d), two(p$m), yr)
  )
  if (explicit) {
    if (w >= 8 || (!nzchar(sep) && w >= 6)) return(paste(parts, collapse = sep))
  } else if (w >= 8) {
    return(paste(parts, collapse = sep))
  }
  if (w >= 6) return(paste(parts, collapse = ""))
  if (w == 5) return(paste(parts[1:2], collapse = sep))
  if (w == 4) return(paste(parts[1:2], collapse = ""))
  parts[1]
}

fmt_time_value <- function(v, base, w, d) {
  nd <- if (is.na(d)) 0L else d
  if (base %in% c("e8601tm", "is8601tm") && (v < 0 || v >= 86400)) return(stars(w))
  if (base == "hour" && v < 0) abort_unsupported("HOUR format of a negative time")
  if (base %in% c("time", "tod", "e8601tm", "is8601tm")) v <- round_half_away(v, nd)
  if (base %in% c("tod", "e8601tm", "is8601tm")) v <- v - floor(v / 86400) * 86400
  av <- abs(v)
  sg <- if (v < 0) "-" else ""
  if (base == "hhmm") {
    if (nd > 0) {
      # minutes with decimals: 10:30.25
      mins <- round_half_away(av / 60, nd)
      hh <- floor(mins / 60)
      mm <- mins - hh * 60
      txt <- formatC(mm, format = "f", digits = nd)
      if (mm < 10) txt <- paste0("0", txt)
      body <- paste0(sg, hh, ":", txt)
      if (nchar(body) <= w) return(just_right(body, w))
      abort_unsupported(sprintf("HHMM%d.%d for this value", w, nd))
    }
    tot <- floor(av / 60 + 0.5)
    body <- paste0(sg, floor(tot / 60), ":", two(tot %% 60))
    if (nchar(body) > w) {
      # too narrow for the minutes: the hours only
      hb <- paste0(sg, floor(av / 3600))
      return(if (nchar(hb) > w) stars(w) else just_right(hb, w))
    }
    return(just_right(body, w))
  }
  if (base == "timeampm") {
    sec <- v - floor(v / 86400) * 86400
    h <- floor(sec / 3600)
    m <- floor((sec - h * 3600) / 60)
    s <- floor(sec - h * 3600 - m * 60)
    ap <- if (h < 12) "AM" else "PM"
    h12 <- h %% 12
    if (h12 == 0) h12 <- 12
    body <- if (w >= 11) paste0(h12, ":", two(m), ":", two(s), " ", ap) else if (w >= 8) paste0(h12, ":", two(m), " ", ap) else paste0(h12, " ", ap)
    if (w == 2) return(ap)
    if (nchar(body) > w) abort_unsupported(sprintf("TIMEAMPM%d.", w))
    return(just_right(body, w))
  }
  if (base == "hour") {
    # the hours with nd decimals: the one display engine
    body <- paste0(sg, display_fixed(av / 3600, nd))
    return(just_right(body, w))
  }
  if (base == "mmss") {
    sv <- round_half_away(av, nd)
    mm <- floor(sv / 60)
    ss <- sv - mm * 60
    stxt <- if (nd > 0) formatC(ss, format = "f", digits = nd) else formatC(ss, format = "d")
    if (ss < 10) stxt <- paste0("0", stxt)
    body <- paste0(sg, mm, ":", stxt)
    if (nchar(body) > w) {
      # too narrow for the seconds: the minutes only (truncated)
      mb <- paste0(sg, floor(av / 60))
      return(if (nchar(mb) > w) stars(w) else just_right(mb, w))
    }
    return(just_right(body, w))
  }
  h <- floor(av / 3600)
  m <- floor((av - h * 3600) / 60)
  s <- av - h * 3600 - m * 60
  pad <- base %in% c("tod", "e8601tm", "is8601tm")
  hss <- unique(c(if (pad) two(h), as.character(h)))
  for (hs in hss) {
    base_txt <- paste0(sg, hs, ":", two(m), ":", two(floor(s)))
    if (nd > 0) {
      k <- min(nd, w - nchar(base_txt) - 1L)
      if (k >= 1) return(just_right(paste0(sg, hs, ":", two(m), ":", sec_text(s, k)), w))
    }
    if (nchar(base_txt) <= w && (w >= 8 || !pad)) return(just_right(base_txt, w))
    if (nchar(base_txt) <= w && pad && hs == hss[length(hss)]) return(just_right(base_txt, w))
  }
  for (hs in hss) {
    short <- paste0(sg, hs, ":", two(m))
    if (nchar(short) <= w) return(just_right(short, w))
  }
  for (hs in hss) if (nchar(paste0(sg, hs)) <= w) return(just_right(paste0(sg, hs), w))
  stars(w)
}

# Main entry -----------------------------------------------------------------

#' Apply a SAS format to values
#'
#' Numeric formats return right-justified fields of width `w`; character
#' formats return left-justified fields (no width: the value unchanged).
#' @param fmt Format spec (see `parse_format()`); `NULL` means BEST12. / `$`.
#' @export
#' @keywords internal
sas_format <- function(x, fmt = NULL) {
  if (is.null(fmt)) {
    if (is.character(x)) return(x)
    return(sas_fmt_best(x, 12L))
  }
  fmt$char <- isTRUE(fmt$char)
  if (is.null(fmt$name)) fmt$name <- ""
  if (is.null(fmt$w)) fmt$w <- NA_integer_
  if (is.null(fmt$d)) fmt$d <- NA_integer_
  uf <- user_fmt(fmt$name, if (is.character(x)) TRUE else fmt$char)
  if (is.null(uf) && is.character(x) && !fmt$char) uf <- user_fmt(fmt$name, TRUE)
  if (!is.null(uf)) return(apply_user_fmt(x, uf, fmt))
  if (is.character(x)) {
    w <- fmt$w
    if (fmt$name == "quote" && is.na(w)) w <- max(nchar(x), 0L) + 2L
    if (!fmt$name %in% char_fmt_names) abort_unsupported(paste0("format ", fmt$text))
    # PUT(x, $HEX.) without a width: SAS writes all bytes (result length 2 * length
    # of the argument), not the default width of the inventory; not modelled
    if (fmt$name == "hex" && is.na(w)) abort_unsupported("format $HEX. without a width")
    out <- switch(fmt$name,
      upcase = toupper(x),
      quote = {
        inner <- gsub("\"", "\"\"", sas_rtrim(x), fixed = TRUE)
        # too long for the width: the text is cut so that the closing quote fits
        if (!is.na(w)) {
          cut <- nchar(inner) > w - 2L
          # a cut through a doubled quote: SAS keeps neither half (seen once); fail loud
          if (any(cut & vapply(seq_along(inner), function(i) {
            if (!cut[i] || w < 3L) return(FALSE)
            k <- w - 2L
            rest <- substr(inner[i], 1L, k)
            m <- regmatches(rest, regexpr("\"+$", rest))
            length(m) > 0 && nchar(m) %% 2L == 1L
          }, logical(1)))) abort_unsupported(paste0("format ", fmt$text, " cutting through an embedded quote"))
          inner <- substr(inner, 1L, w - 2L)
        }
        paste0("\"", inner, "\"")
      },
      hex = fmt_hex_char(x, w),
      x
    )
    if (!is.na(w)) out <- just_left(out, w)
    return(out)
  }
  name <- if (fmt$name %in% c("", "f")) "w" else fmt$name
  bn <- fmt_base_name(name)
  if (is.null(bn)) abort_unsupported(paste0("format ", fmt$text))
  w <- fmt$w
  if (is.na(w)) w <- registry_defw("format", fmt)
  if (is.na(w)) w <- as.integer(num_fmt_defaults[[bn$base]])
  if (bn$base == "pvalue" && is.na(fmt$d)) fmt$d <- 4L
  d <- if (is.na(fmt$d)) 0L else fmt$d
  code <- sas_miss_code(x)
  vapply(seq_along(x), function(i) {
    if (!is.na(code[i])) {
      if (bn$base %in% c("w", "z", "comma", "commax", "dollar", "dollarx") && d > 0 && d < w) {
        return(paste0(just_right(miss_label(code[i]), w - d), strrep(" ", d)))
      }
      # NEGPAREN keeps the column of the closing parenthesis; PVALUE puts the missing value in its units column
      if (bn$base == "negparen" && w > d + 1L) return(paste0(just_right(miss_label(code[i]), w - d - 1L), strrep(" ", d + 1L)))
      if (bn$base == "pvalue" && d < w) return(paste0(just_right(miss_label(code[i]), w - d), strrep(" ", d)))
      if (bn$base == "percent" && w - d - 2L > 0) {
        return(paste0(just_right(miss_label(code[i]), w - d - 2L), strrep(" ", d + 2L)))
      }
      return(just_right(miss_label(code[i]), w))
    }
    v <- x[i]
    switch(bn$base,
      best = sas_fmt_best(v, w),
      w = fit_number(v, w, d),
      z = fit_number(v, w, d, zero = TRUE),
      comma = fit_number(v, w, d, commas = TRUE),
      commax = fit_number(v, w, d, commas = TRUE, swap = TRUE),
      dollar = fit_number(v, w, d, commas = TRUE, dollar = TRUE),
      dollarx = fit_number(v, w, d, commas = TRUE, dollar = TRUE, swap = TRUE),
      percent = fmt_percent(v, w, d),
      negparen = fmt_negparen(v, w, d),
      e = fmt_e(v, w, fmt$d),
      pvalue = fmt_pvalue(v, w, d),
      hex = fmt_hex_num(v, w),
      fmt_date_value(v, bn$base, w, bn$sep, fmt$d)
    )
  }, character(1), USE.NAMES = FALSE)
}

# $HEXw.: two hex digits per byte of the value padded with blanks to w/2 bytes
fmt_hex_char <- function(x, w) {
  n <- (w + 1L) %/% 2L
  vapply(x, function(s) {
    b <- charToRaw(enc2native(s))
    # a width beyond the value: SAS writes the bytes of the value only (blank-padded text)
    substr(toupper(paste(as.character(b[seq_len(min(n, length(b)))]), collapse = "")), 1L, w)
  }, "", USE.NAMES = FALSE)
}

# HEX16.: the IEEE 754 bytes of the value (big-endian); other widths write the
# integer part in two's complement, which is not implemented
fmt_hex_num <- function(v, w) {
  if (w != 16L) abort_unsupported(paste0("format HEX", w, ". for numeric values (only HEX16.)"))
  toupper(paste(as.character(writeBin(as.double(v), raw(), size = 8L, endian = "big")), collapse = ""))
}

#' Format width actually used (explicit or default) for a spec
#' @noRd
fmt_width <- function(fmt, x_char = fmt$char) {
  if (!is.na(fmt$w)) return(fmt$w)
  uf <- user_fmt(fmt$name, x_char)
  if (!is.null(uf)) return(uf$default_w)
  if (fmt$char || x_char) return(NA_integer_)
  bn <- fmt_base_name(if (fmt$name %in% c("", "f")) "w" else fmt$name)
  if (is.null(bn)) return(12L)
  w <- registry_defw("format", fmt)
  if (!is.na(w)) return(w)
  as.integer(num_fmt_defaults[[bn$base]])
}

# User-defined formats (PROC FORMAT) ------------------------------------------

#' Register a user-defined format or informat in the run-time catalogue
#' @param def list(kind = "value"/"invalue", name, char, entries, default_w,
#'   upcase, just); each entry: list(other, lo, hi, lo_excl, hi_excl, label,
#'   nested (format spec or NULL)).
#' @export
#' @keywords internal
sas_define_format <- function(def) {
  key <- paste0(def$kind, ":", if (def$char) "$", def$name)
  rt$catalog[[key]] <- def
  invisible(NULL)
}

match_user_entry <- function(v, uf) {
  ents <- uf$entries
  if (is.character(v)) {
    s <- sas_rtrim(v)
    if (isTRUE(uf$upcase)) s <- toupper(s)
    for (e in ents) {
      if (isTRUE(e$other)) next
      if (is.null(e[["hi"]])) {
        if (identical(sas_rtrim(e[["lo"]]), s)) return(e)
      } else {
        lo <- sas_rtrim(e[["lo"]])
        hi <- sas_rtrim(e[["hi"]])
        lo_ok <- isTRUE(e$lo_low) || (if (e$lo_excl) chr_cmp(s, lo) > 0 else chr_cmp(s, lo) >= 0)
        hi_ok <- isTRUE(e$hi_high) || (if (e$hi_excl) chr_cmp(s, hi) < 0 else chr_cmp(s, hi) <= 0)
        if (lo_ok && hi_ok) return(e)
      }
    }
  } else {
    code <- sas_miss_code(v)
    for (e in ents) {
      if (isTRUE(e$other)) next
      if (!is.na(code)) {
        if (!is.null(e$miss) && e$miss == code) return(e)
        next
      }
      if (!is.null(e$miss)) next
      fz <- 1e-12 * max(1, abs(v))
      lo_ok <- isTRUE(e$lo_low) || (if (e$lo_excl) v > e[["lo"]] + fz else v >= e[["lo"]] - fz)
      hi_ok <- isTRUE(e$hi_high) || (if (e$hi_excl) v < e[["hi"]] - fz else v <= e[["hi"]] + fz)
      if (lo_ok && hi_ok) return(e)
    }
  }
  for (e in ents) if (isTRUE(e$other)) return(e)
  NULL
}

apply_user_fmt <- function(x, uf, fmt) {
  w <- if (is.na(fmt$w)) uf$default_w else fmt$w
  vapply(seq_along(x), function(i) {
    v <- x[i]
    e <- match_user_entry(v, uf)
    if (is.null(e)) {
      if (is.character(v)) return(just_left(sas_rtrim(v), w))
      code <- sas_miss_code(v)
      if (!is.na(code)) return(just_right(miss_label(code), w))
      return(sas_fmt_best(v, w))
    }
    if (!is.null(e$nested)) {
      out <- sas_format(v, e$nested)
      return(if (is.character(v)) just_left(out, w) else just_right(trimws(out), w))
    }
    just_left(e$label, w)
  }, character(1), USE.NAMES = FALSE)
}

# Informats ----------------------------------------------------------------------

num_informats <- c("w", "f", "best", "bestd", "comma", "commax", "dollar", "dollarx", "percent", "e",
                   "date", "yymmdd", "mmddyy", "ddmmyy", "e8601da", "is8601da", "b8601da", "e8601dt",
                   "is8601dt", "datetime", "time", "monyy", "anydtdte", "anydtdtm", "anydttme",
                   "julian", "yymmn", "nlnum", "trailsgn", "e8601tm", "is8601tm", "hhmmss", "yyq", "bz", "hex")
char_informats <- c("", "char", "upcase", "quote", "hex")

infmt_known <- function(fmt) {
  if (!is.null(user_fmt(fmt$name, fmt$char, "invalue"))) return(TRUE)
  if (fmt$char) return(fmt$name %in% char_informats)
  nm <- if (fmt$name == "") "w" else fmt$name
  nm %in% num_informats || (substr(nm, 1L, nchar(nm) - 1L) %in% c("yymmdd", "mmddyy", "ddmmyy") &&
                              substring(nm, nchar(nm)) %in% names(sep_variants))
}

infmt_is_char <- function(fmt) {
  if (fmt$char) return(TRUE)
  FALSE
}

std_num_re <- "^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eEdD][+-]?[0-9]+)?$"

# Standard numeric read; returns list(v, ok)
read_std_num <- function(s, d) {
  t <- strip_both(s)
  if (!nzchar(t) || t == ".") return(list(v = NA_real_, ok = TRUE))
  if (nchar(t) == 2 && substr(t, 1, 1) == "." && grepl("^[A-Za-z_]$", substr(t, 2, 2))) {
    return(list(v = sas_missing(t), ok = TRUE))
  }
  if (!grepl(std_num_re, t)) return(list(v = NA_real_, ok = FALSE))
  t2 <- chartr("dD", "ee", t)
  v <- as.numeric(t2)
  # implied decimals also apply to E notation without a decimal point (SAS 9.4: '1e3' with 8.2 is 10)
  if (!is.na(d) && d > 0 && !grepl(".", t, fixed = TRUE)) v <- v / 10^d
  if (!is.finite(v)) return(list(v = NA_real_, ok = FALSE))
  list(v = v, ok = TRUE)
}

read_comma_num <- function(s, d, swap = FALSE, pct_div = FALSE) {
  t <- strip_both(s)
  if (!nzchar(t) || t == ".") return(list(v = NA_real_, ok = TRUE))
  if (swap) t <- chartr(",.", ".,", t)
  neg <- startsWith(t, "(") && endsWith(t, ")")
  if (neg) t <- substr(t, 2L, nchar(t) - 1L)
  # COMMA/DOLLAR ignore a percent sign (PERCENT divides by 100)
  pct <- pct_div && grepl("%", t, fixed = TRUE)
  t <- gsub("[,$% ]", "", t)
  if (endsWith(t, "-") && !startsWith(t, "-")) t <- paste0("-", substr(t, 1L, nchar(t) - 1L))
  r <- read_std_num(t, d)
  if (r$ok && !is.na(r$v)) {
    if (neg) r$v <- -r$v
    if (pct) r$v <- r$v / 100
  }
  r
}

month_num <- function(s) match(toupper(s), month_abb)

valid_ymd <- function(y, m, d) {
  if (any(is.na(c(y, m, d)))) return(NA_real_)
  if (m < 1 || m > 12 || d < 1 || d > 31 || y < 1582 || y > 20000) return(NA_real_)
  out <- from_ymd(y, m, d)
  if (is.na(out) || sas_f_day(out) != d) return(NA_real_)
  out
}

expand_year <- function(y, digits) if (digits <= 2) (if (y < 40) 2000 + y else 1900 + y) else y

read_date9 <- function(t) {
  m <- regmatches(t, regexec("^([0-9]{1,2})[-/ .]?([A-Za-z]{3})[-/ .]?([0-9]{2}|[0-9]{4})$", t))[[1]]
  if (length(m) == 0) return(NA_real_)
  valid_ymd(expand_year(as.numeric(m[4]), nchar(m[4])), month_num(m[3]), as.numeric(m[2]))
}

# Three numeric date parts in a given order ("ymd", "mdy", "dmy").
read_date3 <- function(t, order) {
  if (grepl("^[0-9]+$", t)) {
    n <- nchar(t)
    pos <- switch(order,
      ymd = if (n == 8) list(c(1, 4), c(5, 6), c(7, 8)) else if (n == 6) list(c(1, 2), c(3, 4), c(5, 6)) else NULL,
      mdy = , dmy = if (n == 8) list(c(1, 2), c(3, 4), c(5, 8)) else if (n == 6) list(c(1, 2), c(3, 4), c(5, 6)) else NULL
    )
    if (is.null(pos)) return(NA_real_)
    parts <- vapply(pos, function(p) substr(t, p[1], p[2]), "")
  } else {
    m <- regmatches(t, regexec("^([0-9]{1,4})[^0-9A-Za-z]+([0-9]{1,2}|[A-Za-z]{3})[^0-9A-Za-z]+([0-9]{1,4})$", t))[[1]]
    if (length(m) == 0) return(NA_real_)
    parts <- m[2:4]
  }
  num <- function(s) if (grepl("^[A-Za-z]{3}$", s)) month_num(s) else as.numeric(s)
  yi <- switch(order, ymd = 1L, 3L)
  mi <- switch(order, ymd = 2L, mdy = 1L, dmy = 2L)
  di <- switch(order, ymd = 3L, mdy = 2L, dmy = 1L)
  # one- and two-digit years use the YEARCUTOFF window ('3/15/2' read with MMDDYY6. is 2002)
  if (!nchar(parts[yi]) %in% c(1L, 2L, 4L)) return(NA_real_)
  valid_ymd(expand_year(as.numeric(parts[yi]), nchar(parts[yi])), num(parts[mi]), num(parts[di]))
}

read_time <- function(t) {
  t <- toupper(t)
  ampm <- ""
  if (grepl("(AM|PM)$", t)) {
    ampm <- substring(t, nchar(t) - 1L)
    t <- trimws(substr(t, 1L, nchar(t) - 2L))
  }
  m <- regmatches(t, regexec("^([0-9]{1,2})[:.]([0-9]{1,2})([:.]([0-9]{1,2}(\\.[0-9]*)?))?$", t))[[1]]
  if (length(m) == 0) {
    if (grepl("^[0-9]{1,2}$", t)) m <- c(t, t, "0", "", "")
    else return(NA_real_)
  }
  h <- as.numeric(m[2])
  mi <- as.numeric(m[3])
  s <- if (nzchar(m[5])) as.numeric(m[5]) else 0
  if (mi > 59 || s >= 60) return(NA_real_)
  if (ampm == "PM" && h < 12) h <- h + 12
  if (ampm == "AM" && h == 12) h <- 0
  h * 3600 + mi * 60 + s
}

read_iso_dt <- function(t) {
  m <- regmatches(t, regexec("^([0-9]{4})-([0-9]{2})-([0-9]{2})[T ]([0-9]{2}):([0-9]{2})(:([0-9]{2}(\\.[0-9]*)?))?$", t))[[1]]
  if (length(m) == 0) return(NA_real_)
  dd <- valid_ymd(as.numeric(m[2]), as.numeric(m[3]), as.numeric(m[4]))
  s <- if (nzchar(m[8])) as.numeric(m[8]) else 0
  h <- as.numeric(m[5])
  mi <- as.numeric(m[6])
  if (h > 24 || mi > 59 || s >= 60) return(NA_real_)
  dd * 86400 + h * 3600 + mi * 60 + s
}

read_datetime <- function(t) {
  m <- regmatches(t, regexec("^([0-9]{1,2}[-/ ]?[A-Za-z]{3}[-/ ]?([0-9]{4}|[0-9]{2}))(([: ]|:[ ]*)(.*))?$", t))[[1]]
  if (length(m) == 0) return(NA_real_)
  dd <- read_date9(gsub("[-/ ]", "", m[2]))
  tm <- if (nzchar(m[6])) read_time(trimws(m[6])) else 0
  dd * 86400 + tm
}

read_anydate <- function(t) {
  for (f in list(
    function(s) read_date9(s),
    function(s) {
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)) read_date3(s, "ymd") else NA_real_
    },
    function(s) {
      v <- read_iso_dt(s)
      if (is.na(v)) NA_real_ else floor(v / 86400)
    },
    function(s) {
      v <- read_datetime(s)
      if (is.na(v) || !grepl(":", s, fixed = TRUE)) NA_real_ else floor(v / 86400)
    },
    function(s) if (grepl("^[0-9]{8}$", s) && as.numeric(substr(s, 1, 4)) > 1581) read_date3(s, "ymd") else NA_real_,
    function(s) {
      # Julian yyyyddd (2012024 is 24 January 2012)
      if (!grepl("^[0-9]{7}$", s)) return(NA_real_)
      y <- as.numeric(substr(s, 1, 4))
      dd <- as.numeric(substr(s, 5, 7))
      if (y < 1582 || dd < 1 || dd > (if (y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)) 366 else 365)) return(NA_real_)
      valid_ymd(y, 1, 1) + dd - 1
    },
    function(s) if (grepl("^[0-9]{4}[^0-9][0-9]{1,2}[^0-9][0-9]{1,2}$", s)) read_date3(s, "ymd") else NA_real_,
    function(s) read_date3(s, "mdy"),
    function(s) read_date3(s, "dmy"),
    function(s) {
      m <- regmatches(s, regexec("^([A-Za-z]{3})[-/ ]?([0-9]{4}|[0-9]{2})$", s))[[1]]
      if (length(m) == 0) NA_real_ else valid_ymd(expand_year(as.numeric(m[3]), nchar(m[3])), month_num(m[2]), 1)
    },
    function(s) {
      m <- regmatches(s, regexec("^([0-9]{4})-([0-9]{2})$", s))[[1]]
      if (length(m) == 0) NA_real_ else valid_ymd(as.numeric(m[2]), as.numeric(m[3]), 1)
    }
  )) {
    v <- f(t)
    if (!is.na(v)) return(v)
  }
  NA_real_
}

# Read one field with a numeric informat; returns list(v, ok).
# HEXw. informat: w = 16 reads the IEEE 754 bit pattern of a double (real
# binary); w < 16 reads a positive integer (fixed-point binary).  Invalid
# digits are invalid data (NOTE + _ERROR_), never a silent missing value.
read_hex_num <- function(t, w) {
  if (!grepl("^[0-9A-Fa-f]+$", t)) return(list(v = NA_real_, ok = FALSE))
  if (identical(as.integer(w), 16L)) {
    if (nchar(t) != 16L) abort_unsupported("HEX16. informat with fewer than 16 hexadecimal digits")
    b <- as.raw(strtoi(substring(t, seq(1L, 15L, 2L), seq(2L, 16L, 2L)), 16L))
    v <- readBin(b, "double", size = 8L, endian = "big")
    # NaN / infinity bit patterns are not SAS values
    if (!is.finite(v)) return(list(v = NA_real_, ok = FALSE))
    return(list(v = v, ok = TRUE))
  }
  if (nchar(t) > 13L) abort_unsupported(paste0("HEX", w, ". informat with more than 13 hexadecimal digits"))
  digits <- strtoi(strsplit(t, "")[[1]], 16L)
  list(v = Reduce(function(a, b) a * 16 + b, digits, 0), ok = TRUE)
}

read_num_field <- function(s, fmt) {
  nm <- if (fmt$name %in% c("", "f", "best", "bestd")) "w" else fmt$name
  # NLNUM in the English locale: thousands separators are ignored
  if (nm == "nlnum") {
    nm <- "w"
    s <- gsub(",", "", s, fixed = TRUE)
  }
  d <- fmt$d
  t <- strip_both(s)
  if (nm == "bz" && nzchar(s) && !nzchar(t)) return(list(v = 0, ok = TRUE))
  if (!nzchar(t) || t == ".") return(list(v = NA_real_, ok = TRUE))
  if (nm == "w") return(read_std_num(s, d))
  if (nm == "bz") return(read_std_num(gsub(" ", "0", s), d))
  if (nm == "hex") return(read_hex_num(t, fmt$w))
  if (nm == "trailsgn" && grepl("\\+$", t)) return(read_comma_num(sub("\\+\\s*$", "", s), d))
  if (nm %in% c("comma", "dollar", "trailsgn")) return(read_comma_num(s, d))
  if (nm %in% c("commax", "dollarx")) return(read_comma_num(s, d, swap = TRUE))
  if (nm == "percent") {
    r <- read_comma_num(s, d, pct_div = TRUE)
    if (r$ok && !is.na(r$v) && !grepl("%", t, fixed = TRUE)) r$v <- r$v
    return(r)
  }
  if (nm == "e") return(read_std_num(s, d))
  base <- nm
  if (substr(nm, 1L, nchar(nm) - 1L) %in% c("yymmdd", "mmddyy", "ddmmyy")) base <- substr(nm, 1L, nchar(nm) - 1L)
  v <- switch(base,
    date = read_date9(t),
    yymmdd = read_date3(t, "ymd"),
    mmddyy = read_date3(t, "mdy"),
    ddmmyy = read_date3(t, "dmy"),
    e8601da = , is8601da = if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", t)) read_date3(t, "ymd") else NA_real_,
    b8601da = if (grepl("^[0-9]{8}$|^[0-9]{4}-[0-9]{2}-[0-9]{2}$", t)) read_date3(t, "ymd") else NA_real_,
    e8601dt = , is8601dt = read_iso_dt(t),
    datetime = read_datetime(t),
    time = {
      # a leading minus sign is allowed; digits alone (no separator) are invalid (SAS 9.4)
      neg <- startsWith(t, "-")
      tt <- if (neg) substring(t, 2L) else t
      if (grepl("^[0-9]+$", tt)) NA_real_ else {
        v <- read_time(tt)
        if (neg) -v else v
      }
    },
    e8601tm = , is8601tm = {
      v <- read_time(t)
      if (!is.na(v) && v >= 86400) abort_unsupported("E8601TM informat with hours above 23")
      v
    },
    hhmmss = {
      if (grepl("^[0-9]+$", t)) {
        if (nchar(t) %% 2 == 1) t <- paste0("0", t)
        parts <- as.numeric(substring(t, seq(1, nchar(t), 2), seq(2, nchar(t), 2)))
        parts <- c(parts, 0, 0)[1:3]
        if (nchar(t) > 6 || parts[2] > 59 || parts[3] > 59) NA_real_ else sum(parts * c(3600, 60, 1))
      } else {
        v <- read_time(t)
        if (is.na(v)) NA_real_ else floor(v)
      }
    },
    monyy = {
      m <- regmatches(t, regexec("^([A-Za-z]{3})[-/ ]?([0-9]{4}|[0-9]{2})$", t))[[1]]
      if (length(m) == 0) NA_real_ else valid_ymd(expand_year(as.numeric(m[3]), nchar(m[3])), month_num(m[2]), 1)
    },
    yymmn = {
      if (grepl("^[0-9]{6}$", t)) valid_ymd(as.numeric(substr(t, 1, 4)), as.numeric(substr(t, 5, 6)), 1)
      else if (grepl("^[0-9]{4}$", t)) valid_ymd(expand_year(as.numeric(substr(t, 1, 2)), 2), as.numeric(substr(t, 3, 4)), 1)
      else NA_real_
    },
    yyq = {
      m <- regmatches(t, regexec("^([0-9]{2}|[0-9]{4})[Qq]([1-4])$", t))[[1]]
      if (length(m) == 0) NA_real_ else valid_ymd(expand_year(as.numeric(m[2]), nchar(m[2])), (as.numeric(m[3]) - 1) * 3 + 1, 1)
    },
    julian = {
      if (grepl("^[0-9]{5}$|^[0-9]{7}$", t)) {
        yd <- nchar(t) - 3L
        y <- expand_year(as.numeric(substr(t, 1, yd)), yd)
        dd <- as.numeric(substring(t, yd + 1L))
        st <- valid_ymd(y, 1, 1)
        if (dd < 1 || dd > 366 || sas_f_year(st + dd - 1) != y) NA_real_ else st + dd - 1
      } else NA_real_
    },
    anydtdte = read_anydate(t),
    anydtdtm = {
      # a time alone is a datetime on 1 January 1960
      if (grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]*)?)?$", t)) return(list(v = read_time(t), ok = TRUE))
      v <- read_iso_dt(t)
      if (is.na(v)) v <- read_datetime(t)
      if (is.na(v)) {
        dd <- read_anydate(t)
        v <- if (is.na(dd)) NA_real_ else dd * 86400
      }
      v
    },
    anydttme = {
      v <- read_time(t)
      if (is.na(v)) {
        # a datetime: its time part
        dtv <- read_datetime(t)
        if (!is.na(dtv)) v <- dtv - floor(dtv / 86400) * 86400
      }
      v
    },
    abort_unsupported(paste0("informat ", fmt$text))
  )
  list(v = v, ok = !is.na(v))
}

read_char_field <- function(s, fmt, keep_lead = FALSE) {
  out <- switch(fmt$name,
    char = s,
    upcase = toupper(s),
    hex = read_hex_field(s, fmt),
    quote = {
      t <- strip_both(s)
      if (nchar(t) >= 2 && substr(t, 1, 1) %in% c("'", "\"") && substring(t, nchar(t)) == substr(t, 1, 1)) {
        q <- substr(t, 1, 1)
        # doubled quotation marks inside become single ones
        t <- gsub(paste0(q, q), q, substr(t, 2L, nchar(t) - 1L), fixed = TRUE)
      }
      t
    },
    {
      t <- if (keep_lead) s else sub("^ +", "", s)
      if (strip_both(t) == ".") "" else t
    }
  )
  out
}

# $HEXw.: two hex digits per byte, so w gives w/2 bytes of output; an odd
# number of digits is completed with 0.  A non-hex field is invalid data: the
# INPUT function gives a blank, _ERROR_ and "Invalid argument to function
# INPUT" without the mathematical-operations NOTE (SAS 9.4 TS1M7 gold:
# fmt_matrix/HEXC_IN).  list(v, ok).
hex_field <- function(s, fmt) {
  w <- fmt$w
  if (is.na(w)) w <- nchar(strip_both(s))
  t <- substr(sub("^ +", "", s), 1L, w)
  if (nchar(t) %% 2L == 1L) t <- paste0(t, "0")
  if (!grepl("^[0-9A-Fa-f]*$", t)) return(list(v = "", ok = FALSE))
  if (!nzchar(t)) return(list(v = "", ok = TRUE))
  hi <- strtoi(substring(t, seq(1L, nchar(t), by = 2L), seq(2L, nchar(t), by = 2L)), 16L)
  list(v = rawToChar(as.raw(hi)), ok = TRUE)
}

# the INPUT statement: invalid $HEX data is not verified there (only the
# INPUT function is), so it fails loud
read_hex_field <- function(s, fmt) {
  r <- hex_field(s, fmt)
  if (!r$ok) abort_unsupported("invalid data for the $HEX informat in an INPUT statement (not verified against SAS)")
  r$v
}

apply_user_invalue <- function(s, uf) {
  t <- strip_both(s)
  if (isTRUE(uf$upcase)) t <- toupper(t)
  e <- NULL
  for (en in uf$entries) {
    if (isTRUE(en$other)) next
    if (en$num_key) {
      r <- read_std_num(t, NA_integer_)
      if (!r$ok || is.na(r$v)) next
      v <- r$v
      lo_ok <- isTRUE(en$lo_low) || (if (en$lo_excl) v > en[["lo"]] else v >= en[["lo"]])
      hi_ok <- isTRUE(en$hi_high) || (if (en$hi_excl) v < en[["hi"]] else v <= en[["hi"]])
      if (lo_ok && hi_ok) {
        e <- en
        break
      }
    } else if (is.null(en[["hi"]])) {
      if (identical(sas_rtrim(en[["lo"]]), t)) {
        e <- en
        break
      }
    } else if (chr_cmp(t, sas_rtrim(en[["lo"]])) >= 0 && chr_cmp(t, sas_rtrim(en[["hi"]])) <= 0) {
      e <- en
      break
    }
  }
  if (is.null(e)) for (en in uf$entries) if (isTRUE(en$other)) e <- en
  if (is.null(e) || identical(e$action, "_same_")) {
    if (uf$char) return(list(v = t, ok = TRUE))
    return(read_std_num(t, NA_integer_))
  }
  if (identical(e$action, "_error_")) return(list(v = if (uf$char) "" else NA_real_, ok = FALSE))
  list(v = e$value, ok = TRUE)
}

#' Read text with an informat (one value)
#' @return list(v, ok): the value and FALSE when the data are invalid.
#' @export
#' @keywords internal
sas_informat_read <- function(s, fmt, keep_lead = FALSE) {
  uf <- user_fmt(fmt$name, fmt$char, "invalue")
  if (!is.null(uf)) return(apply_user_invalue(s, uf))
  if (fmt$char && fmt$name == "hex") return(hex_field(s, fmt))
  if (fmt$char) return(list(v = read_char_field(s, fmt, keep_lead), ok = TRUE))
  read_num_field(s, fmt)
}

infmt_default_w <- function(fmt) {
  if (!is.na(fmt$w)) return(fmt$w)
  uf <- user_fmt(fmt$name, fmt$char, "invalue")
  if (!is.null(uf)) return(uf$default_w)
  nm <- if (fmt$name %in% c("", "f")) "w" else fmt$name
  if (fmt$char) return(NA_integer_)
  switch(nm, date = 7L, yymmdd = 6L, mmddyy = 6L, ddmmyy = 6L, datetime = 18L, time = 8L, monyy = 5L,
         e8601da = 10L, is8601da = 10L, e8601dt = 19L, is8601dt = 19L, anydtdte = 9L, anydtdtm = 19L,
         julian = 5L, comma = 1L, dollar = 1L, percent = 6L, 12L)
}

# PUT() / INPUT() functions ----------------------------------------------------

#' @export
sas_f_put <- function(x, fmt, align = NULL) {
  out <- sas_format(x, fmt)
  if (is.character(x) && !fmt$char && is.null(user_fmt(fmt$name, TRUE))) {
    abort_runtime(paste0("numeric format ", fmt$text, " applied to a character value in PUT()"))
  }
  if (!is.null(align)) {
    w <- nchar(out)
    s <- strip_both(out)
    out <- switch(align,
      l = just_left(s, w[1]),
      r = formatC(s, width = w[1]),
      c = vapply(seq_along(s), function(i) {
        lp <- (w[i] - nchar(s[i])) %/% 2
        just_left(paste0(strrep(" ", lp), s[i]), w[i])
      }, "")
    )
  }
  out
}

#' @export
sas_f_input <- function(s, fmt, qq = "", .at = NULL) {
  # a numeric source is converted with BEST12. (right-aligned, leading blanks kept)
  if (is.numeric(s)) s <- sas_fmt_best(s, 12L)
  w <- fmt$w
  is_char <- fmt$char || isTRUE(user_fmt(fmt$name, TRUE, "invalue")$char)
  # an informat without a width reads its default width (SAS 9.4: YYMMDD. reads 6 columns)
  if (is.na(w) && is.null(user_fmt(fmt$name, fmt$char, "invalue"))) w <- registry_defw("informat", fmt)
  vals <- vector(if (is_char) "character" else "numeric", length(s))
  bad <- FALSE
  for (i in seq_along(s)) {
    t <- s[i]
    if (!is.na(w) && nchar(t) > w) t <- substr(t, 1L, w)
    r <- sas_informat_read(t, fmt)
    if (!r$ok) bad <- TRUE
    vals[i] <- if (is_char) as.character(r$v) else as.numeric(r$v)
  }
  if (bad && qq != "??") {
    rt_error(if (qq == "?") NULL else paste0("NOTE: Invalid argument to function INPUT", at_text(.at), "."))
    # SAS also lists the call in the "Mathematical operations could not be performed" NOTE
    # (not for a character informat: $HEX, fmt_matrix/HEXC_IN)
    if (qq == "" && !is.null(.at) && !is_char) rt_count("math", .at)
  }
  if (is_char && !is.na(w)) vals <- sas_char(vals, w)
  vals
}

# PUTN/PUTC/INPUTN/INPUTC name their format at run time: the analyzer cannot
# check it, so every distinct format is checked against the feature registry
# here (strict mode: an unverified format fails loud; registry.R).
putn_check <- function(f, kind) {
  if (is.null(user_fmt(f$name, f$char, if (kind == "informat") "invalue" else "value"))) {
    feature_check(kind, fmt_registry_name(f), context = "runtime")
  }
  invisible(f)
}

#' @export
sas_f_putn <- function(x, fmt, w = NULL, d = NULL) {
  specs <- unique(fmt)
  for (sp in specs[!is.na(specs) & nzchar(trimws(specs))]) putn_check(putn_spec(sp, w, d), "format")
  if (length(specs) == 1) return(sas_format(x, putn_spec(specs, w, d)))
  vapply(seq_along(x), function(i) sas_format(x[i], putn_spec(rep_len(fmt, length(x))[i], w, d)), "")
}
#' @export
sas_f_putc <- function(x, fmt, w = NULL) sas_f_putn(x, fmt, w)

putn_spec <- function(text, w, d) {
  f <- sas_fmt_text(text)
  # the optional width/decimals override the format's only when valid (else see fn_policy)
  if (!is.null(w) && !is.na(w) && w >= 1 && w < 1e300) f$w <- as.integer(w)
  if (!is.null(d) && !is.na(d) && d >= 0 && d < 1e300) f$d <- as.integer(d)
  # $ without a width is $1.
  if (f$char && !nzchar(f$name) && is.na(f$w)) f$w <- 1L
  f
}

#' @export
sas_f_inputn <- function(s, fmt, w = NULL, d = NULL, .at = NULL) {
  for (sp in Filter(function(t) !is.na(t) && nzchar(trimws(t)), unique(fmt))) putn_check(putn_spec(sp, w, d), "informat")
  # invalid data: missing result, only the mathematical-operations NOTE and _ERROR_ (SAS 9.4)
  out <- vapply(seq_along(s), function(i) {
    f <- putn_spec(rep_len(fmt, length(s))[i], w, d)
    v <- as.numeric(sas_f_input(s[i], f, qq = "??"))
    t <- if (is.numeric(s[i])) sas_fmt_best(s[i], 12L) else s[i]
    if (!is.na(f$w) && nchar(t) > f$w) t <- substr(t, 1L, f$w)
    if (!sas_informat_read(t, f)$ok) note_math(.at)
    v
  }, 0)
  out
}
#' @export
sas_f_inputc <- function(s, fmt, w = NULL) {
  for (sp in Filter(function(t) !is.na(t) && nzchar(trimws(t)), unique(fmt))) {
    f <- putn_spec(sp, w, NULL)
    f$char <- TRUE
    putn_check(f, "informat")
  }
  vapply(seq_along(s), function(i) {
    f <- putn_spec(rep_len(fmt, length(s))[i], w, NULL)
    f$char <- TRUE
    sas_rtrim(sas_f_input(s[i], f))
  }, "")
}
