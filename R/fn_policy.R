# SAS run-time behaviour of function calls with missing or invalid arguments
# (verified against SAS 9.4 in the function verification matrix,
# tests/conformance/fn_matrix).
#
# For a function listed in `fn_policy`, translated code calls
# sas_fcall(name, f, args, .at): the implementation computes the values; the
# wrapper reproduces what SAS writes to the log and to _ERROR_:
#   miss   a missing value (any kind) in one of the positions `pos` (default:
#          every numeric argument):
#            "mv"    result missing, "Missing values were generated" NOTE
#            "math"  result missing, "Mathematical operations could not be
#                    performed" NOTE and _ERROR_ = 1
#            "inv"   result missing, "Invalid argument to function F(args)"
#                    NOTE, the mathematical-operations NOTE and _ERROR_ = 1
#            "err"   _ERROR_ = 1 only (character results: blank)
#            "none"  nothing (the implementation decides)
#   dom    function(args) giving, per row, "ok", "inv" (invalid argument),
#          "math" (mathematical operations NOTE only), "mv" (result missing,
#          missing-values NOTE) or "?" (SAS behaviour not determined: the
#          program stops with sasonr_unsupported)
#   nan    what a non-finite or missing result for non-missing, valid
#          arguments means ("inv", "math", "mv", "none")
#   math   FALSE: an invalid argument is not counted in the mathematical-
#          operations NOTE (functions with character results)

fpol <- function(miss = "none", pos = NULL, dom = NULL, nan = "inv", math = TRUE) {
  list(miss = miss, pos = pos, dom = dom, nan = nan, math = math)
}

# ---- domain helpers --------------------------------------------------------------

dom_n <- function(args, k) {
  n <- max(lengths(args))
  if (k > length(args)) return(rep(NA_real_, n))
  a <- args[[k]]
  if (!is.numeric(a)) return(rep(NA_real_, n))
  rep_len(as.numeric(a), n)
}
# combine row codes: the first non-"ok" code wins
dom_or <- function(...) {
  cs <- list(...)
  out <- cs[[1]]
  for (c in cs[-1]) out <- ifelse(out == "ok", c, out)
  out
}
code_if <- function(cond, code) ifelse(!is.na(cond) & cond, code, "ok")
# |x| <= ok: valid; |x| >= bad: `code`; in between: undetermined ("?")
dom_huge <- function(x, ok, bad = 1e300, code = "inv") {
  ifelse(is.na(x) | abs(x) <= ok, "ok", ifelse(abs(x) >= bad, code, "?"))
}
dom_all_huge <- function(args, ok, bad = 1e300, code = "inv") {
  out <- rep("ok", max(lengths(args)))
  for (k in seq_along(args)) if (is.numeric(args[[k]])) out <- dom_or(out, dom_huge(dom_n(args, k), ok, bad, code))
  out
}
is_int <- function(x) !is.na(x) & is.finite(x) & x == trunc(x)
sas_date_ok <- function(d) !is.na(d) & floor(d) >= -138061 & floor(d) <= 6589335

# date and time parts: valid SAS dates (else invalid), huge values invalid
dom_date <- function(args) {
  x <- dom_n(args, 1)
  ifelse(is.na(x) | sas_date_ok(x), "ok", ifelse(abs(x) >= 1e300 | floor(x) == -138062 | floor(x) == 6589336, "inv", "?"))
}
dom_date_huge <- function(args) {
  x <- dom_n(args, 1)
  ifelse(is.na(x) | sas_date_ok(x), "ok", ifelse(abs(x) >= 1e300, "inv", "?"))
}
dom_time <- function(args) dom_huge(dom_n(args, 1), 1e10)
dom_datetime <- function(args) {
  x <- dom_n(args, 1)
  ifelse(is.na(x) | sas_date_ok(x / 86400), "ok", ifelse(abs(x) >= 1e15, "inv", "?"))
}

# descriptive statistics: an argument of magnitude 1E300 is an invalid argument
# (SAS 9.4 gold fn_matrix MEAN_00b9bb, CSS_8953d9 and the other "huge" cases);
# no gold value lies between 1E150 and 1E300, so that range fails loud ("?";
# below 1E150 the squares of CSS/USS/VAR cannot overflow); fewer than `need`
# non-missing values give a missing result with the missing-values NOTE
dom_stat <- function(need, zero_var = FALSE, zero_mean = FALSE) {
  force(need)
  function(args) {
    m <- args_matrix(lapply(args, as.numeric))
    vapply(seq_len(nrow(m)), function(i) {
      v <- m[i, !is.na(m[i, ])]
      if (any(abs(v) >= 1e300)) return("inv")
      if (any(abs(v) > 1e150)) return("?")
      if (length(v) < need) return("mv")
      if (zero_var && length(unique(v)) == 1L) return("inv")
      if (zero_mean && mean(v) == 0) return("inv")
      "ok"
    }, "")
  }
}

# probability functions: parameters outside their domain give the
# mathematical-operations NOTE (non-finite R results); huge or tiny arguments
# are not reproduced
dom_prob <- function(args) {
  out <- rep("ok", max(lengths(args)))
  for (k in seq_along(args)) {
    x <- dom_n(args, k)
    if (all(is.na(x))) next
    out <- dom_or(out, ifelse(!is.na(x) & (abs(x) > 1e15 | (x != 0 & abs(x) < 1e-200)), "?", "ok"))
  }
  out
}
dom_dist <- function(args) {
  out <- dom_prob(args[-1])
  d <- tolower(strip_both(as_sas_chr(args[[1]])))
  # an unknown distribution name: the implementation writes the ERROR, the place is counted
  out[rep_len(!vapply(d, sas_dist_known, TRUE), length(out))] <- "ok"
  out
}

int_ok <- function(x) is.na(x) | is_int(x)

# ---- the table -----------------------------------------------------------------------

fn_policy <- list(
  # trigonometric, hyperbolic, exponential, logarithms
  arcos = fpol("mv", dom = function(a) code_if(abs(dom_n(a, 1)) > 1, "inv")),
  arsin = fpol("mv", dom = function(a) code_if(abs(dom_n(a, 1)) > 1, "inv")),
  atan = fpol("mv"),
  atan2 = fpol("mv", dom = function(a) code_if(dom_n(a, 1) == 0 & dom_n(a, 2) == 0, "inv")),
  cos = fpol("mv", dom = function(a) dom_huge(dom_n(a, 1), 1e10)),
  sin = fpol("mv", dom = function(a) dom_huge(dom_n(a, 1), 1e10)),
  tan = fpol("mv", dom = function(a) {
    # SAS 9.4 gold fn_matrix TAN_577ff2: TAN(1.5707963267948966), the double
    # nearest pi/2, is an invalid argument.  Other arguments whose cosine is
    # that close to 0 (below 1E-15 in magnitude) have no gold: fail loud.
    x <- dom_n(a, 1)
    dom_or(dom_huge(x, 1e10), code_if(abs(x) == 1.5707963267948966, "inv"), code_if(abs(cos(x)) < 1e-15, "?"))
  }),
  cosh = fpol("mv", dom = function(a) dom_huge(dom_n(a, 1), log(.Machine$double.xmax), 710)),
  sinh = fpol("mv", dom = function(a) dom_huge(dom_n(a, 1), log(.Machine$double.xmax), 710)),
  tanh = fpol("mv"),
  exp = fpol("mv", dom = function(a) code_if(dom_n(a, 1) > log(.Machine$double.xmax), "inv")),
  log = fpol("mv", dom = function(a) code_if(dom_n(a, 1) <= 0, "inv")),
  log10 = fpol("mv", dom = function(a) code_if(dom_n(a, 1) <= 0, "inv")),
  log2 = fpol("mv", dom = function(a) code_if(dom_n(a, 1) <= 0, "inv")),
  sqrt = fpol("mv", dom = function(a) code_if(dom_n(a, 1) < 0, "inv")),
  gamma = fpol("mv", dom = function(a) {
    x <- dom_n(a, 1)
    # SAS 9.4 gold fn_matrix GAMMA_84cec2 / GAMMA_bc82ce: GAMMA(171) is a number,
    # GAMMA(172) an invalid argument; arguments in between have no gold (fail loud)
    dom_or(code_if(x <= 0 & is_int(x), "inv"), code_if(x >= 172, "inv"), code_if(x > 171 & x < 172, "?"),
           code_if(x > 0 & x <= 1e-300, "inv"),
           code_if(x > 1e-300 & x < 1e-15, "?"), code_if(x < -170, "?"))
  }),
  lgamma = fpol("mv", dom = function(a) code_if(dom_n(a, 1) <= 0, "inv")),
  probit = fpol("mv", dom = function(a) {
    p <- dom_n(a, 1)
    code_if(p <= 0 | p >= 1, "inv")
  }),
  probnorm = fpol("mv"),
  # dates and times
  datejul = fpol("mv", dom = function(a) {
    j <- dom_n(a, 1)
    y <- floor(j / 1000)
    dd <- j - y * 1000
    yy <- ifelse(y < 100, ifelse(y < 40, 2000 + y, 1900 + y), y)
    leap <- (yy %% 4 == 0 & yy %% 100 != 0) | yy %% 400 == 0
    dom_or(code_if(abs(j) >= 1e300, "inv"), code_if(abs(j) > 1e9, "?"), code_if(j < 1 | dd < 1 | dd >= ifelse(leap, 367, 366), "inv"),
           code_if(dd != floor(dd), "?"), code_if(yy < 1582 | yy > 20000, "?"))
  }),
  datepart = fpol("mv", dom = dom_datetime),
  timepart = fpol("mv", dom = dom_datetime),
  day = fpol("mv", dom = dom_date),
  month = fpol("mv", dom = dom_date),
  year = fpol("mv", dom = dom_date),
  qtr = fpol("mv", dom = dom_date_huge),
  juldate = fpol("mv", dom = dom_date_huge),
  juldate7 = fpol("mv", dom = dom_date_huge),
  hour = fpol("mv", dom = dom_time),
  minute = fpol("mv", dom = dom_time),
  second = fpol("mv", dom = dom_time),
  hms = fpol("mv", dom = function(a) dom_or(dom_all_huge(a, 1e15, 1e300, "?"),
                                            code_if(dom_n(a, 1) * 3600 + dom_n(a, 2) * 60 + dom_n(a, 3) < 0, "inv"))),
  dhms = fpol("mv", dom = function(a) dom_or(dom_huge(dom_n(a, 1), 6589335 * 1.0, 1e300, "inv"),
                                             dom_all_huge(a[-1], 1e15, 1e300, "?"))),
  # combinatorics
  fact = fpol("inv", dom = function(a) {
    n <- dom_n(a, 1)
    code_if(n < 0 | !int_ok(n) | n > 170, "inv")
  }),
  comb = fpol("math", dom = function(a) {
    n <- dom_n(a, 1)
    r <- dom_n(a, 2)
    dom_or(code_if(n >= 1e300 | r >= 1e300, "inv"), code_if(abs(n) > 1e15 & n > -1e300 | abs(r) > 1e15, "?"),
           code_if(!int_ok(n) | !int_ok(r) | n < 0 | r < 0 | r > n, "math"))
  }, nan = "math"),
  perm = fpol("mv", dom = function(a) {
    n <- dom_n(a, 1)
    r <- dom_n(a, 2)
    dom_or(code_if(abs(n) > 1e15 | (!is.na(r) & abs(r) > 1e15), "?"),
           code_if(!int_ok(n) | !int_ok(r) | n < 0 | (!is.na(r) & (r < 0 | r > n)), "math"),
           code_if(is.na(r) & n > 170, "inv"))
  }, nan = "math"),
  beta = fpol("math", dom = function(a) code_if(dom_n(a, 1) <= 0 | dom_n(a, 2) <= 0, "math"), nan = "math"),
  logbeta = fpol("math", dom = function(a) code_if(dom_n(a, 1) <= 0 | dom_n(a, 2) <= 0, "math"), nan = "math"),
  gcd = fpol("math", dom = function(a) {
    out <- rep("ok", max(lengths(a)))
    for (k in seq_along(a)) {
      x <- dom_n(a, k)
      out <- dom_or(out, code_if(abs(x) >= 1e15, "?"), code_if(!int_ok(x), "math"), code_if(abs(x) >= 2^40, "math"),
                    code_if(abs(x) >= 2^31, "?"))
    }
    out
  }, nan = "math"),
  lcm = fpol("math", dom = function(a) {
    out <- rep("ok", max(lengths(a)))
    for (k in seq_along(a)) {
      x <- dom_n(a, k)
      out <- dom_or(out, code_if(abs(x) >= 1e15, "?"), code_if(!int_ok(x) | x == 0, "math"))
    }
    out
  }, nan = "none"),
  # order statistics: the index must be an integer from 1 to the number of (non-missing) values
  largest = fpol("math", pos = 1L, dom = function(a) kth_dom(a, FALSE), nan = "none"),
  smallest = fpol("math", pos = 1L, dom = function(a) kth_dom(a, FALSE), nan = "none"),
  # dates: mdy, datdif, yrdif
  mdy = fpol("mv", dom = function(a) dom_all_huge(a, 1e15, 1e300, "inv"), nan = "inv"),
  datdif = fpol("inv", pos = 1:2, dom = function(a) dom_or(dom_all_huge(a[1:2], 7e6, 1e300, "inv")), nan = "inv"),
  yrdif = fpol("inv", pos = 1:2, dom = function(a) dom_or(dom_all_huge(a[1:2], 7e6, 1e300, "inv")), nan = "inv"),
  # remainders
  mod = fpol("mv", dom = function(a) dom_or(dom_huge(dom_n(a, 1), 2^53, 1e300, "inv"), code_if(dom_n(a, 2) == 0, "math")),
              nan = "none"),
  modz = fpol("mv", dom = function(a) dom_or(dom_huge(dom_n(a, 1), 2^53, 1e300, "inv"), code_if(dom_n(a, 2) == 0, "inv")),
               nan = "none"),
  constant = fpol(dom = function(a) {
    nm <- tolower(strip_both(as_sas_chr(a[[1]])))
    p <- dom_n(a, 2)
    dom_or(code_if(length(a) > 1 & nm == "exactint" & abs(p) >= 1e15, "?"),
           code_if(length(a) > 1 & !is.na(p) & nm != "exactint" & nm %in% c("big", "e", "euler", "logbig", "logmaceps",
                                                                            "logsmall", "maceps", "pi", "small", "sqrtbig",
                                                                            "sqrtmaceps", "sqrtsmall"), "math"),
           code_if(length(a) > 1 & nm == "exactint" & p == 2, "?"))
  }, nan = "math"),
  # character results with numeric arguments: a missing count or position only sets _ERROR_
  char = fpol("err", pos = 2L, nan = "none", math = FALSE),
  `repeat` = fpol("err", pos = 2L, dom = function(a) {
    n <- dom_n(a, 2)
    dom_or(code_if(n < 0, "err"), code_if(n > 1e15 & n < 1e300, "?"))
  }, nan = "none", math = FALSE),
  substrn = fpol("err", pos = 2:3, dom = function(a) dom_or(code_if(dom_n(a, 2) <= -1e300, "inv"), dom_huge(dom_n(a, 2), 2^31, 1e300, "ok"),
                                                            dom_huge(dom_n(a, 3), 2^31, 1e300, "ok")),
                 nan = "none", math = FALSE),
  collate = fpol("err", dom = function(a) {
    s <- dom_n(a, 1)
    e <- if (length(a) >= 2 && !is.null(a[[2]])) dom_n(a, 2) else rep(NA_real_, length(s))
    has_e <- length(a) >= 2 && !is.null(a[[2]])
    l <- if (length(a) >= 3 && !is.null(a[[3]])) dom_n(a, 3) else rep(NA_real_, length(s))
    last <- if (has_e) e else ifelse(is.na(l), 255, s + l - 1)
    dom_or(code_if(s < 0 | (has_e & e < s) | (has_e & e != trunc(e)), "err"),
           code_if(abs(e) >= 1e300 | abs(l) >= 1e300, "inv"),
           code_if(s < 32 | s != trunc(s) | (last > 126 & last >= s), "?"))
  }, nan = "none", math = FALSE),
  quote = fpol(dom = function(a) {
    x <- as_sas_chr(a[[1]])
    x <- sas_rtrim(x)
    code_if(nchar(x) + 2 + nchar(gsub("[^\"]", "", x)) > 200, "inv")
  }, nan = "none", math = FALSE),
  week = fpol("mv", pos = 1L, dom = function(a) {
    n <- max(lengths(a))
    if (is.character(a[[1]])) return(rep("math", n))
    d <- if (length(a) >= 2) tolower(strip_both(as_sas_chr(a[[2]]))) else "u"
    dom_or(dom_date_huge(a[1]), code_if(rep_len(!substr(d, 1L, 1L) %in% c("u", "v", "w") | nchar(d) != 1L, n), "math"))
  }, nan = "none"),
  nvalid = fpol(dom = function(a) {
    o <- if (length(a) >= 2) tolower(strip_both(as_sas_chr(a[[2]]))) else "v7"
    code_if(!o %in% c("", "v7", "any", "nliteral", "upcase"), "math")
  }, nan = "none"),
  prxchange = fpol("err", pos = 2L, dom = function(a) code_if(dom_n(a, 2) <= -1e300, "inv"), nan = "none", math = FALSE),
  putc = fpol(dom = function(a) width_dom(a, TRUE), nan = "none", math = FALSE),
  putn = fpol(dom = function(a) width_dom(a, FALSE), nan = "none", math = FALSE),
  inputc = fpol(dom = function(a) {
    f <- lapply(as_sas_chr(a[[2]]), sas_fmt_text)
    unknown <- !vapply(f, function(x) {
      x$char <- TRUE
      infmt_known(x)
    }, TRUE)
    w <- dom_n(a, 3)
    dom_or(code_if(rep_len(unknown, max(lengths(a))), "err"), code_if(w < 0 & w != -1 & w > -1e300, "err"), width_dom(a, TRUE))
  }, nan = "none", math = FALSE),
  inputn = fpol(dom = function(a) width_dom(a, FALSE, "math"), nan = "none"),
  ordinal = fpol("math", pos = 1L, dom = function(a) kth_dom(a, TRUE), nan = "none"),
  pctl = fpol("math", pos = 1L, dom = function(a) {
    p <- dom_n(a, 1)
    code_if(p < 0 | p > 100, "math")
  }, nan = "none"),
  # descriptive statistics
  css = fpol(dom = dom_stat(1), nan = "none"),
  uss = fpol(dom = dom_stat(1), nan = "none"),
  var = fpol(dom = dom_stat(2), nan = "none"),
  std = fpol(dom = dom_stat(2), nan = "none"),
  stderr = fpol(dom = dom_stat(2), nan = "none"),
  cv = fpol(dom = dom_stat(2, zero_mean = TRUE), nan = "none"),
  skewness = fpol(dom = dom_stat(3, zero_var = TRUE), nan = "none"),
  kurtosis = fpol(dom = dom_stat(4, zero_var = TRUE), nan = "none"),
  geomean = fpol(dom = function(a) dom_or(all_missing_mv(a), mean_neg(a), mean_extreme(a)), nan = "none"),
  harmean = fpol(dom = function(a) dom_or(all_missing_mv(a), mean_neg(a), mean_extreme(a)), nan = "none"),
  range = fpol(dom = function(a) all_missing_mv(a), nan = "none"),
  sign = fpol("mv", nan = "none"),
  # bitwise operations on unsigned 32-bit integers
  band = fpol("math", dom = function(a) bit_dom(a), nan = "math"),
  bor = fpol("math", dom = function(a) bit_dom(a), nan = "math"),
  bxor = fpol("math", dom = function(a) bit_dom(a), nan = "math"),
  bnot = fpol("inv", dom = function(a) code_if(dom_n(a, 1) < 0 | dom_n(a, 1) >= 2^32, "inv")),
  blshift = fpol("math", dom = function(a) dom_or(bit_dom(a[1]), code_if(dom_n(a, 2) < 0 | dom_n(a, 2) > 31, "math"),
                                                  code_if(abs(dom_n(a, 2)) >= 2^32, "math")), nan = "math"),
  brshift = fpol("math", dom = function(a) dom_or(bit_dom(a[1]), code_if(dom_n(a, 2) < 0 | dom_n(a, 2) > 31, "math"),
                                                  code_if(abs(dom_n(a, 2)) >= 2^32, "math")), nan = "math"),
  # probability functions
  probt = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 2) > 1e7, "?"), code_if(dom_n(a, 2) <= 0, "math")),
               nan = "math"),
  probchi = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 1) < 0 | dom_n(a, 2) <= 0 | dom_n(a, 3) < 0, "math")),
                 nan = "math"),
  probf = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 1) < 0 | dom_n(a, 2) <= 0 | dom_n(a, 3) <= 0 |
                                                                     dom_n(a, 4) < 0, "math")), nan = "math"),
  tinv = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 1) <= 0 | dom_n(a, 1) >= 1 | dom_n(a, 2) <= 0, "math")),
              nan = "math"),
  cinv = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 1) < 0 | dom_n(a, 1) >= 1 | dom_n(a, 2) <= 0 |
                                                                     dom_n(a, 3) < 0, "math")), nan = "math"),
  finv = fpol("mv", dom = function(a) dom_or(dom_prob(a), code_if(dom_n(a, 1) < 0 | dom_n(a, 1) >= 1 | dom_n(a, 2) <= 0 |
                                                                     dom_n(a, 3) <= 0 | dom_n(a, 4) < 0, "math")), nan = "math"),
  probbnml = fpol("mv", dom = function(a) {
    p <- dom_n(a, 1)
    n <- dom_n(a, 2)
    m <- dom_n(a, 3)
    dom_or(dom_prob(a), code_if(p < 0 | p > 1 | !int_ok(n) | n < 1 | !int_ok(m) | m < 0 | m > n, "math"))
  }, nan = "math"),
  poisson = fpol("mv", dom = function(a) {
    m <- dom_n(a, 1)
    n <- dom_n(a, 2)
    dom_or(dom_prob(a), code_if(m < 0 | n < 0 | !int_ok(n), "math"))
  }, nan = "math"),
  cdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  sdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  pdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  logcdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  logsdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  logpdf = fpol("mv", pos = 2:6, dom = dom_dist, nan = "math"),
  quantile = fpol("mv", pos = 2:6, dom = function(a) {
    d <- tolower(strip_both(as_sas_chr(a[[1]])))
    dom_or(dom_dist(a), code_if(dom_n(a, 2) < 0 | dom_n(a, 2) > 1, "math"),
           code_if(rep_len(d %in% c("binomial", "binom"), length(dom_n(a, 2))) & !int_ok(dom_n(a, 4)), "math"),
           code_if(rep_len(d %in% c("binomial", "binom"), length(dom_n(a, 2))) & dom_n(a, 4) == 0, "inv"))
  }, nan = "math"),
  # selection
  choosen = fpol("mv", pos = 1L, dom = function(a) choose_dom(a), nan = "none"),
  choosec = fpol("mv", pos = 1L, dom = function(a) choose_dom(a), nan = "none", math = FALSE)
)

# letters outside ASCII: case mapping depends on the session encoding (not verified)
case_dom <- function(a) {
  x <- as_sas_chr(a[[1]])
  code_if(grepl("[^\\x01-\\x7f]", x, perl = TRUE), "?")
}
for (nm in c("upcase", "lowcase", "propcase")) fn_policy[[nm]] <- fpol(dom = case_dom, nan = "none", math = FALSE)
fn_policy$indexw <- fpol(dom = function(a) {
  # delimiters that also occur in the word: not verified
  if (length(a) < 3) return(rep("ok", max(lengths(a))))
  w <- strip_both(as_sas_chr(a[[2]]))
  d <- as_sas_chr(a[[3]])
  code_if(mapply(function(w, d) any(strsplit(w, "")[[1]] %in% strsplit(d, "")[[1]]), rep_len(w, max(lengths(a))),
                 rep_len(d, max(lengths(a)))), "?")
}, nan = "none")
fn_policy$findw <- fpol(dom = function(a) {
  n <- max(lengths(a))
  if (length(a) >= 3 && is.numeric(a[[3]])) return(rep("?", n))
  rep("ok", n)
}, nan = "none")
fn_policy$weekday <- fpol("mv", dom = function(a) {
  x <- dom_n(a, 1)
  dom_or(dom_date_huge(a), code_if(floor(x) < -137774, "?"))
})
fn_policy$byte <- fpol("mv", dom = function(a) {
  x <- dom_n(a, 1)
  dom_or(code_if(x < 0 | x >= 256, "inv"), code_if(x >= 0 & x < 1, "?"), code_if(x >= 128 & x < 256, "?"))
}, nan = "none", math = FALSE)
for (nm in c("find", "findc")) {
  fn_policy[[nm]] <- fpol("mv", pos = 4L, dom = function(a) {
    n <- max(lengths(a))
    if (length(a) >= 4 && is.character(a[[4]])) return(rep("math", n))
    rep("ok", n)
  }, nan = "none")
}

for (nm in c(paste0("any", c("alnum", "alpha", "digit", "lower", "punct", "space", "upper")),
              paste0("not", c("alnum", "alpha", "digit", "lower", "punct", "space", "upper")))) {
  fn_policy[[nm]] <- fpol(dom = function(a) dom_huge(dom_n(a, 2), 2^31 - 1), nan = "none")
}

kth_dom <- function(a, count_missing) {
  k <- dom_n(a, 1)
  m <- args_matrix(lapply(a[-1], as.numeric))
  n <- if (count_missing) rep(ncol(m), nrow(m)) else rowSums(!is.na(m))
  n <- rep_len(n, length(k))
  # no non-missing value at all: missing result with the missing-values NOTE
  dom_or(code_if(!count_missing & n == 0 & int_ok(k) & k >= 1, "mv"), code_if(!int_ok(k) | k < 1 | k > n, "math"))
}
# values above 1e100 or below 1e-100 (other than 0): SAS 9.4 gives 0 by its own algorithm (not reproduced)
mean_extreme <- function(a) {
  m <- args_matrix(lapply(a, as.numeric))
  code_if(apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    length(unique(v)) > 1 && any(abs(v) > 1e100 | (v != 0 & abs(v) < 1e-100))
  }), "?")
}
# the optional width and decimals of PUTC/PUTN/INPUTC/INPUTN: a missing or huge
# value is ignored, an integer >= 1 is used, 0 or a fraction gives a blank result
# and _ERROR_; negative widths are ignored by PUTC/INPUTC (-1 also by the others)
width_dom <- function(a, char, code = "err") {
  out <- rep("ok", max(lengths(a)))
  for (k in 3:4) {
    if (k > length(a) || is.null(a[[k]])) next
    w <- dom_n(a, k)
    if (k == 4) {
      # decimals: truncated; negative values not verified
      out <- dom_or(out, ifelse(is.na(w) | w >= 0, "ok", "?"))
      next
    }
    out <- dom_or(out, ifelse(is.na(w) | abs(w) >= 1e300 | (w >= 1 & w == trunc(w)), "ok",
                              ifelse(w >= 0, code, ifelse(char | w == -1, "ok", "?"))))
  }
  out
}
# no non-missing argument: missing result with the missing-values NOTE
all_missing_mv <- function(a) {
  m <- args_matrix(lapply(a, as.numeric))
  code_if(rowSums(!is.na(m)) == 0, "mv")
}
mean_neg <- function(a) {
  m <- args_matrix(lapply(a, as.numeric))
  code_if(apply(m, 1, function(r) any(r < 0, na.rm = TRUE)), "math")
}
bit_dom <- function(a) {
  out <- rep("ok", max(lengths(a)))
  for (k in seq_along(a)) {
    x <- dom_n(a, k)
    out <- dom_or(out, code_if(x < 0 | x >= 2^32, "math"))
  }
  out
}
choose_dom <- function(a) {
  k <- dom_n(a, 1)
  n <- length(a) - 1L
  k <- floor(k)
  code_if(k == 0 | abs(k) > n, "inv")
}

#' Call a SAS function with SAS's handling of missing and invalid arguments
#'
#' Used by translated code for the functions in the internal policy table:
#' counts the log NOTEs ("Missing values were generated", "Invalid argument
#' to function", "Mathematical operations could not be performed"), sets
#' `_ERROR_` and gives a missing result where SAS does.
#' @param .name Function name.
#' @param .f Implementation.
#' @param ... Arguments.
#' @param .at `c(line, column)` of the call.
#' @param .exact `FALSE` in vectorised code (counts are redone row by row).
#' @keywords internal
#' @export
sas_fcall <- function(.name, .f, ..., .at = NULL, .exact = TRUE) {
  args <- list(...)
  p <- fn_policy[[.name]]
  n <- if (length(args)) max(lengths(args)) else 1L
  miss <- rep(FALSE, n)
  pos <- p$pos %||% seq_along(args)
  for (k in pos[pos <= length(args)]) {
    a <- args[[k]]
    if (is.numeric(a) && length(a)) miss <- miss | rep_len(is.na(a), n)
  }
  dom <- if (is.null(p$dom)) rep("ok", n) else rep_len(p$dom(args), n)
  if (p$miss != "none") dom[miss] <- "ok"
  if (any(dom == "?")) {
    abort_unsupported(sprintf("%s with these arguments (SAS 9.4 behaviour not verified)", toupper(.name)),
                      .at[1] %||% NULL)
  }
  num <- !identical(fn_table[[.name]]$type, "char")
  blank <- if (num) NA_real_ else ""
  # no valid row: the implementation is not called (arguments may be of the wrong type)
  fargs <- if (".at" %in% names(formals(.f))) c(args, list(.at = .at)) else args
  # R warnings of the implementation (NaNs produced ...) are not SAS messages:
  # the SAS NOTEs come from the policy above.  Only that warning class is
  # muffled; any other warning reaches the caller.
  res <- if (all(miss & p$miss != "none" | dom != "ok")) rep(blank, n) else withCallingHandlers(
    do.call(.f, fargs),
    warning = function(w) if (grepl("NaNs produced|value out of range", conditionMessage(w))) invokeRestart("muffleWarning")
  )
  if (num && p$nan != "none") {
    bad_res <- !miss & dom == "ok" & (is.infinite(res) | is.na(res))
    dom[bad_res] <- p$nan
  }
  note <- function(kind, rows) {
    k <- sum(rows)
    if (k == 0) return()
    if (isTRUE(rt$sql) && kind %in% c("mv", "math")) {
      # PROC SQL prints neither the "Missing values were generated" NOTE nor the
      # mathematical-operations summary (gold standard); an invalid argument has
      # its own PROC SQL NOTE in note_invalid_arg().
      return()
    }
    switch(kind,
      mv = {
        if (!.exact) rt$mv_rerun <- TRUE
        rt_count("mv", .at, k)
      },
      math = note_math(.at, k),
      inv = note_invalid_arg(.name, NULL, .at, args, rows, math = p$math),
      err = rt_error(),
      NULL
    )
  }
  if (p$miss != "none") note(p$miss, miss)
  for (kind in c("inv", "math", "mv", "err")) note(kind, dom == kind)
  set_blank <- (miss & p$miss != "none") | dom != "ok"
  if (any(set_blank)) res[set_blank] <- blank
  if (num && .name %in% plain_missing_fns) res[is.na(res)] <- NA_real_
  if (!num) res[is.na(res)] <- ""
  res
}

# functions whose missing results are always the ordinary missing value
plain_missing_fns <- setdiff(names(fn_policy), c("choosen", "largest", "smallest", "ordinal", "pctl"))
