# PROC UNIVARIATE (spec 05 P0-3), block U1: NOPRINT with OUTPUT OUT=.
#
# Measured:
# - the OUT= data set has one row with one column per requested statistic;
#   the columns follow an internal order of the statistics, not the order of
#   the request; each column has the label "<description>, <variable>";
# - the step writes no "observations read" NOTE.
# Everything else (printed output, BY, CLASS, WEIGHT, FREQ, ID, PCTLPTS=,
# several analysis variables in one OUTPUT statement, statistics whose
# position in the order is not measured) fails loud.

univ_stat_alias <- c(n = "n", nmiss = "nmiss", nobs = "nobs", mean = "mean", sum = "sum", std = "std",
                     stddev = "std", var = "var", skewness = "skew", skew = "skew", kurtosis = "kurt", kurt = "kurt",
                     sumwgt = "sumwgt", max = "max", min = "min", range = "range", q3 = "q3", p75 = "q3",
                     median = "median", p50 = "median", q1 = "q1", p25 = "q1", qrange = "qrange", p1 = "p1",
                     p5 = "p5", p10 = "p10", p90 = "p90", p95 = "p95", p99 = "p99", mode = "mode", css = "css",
                     uss = "uss", cv = "cv", stdmean = "stderr", t = "t", probt = "probt", msign = "msign",
                     probm = "probm", signrank = "signrank", probs = "probs")

# The order of the OUT= columns (measured with all keywords, in both orders)
# and the label of each column (followed by ", <variable>").
univ_out_order <- c("n", "mean", "std", "skew", "uss", "cv", "sumwgt", "sum", "var", "kurt", "css", "stderr",
                    "nmiss", "nobs", "max", "p99", "p95", "p90", "q3", "median", "q1", "p10", "p5", "p1", "min",
                    "range", "qrange", "mode", "t", "msign", "signrank", "probt", "probm", "probs")

univ_stat_label <- c(n = "number of nonmissing values", mean = "the mean", std = "the standard deviation",
                     skew = "skewness", uss = "the uncorrected sum of squares", cv = "the coefficient of variation",
                     sumwgt = "sum of the weights", sum = "the sum", var = "the variance", kurt = "kurtosis",
                     css = "the corrected sum of squares", stderr = "the standard error of the mean",
                     nmiss = "number of missing values", nobs = "number of observations", max = "the largest value",
                     p99 = "the 99th percentile", p95 = "the 95th percentile", p90 = "the 90th percentile",
                     q3 = "the upper quartile", median = "the median", q1 = "the lower quartile",
                     p10 = "the 10th percentile", p5 = "the 5th percentile", p1 = "the 1st percentile",
                     min = "the smallest value", range = "the range", qrange = "the interquartile range",
                     mode = "the most frequent value", t = "Student's t statistic", msign = "the sign statistic",
                     signrank = "the signed rank statistic", probt = "p-value of the t statistic",
                     probm = "p-value of the sign statistic", probs = "p-value of signed rank stat")

parse_proc_univariate <- function(p, line) {
  advance(p)
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC UNIVARIATE", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC UNIVARIATE"), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o == "data") {
        opts$data <- parse_ds_ref(p, "input")
        next
      }
      if (o %in% c("pctldef")) {
        v <- parse_opt_number(p, oline, int = TRUE)
        if (!v %in% 1:5) abort_syntax("PCTLDEF= must be 1 to 5", oline)
        opts$pctldef <- as.integer(v)
        next
      }
      if (o == "vardef") {
        v <- tolower(parse_opt_value(p, o, oline))
        if (v == "wgt") v <- "weight"
        if (!v %in% c("df", "n", "wdf", "weight")) abort_syntax(paste0("Invalid VARDEF= value ", toupper(v)), oline)
        opts$vardef <- v
        next
      }
      abort_unsupported(paste0("PROC UNIVARIATE option ", toupper(o), "="), oline)
    }
    if (o != "noprint") abort_unsupported(paste0("PROC UNIVARIATE option ", toupper(o)), oline)
    opts$noprint <- TRUE
    advance(p)
  }
  advance(p)
  body <- proc_parse_body(p, "univariate", function(p, kw, sline) {
    switch(kw,
      var = parse_items_stmt(p, "var"),
      output = parse_univ_output(p),
      where = parse_where(p),
      format = parse_format_stmt(p),
      label = parse_label(p),
      attrib = parse_attrib(p),
      by = parse_by(p),
      class = parse_items_stmt(p, "class"),
      weight = parse_items_stmt(p, "weight"),
      freq = , id = , histogram = , qqplot = , probplot = , ppplot = , cdfplot = ,
      inset = abort_unsupported(paste0("PROC UNIVARIATE ", toupper(kw), " statement"), sline),
      abort_syntax("Statement is not valid or it is used out of proper order. (ERROR 180-322)", sline, tk_col(p), tk_val(p))
    )
  })
  list(type = "proc", name = "univariate", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

parse_univ_output <- function(p) {
  line <- tk_line(p)
  advance(p)
  out <- list(stmt = "output", specs = list(), line = line)
  while (tk_type(p) == "name") {
    kw <- tk_low(p)
    sline <- tk_line(p)
    if (kw == "out" && is_op(p, "=", 1L)) {
      advance(p, 2L)
      out$out <- parse_ds_ref(p, "output")
      next
    }
    if (kw %in% c("pctlpts", "pctlpre", "pctlname") && is_op(p, "=", 1L)) {
      advance(p, 2L)
      vals <- character()
      while (tk_type(p) %in% c("num", "name") && !is_op(p, "=", 1L)) {
        if (kw == "pctlpts" && tk_type(p) == "name") abort_unsupported("PCTLPTS= with TO / BY", sline)
        vals <- c(vals, tk_val(p))
        advance(p)
        if (tk_type(p) == "comma") advance(p)
      }
      out[[kw]] <- vals
      next
    }
    st <- univ_stat_alias[kw]
    if (is.na(st)) abort_unsupported(paste0("PROC UNIVARIATE OUTPUT statistic ", toupper(kw)), sline)
    advance(p)
    if (!is_op(p, "=")) abort_syntax(paste0("expected '=' after ", toupper(kw), " in the OUTPUT statement"), sline)
    advance(p)
    names <- character()
    disp <- character()
    while (tk_type(p) == "name" && !is_op(p, "=", 1L)) {
      names <- c(names, tk_low(p))
      disp <- c(disp, tk_val(p))
      advance(p)
    }
    out$specs <- c(out$specs, list(list(stat = unname(st), names = names, disp = disp, line = sline)))
  }
  expect_semi(p, "OUTPUT")
  out
}

univ_step_spec <- function(step, meta, last) {
  o <- step$options
  print <- !isTRUE(o$noprint)
  inp <- proc_input(step, meta, last)
  st <- inp$st
  proc_apply_attr_stmts(step$stmts, st)
  of <- function(kind) Filter(function(s) identical(s$stmt, kind), step$stmts)
  vars_in <- inp$ds$out_names
  vs <- of("var")
  bys <- of("by")
  by <- if (length(bys)) bys[[length(bys)]]$vars else character()
  if (length(bys) && any(bys[[length(bys)]]$descending)) abort_unsupported("PROC UNIVARIATE BY DESCENDING", step$line)
  cls <- of("class")
  class <- if (length(cls)) proc_resolve(cls[[length(cls)]]$items, st, cls[[length(cls)]]$line) else character()
  if (length(class) > 1L) abort_unsupported("PROC UNIVARIATE with more than one CLASS variable", step$line)
  miss <- setdiff(c(by, class), vars_in)
  if (length(miss)) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), step$line)
  ws <- of("weight")
  weight <- if (length(ws)) proc_resolve(ws[[length(ws)]]$items, st, ws[[length(ws)]]$line) else NULL
  if (length(weight) > 1L) abort_syntax("expected one variable in the WEIGHT statement", step$line)
  vars <- if (length(vs)) proc_resolve(vs[[length(vs)]]$items, st, vs[[length(vs)]]$line) else
    setdiff(vars_in[vapply(vars_in, function(v) !identical(st$info[[v]]$type, "char"), logical(1))], c(by, class, weight))
  for (v in vars) {
    if (identical(st$info[[v]]$type, "char")) abort_runtime(paste0("Variable ", toupper(v), " in list does not match type prescribed for this list."), step$line)
  }
  col_info <- function(v) proc_var_info(st, v)[[1]]
  outputs <- lapply(of("output"), function(s) {
    if (is.null(s$out)) abort_unsupported("PROC UNIVARIATE OUTPUT without OUT=", s$line)
    stats <- vapply(s$specs, `[[`, "", "stat")
    if (anyDuplicated(stats)) abort_unsupported("PROC UNIVARIATE OUTPUT statistic requested twice", s$line)
    wq <- intersect(stats, c("median", "q1", "q3", "qrange", "p1", "p5", "p10", "p90", "p95", "p99", "mode", "range",
                             "t", "probt", "msign", "probm", "signrank", "probs", "nobs"))
    wq <- c(wq, intersect(stats, c("stderr", "cv", "skew", "kurt")))
    if (length(weight) && length(wq)) {
      abort_unsupported(paste0("PROC UNIVARIATE WEIGHT with the statistic ", toupper(wq[1]), " (not measured)"), s$line)
    }
    if (any(lengths(lapply(s$specs, `[[`, "names")) > length(vars))) {
      abort_syntax("More names than analysis variables in the OUTPUT statement", s$line)
    }
    # the columns: by statistic (in the measured order), then by analysis variable
    sp <- s$specs[order(match(stats, univ_out_order))]
    cols <- list()
    for (x in sp) for (j in seq_along(x$names)) {
      cols[[length(cols) + 1L]] <- list(stat = x$stat, var = vars[j], name = x$names[j], disp = x$disp[j])
    }
    # PCTLPTS=: after the statistics, by percentile, then by variable; the
    # name is the prefix of the variable and the percentile (a "." as "_")
    # or the PCTLNAME= name (measured)
    if (length(s$pctlpts)) {
      pts <- as.numeric(s$pctlpts)
      if (anyNA(pts) || any(pts < 0 | pts > 100)) abort_syntax("PCTLPTS= values must be from 0 to 100", s$line)
      if (!length(s$pctlpre)) abort_unsupported("PCTLPTS= without PCTLPRE=", s$line)
      if (length(s$pctlpre) > length(vars)) abort_syntax("More PCTLPRE= prefixes than analysis variables", s$line)
      if (length(s$pctlname) && length(s$pctlname) != length(pts)) abort_unsupported("PCTLNAME= with another number of names than PCTLPTS=", s$line)
      if (length(weight)) abort_unsupported("PROC UNIVARIATE WEIGHT with PCTLPTS= (not measured)", s$line)
      for (k in seq_along(pts)) for (j in seq_along(s$pctlpre)) {
        suf <- if (length(s$pctlname)) s$pctlname[k] else gsub(".", "_", s$pctlpts[k], fixed = TRUE)
        nm <- paste0(s$pctlpre[j], suf)
        cols[[length(cols) + 1L]] <- list(stat = "pctlpt", pct = pts[k], var = vars[j], name = tolower(nm), disp = nm)
      }
    } else if (length(s$pctlpre) || length(s$pctlname)) {
      abort_syntax("PCTLPRE= or PCTLNAME= without PCTLPTS=", s$line)
    }
    list(name = tolower(s$out$name), cols = cols, line = s$line)
  })
  if (!length(outputs) && !print) abort_unsupported("PROC UNIVARIATE NOPRINT without an OUTPUT statement", step$line)
  if (print) {
    # printed output (proc_univariate_print.R): one analysis variable after
    # another, without BY, CLASS, WEIGHT or OUTPUT (not measured with them)
    if (length(outputs) || length(by) || length(class) || length(weight)) {
      abort_unsupported("PROC UNIVARIATE printed output with BY, CLASS, WEIGHT or OUTPUT", step$line)
    }
    if (!identical(o$vardef %||% "df", "df") || !identical(as.integer(o$pctldef %||% 5L), 5L)) {
      abort_unsupported("PROC UNIVARIATE printed output with VARDEF= or PCTLDEF=", step$line)
    }
    for (v in vars) if (!is.null(col_info(v)$label)) abort_unsupported("PROC UNIVARIATE printed output of a labelled variable", step$line)
  }
  list(spec = list(vars = vars, disp = stats::setNames(lapply(vars, function(v) col_info(v)$disp), vars),
                   groups = lapply(c(by, class), col_info), by = by, class = class, weight = weight, outputs = outputs,
                   vardef = o$vardef %||% "df", pctldef = o$pctldef %||% 5L, line = step$line, print = print),
       inp = inp)
}

translate_proc_univariate <- function(step, meta, last) {
  b <- univ_step_spec(step, meta, last)
  spec <- b$spec
  header <- sprintf("# ---- PROC UNIVARIATE (SAS line %d)", as.integer(step$line))
  code <- c(
    header,
    paste0(".d <- ", proc_source_code(b$inp)),
    paste0(".r <- sas_rt_proc(\"univariate\", .d, ", r_lit(spec), ")"),
    if (isTRUE(spec$print)) ".lst <- sas_lst_c(.lst, sas_univ_titles(.titles, .r$tables))",
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  outs <- vapply(spec$outputs, `[[`, "", "name")
  static_meta <- stats::setNames(lapply(spec$outputs, function(x) univ_out_meta(spec, x)), outs)
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC UNIVARIATE",
       line = step$line, outputs = outs, meta = static_meta)
}

univ_out_meta <- function(spec, x) {
  cols <- c(spec$groups, lapply(x$cols, function(c) list(name = c$name, disp = c$disp, char = FALSE, len = 8L,
                                                         label = paste0(if (c$stat == "pctlpt") sprintf("the %8.4f percentile", c$pct) else univ_stat_label[[c$stat]],
                                                                        ", ", spec$disp[[c$var]]))))
  m <- proc_out_meta(cols)
  # the data set type OUT and label (measured); the percentile definition is
  # named when a percentile is requested
  m$typemem <- "OUT"
  pct <- any(vapply(x$cols, `[[`, "", "stat") %in% c("median", "q1", "q3", "qrange", "p1", "p5", "p10", "p90", "p95", "p99", "pctlpt"))
  m$memlabel <- paste0("Selected Summary Statistics", if (pct) paste0(" (Percentile Definition=", spec$pctldef, ")"))
  m
}

# The statistics of one analysis variable in one group.
univ_var_stats <- function(x, stats, spec, w = NULL) {
  sp <- list(vardef = spec$vardef, qntldef = spec$pctldef, alpha = 0.05, exclnpwgt = FALSE, freq = NULL)
  mstats <- intersect(stats, c("n", "nmiss", "mean", "sum", "std", "var", "skew", "kurt", "sumwgt", "max", "min",
                                "range", "q3", "median", "q1", "qrange", "p1", "p5", "p10", "p90", "p95", "p99",
                                "mode", "css", "uss", "cv", "stderr"))
  val <- as.list(means_cell_stats(x, rep(1, length(x)), w, mstats, sp, TRUE))
  if (!is.null(w)) val[intersect(names(val), c("mean", "css", "var", "std", "stderr", "cv"))] <- NULL
  if (!is.null(w)) val <- c(val, univ_weighted_moments(x, w, spec$vardef))
  val$nobs <- length(x)
  # several modes: the smallest, without a NOTE under NOPRINT (measured)
  if (any(c("t", "probt", "msign", "probm", "signrank", "probs") %in% stats)) {
    lt <- univ_location_tests(x)
    val$t <- lt$t
    val$probt <- lt$p_t
    val$msign <- lt$m
    val$probm <- lt$p_m
    val$signrank <- lt$s
    val$probs <- lt$p_s
  }
  val
}

# Weighted moments (measured): the weighted mean, then the corrected sum of
# squares in a second pass over the values; the divisor of VARDEF=.
univ_weighted_moments <- function(x, w, vardef) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  n <- length(x)
  if (n == 0) return(list(mean = NA_real_, css = NA_real_, var = NA_real_, std = NA_real_))
  W <- sum(w)
  m <- sum(w * x) / W
  css <- sum(w * (x - m)^2)
  div <- switch(vardef, df = n - 1, n = n, wdf = W - 1, weight = W)
  v <- if (div > 0) css / div else NA_real_
  list(mean = m, css = css, var = v, std = sqrt(v))
}

univ_compute <- function(d, spec) {
  if (isTRUE(spec$print)) {
    # one page per analysis variable
    tabs <- lapply(spec$vars, function(v) {
      tb <- new_sasonr_ods_table(tibble::tibble(), name = "Univariate", label = "Univariate", path = "Univariate",
                                 proc = "Univariate", template = "base.univariate.lines")
      attr(tb, "lines") <- univ_print_var(as.numeric(d[[v]]), spec$disp[[v]])
      tb
    })
    return(list(tables = tabs, data = list(), events = character()))
  }
  stats <- setdiff(unique(unlist(lapply(spec$outputs, function(o) vapply(o$cols, `[[`, "", "stat")))), "pctlpt")
  gv <- c(spec$by, spec$class)
  if (length(spec$by)) {
    key <- interaction(lapply(spec$by, function(v) d[[v]]), drop = TRUE, lex.order = TRUE)
    runs <- rle(as.integer(key))$values
    if (anyDuplicated(runs)) abort_runtime("Data set is not sorted in ascending sequence.", spec$line)
  }
  # the groups: BY groups in data order, CLASS levels in sorted order; missing
  # CLASS values are left out
  if (length(spec$class)) d <- d[!is.na(d[[spec$class]]) & !(is.character(d[[spec$class]]) & sas_rtrim(d[[spec$class]]) == ""), , drop = FALSE]
  if (length(gv)) {
    g <- if (length(spec$class)) {
      cl <- d[[spec$class]]
      lv <- if (is.character(cl)) sort(unique(cl), method = "radix") else sort(unique(cl))
      byk <- if (length(spec$by)) as.integer(interaction(lapply(spec$by, function(v) d[[v]]), drop = TRUE, lex.order = TRUE)) else rep(1L, nrow(d))
      byo <- match(byk, unique(byk))
      paste(byo, match(cl, lv), sep = "\r")
    } else {
      byk <- interaction(lapply(spec$by, function(v) d[[v]]), drop = TRUE, lex.order = TRUE)
      as.character(match(as.integer(byk), unique(as.integer(byk))))
    }
    ord <- if (length(spec$class)) order(as.integer(sub("\r.*", "", g)), as.integer(sub(".*\r", "", g))) else seq_len(nrow(d))
    keys <- unique(g[ord])
    rows <- lapply(keys, function(k) which(g == k))
  } else {
    rows <- list(seq_len(nrow(d)))
  }
  data <- list()
  for (o in spec$outputs) {
    recs <- lapply(rows, function(ix) {
      wv <- if (length(spec$weight)) as.numeric(d[[spec$weight]][ix])
      vals <- lapply(spec$vars, function(v) univ_var_stats(as.numeric(d[[v]][ix]), stats, spec, wv))
      names(vals) <- spec$vars
      grp <- lapply(gv, function(v) d[[v]][ix[1]])
      st <- lapply(o$cols, function(c) {
        if (c$stat != "pctlpt") return(unname(vals[[c$var]][[c$stat]]))
        x <- sort(as.numeric(d[[c$var]][ix]))
        means_quantile(x, c$pct / 100, spec$pctldef)
      })
      c(stats::setNames(grp, gv), stats::setNames(st, vapply(o$cols, `[[`, "", "name")))
    })
    cols <- lapply(names(recs[[1]] %||% list()), function(nm) unlist(lapply(recs, `[[`, nm)))
    names(cols) <- names(recs[[1]] %||% list())
    data[[o$name]] <- list(data = tibble::as_tibble(cols), meta = univ_out_meta(spec, o))
  }
  list(tables = list(), data = data, events = character())
}

# Tests for location (Mu0 = `mu0`): Student's t, the sign test and the
# Wilcoxon signed rank test.  M = (n+ - n-) / 2 over the nonzero differences,
# its p value the two-sided binomial tail; S = sum of the ranks of the positive
# differences - nt (nt + 1) / 4 with mid-ranks for ties, its p value from the
# exact distribution of the rank sum (nt <= 20).  A larger nt fails loud until
# its large-sample approximation is measured.
univ_location_tests <- function(x, mu0 = 0) {
  x <- x[!is.na(x)]
  n <- length(x)
  d <- x - mu0
  out <- list(t = NA_real_, p_t = NA_real_, m = NA_real_, p_m = NA_real_, s = NA_real_, p_s = NA_real_)
  if (n > 1) {
    k <- stats_moments(x, "means", need = c("mean", "stderr"))
    se <- k$stderr
    if (!is.na(se) && se > 0) {
      out$t <- (k$mean - mu0) / se
      out$p_t <- 2 * stats::pt(-abs(out$t), n - 1)
    }
  }
  nz <- d[d != 0]
  nt <- length(nz)
  if (nt > 0) {
    np <- sum(nz > 0)
    nm <- nt - np
    out$m <- (np - nm) / 2
    out$p_m <- min(1, 2 * stats::pbinom(min(np, nm), nt, 0.5))
    r <- rank(abs(nz))
    out$s <- sum(r[nz > 0]) - nt * (nt + 1) / 4
    if (nt > 20) abort_unsupported("the signed rank test of more than 20 nonzero differences (its approximation is not measured)")
    # exact null distribution of the sum of the positive mid-ranks: each rank
    # enters with probability 1/2; doubled ranks are integers
    r2 <- as.integer(round(2 * r))
    tot <- sum(r2)
    dist <- c(1, rep(0, tot))
    for (v in r2) dist <- dist + c(rep(0, v), dist[seq_len(length(dist) - v)])
    dist <- dist / 2^nt
    obs <- as.integer(round(2 * sum(r[nz > 0])))
    mid <- tot / 2
    dev <- abs(obs - mid)
    vals <- seq.int(0L, tot)
    out$p_s <- min(1, sum(dist[abs(vals - mid) >= dev - 1e-9]))
  }
  out
}

#' The printed output of PROC UNIVARIATE: the TITLE lines above the page of
#' each analysis variable
#' @param titles The TITLE lines.
#' @param tables The tables of `sas_rt_proc()`.
#' @export
#' @keywords internal
sas_univ_titles <- function(titles, tables) {
  do.call(lst_c, lapply(tables, function(t) sas_titles_once(titles, attr(t, "lines", exact = TRUE))))
}
