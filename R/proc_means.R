# PROC MEANS / PROC SUMMARY: parser, translation and run-time engine.
#
# Statistics are computed in R from the observation values, frequencies and
# weights (no external package); the output data sets follow SAS's OUTPUT OUT=
# layout (_TYPE_ / _FREQ_ / _STAT_ or named statistics) and the listing
# follows the MEANS procedure's printed table.

means_stat_names <- c(
  "n", "nmiss", "mean", "std", "var", "min", "max", "range", "sum", "uss", "css", "stderr", "cv",
  "median", "p50", "q1", "p25", "q3", "p75", "qrange", "p1", "p5", "p10", "p20", "p30", "p40",
  "p60", "p70", "p80", "p90", "p95", "p99", "mode", "lclm", "uclm", "clm", "t", "prt", "sumwgt",
  "skew", "kurt"
)
means_stat_alias <- c(stddev = "std", probt = "prt", skewness = "skew", kurtosis = "kurt")

means_canon_stat <- function(kw) {
  kw <- tolower(kw)
  if (kw %in% names(means_stat_alias)) kw <- unname(means_stat_alias[kw])
  if (kw %in% means_stat_names) kw else NA_character_
}

# Heading parts in the listing (SAS split points) and AUTONAME suffixes.
means_stat_heading <- function(s, alpha = 0.05) {
  pct <- paste0(format_pct_level(alpha), "%")
  switch(s,
    n = "N", nmiss = c("N", "Miss"), mean = "Mean", std = "Std Dev", var = "Variance", min = "Minimum",
    max = "Maximum", range = "Range", sum = "Sum", uss = "USS", css = "Corrected SS", stderr = "Std Error",
    cv = c("Coeff of", "Variation"), median = "Median", p50 = "50th Pctl", q1 = c("Lower", "Quartile"),
    p25 = "25th Pctl", q3 = c("Upper", "Quartile"), p75 = "75th Pctl", qrange = c("Quartile", "Range"),
    p1 = "1st Pctl", p5 = "5th Pctl", p10 = "10th Pctl", p20 = "20th Pctl", p30 = "30th Pctl",
    p40 = "40th Pctl", p60 = "60th Pctl", p70 = "70th Pctl", p80 = "80th Pctl", p90 = "90th Pctl",
    p95 = "95th Pctl", p99 = "99th Pctl", mode = "Mode", lclm = c(paste("Lower", pct), "CL for Mean"),
    uclm = c(paste("Upper", pct), "CL for Mean"), t = "t Value", prt = "Pr > |t|", sumwgt = "Sum Wgts",
    skew = "Skewness", kurt = "Kurtosis"
  )
}

format_pct_level <- function(alpha) {
  v <- (1 - alpha) * 100
  if (abs(v - round(v)) < 1e-9) sprintf("%d", as.integer(round(v))) else sub("0+$", "", sprintf("%.4f", v))
}

means_autoname <- c(
  n = "N", nmiss = "NMiss", mean = "Mean", std = "StdDev", var = "Var", min = "Min", max = "Max",
  range = "Range", sum = "Sum", uss = "USS", css = "CSS", stderr = "StdErr", cv = "CV", median = "Median",
  p50 = "P50", q1 = "Q1", p25 = "P25", q3 = "Q3", p75 = "P75", qrange = "QRange", p1 = "P1", p5 = "P5",
  p10 = "P10", p20 = "P20", p30 = "P30", p40 = "P40", p60 = "P60", p70 = "P70", p80 = "P80", p90 = "P90",
  p95 = "P95", p99 = "P99", mode = "Mode", lclm = "LCLM", uclm = "UCLM", t = "t", prt = "Probt",
  sumwgt = "SumWgt", skew = "Skew", kurt = "Kurt"
)

# Parser ------------------------------------------------------------------------

parse_proc_means <- function(p, line, pname) {
  advance(p)
  opts <- list(stats = character())
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax(paste("missing semicolon after PROC", toupper(pname)), line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC", toupper(pname)), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o == "data") {
        opts$data <- parse_ds_ref(p, "input")
        next
      }
      v <- parse_opt_value(p, o, oline)
      switch(o,
        maxdec = {
          opts$maxdec <- as.integer(v)
          if (is.na(opts$maxdec) || opts$maxdec < 0 || opts$maxdec > 8) abort_syntax("MAXDEC= must be 0 to 8", oline)
        },
        fw = {
          opts$fw <- as.integer(v)
        },
        alpha = {
          opts$alpha <- as.numeric(v)
          if (is.na(opts$alpha) || opts$alpha <= 0 || opts$alpha >= 1) abort_syntax("ALPHA= must be between 0 and 1", oline)
        },
        vardef = {
          v <- tolower(v)
          if (v == "wgt") v <- "weight"
          if (!v %in% c("df", "n", "wdf", "weight")) abort_syntax(paste0("Invalid VARDEF= value ", toupper(v)), oline)
          opts$vardef <- v
        },
        pctldef = ,
        qntldef = {
          opts$qntldef <- as.integer(v)
          if (!opts$qntldef %in% 1:5) abort_syntax("QNTLDEF= must be 1 to 5", oline)
        },
        qmethod = if (tolower(v) != "os") abort_unsupported(paste0("PROC ", toupper(pname), " QMETHOD=", toupper(v)), oline),
        order = {
          v <- tolower(v)
          if (v == "unformatted") v <- "internal"
          if (!v %in% c("internal", "formatted", "freq", "data")) abort_syntax(paste0("Invalid ORDER= value ", toupper(v)), oline)
          opts$order <- v
        },
        abort_unsupported(paste0("PROC ", toupper(pname), " option ", toupper(o), "="), oline)
      )
      next
    }
    st <- means_canon_stat(o)
    if (!is.na(st)) {
      opts$stats <- c(opts$stats, if (st == "clm") c("lclm", "uclm") else st)
      if (st == "clm") opts$clm <- TRUE
      if (st == "lclm") opts$lclm_kw <- TRUE
      if (st == "uclm") opts$uclm_kw <- TRUE
    } else if (o %in% c("noprint", "print", "nway", "missing", "descending", "nonobs", "exclnpwgt", "idmin",
                        "threads", "nothreads", "notrap", "stackodsoutput")) {
      opts[[o]] <- TRUE
    } else {
      abort_unsupported(paste0("PROC ", toupper(pname), " option ", toupper(o)), oline)
    }
    advance(p)
  }
  advance(p)
  body <- proc_parse_body(p, pname, function(p, kw, sline) {
    switch(kw,
      var = parse_items_stmt(p, "var"),
      id = parse_items_stmt(p, "id"),
      class = parse_means_class(p),
      by = parse_by(p),
      where = parse_where(p),
      freq = ,
      weight = {
        s <- parse_items_stmt(p, kw)
        if (length(s$items) != 1 || s$items[[1]]$kind != "name") abort_syntax(paste("expected one variable in the", toupper(kw), "statement"), sline)
        s
      },
      types = parse_means_types(p),
      ways = {
        advance(p)
        w <- integer()
        while (tk_type(p) == "num") {
          w <- c(w, as.integer(tk_val(p)))
          advance(p)
        }
        expect_semi(p, "WAYS")
        list(stmt = "ways", ways = w, line = sline)
      },
      output = parse_means_output(p),
      format = parse_format_stmt(p),
      label = parse_label(p),
      attrib = parse_attrib(p),
      abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC ", toupper(pname),
                          " (ERROR 180-322: Statement is not valid or it is used out of proper order)"), sline)
    )
  })
  list(type = "proc", name = pname, line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

parse_means_class <- function(p) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
  o <- list()
  if (is_op(p, "/")) {
    advance(p)
    while (tk_type(p) == "name") {
      k <- tk_low(p)
      if (is_op(p, "=", 1L)) {
        advance(p, 2L)
        v <- tolower(parse_opt_value(p, k, line))
        if (k != "order") abort_unsupported(paste0("CLASS option ", toupper(k), "="), line)
        if (v == "unformatted") v <- "internal"
        if (!v %in% c("internal", "formatted", "freq", "data")) abort_syntax(paste0("Invalid ORDER= value ", toupper(v)), line)
        o$order <- v
        next
      }
      if (k %in% c("missing", "descending", "ascending")) {
        o[[k]] <- TRUE
      } else {
        abort_unsupported(paste0("CLASS option ", toupper(k)), line)
      }
      advance(p)
    }
  }
  expect_semi(p, "CLASS")
  list(stmt = "class", items = items, opts = o, line = line)
}

# TYPES a a*b (a b)*c () ;  ->  list of character vectors of variable names
parse_means_types <- function(p) {
  line <- tk_line(p)
  advance(p)
  # one factor: a name or a parenthesised list of names (possibly empty)
  factor <- function() {
    if (tk_type(p) == "lparen") {
      advance(p)
      nm <- character()
      while (tk_type(p) == "name") {
        nm <- c(nm, tk_low(p))
        advance(p)
      }
      expect_type(p, "rparen", "')'")
      if (length(nm) == 0) return(list(character()))
      return(as.list(nm))
    }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in TYPES"), line)
    nm <- tk_low(p)
    advance(p)
    list(nm)
  }
  out <- list()
  while (tk_type(p) != "semi") {
    terms <- factor()
    while (is_op(p, "*")) {
      advance(p)
      f <- factor()
      terms <- unlist(lapply(terms, function(a) lapply(f, function(b) c(a, b))), recursive = FALSE)
    }
    out <- c(out, terms)
  }
  advance(p)
  list(stmt = "types", types = out, line = line)
}

parse_means_output <- function(p) {
  line <- tk_line(p)
  advance(p)
  out <- list(stmt = "output", specs = list(), line = line, opts = list())
  while (tk_type(p) == "name") {
    kw <- tk_low(p)
    sline <- tk_line(p)
    if (kw == "out" && is_op(p, "=", 1L)) {
      advance(p, 2L)
      out$out <- parse_ds_ref(p, "output")
      next
    }
    if (kw %in% c("idgroup", "maxid", "minid")) abort_unsupported(paste0("OUTPUT ", toupper(kw)), sline)
    st <- means_canon_stat(kw)
    if (is.na(st)) abort_syntax(paste0("Statistic keyword ", toupper(kw), " is not valid in the OUTPUT statement"), sline)
    if (st == "clm") abort_syntax("CLM is not valid in the OUTPUT statement; use LCLM= and UCLM=", sline)
    advance(p)
    vars <- NULL
    if (tk_type(p) == "lparen") {
      advance(p)
      vars <- list()
      while (tk_type(p) == "name") vars <- c(vars, list(parse_varlist_item(p)))
      expect_type(p, "rparen", "')'")
    }
    if (!is_op(p, "=")) abort_syntax(paste0("expected '=' after ", toupper(kw), " in the OUTPUT statement"), sline)
    advance(p)
    names <- character()
    disp <- character()
    while (tk_type(p) == "name" && !is_op(p, "=", 1L) && tk_type(p, 1L) != "lparen") {
      names <- c(names, tk_low(p))
      disp <- c(disp, tk_val(p))
      advance(p)
    }
    out$specs <- c(out$specs, list(list(stat = st, vars = vars, names = names, disp = disp, line = sline)))
  }
  if (is_op(p, "/")) {
    advance(p)
    while (tk_type(p) == "name") {
      k <- tk_low(p)
      if (!k %in% c("autoname", "autolabel", "noinherit", "inherit", "keeplen", "levels", "ways")) {
        abort_unsupported(paste0("OUTPUT option ", toupper(k)), line)
      }
      out$opts[[k]] <- TRUE
      advance(p)
    }
  }
  expect_semi(p, "OUTPUT")
  out
}

# Translation -------------------------------------------------------------------

translate_proc_means <- function(step, meta, last) {
  pname <- step$name
  o <- step$options
  inp <- proc_input(step, meta, last)
  st <- inp$st
  proc_apply_attr_stmts(step$stmts, st)
  of <- function(kind) Filter(function(s) identical(s$stmt, kind), step$stmts)
  vars_in <- inp$ds$out_names
  num_vars <- vars_in[vapply(vars_in, function(v) !identical(st$info[[v]]$type, "char"), logical(1))]

  class_stmts <- of("class")
  class <- character()
  class_opts <- list()
  for (s in class_stmts) {
    vs <- proc_resolve(s$items, st, s$line)
    for (v in vs) {
      class <- c(setdiff(class, v), v)
      class_opts[[v]] <- s$opts
    }
  }
  by_stmts <- of("by")
  by <- if (length(by_stmts) > 0) by_stmts[[length(by_stmts)]] else NULL
  if (!is.null(by)) {
    miss <- setdiff(by$vars, vars_in)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), by$line)
  }
  one_var <- function(kind) {
    s <- of(kind)
    if (length(s) == 0) return(NULL)
    v <- proc_resolve(s[[length(s)]]$items, st, s[[length(s)]]$line)
    if (identical(st$info[[v]]$type, "char")) abort_runtime(paste0("Variable ", toupper(v), " in list does not match type prescribed for this list."), s[[1]]$line)
    v
  }
  freq <- one_var("freq")
  weight <- one_var("weight")
  ids <- unique(unlist(lapply(of("id"), function(s) proc_resolve(s$items, st, s$line))))
  var_stmts <- of("var")
  vars <- character()
  if (length(var_stmts) > 0) {
    vars <- unlist(lapply(var_stmts, function(s) proc_resolve(s$items, st, s$line)))
    for (v in vars) if (identical(st$info[[v]]$type, "char")) {
      abort_runtime(paste0("Variable ", toupper(v), " in list does not match type prescribed for this list."), var_stmts[[1]]$line)
    }
  }
  # a VAR list that matches nothing (e.g. an unmatched prefix) analyses the default set
  if (length(vars) == 0) vars <- setdiff(num_vars, c(by$vars, class, freq, weight, ids))
  vars <- unique(vars)
  types_stmts <- of("types")
  ways_stmts <- of("ways")
  types <- NULL
  if (length(types_stmts) > 0) {
    types <- list()
    for (s in types_stmts) for (t in s$types) {
      bad <- setdiff(t, class)
      if (length(bad) > 0) abort_runtime(paste0("Variable ", toupper(bad[1]), " in the TYPES statement is not a CLASS variable."), s$line)
      types <- c(types, list(t))
    }
  }
  ways <- unlist(lapply(ways_stmts, `[[`, "ways"))
  for (w in ways) if (w < 0 || w > length(class)) abort_syntax("WAYS value out of range", ways_stmts[[1]]$line)

  stats <- unique(o$stats)
  printing <- if (pname == "summary") isTRUE(o$print) else !isTRUE(o$noprint)
  if (!printing && length(of("output")) == 0 && pname == "summary") {
    abort_runtime("Neither the PRINT option nor a valid output statement has been given.", step$line)
  }
  if (length(stats) == 0) stats <- c("n", "mean", "std", "min", "max")
  # The panel layout is implemented in means_listing(); the one part of it that
  # is not derived from SAS output is the heading split for a narrow FW=: below
  # the default field width SAS re-splits multi-word headings ("Std Error" ->
  # "Std"/"Error") and abbreviates single words ("Minimum" -> "Min",
  # "Coefficient of Variation" -> "Coeff of"/"Variation"), which sasonr does not
  # reproduce.  Ten or more statistics are split into panels like SAS does.
  if (printing && !is.null(o$fw) && o$fw < 12L) {
    abort_unsupported("PROC MEANS listing with FW= below the default 12 (SAS re-splits and abbreviates column headings)", step$line)
  }
  vardef <- o$vardef %||% "df"
  ci_stats <- intersect(stats, c("stderr", "t", "prt", "lclm", "uclm"))

  outputs <- lapply(of("output"), function(s) {
    if (is.null(s$out)) abort_unsupported("OUTPUT statement without OUT= (DATAn naming)", s$line)
    specs <- lapply(s$specs, function(sp) {
      if (!is.null(sp$vars)) {
        sv <- proc_resolve(sp$vars, st, sp$line)
        bad <- setdiff(sv, vars)
        if (length(bad) > 0) abort_runtime(paste0("Variable ", toupper(bad[1]), " in the OUTPUT statement is not an analysis variable."), sp$line)
      } else {
        sv <- vars
      }
      if (length(sp$names) > length(sv)) abort_syntax("Too many names in the OUTPUT statement", sp$line)
      list(stat = sp$stat, vars = sv, names = sp$names, disp = sp$disp)
    })
    if (isTRUE(s$opts$levels) || isTRUE(s$opts$ways)) abort_unsupported("OUTPUT options LEVELS / WAYS", s$line)
    list(name = s$out$name, ds_opts = s$out$options, specs = specs, opts = s$opts, line = s$line,
         two_sided = any(vapply(specs, function(sp) sp$stat == "lclm", TRUE)) && any(vapply(specs, function(sp) sp$stat == "uclm", TRUE)))
  })
  if (length(vars) == 0 && (printing || any(vapply(outputs, function(x) length(x$specs) > 0 || TRUE, TRUE)))) {
    if (length(outputs) > 0 || printing) abort_runtime("No analysis variables are available (no numeric variables in the VAR list)", step$line)
  }
  info <- proc_var_info(st, unique(c(vars, class, by$vars, freq, weight, ids)))
  cls <- lapply(class, function(v) {
    co <- class_opts[[v]]
    list(name = v, missing = isTRUE(co$missing) || isTRUE(o$missing),
         order = co$order %||% o$order %||% "internal",
         descending = isTRUE(co$descending))
  })
  spec <- list(
    pname = pname, line = step$line, data = inp$ds$name, print = printing, stats = stats, vars = vars,
    class = cls, by = by$vars, by_desc = by$descending, freq = freq, weight = weight, ids = ids,
    types = types, ways = ways, nway = isTRUE(o$nway), info = info, descendtypes = isTRUE(o$descending),
    maxdec = o$maxdec, fw = o$fw %||% 12L, alpha = o$alpha %||% 0.05, vardef = vardef,
    qntldef = o$qntldef %||% 5L, exclnpwgt = isTRUE(o$exclnpwgt), nonobs = isTRUE(o$nonobs),
    idmin = isTRUE(o$idmin), two_sided = isTRUE(o$clm) || (isTRUE(o$lclm_kw) && isTRUE(o$uclm_kw)),
    outputs = outputs
  )
  header <- sprintf("# ---- PROC %s (SAS line %d)", toupper(pname), as.integer(step$line))
  code <- c(
    header,
    paste0(".d <- ", proc_source_code(inp)),
    paste0(".r <- sas_proc_means(.d, ", r_lit(spec), ")"),
    ".lst <- c(.lst, .titles_once(.titles, .r$listing))",
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  outs <- vapply(outputs, `[[`, "", "name")
  static_meta <- lapply(outputs, function(x) means_out_layout(spec, x)$meta)
  names(static_meta) <- outs
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = paste("PROC", toupper(pname)),
       line = step$line, outputs = outs, meta = static_meta)
}

# Run time ------------------------------------------------------------------------

# Titles go above the first page of a procedure's output.
.titles_once <- function(titles, lst) {
  if (length(lst) == 0) return(character())
  c(titles, if (length(titles) > 0) "", lst)
}

# Quantile of sorted values `x` (frequencies already expanded) by QNTLDEF.
means_quantile <- function(x, p, def = 5L) {
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (p <= 0) return(x[1])
  if (p >= 1) return(x[n])
  fuzz <- 1e-9
  if (def == 4L) {
    np <- (n + 1) * p
  } else {
    np <- n * p
  }
  j <- floor(np + fuzz)
  g <- np - j
  if (abs(g) < fuzz) g <- 0
  at <- function(k) x[min(max(k, 1L), n)]
  switch(as.character(def),
    "1" = (1 - g) * at(j) + g * at(j + 1L),
    "2" = {
      if (abs(g - 0.5) < fuzz) {
        if (j %% 2 == 0) at(j) else at(j + 1L)
      } else {
        at(round(np))
      }
    },
    "3" = if (g == 0) at(j) else at(j + 1L),
    "4" = if (j < 1) x[1] else if (j >= n) x[n] else (1 - g) * at(j) + g * at(j + 1L),
    "5" = if (g == 0) (at(j) + at(j + 1L)) / 2 else at(j + 1L)
  )
}

# Weighted percentile (SAS definition when a WEIGHT variable is used).
means_wquantile <- function(x, w, p) {
  keep <- w > 0
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0) return(NA_real_)
  o <- order(x)
  x <- x[o]
  w <- w[o]
  W <- sum(w)
  if (p <= 0) return(x[1])
  if (p >= 1) return(x[length(x)])
  cw <- cumsum(w)
  target <- p * W
  eq <- which(abs(cw - target) <= 1e-9 * W)
  if (length(eq) > 0 && eq[1] < length(x)) return((x[eq[1]] + x[eq[1] + 1L]) / 2)
  x[which(cw > target + 1e-9 * W)[1]]
}

pctl_of <- function(s) {
  switch(s, median = , p50 = 0.5, q1 = , p25 = 0.25, q3 = , p75 = 0.75,
         as.numeric(substring(s, 2L)) / 100)
}

# Statistics of one analysis variable in one cell. `x` values, `f` integer
# frequencies (>= 1), `w` weights (NULL when there is no WEIGHT statement).
means_cell_stats <- function(x, f, w, stats, sp, two_sided) {
  miss <- is.na(x)
  out <- stats::setNames(rep(NA_real_, length(stats)), stats)
  if (!is.null(w)) {
    use_w <- !is.na(w)
    if (sp$exclnpwgt) use_w <- use_w & w > 0
    x <- x[use_w]
    f <- f[use_w]
    miss <- miss[use_w]
    w <- pmax(w[use_w], 0)
  }
  nmiss <- sum(f[miss])
  xv <- x[!miss]
  fv <- f[!miss]
  wv <- if (is.null(w)) rep(1, length(xv)) else w[!miss]
  n <- sum(fv)
  wf <- wv * fv
  sw <- sum(wf)
  val <- list(n = n, nmiss = nmiss)
  val$sumwgt <- if (n > 0) sw else NA_real_
  if (n > 0) {
    val$sum <- sum(wf * xv)
    val$min <- min(xv)
    val$max <- max(xv)
    val$range <- val$max - val$min
    val$mean <- if (sw > 0) val$sum / sw else NA_real_
    val$uss <- sum(wf * xv^2)
    val$css <- if (!is.na(val$mean)) sum(wf * (xv - val$mean)^2) else NA_real_
    div <- switch(sp$vardef, df = n - 1, n = n, wdf = sw - 1, weight = sw)
    val$var <- if (div > 0 && !is.na(val$css)) val$css / div else NA_real_
    val$std <- sqrt(val$var)
    val$cv <- if (!is.na(val$std) && !is.na(val$mean) && val$mean != 0) 100 * val$std / val$mean else NA_real_
    if (sp$vardef == "df" && n > 1 && !is.na(val$var) && sw > 0) {
      val$stderr <- sqrt(val$var / sw)
      df <- n - 1
      val$t <- if (val$stderr > 0) val$mean / val$stderr else NA_real_
      val$prt <- if (!is.na(val$t)) 2 * stats::pt(-abs(val$t), df) else NA_real_
      a <- sp$alpha
      q2 <- stats::qt(1 - a / 2, df)
      q1 <- stats::qt(1 - a, df)
      val$lclm2 <- val$mean - q2 * val$stderr
      val$uclm2 <- val$mean + q2 * val$stderr
      val$lclm1 <- val$mean - q1 * val$stderr
      val$uclm1 <- val$mean + q1 * val$stderr
      val$lclm <- if (two_sided) val$lclm2 else val$lclm1
      val$uclm <- if (two_sided) val$uclm2 else val$uclm1
    }
    if (any(stats %in% c("skew", "kurt")) && !is.na(val$std) && val$std > 0) {
      z <- (xv - val$mean) / val$std
      if (sp$vardef == "df") {
        if (n > 2) val$skew <- n / ((n - 1) * (n - 2)) * sum(fv * wv^1.5 * z^3)
        if (n > 3) val$kurt <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(fv * wv^2 * z^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
      } else if (sp$vardef == "n") {
        val$skew <- sum(fv * wv^1.5 * z^3) / n
        val$kurt <- sum(fv * wv^2 * z^4) / n - 3
      } else {
        abort_unsupported(paste0("SKEWNESS/KURTOSIS with VARDEF=", toupper(sp$vardef)))
      }
    }
    q_stats <- intersect(stats, c("median", "p50", "q1", "p25", "q3", "p75", "qrange", "p1", "p5", "p10", "p20",
                                  "p30", "p40", "p60", "p70", "p80", "p90", "p95", "p99"))
    if (length(q_stats) > 0 || "mode" %in% stats) {
      o <- order(xv)
      xs <- rep(xv[o], fv[o])
      qf <- function(pp) {
        if (!is.null(w)) means_wquantile(xv, wf, pp) else means_quantile(xs, pp, sp$qntldef)
      }
      for (s in setdiff(q_stats, "qrange")) val[[s]] <- qf(pctl_of(s))
      if ("qrange" %in% stats) val$qrange <- qf(0.75) - qf(0.25)
      if ("mode" %in% stats) {
        r <- rle(xs)
        mx <- max(r$lengths)
        val$mode <- if (mx > 1 || length(r$values) == 1) r$values[which(r$lengths == mx)[1]] else NA_real_
      }
    }
  }
  if (any(c("lclm", "uclm") %in% stats)) {
    stats <- c(stats, "lclm1", "uclm1", "lclm2", "uclm2")
    out <- stats::setNames(rep(NA_real_, length(stats)), stats)
  }
  for (s in stats) if (!is.null(val[[s]])) out[[s]] <- as.numeric(val[[s]])
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  out
}

# Level table of one CLASS variable within a BY group.
means_class_levels <- function(x, f, cl, info) {
  key <- proc_group_key(x, info)
  u <- unique(key)
  first <- match(u, key)
  # representative value: the lowest internal value of the group
  rep_idx <- vapply(u, function(k) {
    idx <- which(key == k)
    idx[proc_internal_order(x[idx])[1]]
  }, 1L)
  rep_val <- x[rep_idx]
  ord <- switch(cl$order,
    internal = proc_internal_order(rep_val),
    data = order(first),
    formatted = order(proc_value_text(rep_val, info), method = "radix"),
    freq = {
      cnt <- vapply(u, function(k) sum(f[key == k]), 1)
      io <- proc_internal_order(rep_val)
      rank_int <- match(seq_along(u), io)
      order(-cnt, rank_int)
    }
  )
  if (cl$descending) ord <- rev(ord)
  list(keys = u[ord], values = rep_val[ord], key = key)
}

# The class-variable subsets (as logical masks over `class`) for each _TYPE_.
means_type_vars <- function(type, k) {
  if (k == 0) return(logical())
  bits <- rev(as.integer(intToBits(type))[seq_len(k)])
  bits == 1L
}

means_types_wanted <- function(sp, k, for_print) {
  all_max <- 2^k - 1
  if (sp$nway) return(all_max)
  explicit <- !is.null(sp$types) || length(sp$ways) > 0
  if (!explicit) return(if (for_print) all_max else 0:all_max)
  out <- integer()
  cn <- vapply(sp$class, `[[`, "", "name")
  for (t in sp$types) out <- c(out, sum(2^(k - match(t, cn))))
  for (w in sp$ways) {
    for (ty in 0:all_max) if (sum(means_type_vars(ty, k)) == w) out <- c(out, ty)
  }
  sort(unique(as.integer(out)))
}

#' PROC MEANS / SUMMARY engine
#' @param d Input data (after WHERE).
#' @param sp Specification built by the translator.
#' @export
#' @keywords internal
sas_proc_means <- function(d, sp) {
  cn <- vapply(sp$class, `[[`, "", "name")
  k <- length(cn)
  info <- sp$info
  if (!is.null(sp$by)) {
    groups <- proc_by_groups(d, sp$by, sp$by_desc, sp$data, info)
  } else {
    groups <- list(seq_len(nrow(d)))
  }
  if (sp$vardef != "df" && any(sp$stats %in% c("stderr", "t", "prt", "lclm", "uclm"))) {
    rt$proc_notes <- c(rt$proc_notes, "WARNING: STDERR and related statistics, T, PROBT, LCLM and UCLM can only be computed when VARDEF is set to its default value of DF.")
  }
  need_types <- sort(unique(c(
    if (sp$print) means_types_wanted(sp, k, TRUE),
    if (length(sp$outputs) > 0) means_types_wanted(sp, k, FALSE)
  )))
  all_stats <- unique(c(sp$stats, unlist(lapply(sp$outputs, function(o) {
    if (length(o$specs) == 0) c("n", "min", "max", "mean", "std") else vapply(o$specs, `[[`, "", "stat")
  }))))
  cells_by <- list()
  for (gi in seq_along(groups)) {
    rows <- groups[[gi]]
    db <- d[rows, , drop = FALSE]
    f <- if (is.null(sp$freq)) rep(1, nrow(db)) else trunc(db[[sp$freq]])
    keep <- !is.na(f) & f >= 1
    for (cl in sp$class) if (!cl$missing) keep <- keep & !sas_is_missing(db[[cl$name]])
    db <- db[keep, , drop = FALSE]
    f <- f[keep]
    lv <- lapply(sp$class, function(cl) means_class_levels(db[[cl$name]], f, cl, info[[cl$name]]))
    cells <- list()
    for (ty in need_types) {
      sel <- means_type_vars(ty, k)
      if (k > 0 && any(sel)) {
        idx_per <- lapply(which(sel), function(j) match(lv[[j]]$key, lv[[j]]$keys))
        ord <- do.call(order, unname(idx_per))
        combo <- do.call(paste, c(lapply(idx_per, function(v) v[ord]), sep = "\r"))
        ucombo <- unique(combo)
        split_rows <- split(ord, factor(combo, levels = ucombo))
      } else {
        split_rows <- if (nrow(db) > 0 || k == 0) list(seq_len(nrow(db))) else list()
      }
      for (r in split_rows) {
        cell <- list(type = ty, by_row = rows[1], rows = r, freq = sum(f[r]))
        cell$class_vals <- lapply(seq_len(k), function(j) {
          if (!sel[j]) return(NULL)
          lvl <- match(lv[[j]]$key[r[1]], lv[[j]]$keys)
          lv[[j]]$values[lvl]
        })
        x_stats <- lapply(sp$vars, function(v) {
          means_cell_stats(db[[v]][r], f[r], if (is.null(sp$weight)) NULL else db[[sp$weight]][r], all_stats, sp,
                           two_sided = sp$two_sided)
        })
        names(x_stats) <- sp$vars
        cell$stats <- x_stats
        cell$ids <- lapply(sp$ids, function(v) means_id_value(db[[v]][r], sp$idmin))
        cells[[length(cells) + 1L]] <- cell
      }
    }
    cells_by[[gi]] <- list(rows = rows, cells = cells, first = if (length(rows) > 0) rows[1] else NA_integer_)
  }
  out <- list()
  for (o in sp$outputs) {
    lay <- means_out_layout(sp, o)
    out_types <- means_types_wanted(sp, k, FALSE)
    o_two <- o$two_sided
    data <- means_out_data(d, sp, o, lay, cells_by, out_types, all_stats, o_two)
    res <- proc_out_options(data, lay$meta, o$ds_opts, o$line)
    out[[o$name]] <- res
  }
  listing <- if (sp$print) means_listing(d, sp, cells_by, means_types_wanted(sp, k, TRUE)) else character()
  list(listing = listing, out = out)
}

means_id_value <- function(x, idmin) {
  if (is.character(x)) {
    xs <- x
    if (length(xs) == 0) return("")
    s <- sort(xs, method = "radix")
    return(if (idmin) s[1] else s[length(s)])
  }
  v <- x[!is.na(x)]
  if (length(v) == 0) return(NA_real_)
  if (idmin) min(v) else max(v)
}

# Column layout (names + metadata) of one OUTPUT data set.
means_out_layout <- function(sp, o) {
  info <- sp$info
  col_of <- function(v) {
    i <- info[[v]]
    list(name = v, disp = i$disp, char = i$char, len = i$len, fmt = i$fmt, informat = i$informat, label = i$label)
  }
  cols <- c(lapply(sp$by, col_of), lapply(vapply(sp$class, `[[`, "", "name"), col_of), lapply(sp$ids, col_of))
  cols <- c(cols, list(list(name = "_type_", disp = "_TYPE_", char = FALSE, len = 8L),
                       list(name = "_freq_", disp = "_FREQ_", char = FALSE, len = 8L)))
  stat_cols <- list()
  if (length(o$specs) == 0) {
    cols <- c(cols, list(list(name = "_stat_", disp = "_STAT_", char = TRUE, len = 8L)))
    for (v in sp$vars) {
      i <- info[[v]]
      stat_cols[[length(stat_cols) + 1L]] <- list(name = v, disp = i$disp, char = FALSE, len = 8L, fmt = i$fmt, label = i$label,
                                                 var = v, stat = NA_character_)
    }
  } else {
    for (spc in o$specs) {
      for (j in seq_along(spc$vars)) {
        v <- spc$vars[j]
        i <- info[[v]]
        if (j <= length(spc$names)) {
          nm <- spc$names[j]
          disp <- spc$disp[j]
        } else if (length(spc$names) > 0) {
          next
        } else if (isTRUE(o$opts$autoname)) {
          disp <- paste0(i$disp, "_", means_autoname[[spc$stat]])
          nm <- tolower(disp)
        } else {
          nm <- v
          disp <- i$disp
        }
        inherit <- !isTRUE(o$opts$noinherit)
        lab <- if (isTRUE(o$opts$autolabel)) paste0(i$label %||% i$disp, "_", means_autoname[[spc$stat]]) else if (inherit) i$label
        stat_cols[[length(stat_cols) + 1L]] <- list(name = nm, disp = disp, char = FALSE, len = 8L,
                                                   fmt = if (inherit) i$fmt, label = lab, var = v, stat = spc$stat)
      }
    }
  }
  nms <- vapply(c(cols, stat_cols), `[[`, "", "name")
  dup <- nms[duplicated(nms)]
  if (length(dup) > 0) {
    # SAS keeps the first and warns that the variable already exists
    keep <- !duplicated(vapply(stat_cols, `[[`, "", "name")) & !vapply(stat_cols, `[[`, "", "name") %in% vapply(cols, `[[`, "", "name")
    stat_cols <- stat_cols[keep]
  }
  list(cols = cols, stat_cols = stat_cols, meta = proc_out_meta(c(cols, stat_cols)))
}

means_out_data <- function(d, sp, o, lay, cells_by, out_types, all_stats, two_sided) {
  rows <- list()
  cn <- vapply(sp$class, `[[`, "", "name")
  default_stats <- c("n", "min", "max", "mean", "std")
  for (g in cells_by) {
    gcells <- g$cells
    if (isTRUE(sp$descendtypes) && length(gcells) > 0) {
      ty <- vapply(gcells, `[[`, 1, "type")
      gcells <- gcells[order(-ty, seq_along(ty))]
    }
    for (cell in gcells) {
      if (!cell$type %in% out_types) next
      base <- list()
      for (v in sp$by) base[[v]] <- d[[v]][cell$by_row]
      for (j in seq_along(cn)) {
        v <- cn[j]
        val <- cell$class_vals[[j]]
        base[[v]] <- if (is.null(val)) (if (sp$info[[v]]$char) "" else NA_real_) else val
      }
      for (j in seq_along(sp$ids)) base[[sp$ids[j]]] <- cell$ids[[j]]
      base[["_type_"]] <- as.numeric(cell$type)
      base[["_freq_"]] <- as.numeric(cell$freq)
      st_of <- function(v, s) {
        if (s %in% c("lclm", "uclm")) s <- paste0(s, if (two_sided) "2" else "1")
        cell$stats[[v]][[s]]
      }
      if (length(o$specs) == 0) {
        for (s in default_stats) {
          r <- base
          r[["_stat_"]] <- toupper(s)
          for (sc in lay$stat_cols) r[[sc$name]] <- cell$stats[[sc$var]][[s]]
          rows[[length(rows) + 1L]] <- r
        }
      } else {
        r <- base
        for (sc in lay$stat_cols) r[[sc$name]] <- st_of(sc$var, sc$stat)
        rows[[length(rows) + 1L]] <- r
      }
    }
  }
  nm <- lay$meta$vars
  if (length(rows) == 0) {
    cols <- lapply(c(lay$cols, lay$stat_cols), function(c) if (c$char) character() else numeric())
    names(cols) <- nm
    return(tibble::as_tibble(cols))
  }
  cols <- lapply(c(lay$cols, lay$stat_cols), function(c) {
    v <- lapply(rows, function(r) r[[c$name]])
    if (c$char) vapply(v, function(x) if (is.null(x)) "" else as.character(x), "") else vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 1)
  })
  names(cols) <- nm
  tibble::as_tibble(cols)
}

# Listing --------------------------------------------------------------------------

# Default (no MAXDEC=) display of a statistic in a field of width w: a fixed
# number of decimals for values of ordinary magnitude, fewer for large values.
means_num_text <- function(v, w, maxdec) {
  if (is.na(v)) return(".")
  if (!is.null(maxdec)) {
    # too wide: fewer decimals first (12345678.500 with MAXDEC=4), then BEST
    for (dd in seq(maxdec, 0L)) {
      # MAXDEC= rounds the exact binary value half up, without the format fuzz
      # (gold: a mean of 1.2249999999999999 shows as 1.22)
      s <- sprintf("%.*f", dd, display_round(v, dd, exact = TRUE))
      if (nchar(s) <= w) return(s)
    }
    return(trimws(sas_fmt_best(v, w)))
  }
  if (v == 0) return("0")
  if (v != trunc(v) && (abs(v) >= 1e6 || abs(v) < 1e-4)) {
    # the gold standard shows SAS choosing other decimals here (1622403.34,
    # 0.000012500); the rule is not derived yet
    abort_unsupported("PROC MEANS default decimals for a non-integer statistic of 1E6 or more or below 1E-4 (not verified against SAS; use MAXDEC=)")
  }
  dec <- max(0L, as.integer(round((w - 2) * 0.7)))
  for (dd in dec:0) {
    s <- sprintf("%.*f", dd, display_round(v, dd))
    # the minus sign does not count against the field (-218.9158575)
    if (nchar(sub("^-", "", s)) <= w - 1L) return(s)
  }
  trimws(sas_fmt_best(v, w))
}

means_stat_text <- function(s, v, sp) {
  if (s %in% c("n", "nmiss")) return(if (is.na(v)) "." else sprintf("%.0f", v))
  if (s == "t") return(if (is.na(v)) "." else sprintf("%.2f", display_round(v, 2L)))
  if (s == "prt") return(trimws(fmt_pvalue(v, 6L, 4L)))
  means_num_text(v, sp$fw, sp$maxdec)
}

means_stat_col <- function(s, cells, sp) {
  parts <- means_stat_heading(s, sp$alpha)
  minw <- max(nchar(parts))
  vw <- if (s %in% c("n", "nmiss")) (if (!is.null(sp$freq)) sp$fw else max(c(1L, nchar(cells)))) else if (s == "t") 7L else if (s == "prt") 6L else sp$fw
  list(parts = parts, width = max(vw, minw), right = TRUE, cells = cells)
}

# Printed-table geometry ------------------------------------------------------------
#
# Measured on SAS 9.4 TS1M2 with LINESIZE= 72..200, 3 to 22 statistics, FW=
# 4..20, MAXDEC= 0..7, with and without CLASS.  The line is filled left to
# right as follows.
#
#  * Every statistic column is a field of FW= columns (default 12), except
#    N / N Miss (as wide as the printed counts) and t Value / Pr > |t|; a
#    column is never narrower than its heading.  The field width does *not*
#    depend on MAXDEC= or on how many characters a value actually uses: the
#    same 12-statistic table is 128 + 28 columns wide with MAXDEC=0 and 7.
#  * The fixed columns (CLASS levels, "N Obs", Variable/Label) and the gap
#    between the fixed part and the first statistic use the same gap as panel 1
#    of that table; a CLASS variable keeps its declared length as field width
#    ("grp $8" prints in 8 columns even though the levels are one character).
#  * A panel takes as many statistics as fit with two-blank gaps; the gap is
#    then widened to 4, or 3, if the panel still fits (so consecutive values
#    are 16, 15 or 14 columns apart).  Remaining statistics go to the next
#    panel, which repeats the fixed columns.
#  * The dashes under a table are exactly the table width, so a 12-statistic
#    table at LINESIZE=132 is 128 + 28 (no CLASS) or 129 + 59 (one CLASS
#    variable of length 8).
means_panel_layout <- function(widths, fixed_widths, ls, min_gap = 2L, max_gap = 4L) {
  n <- length(widths)
  fixed_width <- function(g) if (length(fixed_widths) > 0) sum(fixed_widths) + g * length(fixed_widths) else 0L
  fit <- function(from, g, F) {
    take <- 0L
    acc <- 0L
    for (j in seq.int(from, n)) {
      acc <- acc + widths[j]
      if (F + acc + (j - from) * g <= ls) take <- j - from + 1L else break
    }
    take
  }
  # panel 1 also fixes the gap used by the fixed columns
  best <- NULL
  for (g in seq.int(max_gap, min_gap)) {
    F <- fixed_width(g)
    take <- fit(1L, g, F)
    if (is.null(best) || take > best$take) best <- list(take = take, gap = g, fixed = F)
  }
  g_fixed <- best$gap
  F0 <- best$fixed
  if (best$take == 0L) best$take <- 1L
  panels <- list()
  pos <- 1L
  first <- TRUE
  while (pos <= n) {
    if (first) {
      take <- best$take
      gap <- best$gap
    } else {
      take <- fit(pos, min_gap, F0)
      if (take == 0L) take <- 1L
      acc <- sum(widths[pos:(pos + take - 1L)])
      gap <- min_gap
      for (g in seq.int(max_gap, min_gap)) {
        if (F0 + acc + (take - 1L) * g <= ls) {
          gap <- g
          break
        }
      }
    }
    acc <- sum(widths[pos:(pos + take - 1L)])
    panels[[length(panels) + 1L]] <- list(first = pos, last = pos + take - 1L, gap = gap, fixed_gap = g_fixed,
                                          width = F0 + acc + (take - 1L) * gap)
    pos <- pos + take
    first <- FALSE
  }
  panels
}

# render_table() with one gap per column boundary: the fixed part keeps the gap
# of panel 1, the statistics use each panel's own gap.
means_render <- function(cols, gaps) {
  heads <- lapply(cols, function(c) heading_lines(c$parts, c$width))
  nh <- max(vapply(heads, length, 1L))
  heads <- lapply(heads, function(h) c(rep("", nh - length(h)), h))
  widths <- vapply(cols, function(c) as.integer(c$width), 1L)
  right <- vapply(cols, `[[`, TRUE, "right")
  join <- function(vals) {
    cells <- mapply(function(x, w, r) formatC(x, width = if (r) w else -w), vals, widths, right)
    if (length(cells) == 1L) return(cells)
    pieces <- character(2L * length(cells) - 1L)
    pieces[seq.int(1L, 2L * length(cells) - 1L, 2L)] <- cells
    pieces[seq.int(2L, 2L * length(cells) - 2L, 2L)] <- strrep(" ", gaps)
    paste(pieces, collapse = "")
  }
  hl <- vapply(seq_len(nh), function(j) sas_rtrim(join(vapply(heads, `[`, "", j))), "")
  n <- length(cols[[1L]]$cells)
  body <- if (n > 0) vapply(seq_len(n), function(i) sas_rtrim(join(vapply(cols, function(c) c$cells[i], ""))), "") else character()
  rule <- strrep("-", sum(widths) + sum(gaps))
  c(hl, rule, body, rule)
}

means_listing <- function(d, sp, cells_by, print_types) {
  info <- sp$info
  cn <- vapply(sp$class, `[[`, "", "name")
  k <- length(cn)
  stats <- sp$stats
  multi <- length(sp$vars) > 1
  has_label <- any(vapply(sp$vars, function(v) !is.null(info[[v]]$label), TRUE))
  out <- character()
  proc_title <- paste0("The ", toupper(sp$pname), " Procedure")
  titled <- FALSE
  for (g in cells_by) {
    if (!is.null(sp$by)) {
      if (length(g$rows) == 0) next
      out <- c(out, "", proc_by_line(d, g$first, sp$by, info), "")
    }
    if (!titled) {
      out <- c(out, proc_title, "")
      titled <- TRUE
    }
    for (ty in print_types) {
      cells <- Filter(function(c) c$type == ty, g$cells)
      sel <- means_type_vars(ty, k)
      tvars <- cn[sel]
      # fixed columns
      fixed <- list()
      nrow_cell <- if (multi) length(sp$vars) else 1L
      prev <- NULL
      class_cells <- lapply(seq_along(tvars), function(z) character())
      nobs_cells <- character()
      var_cells <- character()
      lab_cells <- character()
      stat_vals <- lapply(stats, function(s) character())
      for (cell in cells) {
        vals <- lapply(which(sel), function(j) cell$class_vals[[j]])
        txt <- vapply(seq_along(tvars), function(z) proc_value_text(vals[[z]], info[[tvars[z]]]), "")
        same <- if (is.null(prev)) rep(FALSE, length(txt)) else cumprod(txt == prev) == 1
        prev <- txt
        for (z in seq_along(tvars)) class_cells[[z]] <- c(class_cells[[z]], if (same[z]) "" else txt[z], rep("", nrow_cell - 1L))
        nobs_cells <- c(nobs_cells, sprintf("%.0f", cell$freq), rep("", nrow_cell - 1L))
        for (v in sp$vars) {
          var_cells <- c(var_cells, info[[v]]$disp)
          lab_cells <- c(lab_cells, info[[v]]$label %||% "")
          for (si in seq_along(stats)) stat_vals[[si]] <- c(stat_vals[[si]], means_stat_text(stats[si], cell$stats[[v]][[stats[si]]], sp))
          if (!multi) break
        }
      }
      for (z in seq_along(tvars)) {
        i <- info[[tvars[z]]]
        w <- if (i$char) (if (is.null(i$fmt)) i$len else fmt_width(i$fmt, TRUE) %|NA|% i$len) else if (is.null(i$fmt)) 12L else fmt_width(i$fmt, FALSE)
        w <- max(w, nchar(i$disp), nchar(class_cells[[z]]))
        fixed[[length(fixed) + 1L]] <- list(parts = i$disp, width = w, right = proc_right_align(i), cells = class_cells[[z]])
      }
      if (k > 0 && any(sel) && !sp$nonobs) {
        fixed[[length(fixed) + 1L]] <- list(parts = c("N", "Obs"), width = max(3L, nchar(nobs_cells)), right = TRUE, cells = nobs_cells)
      }
      if (multi) {
        fixed[[length(fixed) + 1L]] <- list(parts = "Variable", width = max(8L, nchar(var_cells)), right = FALSE, cells = var_cells)
        if (has_label) fixed[[length(fixed) + 1L]] <- list(parts = "Label", width = max(5L, nchar(lab_cells)), right = FALSE, cells = lab_cells)
      }
      scols <- lapply(seq_along(stats), function(si) means_stat_col(stats[si], stat_vals[[si]], sp))
      # Panel layout (measured on SAS 9.4 TS1M2, rounding/proc/means_* and the
      # experiments behind means_panel_layout()).
      ls <- rt$ls %||% 132L
      panels <- means_panel_layout(vapply(scols, function(c) as.integer(c$width), 1L),
                                   vapply(fixed, function(c) as.integer(c$width), 1L), ls)
      bad <- Filter(function(p) p$width > ls, panels)
      if (length(bad) > 0) {
        # SAS squeezes an over-long fixed part (a CLASS variable with a long
        # length, or a very wide Variable column) by narrowing that column and
        # redistributing the spare columns; that rule is not determined
        abort_unsupported("PROC MEANS listing wider than LINESIZE= (SAS narrows the class/fixed columns; rule not determined)")
      }
      if (length(fixed) > 0 && (length(panels) >= 3L || ls < 120L)) {
        # Fixed columns are verified exact for one or two panels at LINESIZE=
        # 120..200 (CLASS length 1, 4 and 8, one or two CLASS variables, the 38
        # rounding/proc/means_* gold cases; two analysis variables).  Below
        # LINESIZE=120 and across three or more statistic panels SAS narrows the
        # fixed part (the "N Obs" field and the gaps around the CLASS columns)
        # in a way these measurements do not determine: at LINESIZE=100 a
        # 12-statistic CLASS table is 92 columns wide where this layout gives 94.
        abort_unsupported("PROC MEANS class table with three or more panels or LINESIZE= below 120 (fixed-column layout not determined)")
      }
      for (p in panels) {
        cols <- c(fixed, scols[p$first:p$last])
        gaps <- c(rep.int(p$fixed_gap, length(fixed)), rep.int(p$gap, p$last - p$first))
        if (!multi) {
          v <- sp$vars[1]
          out <- c(out, paste0("Analysis Variable : ", info[[v]]$disp, " ", info[[v]]$label %||% ""), "")
        }
        out <- c(out, means_render(cols, gaps), "")
      }
    }
  }
  out
}
