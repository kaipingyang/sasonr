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

univ_stat_alias <- c(n = "n", nmiss = "nmiss", mean = "mean", sum = "sum", std = "std", stddev = "std",
                     var = "var", css = "css", uss = "uss", max = "max", min = "min", median = "median",
                     p50 = "median", q1 = "q1", p25 = "q1", q3 = "q3", p75 = "q3")

univ_stat_label <- c(n = "number of nonmissing values", nmiss = "number of missing values", mean = "the mean",
                     sum = "the sum", std = "the standard deviation", var = "the variance",
                     css = "the corrected sum of squares", uss = "the uncorrected sum of squares",
                     max = "the largest value", min = "the smallest value", median = "the median",
                     q1 = "the lower quartile", q3 = "the upper quartile")

# The measured orders of the OUT= columns: a request whose statistics
# all lie on one of them takes that order.
univ_out_orders <- list(
  c("n", "nmiss"),
  c("n", "mean", "std", "max", "q3", "median", "q1", "min"),
  c("n", "mean", "uss", "sum", "var", "css")
)

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
      by = , class = , weight = , freq = , id = , histogram = , qqplot = , probplot = , ppplot = , cdfplot = ,
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
  if (!isTRUE(o$noprint)) abort_unsupported("PROC UNIVARIATE printed output (supported: NOPRINT with OUTPUT OUT=)", step$line)
  inp <- proc_input(step, meta, last)
  st <- inp$st
  proc_apply_attr_stmts(step$stmts, st)
  of <- function(kind) Filter(function(s) identical(s$stmt, kind), step$stmts)
  vars_in <- inp$ds$out_names
  vs <- of("var")
  vars <- if (length(vs)) proc_resolve(vs[[length(vs)]]$items, st, vs[[length(vs)]]$line) else
    vars_in[vapply(vars_in, function(v) !identical(st$info[[v]]$type, "char"), logical(1))]
  if (length(vars) != 1L) abort_unsupported("PROC UNIVARIATE with other than one analysis variable", step$line)
  if (identical(st$info[[vars]]$type, "char")) abort_runtime(paste0("Variable ", toupper(vars), " in list does not match type prescribed for this list."), step$line)
  info <- list(disp = st$info[[vars]]$disp %||% vars)
  outputs <- lapply(of("output"), function(s) {
    if (is.null(s$out)) abort_unsupported("PROC UNIVARIATE OUTPUT without OUT=", s$line)
    stats <- vapply(s$specs, `[[`, "", "stat")
    if (any(lengths(lapply(s$specs, `[[`, "names")) != 1L)) {
      abort_unsupported("PROC UNIVARIATE OUTPUT statistic with other than one name", s$line)
    }
    if (anyDuplicated(stats)) abort_unsupported("PROC UNIVARIATE OUTPUT statistic requested twice", s$line)
    ord <- Find(function(seq) all(stats %in% seq), univ_out_orders)
    if (is.null(ord)) {
      abort_unsupported(paste0("PROC UNIVARIATE OUTPUT with the statistics ", paste(toupper(stats), collapse = " "),
                               " (the order of their columns is not measured)"), s$line)
    }
    k <- order(match(stats, ord))
    list(name = tolower(s$out$name), specs = s$specs[k], line = s$line)
  })
  if (!length(outputs)) abort_unsupported("PROC UNIVARIATE NOPRINT without an OUTPUT statement", step$line)
  list(spec = list(var = vars, disp = info$disp, outputs = outputs, vardef = o$vardef %||% "df",
                   pctldef = o$pctldef %||% 5L, line = step$line),
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
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  outs <- vapply(spec$outputs, `[[`, "", "name")
  static_meta <- stats::setNames(lapply(spec$outputs, function(x) univ_out_meta(spec, x)), outs)
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC UNIVARIATE",
       line = step$line, outputs = outs, meta = static_meta)
}

univ_out_meta <- function(spec, x) {
  cols <- lapply(x$specs, function(s) list(name = s$names, disp = s$disp, char = FALSE, len = 8L,
                                           label = paste0(univ_stat_label[[s$stat]], ", ", spec$disp)))
  proc_out_meta(cols)
}

univ_compute <- function(d, spec) {
  x <- as.numeric(d[[spec$var]])
  stats <- unique(unlist(lapply(spec$outputs, function(o) vapply(o$specs, `[[`, "", "stat"))))
  sp <- list(vardef = spec$vardef, qntldef = spec$pctldef, alpha = 0.05, exclnpwgt = FALSE, freq = NULL)
  val <- means_cell_stats(x, rep(1, length(x)), NULL, stats, sp, TRUE)
  data <- list()
  for (o in spec$outputs) {
    cols <- lapply(o$specs, function(s) unname(val[[s$stat]]))
    names(cols) <- vapply(o$specs, `[[`, "", "names")
    data[[o$name]] <- list(data = tibble::as_tibble(cols), meta = univ_out_meta(spec, o))
  }
  list(tables = list(), data = data, events = character())
}
