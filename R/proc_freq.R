# PROC FREQ: parser, translation and run-time engine (frequency tables,
# OUT= data sets, OUTPUT statement statistics and the listing).

freq_table_opts <- c("list", "missing", "missprint", "nocum", "nopercent", "norow", "nocol", "nofreq", "sparse",
                     "outpct", "outcum", "chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "noprint", "nowarn",
                     "exact", "expected", "deviation", "cellchi2", "relrisk", "riskdiff")
freq_output_kw <- c("n", "nmiss", "pchi", "lrchi", "ajchi", "mhchi", "chisq", "phi", "contgy", "cramv", "cmh",
                    "cmh1", "cmh2", "cmhcor", "cmhrms", "cmhga", "binomial", "bin")

# Every statistic request (TABLES options, EXACT, OUTPUT keywords) goes
# through the PROC statistics registry (inst/registry/proc_stats.csv, rows with
# proc FREQ; proc_stat_check(), the same strict gate as every other PROC
# statistic and function).  CHISQ, FISHER, EXACT FISHER, BINOMIAL, ALPHA and the
# crosstab cell statistics are supported (values and listing); the others are
# unsupported and fail loud with sasonr.strict = TRUE.
freq_stat_gate <- function(k, line) {
  k <- tolower(k)
  if (k == "bin") k <- "binomial"
  proc_stat_check("FREQ", k, line)
  invisible(k)
}

# Parser ------------------------------------------------------------------------

parse_proc_freq <- function(p, line) {
  advance(p)
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC FREQ", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC FREQ"), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o == "data") {
        opts$data <- parse_ds_ref(p, "input")
        next
      }
      v <- tolower(parse_opt_value(p, o, oline))
      if (o != "order") abort_unsupported(paste0("PROC FREQ option ", toupper(o), "="), oline)
      if (v == "unformatted") v <- "internal"
      if (!v %in% c("internal", "formatted", "freq", "data")) abort_syntax(paste0("Invalid ORDER= value ", toupper(v)), oline)
      opts$order <- v
      next
    }
    if (!o %in% c("noprint", "nlevels", "page", "compress")) abort_unsupported(paste0("PROC FREQ option ", toupper(o)), oline)
    opts[[o]] <- TRUE
    advance(p)
  }
  advance(p)
  body <- proc_parse_body(p, "freq", function(p, kw, sline) {
    switch(kw,
      table = ,
      tables = parse_freq_tables(p),
      by = parse_by(p),
      where = parse_where(p),
      weight = {
        advance(p)
        if (tk_type(p) != "name") abort_syntax("expected a variable in the WEIGHT statement", sline)
        v <- tk_low(p)
        advance(p)
        zeros <- FALSE
        if (is_op(p, "/")) {
          advance(p)
          while (tk_type(p) == "name") {
            if (tk_low(p) != "zeros") abort_unsupported(paste0("WEIGHT option ", toupper(tk_val(p))), sline)
            zeros <- TRUE
            advance(p)
          }
        }
        expect_semi(p, "WEIGHT")
        list(stmt = "weight", var = v, zeros = zeros, line = sline)
      },
      exact = {
        advance(p)
        kws <- character()
        while (tk_type(p) == "name") {
          k <- tk_low(p)
          if (!k %in% c("fisher", "binomial", "bin", "relrisk", "riskdiff", "chisq")) abort_unsupported(paste0("EXACT ", toupper(k)), sline)
          freq_stat_gate(paste0("exact_", if (k == "bin") "binomial" else k), sline)
          kws <- c(kws, if (k == "bin") "binomial" else k)
          advance(p)
        }
        if (is_op(p, "/")) abort_unsupported("EXACT statement options", sline)
        expect_semi(p, "EXACT")
        list(stmt = "exact", kws = kws, line = sline)
      },
      output = {
        advance(p)
        o <- list(stmt = "output", kws = character(), line = sline)
        while (tk_type(p) == "name") {
          k <- tk_low(p)
          if (k == "out" && is_op(p, "=", 1L)) {
            advance(p, 2L)
            o$out <- parse_ds_ref(p, "output")
            next
          }
          if (!k %in% freq_output_kw) abort_unsupported(paste0("PROC FREQ OUTPUT statistic ", toupper(k)), sline)
          freq_stat_gate(paste0("output_", if (k == "bin") "binomial" else k), sline)
          o$kws <- c(o$kws, if (k == "bin") "binomial" else k)
          advance(p)
        }
        expect_semi(p, "OUTPUT")
        o
      },
      format = parse_format_stmt(p),
      label = parse_label(p),
      attrib = parse_attrib(p),
      abort_syntax("Statement is not valid or it is used out of proper order. (ERROR 180-322)", sline, tk_col(p), tk_val(p))
    )
  })
  list(type = "proc", name = "freq", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

# BINOMIAL( options ): P=, LEVEL=, CL= (single value or a list),
# CORRECT, EXACT.  Unsupported options fail loud rather than being ignored.
parse_freq_binomial_opts <- function(p, line) {
  opts <- list(p0 = NULL, level = NULL, cl = NULL, correct = FALSE, exact = FALSE)
  expect_type(p, "lparen", "'(' after BINOMIAL")
  value_list <- function() {
    vals <- character()
    if (tk_type(p) == "lparen") {
      advance(p)
      while (tk_type(p) != "rparen") {
        if (tk_type(p) == "comma") { advance(p); next }
        vals <- c(vals, tolower(as.character(parse_opt_value(p, "CL", line))))
      }
      advance(p)
    } else {
      vals <- tolower(as.character(parse_opt_value(p, "CL", line)))
    }
    vals
  }
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") { advance(p); next }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in BINOMIAL()"), line)
    k <- tk_low(p)
    kl <- tk_line(p)
    if (k == "alpha") {
      # ALPHA= is a TABLES option, not a BINOMIAL suboption
      abort_syntax(paste("expecting one of the following: AC, AGRESTICOULL, ALL, BLAKER, CL, CLOPPERPEARSON, CLTYPE,",
                         "CORRECT, CP, EQUIV, EQUIVALENCE, EXACT, J, JEFFREYS, LEVEL, LIKELIHOODRATIO, LOG, LOGIT, LR,",
                         "MARGIN, MIDP, NONINF, NONINFERIORITY, NULL, OUTLEVEL, P, SCORE, SUP, SUPERIORITY, TESTP, VAR,",
                         "W, WALD, WALDC, WILSON, WILSONC."), kl)
    }
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (k == "p") {
        p0 <- parse_opt_number(p, kl)
        # a value above 1 is a percentage (P=1.5 tests H0: proportion = 0.015)
        if (p0 > 1 && p0 < 100) p0 <- p0 / 100
        if (!(p0 > 0 && p0 < 1)) abort_unsupported(paste0("BINOMIAL P=", p0, " (only 0 < P < 1 or a percentage below 100)"), kl)
        opts$p0 <- p0
      } else if (k == "level") {
        # LEVEL=2 (a number) selects the second level of the table; LEVEL='x'
        # selects the level whose formatted value is x.
        if (tk_type(p) == "num") {
          if (!tk_int(p)) abort_syntax("expecting one of the following: a quoted string, an integer constant.", kl)
          opts$level_index <- as.integer(as.numeric(tk_val(p)))
          advance(p)
        } else {
          opts$level <- parse_opt_value(p, "LEVEL", kl)
        }
      } else if (k == "cl") {
        opts$cl <- value_list()
      } else {
        abort_unsupported(paste0("BINOMIAL option ", toupper(k), "="), kl)
      }
      next
    }
    advance(p)
    if (k == "correct") {
      opts$correct <- TRUE
    } else if (k == "exact") {
      opts$exact <- TRUE
    } else {
      abort_unsupported(paste0("BINOMIAL option ", toupper(k)), kl)
    }
  }
  advance(p)
  opts
}

# One CL= type list: a single name or a parenthesised list of names.
freq_parse_cl_types <- function(p, line, what) {
  read_one <- function() {
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in", what, "CL="), line)
    v <- tk_low(p)
    advance(p)
    v
  }
  if (tk_type(p) != "lparen") return(read_one())
  advance(p)
  out <- character()
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    out <- c(out, read_one())
  }
  advance(p)
  unique(out)
}

# RELRISK <( relrisk-options )>: CL= and COLUMN=1 are implemented; COLUMN=2,
# the tests (EQUAL, EQUIV, NONINF, SUP), MARGIN= and METHOD= are not supported
# and fail loud.
parse_freq_relrisk_opts <- function(p, line) {
  opts <- list(cl = NULL, column = 1L, printall = FALSE)
  expect_type(p, "lparen", "'(' after RELRISK")
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in RELRISK()"), line)
    k <- tk_low(p)
    kl <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (k == "cl") {
        types <- freq_parse_cl_types(p, kl, "RELRISK")
        bad <- setdiff(types, c("wald", "exact", "score"))
        if (length(bad) > 0) abort_unsupported(paste0("RELRISK CL=", toupper(bad[1])), kl)
        opts$cl <- types
      } else if (k == "column") {
        opts$column <- as.integer(parse_opt_number(p, kl, int = TRUE))
      } else {
        abort_unsupported(paste0("RELRISK option ", toupper(k), "="), kl)
      }
      next
    }
    advance(p)
    if (k == "printall") {
      abort_unsupported("RELRISK option PRINTALL", kl)
    } else {
      abort_unsupported(paste0("RELRISK option ", toupper(k)), kl)
    }
  }
  advance(p)
  opts
}

# RISKDIFF <( riskdiff-options )>: CL= and COLUMN=1 are implemented; COLUMN=2,
# CORRECT, NORISKS, COMMON and the risk-difference tests are not supported and
# fail loud.
parse_freq_riskdiff_opts <- function(p, line) {
  opts <- list(cl = NULL, column = 1L, correct = FALSE, norisks = FALSE)
  expect_type(p, "lparen", "'(' after RISKDIFF")
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in RISKDIFF()"), line)
    k <- tk_low(p)
    kl <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (k == "cl") {
        types <- freq_parse_cl_types(p, kl, "RISKDIFF")
        types <- sub("^agresticafo$", "ac", types)
        bad <- setdiff(types, c("wald", "newcombe", "exact", "ac", "ha"))
        if (length(bad) > 0) abort_unsupported(paste0("RISKDIFF CL=", toupper(bad[1])), kl)
        opts$cl <- unique(types)
      } else if (k == "column") {
        opts$column <- as.integer(as.numeric(parse_opt_value(p, "COLUMN", kl)))
      } else {
        abort_unsupported(paste0("RISKDIFF option ", toupper(k), "="), kl)
      }
      next
    }
    advance(p)
    if (k == "correct") {
      opts$correct <- TRUE
    } else if (k == "norisks") {
      abort_unsupported("RISKDIFF option NORISKS", kl)
    } else {
      abort_unsupported(paste0("RISKDIFF option ", toupper(k)), kl)
    }
  }
  advance(p)
  opts
}

# a parenthesised list of numbers: TESTP=(0.2 0.3 0.5) or TESTF=(10 30 40)
freq_parse_num_list <- function(p, line) {
  expect_type(p, "lparen", "'(' after TESTP= / TESTF=")
  vals <- numeric()
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    if (tk_type(p) != "num") abort_syntax(paste("unexpected", tok_desc(p), "in a TESTP= / TESTF= list"), line)
    vals <- c(vals, as.numeric(tk_val(p)))
    advance(p)
  }
  advance(p)
  vals
}

# CHISQ <( chisq-options )>: the one-way goodness-of-fit null frequencies and
# proportions TESTF= and TESTP= (manual: TABLES statement, CHISQ option).  The
# other chisq-options (DF=, LRCHI, WARN=) are not supported and fail loud.
parse_freq_chisq_opts <- function(p, line) {
  opts <- list(testp = NULL, testf = NULL)
  expect_type(p, "lparen", "'(' after CHISQ")
  while (tk_type(p) != "rparen") {
    if (tk_type(p) == "comma") {
      advance(p)
      next
    }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in CHISQ()"), line)
    k <- tk_low(p)
    kl <- tk_line(p)
    if (!is_op(p, "=", 1L)) abort_unsupported(paste0("CHISQ option ", toupper(k)), kl)
    advance(p, 2L)
    if (k == "testp") {
      freq_stat_gate("testp", kl)
      opts$testp <- freq_parse_num_list(p, kl)
    } else if (k == "testf") {
      freq_stat_gate("testf", kl)
      opts$testf <- freq_parse_num_list(p, kl)
    } else {
      abort_unsupported(paste0("CHISQ option ", toupper(k), "="), kl)
    }
  }
  advance(p)
  if (!is.null(opts$testp) && !is.null(opts$testf)) {
    abort_unsupported("CHISQ with both TESTP= and TESTF=", line)
  }
  opts
}

parse_freq_tables <- function(p) {
  line <- tk_line(p)
  advance(p)
  # a factor: a variable list item or a parenthesised list of them
  factor <- function() {
    if (tk_type(p) == "lparen") {
      advance(p)
      items <- list()
      while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
      expect_type(p, "rparen", "')'")
      return(items)
    }
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in TABLES"), line)
    list(parse_varlist_item(p))
  }
  reqs <- list()
  # a request is a list of factors; each factor is a list of variable list
  # items (expanded and crossed at translation)
  while (tk_type(p) == "name" || tk_type(p) == "lparen") {
    fs <- list(factor())
    while (is_op(p, "*")) {
      advance(p)
      fs <- c(fs, list(factor()))
    }
    reqs <- c(reqs, list(fs))
  }
  o <- list()
  if (is_op(p, "/")) {
    advance(p)
    while (tk_type(p) == "name") {
      k <- tk_low(p)
      kl <- tk_line(p)
      if (is_op(p, "=", 1L)) {
        advance(p, 2L)
        if (k == "out") {
          o$out <- parse_ds_ref(p, "output")
        } else if (k == "alpha") {
          freq_stat_gate("alpha", kl)
          o$alpha <- parse_opt_number(p, kl)
          if (!(o$alpha > 0 && o$alpha < 1)) abort_unsupported(paste0("TABLES option ALPHA=", o$alpha, " outside 0 < ALPHA < 1"), kl)
        } else if (k == "scores") {
          # SCORES= chooses the row/column scores of the score-based tests and
          # the CMH statistics (manual: "Scores"); TABLE and RANK are supported
          # for the multiway CMH listing, RIDIT types are not.
          freq_stat_gate("scores", kl)
          v <- tolower(parse_opt_value(p, k, kl))
          if (!v %in% c("table", "rank")) abort_unsupported(paste0("TABLES option SCORES=", toupper(v)), kl)
          o$scores <- v
        } else if (k == "plots") {
          v <- tolower(parse_opt_value(p, k, kl))
          if (v != "none") abort_unsupported(paste0("TABLES option PLOTS=", toupper(v)), kl)
        } else if (k == "testp" || k == "testf") {
          # TESTP= / TESTF= are also accepted as TABLES statement options next
          # to CHISQ; they give the null proportions / frequencies of the
          # one-way goodness-of-fit test
          freq_stat_gate(k, kl)
          if (is.null(o$chisq_opts)) o$chisq_opts <- list(testp = NULL, testf = NULL)
          o$chisq_opts[[k]] <- freq_parse_num_list(p, kl)
        } else {
          abort_unsupported(paste0("TABLES option ", toupper(k), "="), kl)
        }
        next
      }
      if (!k %in% freq_table_opts) abort_unsupported(paste0("TABLES option ", toupper(k)), kl)
      if (k %in% c("chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "exact", "relrisk", "riskdiff",
                   "expected", "deviation", "cellchi2")) freq_stat_gate(k, kl)
      advance(p)
      if (k == "binomial" && tk_type(p) == "lparen") {
        o$binomial_opts <- parse_freq_binomial_opts(p, kl)
      } else if (k == "chisq" && tk_type(p) == "lparen") {
        o$chisq_opts <- parse_freq_chisq_opts(p, kl)
      } else if (k == "relrisk" && tk_type(p) == "lparen") {
        o$relrisk_opts <- parse_freq_relrisk_opts(p, kl)
      } else if (k == "riskdiff" && tk_type(p) == "lparen") {
        o$riskdiff_opts <- parse_freq_riskdiff_opts(p, kl)
      }
      o[[k]] <- TRUE
    }
  }
  expect_semi(p, "TABLES")
  list(stmt = "tables", reqs = reqs, opts = o, line = line)
}

# Translation -------------------------------------------------------------------

# The specification of a PROC FREQ step (shared by the translator and
# sas_freq_df()): list(spec, inp, warnings).
freq_step_spec <- function(step, meta, last) {
  o <- step$options
  inp <- proc_input(step, meta, last)
  st <- inp$st
  proc_apply_attr_stmts(step$stmts, st)
  of <- function(kind) Filter(function(s) identical(s$stmt, kind), step$stmts)
  vars_in <- inp$ds$out_names
  by <- of("by")
  by <- if (length(by) > 0) by[[length(by)]] else NULL
  if (!is.null(by)) {
    miss <- setdiff(by$vars, vars_in)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), by$line)
  }
  w <- of("weight")
  w <- if (length(w) > 0) w[[length(w)]] else NULL
  if (!is.null(w)) {
    if (!w$var %in% vars_in) abort_runtime(paste0("Variable ", toupper(w$var), " not found."), w$line)
    if (identical(st$info[[w$var]]$type, "char")) abort_runtime(paste0("Variable ", toupper(w$var), " in list does not match type prescribed for this list."), w$line)
  }
  exact <- unique(vctrs::list_unchop(lapply(of("exact"), `[[`, "kws"), ptype = character(), name_spec = rlang::zap()))
  tabs <- of("tables")
  requests <- list()
  for (s in tabs) {
    expanded <- list()
    for (r in s$reqs) {
      terms <- list(character())
      for (f in r) {
        vs <- proc_resolve(f, st, s$line)
        terms <- vctrs::list_unchop(lapply(terms, function(a) lapply(vs, function(b) c(a, b))), ptype = list(), name_spec = rlang::zap())
      }
      expanded <- c(expanded, terms)
    }
    for (k in seq_along(expanded)) {
      ro <- s$opts
      # OUT= takes the last table request of the statement
      if (k != length(expanded)) ro$out <- NULL
      requests[[length(requests) + 1L]] <- list(vars = expanded[[k]], opts = ro, line = s$line)
    }
  }
  if (length(tabs) == 0) {
    # no TABLES statement: one-way tables of every variable
    for (v in setdiff(vars_in, c(by$vars, w$var))) requests[[length(requests) + 1L]] <- list(vars = v, opts = list(), line = step$line)
  }
  for (rq in requests) {
    kv <- length(rq$vars)
    if (kv > 1 && any(c("binomial") %in% names(rq$opts))) abort_syntax("BINOMIAL is valid only for one-way tables", rq$line)
    if (kv != 2 && any(c("relrisk", "riskdiff") %in% names(rq$opts))) {
      abort_unsupported("RELRISK / RISKDIFF for a table that is not 2x2 (only the 2x2 layout is supported)", rq$line)
    }
    # NOPRINT suppresses the frequency tables, not the statistics [measured,
    # chisq / fisher / two-way, cmh / multiway, binomial / one-way]; the
    # per-stratum statistics of a multiway table are not measured
    if (isTRUE(rq$opts$noprint) && kv > 2 &&
        (any(c("chisq", "fisher", "binomial", "relrisk", "riskdiff") %in% names(rq$opts)) || length(exact) > 0)) {
      abort_unsupported("TABLES option NOPRINT with per-stratum statistics of a multiway table", rq$line)
    }

    # --- option combinations whose listing layout is not supported ----------
    if (isTRUE(rq$opts$missprint) && any(c("relrisk", "riskdiff") %in% names(rq$opts))) {
      abort_unsupported("MISSPRINT with RELRISK / RISKDIFF (the combination is not supported)", rq$line)
    }
    if (kv > 1 && isTRUE(rq$opts$list) &&
        (any(c("chisq", "fisher", "cmh", "cmh1", "cmh2", "relrisk", "riskdiff", "binomial") %in% names(rq$opts)) ||
         length(exact) > 0)) {
      abort_unsupported("PROC FREQ statistics with the LIST option (the statistics layout after a LIST table is not supported)", rq$line)
    }
    rd0 <- rq$opts$riskdiff_opts %||% list()
    rel0 <- rq$opts$relrisk_opts %||% list()
    # measured: a 2x2 table with only CL= in both (the risk difference limits
    # after the column 1 risks, the relative risk limits after the column 2
    # risks); other suboptions, ALPHA=, EXACT or strata are not
    if (isTRUE(rq$opts$relrisk) && isTRUE(rq$opts$riskdiff) &&
        length(rel0$cl) > 0 && length(rd0$cl) > 0 &&
        (kv != 2L || !identical(rd0$column, 1L) || isTRUE(rd0$correct) || isTRUE(rd0$norisks) ||
         !identical(rel0$column, 1L) || isTRUE(rel0$printall) || !is.null(rq$opts$alpha) ||
         any(c("riskdiff", "relrisk", "rrc1", "rrc2", "or") %in% exact))) {
      abort_unsupported("RELRISK(CL=) together with RISKDIFF(CL=) with other options (supported: CL= alone in both, a 2x2 table)", rq$line)
    }
    if ("riskdiff" %in% exact && isTRUE(rq$opts$riskdiff) && is.null(rd0$cl)) {
      abort_unsupported("EXACT RISKDIFF without the RISKDIFF(CL=) confidence-limit table (that layout is not supported)", rq$line)
    }
    alpha <- rq$opts$alpha
    if (!is.null(alpha) && abs(alpha - 0.05) > 1e-9 &&
        any(c("cmh", "cmh1", "cmh2", "relrisk", "riskdiff") %in% names(rq$opts))) {
      abort_unsupported("TABLES option ALPHA= with CMH / RELRISK / RISKDIFF (only the binomial confidence limits are supported)", rq$line)
    }
    # SCORES= affects the Mantel-Haenszel chi-square of a two-way table and the
    # score-based statistics; it is supported for multiway tables only, and
    # SCORES=RANK only for the CMH summary without CHISQ / FISHER
    if (!is.null(rq$opts$scores) && kv < 3) {
      abort_unsupported(paste0("TABLES option SCORES=", toupper(rq$opts$scores),
                               " for a one-way or two-way table (only the multiway CMH listing is supported)"), rq$line)
    }
    if (identical(rq$opts$scores, "rank") &&
        (!isTRUE(rq$opts$cmh) || isTRUE(rq$opts$chisq) || isTRUE(rq$opts$fisher))) {
      abort_unsupported("SCORES=RANK is only supported for the multiway CMH summary without CHISQ / FISHER", rq$line)
    }
    if (length(exact) > 0 && kv > 2) {
      abort_unsupported("EXACT statistics for a multiway table (per-stratum exact tests are not supported)", rq$line)
    }
    if ("chisq" %in% exact && kv != 2) {
      abort_unsupported("EXACT CHISQ for a one-way table (only the 2x2 exact enumeration is supported)", rq$line)
    }
    if ("chisq" %in% exact && kv == 2 && !isTRUE(rq$opts$chisq)) {
      abort_unsupported("EXACT CHISQ requires the CHISQ option in the TABLES statement", rq$line)
    }
    if (!is.null(rq$opts$chisq_opts) && !isTRUE(rq$opts$chisq)) {
      abort_unsupported("TESTP= / TESTF= without the CHISQ option", rq$line)
    }
  }
  if (any(c("relrisk", "riskdiff") %in% exact)) {
    for (k in intersect(c("relrisk", "riskdiff"), exact)) {
      if (!any(vapply(requests, function(r) isTRUE(r$opts[[k]]), TRUE))) {
        abort_unsupported(paste0("EXACT ", toupper(k), " without the ", toupper(k), " option in the TABLES statement"), step$line)
      }
    }
  }
  stat_opts <- c("chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "relrisk", "riskdiff")
  if (!is.null(by) && (length(exact) > 0 ||
                       any(vapply(requests, function(r) any(c("binomial", "cmh", "cmh1", "cmh2", "relrisk", "riskdiff") %in% names(r$opts)), logical(1))))) {
    # only the per-BY-group CHISQ / FISHER listing on a 2x2 table is supported;
    # the other statistics and shapes have unsupported BY layouts
    abort_unsupported("PROC FREQ statistics with BY groups (only CHISQ / FISHER per BY group is supported)", by$line)
  }
  if (!is.null(by) &&
      any(vapply(requests, function(r) length(r$vars) != 2 && any(c("chisq", "fisher") %in% names(r$opts)), logical(1)))) {
    abort_unsupported("CHISQ / FISHER with BY groups for a table that is not two-way (only the 2x2 table is supported)", by$line)
  }
  outs <- of("output")
  if (length(outs) > 1) abort_unsupported("more than one OUTPUT statement in PROC FREQ", outs[[2]]$line)
  out_stmt <- if (length(outs) > 0) outs[[1]] else NULL
  if (!is.null(out_stmt) && is.null(out_stmt$out)) abort_unsupported("OUTPUT statement without OUT=", out_stmt$line)
  allv <- unique(c(vctrs::list_unchop(lapply(requests, `[[`, "vars"), ptype = character(), name_spec = rlang::zap()), by$vars, w$var))
  spec <- list(
    line = step$line, data = inp$ds$name, print = !isTRUE(o$noprint), order = o$order %||% "internal",
    nlevels = isTRUE(o$nlevels), by = by$vars, by_desc = by$descending, weight = w$var, zeros = isTRUE(w$zeros),
    requests = requests, exact = exact, info = proc_var_info(st, allv),
    output = if (!is.null(out_stmt)) list(name = out_stmt$out$name, ds_opts = out_stmt$out$options, kws = out_stmt$kws)
  )
  list(spec = spec, inp = inp, warnings = character())
}

translate_proc_freq <- function(step, meta, last) {
  b <- freq_step_spec(step, meta, last)
  spec <- b$spec
  inp <- b$inp
  requests <- spec$requests
  header <- sprintf("# ---- PROC FREQ (SAS line %d)", as.integer(step$line))
  code <- c(
    header,
    paste0(".d <- ", proc_source_code(inp)),
    paste0(".r <- sas_rt_proc(\"freq\", .d, ", r_lit(spec), ")"),
    ".lst <- c(.lst, sas_titles_once(.titles, .r$listing))",
    ".ods <- c(.ods, Filter(function(tb) !identical(attr(tb, \"template\", exact = TRUE), \"base.freq.lines\"), .r$tables))",
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  out_names <- c(vctrs::list_unchop(lapply(requests, function(r) r$opts$out$name), ptype = character(), name_spec = rlang::zap()), spec$output$name)
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC FREQ", line = step$line,
       outputs = unique(out_names), meta = list())
}

# Run time --------------------------------------------------------------------------

# Levels of one variable: grouping by formatted value; representative value =
# lowest internal value of the group.
freq_levels <- function(x, wt, info, order) {
  key <- proc_value_text(x, info)
  if (is.numeric(x)) {
    code <- sas_miss_code(x)
    # all missing values of one kind form one level (shown as . / A ...)
    key[!is.na(code)] <- paste0("\001", key[!is.na(code)])
  }
  u <- unique(key)
  if (length(u) == 0) return(list(keys = character(), values = x[0], text = character(), key = key))
  first <- match(u, key)
  rep_idx <- vapply(u, function(k) {
    idx <- which(key == k)
    idx[proc_internal_order(x[idx])[1]]
  }, 1L)
  rv <- x[rep_idx]
  txt <- proc_value_text(rv, info)
  io <- proc_internal_order(rv)
  if (is.numeric(rv)) {
    # PROC FREQ lists ._ after .Z
    code <- sas_miss_code(rv)
    io <- order(ifelse(is.na(code), 99L, ifelse(code == 0L, 28L, code)), match(seq_along(rv), io))
  }
  ord <- switch(order,
    internal = io,
    data = order(first),
    formatted = if (is.numeric(x) && is.null(info$fmt)) io else order(txt, method = "radix"),
    freq = {
      cnt <- vapply(u, function(k) sum(wt[key == k]), 1)
      miss <- if (is.numeric(x)) !is.na(sas_miss_code(rv)) else !nzchar(sas_rtrim(rv))
      order(!miss, -cnt, txt, method = "radix")
    }
  )
  list(keys = u[ord], values = rv[ord], text = txt[ord], key = key)
}

freq_is_missing <- function(x) {
  if (is.character(x)) !nzchar(sas_rtrim(x)) else is.na(x)
}

# subset the levels of a freq_levels() result (MISSPRINT statistics use the
# non-missing levels only)
freq_lv_subset <- function(l, keep) {
  list(keys = l$keys[keep], values = l$values[keep], text = l$text[keep], key = l$key)
}

freq_num <- function(v) {
  if (is.na(v)) return(".")
  # within 1E-9 of an integer: no decimals (the one display engine rounds it)
  if (abs(v - round(v)) < 1e-9 * max(1, abs(v))) return(display_fixed(v, 0L))
  trimws(sas_fmt_best(v, 8L))
}
freq_pct <- function(v) if (is.na(v)) "." else display_fixed(v, 2L)
# Cell statistics (expected, deviation, cell chi-square) are printed with a
# six-column width (BEST6.), so 8.7179, -3.718 and 10.909 all take six
# characters and trailing zeros are dropped.
freq_stat_num <- function(v) {
  if (is.na(v)) return(".")
  trimws(sas_fmt_best(v, 6L))
}

#' PROC FREQ engine
#'
#' Runs the FREQ entry of the PROC registry (`sas_rt_proc("freq", ...)`).
#' @param d Input data (after WHERE).
#' @param sp Specification built by the translator.
#' @export
#' @keywords internal
sas_rt_freq <- function(d, sp) {
  r <- sas_rt_proc("freq", d, sp)
  list(listing = r$listing, out = r$out)
}

# The tables of a PROC FREQ step (registry compute()).  Each table holds the
# listing lines of one piece of output (attr `lines`; the NLEVELS table or
# one table request) until the ODS tables of FREQ carry their own columns
# [plan: freq_registry, F1]; `freq_group` is its BY group.
freq_table_lines <- function(lines, k, by) {
  tb <- new_sasonr_ods_table(tibble::tibble(), name = "Freq", label = "Freq", path = "Freq.Freq", proc = "Freq",
                             template = "base.freq.lines", by = by)
  attr(tb, "lines") <- lines
  attr(tb, "freq_group") <- k
  tb
}

# Listing of the FREQ tables: the procedure title once at the top, or after
# the BY line of every BY group (each group starts its own page).
freq_listing_tables <- function(tables) {
  lst <- character()
  last <- 0L
  for (tb in tables) {
    k <- attr(tb, "freq_group", exact = TRUE)
    by <- attr(tb, "by", exact = TRUE)
    if (k != last) {
      lst <- c(lst, if (!is.null(by)) c("", by$text, "", "The FREQ Procedure", "") else if (last == 0L) c("The FREQ Procedure", ""))
      last <- k
    }
    lst <- c(lst, switch(attr(tb, "template", exact = TRUE),
      base.freq.oneway = freq_oneway_listing(tb),
      base.freq.crosstab = freq_crosstab_listing(tb),
      base.freq.list = freq_list_listing(tb),
      base.freq.chisq = freq_chisq_listing(tb),
      base.freq.fisher = freq_fisher_listing(tb),
      base.freq.cmh = freq_cmh_listing(tb),
      base.freq.commonrr = freq_commonrr_listing(tb),
      base.freq.breslow = freq_breslow_listing(tb),
      base.freq.relrisk = , base.freq.relriskcl = freq_relrisk_listing(tb),
      base.freq.riskdiff = , base.freq.riskdiffcl = freq_riskdiff_listing(tb),
      base.freq.onewaychisq = freq_onewaychisq_listing(tb),
      base.freq.nlevels = freq_nlevels_listing(tb),
      attr(tb, "lines", exact = TRUE)))
  }
  lst
}

freq_compute <- function(d, sp) {
  info <- sp$info
  # every request NOPRINT without statistics, no OUT= and no OUTPUT
  # statement: the step reads nothing [measured, TABLES / NOPRINT]
  stat_opts <- c("chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "relrisk", "riskdiff")
  idle <- length(sp$requests) > 0L && sp$print && !sp$nlevels && is.null(sp$output) && length(sp$exact) == 0L &&
    all(vapply(sp$requests, function(r) isTRUE(r$opts$noprint) && is.null(r$opts$out) && !any(stat_opts %in% names(r$opts)), TRUE))
  if (idle) {
    rt <- rt_diag()
    rt$reads <- list()
    return(list(tables = list(), data = list(),
                events = "NOTE: Processing will terminate because there are no valid requests for displayed output or output data sets."))
  }
  groups <- if (!is.null(sp$by)) proc_by_groups(d, sp$by, sp$by_desc, sp$data, info) else list(seq_len(nrow(d)))
  tables <- list()
  outs <- list()
  out_rows <- list()
  stat_rows <- list()
  for (gi in seq_along(groups)) {
    g <- groups[[gi]]
    db <- d[g, , drop = FALSE]
    wt <- if (is.null(sp$weight)) rep(1, nrow(db)) else db[[sp$weight]]
    use <- !is.na(wt)
    if (!sp$zeros && !is.null(sp$weight)) use <- use & wt != 0
    db <- db[use, , drop = FALSE]
    wt <- wt[use]
    pieces <- list()
    by <- if (!is.null(sp$by) && length(g) > 0) list(values = lapply(stats::setNames(sp$by, sp$by), function(v) d[[v]][g[1]]),
                                                     text = proc_by_line(d, g[1], sp$by, info), group = gi)
    if (sp$nlevels) pieces <- c(pieces, list(freq_nlevels_table(db, sp)))
    for (ri in seq_along(sp$requests)) {
      rq <- sp$requests[[ri]]
      res <- freq_one_request(db, wt, rq, sp)
      for (ti in seq_along(res$tables)) {
        if (inherits(res$tables[[ti]], "sasonr_ods_table")) {
          attr(res$tables[[ti]], "path") <- paste0("Freq.", if (!is.null(sp$by)) paste0("ByGroup", gi, "."), "Table", ri, ".",
                                                   attr(res$tables[[ti]], "name", exact = TRUE))
        }
      }
      if (isTRUE(rq$opts$noprint)) {
        st_segs <- res$tables[-seq_len(res$n_freq)]
        # the statistics of a one-way table get the heading of the table they
        # belong to [measured, binomial]
        if (length(rq$vars) == 1L && length(st_segs) > 0L) {
          st_segs <- c(list(freq_lines_seg(c(paste("Statistics for Table of", info[[rq$vars]]$disp), ""), decor = TRUE)), st_segs)
        }
        # the multiway CMH summary starts a page of its own after the stratum
        # tables; without them it is the first output and the procedure
        # title is written once [measured, cmh]
        if (length(rq$vars) > 2L && length(st_segs) > 0L && identical(utils::head(as.character(st_segs[[1]]), 1L), "The FREQ Procedure")) {
          if (length(pieces) > 0L) abort_unsupported("TABLES option NOPRINT with a multiway table after other output of the step", rq$line)
          st_segs[[1]] <- freq_lines_seg(as.character(st_segs[[1]])[-(1:2)], decor = TRUE)
        }
        pieces <- c(pieces, st_segs)
      } else {
        pieces <- c(pieces, res$tables)
      }
      if (!is.null(rq$opts$out)) {
        nm <- rq$opts$out$name
        byv <- lapply(sp$by, function(v) rep(d[[v]][g[1]], nrow(res$out)))
        names(byv) <- sp$by
        od <- if (length(sp$by) > 0) dplyr::bind_cols(tibble::as_tibble(byv), res$out) else res$out
        out_rows[[nm]] <- list(data = dplyr::bind_rows(out_rows[[nm]]$data, od), cols = res$out_cols, opts = rq$opts$out$options,
                               line = rq$line)
      }
      if (!is.null(sp$output)) stat_rows[[length(stat_rows) + 1L]] <- c(lapply(stats::setNames(sp$by, sp$by), function(v) d[[v]][g[1]]),
                                                                     res$stats)
    }
    if (sp$print) {
      tables <- c(tables, lapply(pieces, function(pc) {
        if (!inherits(pc, "sasonr_ods_table")) {
          tl <- freq_table_lines(as.character(pc), gi, by)
          attr(tl, "decor") <- isTRUE(attr(pc, "decor", exact = TRUE))
          if (!attr(tl, "decor") && length(pc) > 0L) {
            rt <- rt_diag()
            rt$freq_lines <- TRUE
          }
          return(tl)
        }
        attr(pc, "by") <- by
        attr(pc, "freq_group") <- gi
        pc
      }))
    }
  }
  for (nm in names(out_rows)) {
    x <- out_rows[[nm]]
    cols <- c(lapply(sp$by, function(v) freq_col_of(v, info)), x$cols)
    data <- x$data
    if (is.null(data) || ncol(data) == 0) {
      data <- tibble::as_tibble(stats::setNames(lapply(cols, function(c) if (c$char) character() else numeric()),
                                                vapply(cols, `[[`, "", "name")))
    }
    m <- proc_out_meta(cols)
    # the data set label of a TABLES OUT= data set (measured)
    m$memlabel <- "Frequency Counts and Percentages"
    outs[[nm]] <- proc_out_options(data, m, x$opts, x$line)
  }
  if (!is.null(sp$output)) outs[[sp$output$name]] <- freq_output_data(stat_rows, sp)
  list(tables = tables, data = outs, events = character())
}

freq_col_of <- function(v, info) {
  i <- info[[v]]
  list(name = v, disp = i$disp, char = i$char, len = i$len, fmt = i$fmt, informat = i$informat, label = i$label)
}

freq_nlevels_table <- function(db, sp) {
  vars <- unique(vctrs::list_unchop(lapply(sp$requests, `[[`, "vars"), ptype = character(), name_spec = rlang::zap()))
  nl <- vapply(vars, function(v) length(unique(proc_value_text(db[[v]], sp$info[[v]]))), 1L)
  miss <- vapply(vars, function(v) length(unique(proc_value_text(db[[v]][freq_is_missing(db[[v]])], sp$info[[v]]))), 1L)
  if (any(miss > 0)) abort_unsupported("NLEVELS with missing levels (Missing Levels columns)")
  disp <- vapply(vars, function(v) sp$info[[v]]$disp, "")
  new_sasonr_ods_table(tibble::tibble(TableVar = unname(disp), NLevels = unname(as.numeric(nl))), name = "NLevels",
                       label = "Number of Variable Levels", path = "Freq.NLevels", proc = "Freq", template = "base.freq.nlevels",
                       cols = ods_cols(c("TableVar", "NLevels"), label = c("Variable", "Levels"), role = c("label", "stat")))
}

freq_nlevels_listing <- function(tb) {
  w <- max(8L, nchar(tb$TableVar))
  c("Number of Variable Levels", "", paste0(formatC("Variable", width = -w), "      Levels"), strrep("-", w + 12L),
    paste0(formatC(tb$TableVar, width = -w), formatC(sprintf("%.0f", tb$NLevels), width = 12L)), "")
}

# One table request (one-way, two-way or n-way) within a BY group.
freq_one_request <- function(db, wt, rq, sp) {
  vars <- rq$vars
  o <- rq$opts
  info <- sp$info
  miss_opt <- isTRUE(o$missing)
  missprint <- isTRUE(o$missprint) && !miss_opt
  anymiss <- Reduce(`|`, lapply(vars, function(v) freq_is_missing(db[[v]])), rep(FALSE, nrow(db)))
  nmiss <- if (miss_opt) 0 else sum(wt[anymiss])
  # The TABLES statement options EXPECTED / DEVIATION / CELLCHI2 apply to
  # two-way and multiway tables in the default crosstabulation cell format; the
  # manual states that they have no effect for one-way frequency tables, so
  # they are accepted and ignored there.  The request itself is gated by the
  # statistics registry (freq_stat_gate).  The
  # n-way (CROSSLIST-style) cell rows are not printed by sasonr yet, so that
  # case still fails loud.
  if (length(vars) > 2 && any(c("expected", "deviation", "cellchi2") %in% names(o))) {
    abort_unsupported("EXPECTED / DEVIATION / CELLCHI2 for an n-way table (only the two-way crosstab layout is supported)", rq$line)
  }
  if (missprint && length(vars) > 2) abort_unsupported("MISSPRINT for multi-way tables", rq$line)
  if (missprint && length(vars) > 1 && isTRUE(o$list)) {
    abort_unsupported("MISSPRINT with the LIST option (the combination is not supported)", rq$line)
  }
  keep <- if (miss_opt || missprint) rep(TRUE, nrow(db)) else !anymiss
  dd <- db[keep, , drop = FALSE]
  ww <- wt[keep]
  if (any(ww < 0)) abort_unsupported("negative weights are not supported in PROC FREQ", rq$line)
  clash <- intersect(toupper(vars), toupper(c("Frequency", "TestFrequency", "Percent", "TestPercent", "CumFrequency", "CumPercent",
                                                  "_TYPE_", "Expected", "Deviation", "CellChiSquare", "RowPercent", "ColPercent")))
  if (length(clash) > 0L || anyDuplicated(toupper(vars))) {
    abort_unsupported(paste0("a PROC FREQ table variable named like a column of its ODS table (", clash[1] %||% toupper(vars[1]), ")"), rq$line)
  }
  lv <- lapply(vars, function(v) freq_levels(dd[[v]], ww, info[[v]], sp$order))
  names(lv) <- vars
  idx <- lapply(vars, function(v) match(lv[[v]]$key, lv[[v]]$keys))
  k <- length(vars)
  # cell counts over all level combinations (array, levels in display order)
  dims <- vapply(lv, function(l) length(l$keys), 1L)
  tab <- array(0, dim = pmax(dims, 0L))
  if (nrow(dd) > 0 && all(dims > 0)) {
    lin <- Reduce(function(a, j) a + (idx[[j]] - 1) * prod(dims[seq_len(j - 1)]), seq_len(k)[-1], idx[[1]])
    s <- tapply(ww, lin, sum)
    tab[as.integer(names(s))] <- s
    present <- tapply(rep(1, length(lin)), lin, sum)
    obs <- array(0, dim = dims)
    obs[as.integer(names(present))] <- present
  } else {
    obs <- tab
  }
  neg <- any(ww < 0)
  res <- list(listing = character(), out = NULL, out_cols = NULL, stats = list())
  if (k == 1) {
    res <- freq_oneway(tab, obs, lv[[1]], vars, info, o, nmiss, neg, missprint)
    # OUT= keeps the missing levels (excluded from the table) with their
    # count and a missing PERCENT
    if (!miss_opt && !missprint && nmiss > 0) {
      dm <- db[anymiss, , drop = FALSE]
      ml <- freq_levels(dm[[vars[1]]], wt[anymiss], info[[vars[1]]], "internal")
      mc <- vapply(ml$keys, function(kk) sum(wt[anymiss][ml$key == kk]), 1)
      mo <- list()
      mo[[vars[1]]] <- ml$values
      mo$count <- unname(mc)
      mo$percent <- rep(NA_real_, length(mc))
      if (isTRUE(o$outcum)) {
        mo$cum_freq <- rep(NA_real_, length(mc))
        mo$cum_pct <- rep(NA_real_, length(mc))
      }
      res$out <- dplyr::bind_rows(tibble::as_tibble(mo), res$out)
    }
  } else if (k == 2 && !isTRUE(o$list)) {
    res <- freq_twoway(tab, obs, lv, vars, info, o, nmiss, sp)
  } else {
    res <- freq_nway(tab, obs, lv, vars, info, o, nmiss, sp)
  }
  # MISSPRINT lists the missing level in the table but excludes it from the
  # statistics: hand the statistics the non-missing sub-table
  stat_tab <- tab
  stat_lv <- lv
  if (k == 2 && missprint) {
    m1 <- freq_is_missing(lv[[1]]$values)
    m2 <- freq_is_missing(lv[[2]]$values)
    if (any(m1) || any(m2)) {
      stat_tab <- tab[!m1, !m2, drop = FALSE]
      stat_lv <- list(freq_lv_subset(lv[[1]], !m1), freq_lv_subset(lv[[2]], !m2))
    }
  }
  if (k == 2 && missprint && (nrow(stat_tab) != 2L || ncol(stat_tab) != 2L) &&
      (isTRUE(o$chisq) || isTRUE(o$fisher) || length(sp$exact) > 0)) {
    # MISSPRINT statistics are supported for a non-missing 2x2 table only
    abort_unsupported("MISSPRINT with statistics for a table that is not 2x2 (only the 2x2 layout is supported)", rq$line)
  }
  res$stats <- freq_request_stats(stat_tab, stat_lv, vars, info, o, sp, nmiss)
  segs <- c(res$pieces, if (length(res$listing)) list(freq_lines_seg(res$listing)),
            freq_stats_listing(res$stats, tab, vars, info, o, sp, pieces = TRUE))
  # the tables of the request: OneWayFreqs / CrossTabFreqs, then the
  # statistics output (ODS tables and listing lines) in listing order
  res$tables <- c(if (!is.null(res[["table"]])) list(res[["table"]]), segs)
  # the frequency (or crosstabulation) output: the table, or the lines of the
  # stratum tables of a multiway request
  res$n_freq <- as.integer(!is.null(res[["table"]])) + length(res$pieces) + as.integer(length(res$listing) > 0L)
  if (!length(res$tables)) res$tables <- list(freq_lines_seg(character()))
  res
}

freq_value_cell <- function(lvl, j, info, v) lvl$text[j]

# The null hypothesis of the one-way chi-square test from CHISQ(TESTP=) /
# CHISQ(TESTF=) (manual: TABLES statement, CHISQ chisq-options): TESTP= gives
# proportions (or percentages), TESTF= gives frequencies that must sum to the
# table total.  Returns NULL for the default equal-proportions test.
freq_oneway_test_spec <- function(o, cnt) {
  op <- o$chisq_opts
  if (is.null(op) || (is.null(op$testp) && is.null(op$testf))) return(NULL)
  n <- sum(cnt)
  if (!is.null(op$testp)) {
    p <- op$testp
    if (length(p) != length(cnt)) {
      abort_unsupported("CHISQ TESTP= with a number of values that does not match the number of table levels")
    }
    if (any(p <= 0)) abort_unsupported("CHISQ TESTP= values must be positive")
    if (abs(sum(p) - 1) < 1e-8) {
      # proportions
    } else if (abs(sum(p) - 100) < 1e-6) {
      p <- p / 100
    } else {
      abort_unsupported("CHISQ TESTP= proportions do not sum to 1 or to 100")
    }
    return(list(kind = "p", values = p, expected = p * n))
  }
  f <- op$testf
  if (length(f) != length(cnt)) {
    abort_unsupported("CHISQ TESTF= with a number of values that does not match the number of table levels")
  }
  if (any(f <= 0)) abort_unsupported("CHISQ TESTF= values must be positive")
  if (abs(sum(f) - n) > 1e-9 * max(1, abs(n))) {
    abort_unsupported("CHISQ TESTF= frequencies do not sum to the total frequency")
  }
  list(kind = "f", values = f, expected = f)
}

freq_oneway <- function(tab, obs, l, vars, info, o, nmiss, neg, missprint) {
  v <- vars[1]
  i <- info[[v]]
  n <- length(l$keys)
  cnt <- as.numeric(tab)
  is_m <- if (n > 0) freq_is_missing(l$values) else logical()
  if (missprint) {
    tot <- sum(cnt[!is_m])
  } else {
    tot <- sum(cnt)
  }
  test <- freq_oneway_test_spec(o, cnt)
  if (!is.null(test)) {
    if (any(is_m)) abort_unsupported("CHISQ TESTP= / TESTF= with missing levels (not supported)")
    if (isTRUE(o$nofreq) || isTRUE(o$nopercent) || isTRUE(o$nocum)) {
      abort_unsupported("CHISQ TESTP= / TESTF= with NOFREQ / NOPERCENT / NOCUM (not supported)")
    }
  }
  pct <- cnt / tot * 100
  pct[is_m & missprint] <- NA
  cum <- cumsum(ifelse(is_m & missprint, 0, cnt))
  cpct <- cum / tot * 100
  show_pct <- !isTRUE(o$nopercent) && !neg
  show_cum <- !isTRUE(o$nocum)
  # the OneWayFreqs table: the raw class values and the statistics; the
  # columns the options leave out are hidden
  x <- list()
  x[[v]] <- l$values
  x$Frequency <- cnt
  if (!is.null(test) && identical(test$kind, "f")) x$TestFrequency <- test$values
  x$Percent <- pct
  if (!is.null(test) && identical(test$kind, "p")) x$TestPercent <- test$values * 100
  mp <- is_m & missprint
  x$CumFrequency <- ifelse(mp, NA_real_, cum)
  x$CumPercent <- ifelse(mp, NA_real_, cpct)
  x <- tibble::as_tibble(x)
  hidden <- c(FALSE, isTRUE(o$nofreq), if (!is.null(test) && identical(test$kind, "f")) FALSE, !show_pct,
              if (!is.null(test) && identical(test$kind, "p")) FALSE, !show_cum || isTRUE(o$nofreq), !show_cum || !show_pct)
  tb <- new_sasonr_ods_table(x, name = "OneWayFreqs", label = "One-Way Frequencies", path = paste0("Freq.Table1.OneWayFreqs"),
                             proc = "Freq", template = "base.freq.oneway",
                             cols = ods_cols(names(x), label = c(i$disp, names(x)[-1]), hidden = hidden,
                                             role = c("class", rep("stat", ncol(x) - 1L))))
  attr(tb, "freq") <- list(disp = i$disp, char = i$char, text = l$text, test = test$kind, nmiss = nmiss)
  # OUT= (missing levels keep their count with a missing PERCENT)
  out_cols <- list(freq_col_of(v, info), list(name = "count", disp = "COUNT", char = FALSE, len = 8L, label = "Frequency Count"),
                   list(name = "percent", disp = "PERCENT", char = FALSE, len = 8L, label = "Percent of Total Frequency"))
  od <- list()
  od[[v]] <- l$values
  od$count <- cnt
  od$percent <- pct
  if (isTRUE(o$outcum)) {
    out_cols <- c(out_cols, list(list(name = "cum_freq", disp = "CUM_FREQ", char = FALSE, len = 8L, label = "Cumulative Frequency Count"),
                                 list(name = "cum_pct", disp = "CUM_PCT", char = FALSE, len = 8L, label = "Cumulative Percent")))
    od$cum_freq <- cum
    od$cum_pct <- cpct
  }
  keep <- isTRUE(o$sparse) | cnt != 0 | obs > 0
  od <- tibble::as_tibble(lapply(od, function(x) x[keep]))
  list(table = tb, out = od, out_cols = out_cols)
}

# Listing of a OneWayFreqs table (template base.freq.oneway).
freq_oneway_listing <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  cols <- attr(tb, "cols", exact = TRUE)
  shown <- function(nm) nm %in% cols$name[!cols$hidden]
  i <- list(disp = a$disp, char = a$char)
  o <- list(nofreq = !shown("Frequency"))
  show_pct <- shown("Percent")
  show_cum <- shown("CumFrequency") || shown("CumPercent")
  test <- if (!is.null(a$test)) list(kind = a$test, values = if (identical(a$test, "f")) tb$TestFrequency else tb$TestPercent / 100)
  cnt <- tb$Frequency
  pct <- tb$Percent
  cum <- tb$CumFrequency
  cpct <- tb$CumPercent
  n <- nrow(tb)
  nmiss <- a$nmiss
  vals <- a$text
  right <- !i$char
  W <- max(c(nchar(i$disp), nchar(vals)))
  pad <- function(x) formatC(x, width = if (right) W else -W)
  top <- strrep(" ", W)
  head <- pad(i$disp)
  width <- W
  if (!isTRUE(o$nofreq)) {
    head <- paste0(head, "    Frequency")
    top <- paste0(top, strrep(" ", 13))
    width <- width + 13L
  }
  if (!is.null(test) && identical(test$kind, "f")) {
    # the Test Frequency column sits between Frequency and Percent
    head <- paste0(head, "    Frequency")
    top <- paste0(top, "                Test")
    width <- width + 13L
  }
  if (show_pct) {
    head <- paste0(head, "     Percent")
    top <- paste0(top, strrep(" ", 12))
    width <- width + 12L
  }
  if (!is.null(test) && identical(test$kind, "p")) {
    # the Test Percent column sits after Percent
    head <- paste0(head, "     Percent")
    top <- paste0(top, "        Test")
    width <- width + 12L
  }
  if (show_cum) {
    if (!isTRUE(o$nofreq)) {
      head <- paste0(head, "     Frequency")
      top <- paste0(top, "    Cumulative")
      width <- width + 14L
    }
    if (show_pct) {
      head <- paste0(head, "      Percent")
      top <- paste0(top, "    Cumulative")
      width <- width + 14L
    }
  }
  body <- vapply(seq_len(n), function(j) {
    s <- pad(vals[j])
    if (!isTRUE(o$nofreq)) s <- paste0(s, formatC(freq_num(cnt[j]), width = 12L), " ")
    if (!is.null(test) && identical(test$kind, "f")) s <- paste0(s, formatC(freq_num(test$values[j]), width = 12L), " ")
    if (show_pct) s <- paste0(s, formatC(freq_pct(pct[j]), width = 11L), " ")
    if (!is.null(test) && identical(test$kind, "p")) s <- paste0(s, formatC(freq_pct(test$values[j] * 100), width = 11L), " ")
    if (show_cum) {
      mp <- is.na(cum[j])
      if (!isTRUE(o$nofreq)) s <- paste0(s, formatC(if (mp) "." else freq_num(cum[j]), width = 13L), " ")
      if (show_pct) s <- paste0(s, formatC(if (mp) "." else freq_pct(cpct[j]), width = 12L), "  ")
    }
    sas_rtrim(s)
  }, "")
  lst <- c(if (show_cum) sas_rtrim(top), head, strrep("-", width), body)
  if (nmiss > 0) lst <- c(lst, "", paste0(strrep(" ", max(0L, (width - 21L) %/% 2L)), "Frequency Missing = ", freq_num(nmiss)))
  lst <- c(lst, "")
  lst
}

freq_twoway <- function(tab, obs, lv, vars, info, o, nmiss, sp, title = NULL, stratum = NULL) {
  lr <- lv[[1]]
  lc <- lv[[2]]
  nr <- length(lr$keys)
  nc <- length(lc$keys)
  ri <- info[[vars[1]]]
  ci <- info[[vars[2]]]
  tot <- sum(tab)
  rt_ <- rowSums(tab)
  ct_ <- colSums(tab)
  # MISSPRINT: the missing-value level is listed as an extra row and column
  # (its cells show the observed counts) but excludes it from the totals,
  # percentages and statistics
  missprint <- isTRUE(o$missprint) && !isTRUE(o$missing)
  missr <- if (missprint && nr > 0) freq_is_missing(lr$values) else rep(FALSE, nr)
  missc <- if (missprint && nc > 0) freq_is_missing(lc$values) else rep(FALSE, nc)
  mptab <- missprint && (any(missr) || any(missc))
  if (mptab) {
    if (isTRUE(o$expected) || isTRUE(o$deviation) || isTRUE(o$cellchi2)) {
      abort_unsupported("EXPECTED / DEVIATION / CELLCHI2 together with MISSPRINT (not supported)")
    }
    if (!is.null(o$out)) abort_unsupported("OUT= together with MISSPRINT (data set layout not supported)")
    tot_nm <- sum(tab[!missr, !missc, drop = FALSE])
    rt_nm <- rowSums(tab[, !missc, drop = FALSE])
    ct_nm <- colSums(tab[!missr, , drop = FALSE])
  }
  show <- c(freq = !isTRUE(o$nofreq), expected = isTRUE(o$expected), deviation = isTRUE(o$deviation),
            cellchi2 = isTRUE(o$cellchi2), pct = !isTRUE(o$nopercent), row = !isTRUE(o$norow),
            col = !isTRUE(o$nocol))
  # the CrossTabFreqs table: one row per cell (_TYPE_ "11"), row total ("10"),
  # column total ("01") and the grand total ("00"); a statistic that is not
  # defined for a cell (MISSPRINT levels, an empty row) is missing
  rows <- rbind(
    data.frame(r = rep(seq_len(nr), each = nc + 1L), c = rep(c(seq_len(nc), NA), nr)),
    data.frame(r = NA, c = c(seq_len(nc), NA))
  )
  type <- ifelse(!is.na(rows$r) & !is.na(rows$c), "11", ifelse(!is.na(rows$r), "10", ifelse(!is.na(rows$c), "01", "00")))
  na <- NA_real_
  stat <- function(r, cc, s) {
    if (!is.na(r) && !is.na(cc)) {
      x <- tab[r, cc]
      ex <- if (tot > 0) rt_[r] * ct_[cc] / tot else na
      m <- mptab && (missr[r] || missc[cc])
      return(switch(s, freq = x,
        pct = if (m) na else x / (if (mptab) tot_nm else tot) * 100,
        expected = ex, deviation = if (is.na(ex)) na else x - ex,
        cellchi2 = if (is.na(ex) || ex == 0) na else (x - ex)^2 / ex,
        row = { d <- if (mptab) rt_nm[r] else rt_[r]; if (m || d == 0) na else x / d * 100 },
        col = { d <- if (mptab) ct_nm[cc] else ct_[cc]; if (m || d == 0) na else x / d * 100 }))
    }
    if (!is.na(r)) {
      if (mptab && missr[r]) return(na)
      return(switch(s, freq = if (mptab) rt_nm[r] else rt_[r], pct = if (mptab) rt_nm[r] / tot_nm * 100 else rt_[r] / tot * 100, na))
    }
    if (!is.na(cc)) {
      if (mptab && missc[cc]) return(na)
      return(switch(s, freq = if (mptab) ct_nm[cc] else ct_[cc], pct = if (mptab) ct_nm[cc] / tot_nm * 100 else ct_[cc] / tot * 100, na))
    }
    switch(s, freq = if (mptab) tot_nm else tot, pct = if (mptab || tot > 0) 100 else na, na)
  }
  sv <- function(s) vapply(seq_len(nrow(rows)), function(i) stat(rows$r[i], rows$c[i], s), 0)
  x <- list()
  x[[vars[1]]] <- lr$values[rows$r]
  x[[vars[2]]] <- lc$values[rows$c]
  x[["_TYPE_"]] <- type
  x$Frequency <- sv("freq")
  x$Expected <- sv("expected")
  x$Deviation <- sv("deviation")
  x$CellChiSquare <- sv("cellchi2")
  x$Percent <- sv("pct")
  x$RowPercent <- sv("row")
  x$ColPercent <- sv("col")
  x <- tibble::as_tibble(x)
  stat_cols <- c(freq = "Frequency", expected = "Expected", deviation = "Deviation", cellchi2 = "CellChiSquare",
                 pct = "Percent", row = "RowPercent", col = "ColPercent")
  hidden <- c(FALSE, FALSE, TRUE, !show[names(stat_cols)])
  tb <- new_sasonr_ods_table(x, name = "CrossTabFreqs", label = "Cross-Tabular Freq Table", path = "Freq.Table1.CrossTabFreqs",
                             proc = "Freq", template = "base.freq.crosstab",
                             cols = ods_cols(names(x), label = c(ri$disp, ci$disp, "_TYPE_", stat_cols),
                                             hidden = unname(hidden), role = c("class", "class", "label", rep("stat", 7L))))
  # a user-format label keeps its leading blanks in the column heading ('  50+ ')
  C <- max(c(8L, nchar(lc$text), 7L))
  chv <- lc$text
  if (!is.null(ci$fmt) && !is.null(user_fmt(ci$fmt$name, is.character(lc$values) || isTRUE(ci$fmt$char)))) {
    chv <- substr(sas_rtrim(sas_format(lc$values, ci$fmt)), 1L, C)
    chv[is.na(chv)] <- lc$text[is.na(chv)]
  }
  attr(tb, "freq") <- list(ri = ri, ci = ci, rtext = lr$text, ctext = lc$text, chead = chv, title = title, stratum = stratum,
                           nmiss = nmiss, nr = nr, nc = nc)
  R <- max(c(9L, nchar(all_labels_freq[names(show)[show]]), nchar(lr$text) + 1L))
  if (R + 1L + (C + 1L) * nc + 7L > 132L) abort_unsupported("a crosstabulation wider than the line size (multi-panel tables)")
  # OUT=
  grid <- expand.grid(r = seq_len(nr), c = seq_len(nc))
  grid <- grid[order(grid$r, grid$c), ]
  cnt <- tab[cbind(grid$r, grid$c)]
  keep <- isTRUE(o$sparse) | cnt != 0 | obs[cbind(grid$r, grid$c)] > 0
  grid <- grid[keep, , drop = FALSE]
  cnt <- cnt[keep]
  od <- list()
  od[[vars[1]]] <- lr$values[grid$r]
  od[[vars[2]]] <- lc$values[grid$c]
  od$count <- cnt
  od$percent <- cnt / tot * 100
  out_cols <- list(freq_col_of(vars[1], info), freq_col_of(vars[2], info),
                   list(name = "count", disp = "COUNT", char = FALSE, len = 8L, label = "Frequency Count"),
                   list(name = "percent", disp = "PERCENT", char = FALSE, len = 8L, label = "Percent of Total Frequency"))
  if (isTRUE(o$outpct)) {
    od$pct_row <- ifelse(rt_[grid$r] == 0, NA, cnt / rt_[grid$r] * 100)
    od$pct_col <- ifelse(ct_[grid$c] == 0, NA, cnt / ct_[grid$c] * 100)
    out_cols <- c(out_cols, list(list(name = "pct_row", disp = "PCT_ROW", char = FALSE, len = 8L, label = "Percent of Row Frequency"),
                                 list(name = "pct_col", disp = "PCT_COL", char = FALSE, len = 8L, label = "Percent of Column Frequency")))
  }
  list(table = tb, out = tibble::as_tibble(od), out_cols = out_cols)
}

all_labels_freq <- c(freq = "Frequency", expected = "Expected", deviation = "Deviation",
                     cellchi2 = "Cell Chi-Square", pct = "Percent", row = "Row Pct", col = "Col Pct")

# Listing of a CrossTabFreqs table (template base.freq.crosstab).
freq_crosstab_listing <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  cols <- attr(tb, "cols", exact = TRUE)
  stat_cols <- c(freq = "Frequency", expected = "Expected", deviation = "Deviation", cellchi2 = "CellChiSquare",
                 pct = "Percent", row = "RowPercent", col = "ColPercent")
  shown <- !cols$hidden[match(stat_cols, cols$name)]
  labels <- all_labels_freq[names(stat_cols)[shown]]
  ri <- a$ri
  ci <- a$ci
  nr <- a$nr
  nc <- a$nc
  rvals <- a$rtext
  cvals <- a$ctext
  type <- tb[["_TYPE_"]]
  # the value of statistic s in the cell (r, cc); r or cc NA for a total
  at <- function(r, cc, s) {
    i <- if (!is.na(r) && !is.na(cc)) (r - 1L) * (nc + 1L) + cc else if (!is.na(r)) r * (nc + 1L) else nr * (nc + 1L) + (if (is.na(cc)) nc + 1L else cc)
    tb[[stat_cols[[s]]]][i]
  }
  # a row value is written in pieces of 16 characters (measured): the row
  # label column is at most 17 wide for the values
  R <- max(c(9L, nchar(labels), pmin(nchar(rvals), 16L) + 1L))
  C <- max(c(8L, nchar(cvals), 7L))
  lst <- c(a$title %||% paste("Table of", ri$disp, "by", ci$disp), a$stratum, "")
  # variable headings carry the label in parentheses; a row heading that does
  # not fit before the first column puts the column heading on the next line
  rh <- paste0(ri$disp, if (!is.null(ri$label)) paste0("(", ri$label, ")"))
  ch <- paste0(ci$disp, if (!is.null(ci$label)) paste0("(", ci$label, ")"))
  # measured: a row heading longer than the row label column with the column
  # heading on the next line when both would not fit the table width; when they
  # would, the measured output has them on one line, at a column that is not measured
  # (measured): five blanks between them on one line when the row heading is
  # longer than the row label column and both fit the table width
  one_line <- nchar(rh) + 5L + nchar(ch) <= R + 1L + (C + 1L) * nc + 7L
  lst <- c(lst, if (nchar(rh) <= R) paste0(formatC(rh, width = -(R + 1L)), ch) else if (one_line) paste0(rh, strrep(" ", 5L), ch) else
    c(rh, paste0(strrep(" ", R + 1L), ch)))
  chead <- paste(vapply(a$chead, function(x) formatC(x, width = if (proc_right_align(ci)) C else -C), ""), collapse = "|")
  for (j in seq_along(labels)) {
    lab <- formatC(labels[j], width = -R)
    lst <- c(lst, if (j < length(labels)) paste0(lab, "|") else paste0(lab, "|", chead, "|  Total"))
  }
  sep <- paste0(strrep("-", R), "+", strrep(paste0(strrep("-", C), "+"), nc))
  lst <- c(lst, sep)
  cell <- function(x) paste0(formatC(x, width = C - 1L), " ")
  txt <- function(s, v) switch(s, freq = freq_num(v), pct = , row = , col = freq_pct(v), freq_stat_num(v))
  for (r in seq_len(nr)) {
    pieces <- if (nchar(rvals[r]) > 16L) substring(rvals[r], seq(1L, nchar(rvals[r]), 16L), pmin(seq(1L, nchar(rvals[r]), 16L) + 15L, nchar(rvals[r]))) else rvals[r]
    if (length(pieces) > 1L && (proc_right_align(ri) || length(pieces) > length(labels))) {
      abort_unsupported("a PROC FREQ crosstable row value longer than 16 characters (right-aligned, or more pieces than statistic lines)")
    }
    rv <- if (!proc_right_align(ri)) formatC(pieces, width = -R) else paste0(formatC(pieces, width = R - 1L), " ")
    lines <- character()
    k <- 0L
    for (s in names(labels)) {
      k <- k + 1L
      vals <- vapply(seq_len(nc), function(cc) txt(s, at(r, cc, s)), "")
      totv <- if (s %in% c("freq", "pct")) txt(s, at(r, NA, s)) else ""
      lines <- c(lines, sas_rtrim(paste0(if (k <= length(rv)) rv[k] else strrep(" ", R), "|", paste(vapply(vals, cell, ""), collapse = "|"), "|",
                                         formatC(totv, width = 7L))))
    }
    lst <- c(lst, lines, sep)
  }
  tl <- paste0(formatC("Total", width = -R), " ", paste(vapply(seq_len(nc), function(cc) cell(freq_num(at(NA, cc, "freq"))), ""), collapse = " "),
               " ", formatC(freq_num(at(NA, NA, "freq")), width = 7L))
  lst <- c(lst, sas_rtrim(tl))
  if ("pct" %in% names(labels)) {
    lst <- c(lst, sas_rtrim(paste0(strrep(" ", R), " ", paste(vapply(seq_len(nc), function(cc) cell(freq_pct(at(NA, cc, "pct"))), ""), collapse = " "),
                                   " ", formatC(freq_pct(at(NA, NA, "pct")), width = 7L))))
  }
  if (a$nmiss > 0 && is.null(a$stratum)) lst <- c(lst, "", paste0("Frequency Missing = ", freq_num(a$nmiss)))
  c(lst, "")
}

freq_nway <- function(tab, obs, lv, vars, info, o, nmiss, sp) {
  k <- length(vars)
  dims <- dim(tab)
  tot <- sum(tab)
  combos <- as.matrix(expand.grid(lapply(rev(dims), seq_len)))[, k:1, drop = FALSE]
  combos <- combos[rlang::exec(order, !!!as.data.frame(combos)), , drop = FALSE]
  cnt <- tab[combos]
  nobs <- obs[combos]
  lst <- character()
  pieces <- list()
  if (isTRUE(o$list)) {
    keep <- cnt != 0 | nobs > 0 | isTRUE(o$sparse)
    cmb <- combos[keep, , drop = FALSE]
    cc <- cnt[keep]
    cum <- cumsum(cc)
    x <- list()
    for (j in seq_len(k)) x[[vars[j]]] <- lv[[j]]$values[cmb[, j]]
    x$Frequency <- cc
    x$Percent <- cc / tot * 100
    x$CumFrequency <- cum
    x$CumPercent <- cum / tot * 100
    x <- tibble::as_tibble(x)
    show_pct <- !isTRUE(o$nopercent)
    show_cum <- !isTRUE(o$nocum)
    tb <- new_sasonr_ods_table(x, name = "List", label = "LIST Frequencies", path = "Freq.Table1.List", proc = "Freq",
                               template = "base.freq.list",
                               cols = ods_cols(names(x), label = c(vapply(vars, function(v) info[[v]]$disp, ""), names(x)[-seq_len(k)]),
                                               hidden = c(rep(FALSE, k + 1L), !show_pct, !show_cum, !show_cum || !show_pct),
                                               role = c(rep("class", k), rep("stat", 4L))))
    attr(tb, "freq") <- list(k = k, disp = vapply(vars, function(v) info[[v]]$disp, ""), right = vapply(vars, function(v) !info[[v]]$char, TRUE),
                             text = lapply(seq_len(k), function(j) lv[[j]]$text[cmb[, j]]), nmiss = nmiss)
    pieces <- list(tb)
  } else {
    # one crosstabulation per stratum (combination of the leading variables)
    sdims <- dims[seq_len(k - 2L)]
    strata <- as.matrix(expand.grid(lapply(rev(sdims), seq_len)))[, (k - 2L):1, drop = FALSE]
    strata <- strata[rlang::exec(order, !!!as.data.frame(strata)), , drop = FALSE]
    ti <- 0L
    for (s in seq_len(nrow(strata))) {
      sidx <- strata[s, ]
      sub <- tab[cbind(matrix(rep(sidx, each = dims[k - 1L] * dims[k]), ncol = k - 2L),
                       rep(seq_len(dims[k - 1L]), times = dims[k]), rep(seq_len(dims[k]), each = dims[k - 1L]))]
      sub <- matrix(sub, nrow = dims[k - 1L])
      if (sum(sub) == 0 && sum(obs[cbind(matrix(rep(sidx, each = dims[k - 1L] * dims[k]), ncol = k - 2L),
                                         rep(seq_len(dims[k - 1L]), times = dims[k]), rep(seq_len(dims[k]), each = dims[k - 1L]))]) == 0) next
      ti <- ti + 1L
      ctl <- paste(vapply(seq_len(k - 2L), function(j) paste0(info[[vars[j]]]$disp, "=", lv[[j]]$text[sidx[j]]), ""), collapse = " ")
      sub_obs <- sub
      tw <- freq_twoway(sub, sub_obs + 1, lv[(k - 1L):k], vars[(k - 1L):k], info, o, 0, sp,
                        title = paste("Table", ti, "of", info[[vars[k - 1L]]]$disp, "by", info[[vars[k]]]$disp),
                        stratum = paste("Controlling for", ctl))
      pieces <- c(pieces, list(tw$table))
      # CHISQ / FISHER of a multiway request print one statistics block after
      # each stratum table ("Statistics for Table k of ...")
      if (isTRUE(o$chisq) || isTRUE(o$fisher)) {
        o2 <- o
        o2$cmh <- o2$cmh1 <- o2$cmh2 <- NULL
        o2$relrisk <- o2$riskdiff <- NULL
        sp2 <- sp
        sp2$exact <- character()
        sp2$output <- NULL
        stv <- freq_request_stats(sub, lv[(k - 1L):k], vars[(k - 1L):k], info, o2, sp2, 0)
        sseg <- freq_stats_listing(
          stv, sub, vars[(k - 1L):k], info, o2, sp2,
          title = paste("Statistics for Table", ti, "of", info[[vars[k - 1L]]]$disp, "by", info[[vars[k]]]$disp),
          stratum_line = paste("Controlling for", ctl), pieces = TRUE)
        # the first statistics table of a stratum carries its heading (in the
        # HTML document a title block before the table) [measured, f21]
        first <- TRUE
        sseg <- lapply(sseg, function(x) {
          if (inherits(x, "sasonr_ods_table") && first) {
            attr(x, "heading") <- c(paste("Statistics for Table", ti, "of", info[[vars[k - 1L]]]$disp, "by", info[[vars[k]]]$disp),
                                    paste("Controlling for", ctl))
            first <<- FALSE
          }
          x
        })
        pieces <- c(pieces, sseg)
      }
    }
  }
  # OUT=
  keep <- isTRUE(o$sparse) | cnt != 0 | nobs > 0
  cmb <- combos[keep, , drop = FALSE]
  cc <- cnt[keep]
  od <- list()
  for (j in seq_len(k)) od[[vars[j]]] <- lv[[j]]$values[cmb[, j]]
  od$count <- cc
  od$percent <- cc / tot * 100
  out_cols <- c(lapply(vars, freq_col_of, info = info),
                list(list(name = "count", disp = "COUNT", char = FALSE, len = 8L, label = "Frequency Count"),
                     list(name = "percent", disp = "PERCENT", char = FALSE, len = 8L, label = "Percent of Total Frequency")))
  if (isTRUE(o$outpct)) abort_unsupported("OUTPCT for n-way tables")
  list(pieces = pieces, listing = character(), out = tibble::as_tibble(od), out_cols = out_cols)
}

# Listing of a List table (template base.freq.list): the LIST layout of an
# n-way (or two-way) request.
freq_list_listing <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  cols <- attr(tb, "cols", exact = TRUE)
  k <- a$k
  texts <- a$text
  Ws <- vapply(seq_len(k), function(j) max(c(nchar(a$disp[j]), nchar(texts[[j]]))), 1L)
  padj <- function(x, j) formatC(x, width = if (a$right[j]) Ws[j] else -Ws[j])
  show_pct <- !cols$hidden[cols$name == "Percent"]
  show_cum <- !cols$hidden[cols$name == "CumFrequency"]
  W <- sum(Ws) + 4L * (k - 1L)
  head <- paste(vapply(seq_len(k), function(j) padj(a$disp[j], j), ""), collapse = "    ")
  top <- strrep(" ", W)
  width <- W + 13L
  head <- paste0(head, "    Frequency")
  top <- paste0(top, strrep(" ", 13))
  if (show_pct) {
    head <- paste0(head, "     Percent")
    top <- paste0(top, strrep(" ", 12))
    width <- width + 12L
  }
  if (show_cum) {
    head <- paste0(head, "     Frequency")
    top <- paste0(top, "    Cumulative")
    width <- width + 14L
    if (show_pct) {
      head <- paste0(head, "      Percent")
      top <- paste0(top, "    Cumulative")
      width <- width + 14L
    }
  }
  body <- vapply(seq_len(nrow(tb)), function(r) {
    s <- paste(vapply(seq_len(k), function(j) padj(texts[[j]][r], j), ""), collapse = "    ")
    s <- paste0(s, formatC(freq_num(tb$Frequency[r]), width = 12L), " ")
    if (show_pct) s <- paste0(s, formatC(freq_pct(tb$Percent[r]), width = 11L), " ")
    if (show_cum) {
      s <- paste0(s, formatC(freq_num(tb$CumFrequency[r]), width = 13L), " ")
      if (show_pct) s <- paste0(s, formatC(freq_pct(tb$CumPercent[r]), width = 12L), "  ")
    }
    sas_rtrim(s)
  }, "")
  lst <- c(if (show_cum) sas_rtrim(top), head, strrep("-", width), body)
  if (a$nmiss > 0) lst <- c(lst, "", paste0(strrep(" ", max(0L, (width - 21L) %/% 2L)), "Frequency Missing = ", freq_num(a$nmiss)))
  c(lst, "")
}

# Statistics ---------------------------------------------------------------------------

# Table scores for the Mantel-Haenszel statistic: numeric values, else ranks.
freq_scores <- function(l) {
  v <- l$values
  if (is.numeric(v) && all(!is.na(v))) as.numeric(v) else seq_along(v)
}

# TRUE when a numeric level vector contains missing values: with the MISSING
# option the numeric levels include NA and freq_scores() would silently fall
# back to the level indices, so the table-score statistics fail loud instead.
freq_table_score_na <- function(lv) {
  any(vapply(lv, function(l) is.numeric(l$values) && length(l$values) > 0L && any(is.na(l$values)), TRUE))
}

freq_chisq <- function(tb, rs, cs) {
  n <- sum(tb)
  tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  rs <- rs[seq_len(nrow(tb))]
  R <- nrow(tb)
  C <- ncol(tb)
  e <- outer(rowSums(tb), colSums(tb)) / n
  df <- (R - 1) * (C - 1)
  pchi <- sum((tb - e)^2 / e)
  lr <- 2 * sum(ifelse(tb > 0, tb * log(tb / e), 0))
  out <- list(n = n, df = df, pchi = pchi, p_pchi = stats::pchisq(pchi, df, lower.tail = FALSE),
              lrchi = lr, p_lrchi = stats::pchisq(lr, df, lower.tail = FALSE), small = mean(e < 5), R = R, C = C)
  if (R == 2 && C == 2) {
    aj <- sum(pmax(0, abs(tb - e) - 0.5)^2 / e)
    out$ajchi <- aj
    out$p_ajchi <- stats::pchisq(aj, 1, lower.tail = FALSE)
    out$phi <- (tb[1, 1] * tb[2, 2] - tb[1, 2] * tb[2, 1]) / sqrt(prod(rowSums(tb)) * prod(colSums(tb)))
  } else {
    out$phi <- sqrt(pchi / n)
  }
  out$contgy <- sqrt(pchi / (pchi + n))
  out$cramv <- if (R == 2 && C == 2) out$phi else sqrt(pchi / n / min(R - 1, C - 1))
  out
}

freq_mhchi <- function(tb, rs, cs) {
  n <- sum(tb)
  i <- rep(rs, times = length(cs))
  j <- rep(cs, each = length(rs))
  w <- as.numeric(tb)
  mx <- sum(w * i) / n
  my <- sum(w * j) / n
  r <- sum(w * (i - mx) * (j - my)) / sqrt(sum(w * (i - mx)^2) * sum(w * (j - my)^2))
  q <- (n - 1) * r^2
  list(mhchi = q, p_mhchi = stats::pchisq(q, 1, lower.tail = FALSE))
}

freq_fisher <- function(tb) {
  tb <- round(tb)
  if (nrow(tb) == 2 && ncol(tb) == 2) {
    m <- sum(tb[1, ])
    nn <- sum(tb[2, ])
    k <- sum(tb[, 1])
    x <- tb[1, 1]
    lo <- max(0, k - nn)
    hi <- min(k, m)
    support <- lo:hi
    d <- stats::dhyper(support, m, nn, k)
    px <- stats::dhyper(x, m, nn, k)
    list(fisher_f = x, xpl = sum(d[support <= x]), xpr = sum(d[support >= x]), xpt = px,
         xp2 = min(1, sum(d[d <= px * (1 + 1e-7)])))
  } else {
    # probability of the observed table (multivariate hypergeometric)
    pt <- exp(sum(lfactorial(rowSums(tb))) + sum(lfactorial(colSums(tb))) - lfactorial(sum(tb)) - sum(lfactorial(tb)))
    list(xpt = pt, xp2 = stats::fisher.test(tb, workspace = 2e7)$p.value)
  }
}

# Exact conditional p-values of the three 2x2 chi-square tests (manual: EXACT
# statement, CHISQ option; "Exact Statistics"): all tables with the observed
# margins are enumerated, the statistic of each table is compared with the
# observed one, and the hypergeometric probabilities of the tables in the tail
# are summed.
freq_exact_chisq_2x2 <- function(tb) {
  n <- sum(tb)
  row1 <- tb[1, 1] + tb[1, 2]
  col1 <- tb[1, 1] + tb[2, 1]
  lo <- max(0, col1 - (n - row1))
  hi <- min(row1, col1)
  a <- lo:hi
  p <- exp(lchoose(row1, a) + lchoose(n - row1, col1 - a) - lchoose(n, col1))
  p <- p / sum(p)
  stat <- function(x) {
    t <- matrix(c(x, col1 - x, row1 - x, n - row1 - col1 + x), 2, 2)
    e <- outer(rowSums(t), colSums(t)) / n
    pchi <- sum((t - e)^2 / e)
    lr <- 2 * sum(ifelse(t > 0, t * log(t / e), 0))
    c(pchi = pchi, lr = lr, mh = (n - 1) / n * pchi)
  }
  obs <- stat(tb[1, 1])
  s <- vapply(a, function(x) stat(x), numeric(3))
  tail <- function(k) sum(p[s[k, ] >= obs[k] - 1e-12])
  c(pearson = tail("pchi"), lr = tail("lr"), mh = tail("mh"))
}

# Binomial proportion, its confidence limits (Wald, Wilson, Clopper-Pearson,
# Agresti-Coull, Jeffreys, and the continuity-corrected Wald/Wilson forms) and
# the asymptotic test of H0: p = p0.  `cnt` is the count per level (in table
# order, missing levels excluded), `x_index` selects the level the proportion
# refers to.
freq_binomial <- function(cnt, alpha = 0.05, p0 = 0.5, x_index = 1L, cl = NULL, correct = FALSE) {
  n <- sum(cnt)
  x <- cnt[x_index]
  p <- x / n
  z <- stats::qnorm(1 - alpha / 2)
  ase <- sqrt(p * (1 - p) / n)
  z2 <- z * z
  lim <- function(v) c(max(0, v[1]), min(1, v[2]))
  wald <- lim(c(p - z * ase, p + z * ase))
  cp <- c(if (x <= 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1),
          if (x >= n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x))
  den <- 1 + z2 / n
  centre <- (p + z2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z2 / (4 * n * n)) / den
  wilson <- lim(c(centre - half, centre + half))
  nt <- n + z2
  pt <- (x + z2 / 2) / nt
  hac <- z * sqrt(pt * (1 - pt) / nt)
  agresti <- lim(c(pt - hac, pt + hac))
  jeffreys <- lim(c(if (x <= 0) 0 else stats::qbeta(alpha / 2, x + 0.5, n - x + 0.5),
                    if (x >= n) 1 else stats::qbeta(1 - alpha / 2, x + 0.5, n - x + 0.5)))
  # continuity-corrected forms (a 1/(2n) shift for Wald, the score interval
  # with the ±1 terms for Wilson)
  wald_c <- lim(c(p - (z * ase + 0.5 / n), p + (z * ase + 0.5 / n)))
  q <- 1 - p
  wl <- (2 * n * p + z2 - 1 - z * sqrt(max(0, z2 - 2 - 1 / n + 4 * p * (n * q + 1)))) / (2 * (n + z2))
  wu <- (2 * n * p + z2 + 1 + z * sqrt(max(0, z2 + 2 - 1 / n + 4 * p * (n * q - 1)))) / (2 * (n + z2))
  wilson_c <- lim(c(wl, wu))
  ase0 <- sqrt(p0 * (1 - p0) / n)
  zt <- (p - p0) / ase0
  ztc <- (abs(p - p0) - 0.5 / n) / ase0 * if (p >= p0) 1 else -1
  ptail <- function(zv) if (zv >= 0) stats::pnorm(zv, lower.tail = FALSE) else stats::pnorm(zv)
  pone <- ptail(zt)
  ponec <- ptail(ztc)
  # exact test: one-sided toward the observed side, two-sided = 2 * one-sided
  xone <- if (p >= p0) stats::pbinom(x - 1, n, p0, lower.tail = FALSE) else stats::pbinom(x, n, p0)
  list(n = n, x = x, bin = p, e_bin = ase,
       cl = list(wald = wald, wilson = wilson, exact = cp, agresticoull = agresti,
                 jeffreys = jeffreys, wald_corrected = wald_c, wilson_corrected = wilson_c),
       correct = correct, alpha = alpha, p0 = p0,
       e0_bin = ase0, z_bin = zt, pl_bin = pone, p2_bin = min(1, 2 * pone),
       zc_bin = ztc, plc_bin = ponec, p2c_bin = min(1, 2 * ponec),
       xpl_bin = xone, xp2_bin = min(1, 2 * xone))
}

# Rank scores of the levels of one variable within one stratum (manual:
# "Scores"): R_i = sum_{k<i} n_k + (n_i + 1) / 2, which gives midranks for
# tied values.
freq_rank_scores <- function(marg) {
  cm <- cumsum(marg)
  prev <- c(0, cm[-length(cm)])
  prev + (marg + 1) / 2
}

# Cochran-Mantel-Haenszel statistics over the strata of an n-way table (array
# with the row and column variables last).  The manual's "Cochran-Mantel-
# Haenszel Statistics" section defines all three as the generalized CMH
# statistic G' V_G^-1 G with the same expected values and covariance matrices
# and different score matrices:
#   correlation           row and column scores from SCORES=;
#   ANOVA (row mean scores) column scores and the R-1 row contrasts;
#   general association   R-1 row and C-1 column contrasts.
# RANK scores are computed within each stratum from that stratum's marginals.
freq_cmh <- function(tab, lv, scores = "table") {
  dims <- dim(tab)
  k <- length(dims)
  R <- dims[k - 1L]
  C <- dims[k]
  q <- if (k == 2L) 1L else prod(dims[seq_len(k - 2L)])
  m <- matrix(as.numeric(tab), ncol = R * C)
  IR <- cbind(diag(R - 1), -1)
  IC <- cbind(diag(C - 1), -1)
  tbl_r <- freq_scores(lv[[k - 1L]])
  tbl_c <- freq_scores(lv[[k]])
  Gc <- 0
  Vc <- matrix(0, 1, 1)
  Ga <- numeric(R - 1)
  Va <- matrix(0, R - 1, R - 1)
  Gg <- numeric((R - 1) * (C - 1))
  Vg <- matrix(0, (R - 1) * (C - 1), (R - 1) * (C - 1))
  for (h in seq_len(nrow(m))) {
    th <- matrix(m[h, ], nrow = R)
    nh <- sum(th)
    if (nh < 2) next
    pr <- rowSums(th) / nh
    pc <- colSums(th) / nh
    mm <- nh * outer(pr, pc)
    vr <- diag(pr, R) - outer(pr, pr)
    vc <- diag(pc, C) - outer(pc, pc)
    V <- (nh^2 / (nh - 1)) * kronecker(vc, vr)
    if (identical(scores, "rank")) {
      rs <- freq_rank_scores(rowSums(th))
      cs <- freq_rank_scores(colSums(th))
    } else {
      rs <- tbl_r
      cs <- tbl_c
    }
    Bc <- kronecker(matrix(cs, 1), matrix(rs, 1))
    Ba <- kronecker(matrix(cs, 1), IR)
    Bg <- kronecker(IC, IR)
    d <- as.numeric(th - mm)
    Gc <- Gc + Bc %*% d
    Vc <- Vc + Bc %*% V %*% t(Bc)
    Ga <- Ga + Ba %*% d
    Va <- Va + Ba %*% V %*% t(Ba)
    Gg <- Gg + Bg %*% d
    Vg <- Vg + Bg %*% V %*% t(Bg)
  }
  qv <- function(g, v) {
    if (rcond(v) < .Machine$double.eps) {
      abort_unsupported("CMH statistic with a singular covariance matrix (PROC FREQ sets the statistic to missing; not supported)")
    }
    as.numeric(t(g) %*% solve(v, g))
  }
  out <- list(cmhcor = qv(Gc, Vc), df_cmhcor = 1L)
  out$p_cmhcor <- stats::pchisq(out$cmhcor, 1, lower.tail = FALSE)
  out$cmhrms <- qv(Ga, Va)
  out$df_cmhrms <- R - 1L
  out$p_cmhrms <- stats::pchisq(out$cmhrms, R - 1L, lower.tail = FALSE)
  out$cmhga <- qv(Gg, Vg)
  out$df_cmhga <- (R - 1L) * (C - 1L)
  out$p_cmhga <- stats::pchisq(out$cmhga, (R - 1L) * (C - 1L), lower.tail = FALSE)
  out
}

# Common odds ratio / relative risks and the Breslow-Day test for stratified
# 2x2 tables (manual: "Adjusted Odds Ratio and Relative Risk Estimates" and
# "Breslow-Day Test for Homogeneity of the Odds Ratios").  Tables that contain
# a row or column with a total frequency of 0 are not part of the analysis
# (the Breslow-Day section says so explicitly; the common estimates use the
# same nonempty strata).  A zero cell in a nonempty stratum would need the
# documented 0.5 correction and its warning; that case is not supported, so it
# fails loud.
freq_cmh_common <- function(tab, alpha) {
  dims <- dim(tab)
  k <- length(dims)
  if (dims[k - 1L] != 2L || dims[k] != 2L) return(NULL)
  m <- matrix(as.numeric(tab), ncol = 4L)
  z <- stats::qnorm(1 - alpha / 2)
  # column-major order of one 2x2 stratum: n11, n21, n12, n22
  a <- m[, 1]
  cc <- m[, 2]
  b <- m[, 3]
  dd <- m[, 4]
  n <- a + b + cc + dd
  m1 <- a + b
  m2 <- cc + dd
  nc1 <- a + cc
  nc2 <- b + dd
  valid <- n > 0 & m1 > 0 & m2 > 0 & nc1 > 0 & nc2 > 0
  if (!any(valid)) abort_unsupported("CMH common odds ratio / relative risk: no nonempty stratum")
  if (any(a[valid] == 0 | b[valid] == 0 | cc[valid] == 0 | dd[valid] == 0)) {
    abort_unsupported("CMH common odds ratio / relative risk with a zero cell in a nonempty stratum (the 0.5 correction and its warning are not supported)")
  }
  v <- which(valid)
  ad <- a * dd / n
  bc <- b * cc / n
  or_mh <- sum(ad[v]) / sum(bc[v])
  if (!is.finite(or_mh) || or_mh <= 0) {
    abort_unsupported("CMH common odds ratio is undefined (zero denominator)")
  }
  # Robins, Breslow and Greenland (1986) variance of log(OR_MH)
  s2 <- sum((a + dd)[v] * (a * dd)[v] / n[v]^2) / (2 * sum(ad[v])^2) +
    sum(((a + dd) * (b * cc) + (b + cc) * (a * dd))[v] / n[v]^2) / (2 * sum(ad[v]) * sum(bc[v])) +
    sum((b + cc)[v] * (b * cc)[v] / n[v]^2) / (2 * sum(bc[v])^2)
  or_mh_ci <- or_mh * exp(c(-z, z) * sqrt(s2))
  # Mantel-Haenszel relative risks with the Greenland-Robins variance
  rr_mh <- function(x1, x2, ncol) {
    num <- sum((x1 * m2 / n)[v])
    den <- sum((x2 * m1 / n)[v])
    rr <- num / den
    vr <- sum(((m1 * m2 * ncol - x1 * x2 * n) / n^2)[v]) / (num * den)
    c(rr, rr * exp(c(-z, z) * sqrt(vr)))
  }
  rr1 <- rr_mh(a, cc, nc1)
  rr2 <- rr_mh(b, dd, nc2)
  # logit (Woolf) estimates over the valid strata
  orh <- a * dd / (b * cc)
  vh <- 1 / a + 1 / b + 1 / cc + 1 / dd
  w <- 1 / vh
  or_l <- exp(sum(w[v] * log(orh[v])) / sum(w[v]))
  or_l_ci <- or_l * exp(c(-z, z) / sqrt(sum(w[v])))
  logit_rr <- function(x1, x2) {
    rrh <- (x1 / m1) / (x2 / m2)
    vh <- 1 / x1 - 1 / m1 + 1 / x2 - 1 / m2
    w <- 1 / vh
    est <- exp(sum(w[v] * log(rrh[v])) / sum(w[v]))
    c(est, est * exp(c(-z, z) / sqrt(sum(w[v]))))
  }
  rr1l <- logit_rr(a, cc)
  rr2l <- logit_rr(b, dd)
  # Breslow-Day: the expected cell (1,1) frequency solves the odds ratio
  # equation psi = E_b E_c / (E_a E_d) with the stratum margins fixed; the
  # variance is 1 / (1/E_a + 1/E_b + 1/E_c + 1/E_d)
  E <- rep(NA_real_, length(n))
  for (h in v) {
    if (abs(or_mh - 1) < 1e-10) {
      E[h] <- m1[h] * nc1[h] / n[h]
      next
    }
    A <- or_mh - 1
    B <- -(or_mh * (m1[h] + nc1[h]) + m2[h] - nc1[h])
    Cq <- or_mh * m1[h] * nc1[h]
    disc <- B^2 - 4 * A * Cq
    if (!is.finite(disc) || disc < 0) next
    lo <- max(0, nc1[h] - m2[h])
    hi <- min(m1[h], nc1[h])
    roots <- c((-B + sqrt(disc)) / (2 * A), (-B - sqrt(disc)) / (2 * A))
    roots <- roots[roots >= lo - 1e-9 & roots <= hi + 1e-9]
    if (length(roots) > 0) E[h] <- roots[1]
  }
  kept <- !is.na(E)
  nev <- sum(kept)
  bd <- NULL
  if (nev >= 2L) {
    Eb <- m1 - E
    Ec <- nc1 - E
    Ed <- n - m1 - nc1 + E
    vE <- 1 / (1 / E + 1 / Eb + 1 / Ec + 1 / Ed)
    chisq <- sum((a[kept] - E[kept])^2 / vE[kept])
    df <- nev - 1L
    bd <- list(chisq = chisq, df = df, p = stats::pchisq(chisq, df, lower.tail = FALSE))
  }
  list(ormh = c(or_mh, or_mh_ci), orl = c(or_l, or_l_ci), rr1 = rr1, rr1l = rr1l,
       rr2 = rr2, rr2l = rr2l, bd = bd)
}

# RISKDIFF / RELRISK ----------------------------------------------------------------
#
# The risk estimates, risk differences and the odds ratio / relative risk table
# follow the manual sections "Risks and Risk Differences" and "Odds Ratio and
# Relative Risks" (docset procstat, FREQ chapter).
# Exact unconditional confidence limits (the "Exact (Score)" rows) invert two
# one-sided exact tests based on the per-table score statistic and eliminate the
# nuisance parameter by the maximum p-value over it, exactly as the manual's
# subsection "Exact Unconditional Confidence Limits" describes.

freq_clip01 <- function(v) c(max(0, v[1]), min(1, v[2]))

# exact (Clopper-Pearson) confidence limits of a binomial proportion
freq_cp_ci <- function(x, n, alpha) {
  c(if (x <= 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1),
    if (x >= n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x))
}

# Wilson (score) confidence limits of a binomial proportion
freq_wilson_ci <- function(x, n, alpha) {
  p <- x / n
  z <- stats::qnorm(1 - alpha / 2)
  z2 <- z * z
  den <- 1 + z2 / n
  centre <- (p + z2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z2 / (4 * n * n)) / den
  freq_clip01(c(centre - half, centre + half))
}

# constrained maximum likelihood estimates under p1 - p2 = delta (the cubic
# solution given in the manual; vectorised over the reference-set tables)
freq_mle_rd_vec <- function(delta, n1, n2, p1h, p2h) {
  theta <- n2 / n1
  a <- 1 + theta
  b <- -(1 + theta + p1h + theta * p2h + delta * (theta + 2))
  cc <- delta^2 + delta * (2 * p1h + theta + 1) + p1h + theta * p2h
  dd <- -p1h * delta * (1 + delta)
  v <- b^3 / (3 * a)^3 - b * cc / (6 * a^2) + dd / (2 * a)
  inside <- b^2 / (3 * a)^2 - cc / (3 * a)
  bad <- !is.finite(inside) | inside < 0
  inside[bad] <- 0
  u <- sign(v) * sqrt(inside)
  arg <- ifelse(u == 0, 0, v / u^3)
  arg[!is.finite(arg)] <- 0
  arg <- pmin(pmax(arg, -1), 1)
  w <- (pi + acos(arg)) / 3
  pt1 <- 2 * u * cos(w) - b / (3 * a)
  out <- cbind(pt1, pt1 - delta)
  out[bad, ] <- NA_real_
  out
}

# constrained maximum likelihood estimates under p1 / p2 = r
freq_mle_rr_vec <- function(r, n1, n2, p1h, p2h) {
  theta <- n2 / n1
  a <- 1 + theta
  b <- -(r * (1 + theta * p2h) + theta + p1h)
  cc <- r * (p1h + theta * p2h)
  disc <- b^2 - 4 * a * cc
  ok <- is.finite(disc) & disc >= 0
  disc[!ok] <- 0
  pt1 <- (-b - sqrt(disc)) / (2 * a)
  out <- cbind(pt1, pt1 / r)
  out[!ok, ] <- NA_real_
  out
}

# number of grid points for the nuisance-parameter maximum: the p-values are
# smooth in p2, so a moderately dense grid plus a local refinement of the
# largest maxima is accurate and much faster than an exhaustive dense grid
freq_exact_grid <- function(n1, n2) {
  if (as.numeric(n1 + 1L) * (n2 + 1L) > 5e5) {
    abort_unsupported("EXACT RELRISK / RISKDIFF with a reference set this large (only small 2x2 tables are supported)")
  }
  if (as.numeric(n1 + 1L) * (n2 + 1L) <= 5000) 4001L else 2001L
}

# binomial probability matrices on the nuisance grid: A[a, k] = P(a | n1, p1_k)
freq_prob_grid <- function(x, n, p, K) {
  p <- pmin(pmax(p, 1e-300), 1 - 1e-16)
  exp(outer(x, log(p)) + outer(n - x, log1p(-p)) + rep(lchoose(n, x), K))
}

# maximum over the nuisance parameter of a sum over the selected tables
freq_sup_nuisance <- function(sel, A, B, g, ff) {
  v <- colSums(A * (sel %*% B))
  best <- max(v)
  if (length(g) < 20001L) {
    # a coarser grid: refine the largest local maxima
    K <- length(g)
    cand <- which(v >= c(-Inf, v[-1L]) & v >= c(v[-1L], -Inf))
    cand <- cand[order(v[cand], decreasing = TRUE)][seq_len(min(5L, length(cand)))]
    for (kk in cand) {
      if (kk > 1L && kk < K) {
        o <- stats::optimize(function(p2) -ff(p2), interval = c(g[kk - 1L], g[kk + 1L]), maximum = FALSE)
        best <- max(best, -o$objective)
      }
    }
  }
  best
}

# One-sided p-values of the exact unconditional score test of
# H0: risk difference = delta.  The reference set holds all 2x2 tables with the
# observed row totals; PU/PL are the maximum over p2 of the probability of the
# tables with T(a) >= t0 / T(a) <= t0.
freq_rd_exact_p <- function(delta, x1, x2, n1, n2) {
  K <- freq_exact_grid(n1, n2)
  aa <- seq.int(0L, n1)
  bb <- seq.int(0L, n2)
  ta <- rep(aa, times = n2 + 1L)
  tb <- rep(bb, each = n1 + 1L)
  p1h <- ta / n1
  p2h <- tb / n2
  m <- freq_mle_rd_vec(delta, n1, n2, p1h, p2h)
  if (!all(is.finite(m))) return(c(PU = NA_real_, PL = NA_real_))
  V <- (n1 + n2) / (n1 + n2 - 1) * (m[, 1] * (1 - m[, 1]) / n1 + m[, 2] * (1 - m[, 2]) / n2)
  if (any(!is.finite(V)) || any(V <= 0)) return(c(PU = NA_real_, PL = NA_real_))
  T <- (p1h - p2h - delta) / sqrt(V)
  t0 <- T[x1 + 1L + x2 * (n1 + 1L)]
  PU_sel <- matrix(T >= t0, n1 + 1L, n2 + 1L)
  PL_sel <- matrix(T <= t0, n1 + 1L, n2 + 1L)
  lo <- max(0, -delta)
  hi <- min(1, 1 - delta)
  if (hi <= lo) return(c(PU = NA_real_, PL = NA_real_))
  g <- seq(lo, hi, length.out = K)
  p1 <- delta + g
  A <- freq_prob_grid(aa, n1, p1, K)
  B <- freq_prob_grid(bb, n2, g, K)
  ff <- function(p2) sum(PU_sel * outer(stats::dbinom(aa, n1, delta + p2), stats::dbinom(bb, n2, p2)))
  ffl <- function(p2) sum(PL_sel * outer(stats::dbinom(aa, n1, delta + p2), stats::dbinom(bb, n2, p2)))
  c(PU = freq_sup_nuisance(PU_sel, A, B, g, ff), PL = freq_sup_nuisance(PL_sel, A, B, g, ffl))
}

# One-sided p-values of the exact unconditional score test of H0: RR = r.
freq_rr_exact_p <- function(r, x1, x2, n1, n2) {
  K <- freq_exact_grid(n1, n2)
  aa <- seq.int(0L, n1)
  bb <- seq.int(0L, n2)
  ta <- rep(aa, times = n2 + 1L)
  tb <- rep(bb, each = n1 + 1L)
  p1h <- ta / n1
  p2h <- tb / n2
  m <- freq_mle_rr_vec(r, n1, n2, p1h, p2h)
  if (!all(is.finite(m))) return(c(PU = NA_real_, PL = NA_real_))
  V <- m[, 1] * (1 - m[, 1]) / n1 + r^2 * m[, 2] * (1 - m[, 2]) / n2
  T <- (p1h - r * p2h) / sqrt(V)
  # the all-zero table has both maximum likelihood estimates at the boundary:
  # its statistic is the limit 0 (the table is consistent with any relative risk)
  T[1] <- 0
  if (any(!is.finite(T))) return(c(PU = NA_real_, PL = NA_real_))
  t0 <- T[x1 + 1L + x2 * (n1 + 1L)]
  PU_sel <- matrix(T >= t0, n1 + 1L, n2 + 1L)
  PL_sel <- matrix(T <= t0, n1 + 1L, n2 + 1L)
  lo <- 0
  hi <- min(1, 1 / r)
  if (hi <= lo) return(c(PU = NA_real_, PL = NA_real_))
  g <- seq(lo, hi, length.out = K)
  p1 <- r * g
  A <- freq_prob_grid(aa, n1, p1, K)
  B <- freq_prob_grid(bb, n2, g, K)
  ff <- function(p2) sum(PU_sel * outer(stats::dbinom(aa, n1, r * p2), stats::dbinom(bb, n2, p2)))
  ffl <- function(p2) sum(PL_sel * outer(stats::dbinom(aa, n1, r * p2), stats::dbinom(bb, n2, p2)))
  c(PU = freq_sup_nuisance(PU_sel, A, B, g, ff), PL = freq_sup_nuisance(PL_sel, A, B, g, ffl))
}

# bracket [a, b] around a root of f, walking from `start` (f > 0 there) toward
# `dir` until f turns <= 0; NA counts as "not positive"
freq_bracket <- function(f, start, dir, step = 0.02, n = 500L) {
  fs <- f(start)
  if (!is.finite(fs) || fs <= 0) abort_unsupported("exact confidence limits: the p-value function did not start positive")
  b <- start
  for (i in seq_len(n)) {
    a <- b + dir * step
    fa <- f(a)
    if (is.na(fa) || fa <= 0) return(c(a, b))
    b <- a
  }
  abort_unsupported("exact confidence limits: the p-value function did not cross the confidence level")
}

# bisection for a root between a (f <= 0) and b (f > 0)
freq_bisect <- function(f, a, b, tol = 1e-7) {
  fa <- f(a)
  if (is.na(fa)) abort_unsupported("exact confidence limits: p-value evaluation failed")
  for (i in seq_len(200L)) {
    m <- (a + b) / 2
    fm <- f(m)
    if (is.na(fm)) abort_unsupported("exact confidence limits: p-value evaluation failed")
    if ((fm > 0) == (fa > 0)) {
      a <- m
      fa <- fm
    } else {
      b <- m
    }
    if (abs(b - a) < tol) break
  }
  (a + b) / 2
}

# exact unconditional confidence limits for the risk difference (tail method,
# per-table score statistic; the manual's "Exact Unconditional Confidence Limits")
freq_rd_exact_ci <- function(x1, x2, n1, n2, alpha) {
  a2 <- alpha / 2
  dhat <- x1 / n1 - x2 / n2
  fU <- function(d) {
    v <- freq_rd_exact_p(d, x1, x2, n1, n2)["PU"]
    if (is.na(v)) -Inf else v - a2
  }
  fL <- function(d) {
    v <- freq_rd_exact_p(d, x1, x2, n1, n2)["PL"]
    if (is.na(v)) -Inf else v - a2
  }
  bl <- freq_bracket(fU, dhat, -1, step = 0.05)
  bu <- freq_bracket(fL, dhat, 1, step = 0.05)
  c(lower = freq_bisect(fU, bl[1], bl[2], tol = 1e-6), upper = freq_bisect(fL, bu[1], bu[2], tol = 1e-6))
}

# exact unconditional confidence limits for the relative risk
freq_rr_exact_ci <- function(x1, x2, n1, n2, alpha) {
  a2 <- alpha / 2
  rhat <- (x1 / n1) / (x2 / n2)
  fU <- function(lr) {
    v <- freq_rr_exact_p(exp(lr), x1, x2, n1, n2)["PU"]
    if (is.na(v)) -Inf else v - a2
  }
  fL <- function(lr) {
    v <- freq_rr_exact_p(exp(lr), x1, x2, n1, n2)["PL"]
    if (is.na(v)) -Inf else v - a2
  }
  bl <- freq_bracket(fU, log(rhat), -1, step = 0.05)
  bu <- freq_bracket(fL, log(rhat), 1, step = 0.05)
  c(lower = exp(freq_bisect(fU, bl[1], bl[2], tol = 1e-6)), upper = exp(freq_bisect(fL, bu[1], bu[2], tol = 1e-6)))
}

# Miettinen-Nurminen (score) confidence limits for the relative risk; the
# manual's score limits include the n/(n-1) bias correction by default
freq_rr_score_ci <- function(x1, x2, n1, n2, alpha) {
  n <- n1 + n2
  p1h <- x1 / n1
  p2h <- x2 / n2
  rhat <- p1h / p2h
  crit <- stats::qchisq(1 - alpha, 1)
  stat <- function(r) {
    m <- freq_mle_rr_vec(r, n1, n2, p1h, p2h)
    if (!all(is.finite(m))) return(Inf)
    V <- n / (n - 1) * (m[1, 1] * (1 - m[1, 1]) / n1 + r^2 * m[1, 2] * (1 - m[1, 2]) / n2)
    (p1h - r * p2h)^2 / V
  }
  f <- function(lr) {
    v <- stat(exp(lr))
    if (is.na(v)) Inf else v - crit
  }
  lrhat <- log(rhat)
  lo <- lrhat
  while (f(lo) <= 0 && lo > lrhat - 60) lo <- lo - 0.1
  hi <- lrhat
  while (f(hi) <= 0 && hi < lrhat + 60) hi <- hi + 0.1
  if (f(lo) <= 0 || f(hi) <= 0) abort_unsupported("relative risk score confidence limits did not converge")
  c(lower = exp(freq_bisect(f, lo, lrhat)), upper = exp(freq_bisect(f, hi, lrhat)))
}

# Wald confidence limits (log scale) for the odds ratio and the relative risk
freq_or_wald_ci <- function(a, b, cc, dd, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  or <- a * dd / (b * cc)
  v <- 1 / a + 1 / b + 1 / cc + 1 / dd
  c(or * exp(-z * sqrt(v)), or * exp(z * sqrt(v)))
}

freq_rr_wald_ci <- function(x1, x2, n1, n2, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  rr <- (x1 / n1) / (x2 / n2)
  v <- 1 / x1 + 1 / x2 - 1 / n1 - 1 / n2
  c(rr * exp(-z * sqrt(v)), rr * exp(z * sqrt(v)))
}

# Agresti-Caffo confidence limits for the risk difference
freq_rd_ac_ci <- function(x1, x2, n1, n2, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  p1 <- (x1 + 1) / (n1 + 2)
  p2 <- (x2 + 1) / (n2 + 2)
  d <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / (n1 + 2) + p2 * (1 - p2) / (n2 + 2))
  c(d - z * se, d + z * se)
}

# Hauck-Anderson confidence limits for the risk difference
freq_rd_ha_ci <- function(x1, x2, n1, n2, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  p1 <- x1 / n1
  p2 <- x2 / n2
  d <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / (n1 - 1) + p2 * (1 - p2) / (n2 - 1))
  cc <- 1 / (2 * min(n1, n2))
  c(d - (cc + z * se), d + (cc + z * se))
}

# Newcombe hybrid-score confidence limits for the risk difference
freq_rd_newcombe_ci <- function(x1, x2, n1, n2, alpha) {
  p1 <- x1 / n1
  p2 <- x2 / n2
  d <- p1 - p2
  w1 <- freq_wilson_ci(x1, n1, alpha)
  w2 <- freq_wilson_ci(x2, n2, alpha)
  c(d - sqrt((p1 - w1[1])^2 + (w2[2] - p2)^2),
    d + sqrt((w1[2] - p1)^2 + (p2 - w2[1])^2))
}

# Everything RISKDIFF / RELRISK need for one 2x2 table request.
freq_rr_rd_stats <- function(tab, lv, vars, info, o, sp) {
  k <- length(vars)
  if (k != 2) abort_unsupported("RELRISK / RISKDIFF for a table that is not 2x2 (only the 2x2 layout is supported)")
  tb <- matrix(as.numeric(tab), nrow = dim(tab)[1])
  if (nrow(tb) != 2L || ncol(tb) != 2L) {
    abort_unsupported("RELRISK / RISKDIFF for a table that is not 2x2 (only the 2x2 layout is supported)")
  }
  if (any(tb < 0)) abort_unsupported("RELRISK / RISKDIFF with negative weights")
  if (any(abs(tb - round(tb)) > 1e-9)) {
    abort_unsupported("RELRISK / RISKDIFF with non-integer weights (only integer weights are supported)")
  }
  n <- sum(tb)
  n1 <- sum(tb[1, ])
  n2 <- sum(tb[2, ])
  if (n1 <= 0 || n2 <= 0) abort_unsupported("RELRISK / RISKDIFF for a table with an empty row")
  alpha <- o$alpha %||% 0.05
  z <- stats::qnorm(1 - alpha / 2)
  exact_rd <- "riskdiff" %in% sp$exact
  exact_rel <- "relrisk" %in% sp$exact
  stat <- list()
  rate <- function(x, m) {
    p <- x / m
    ase <- sqrt(p * (1 - p) / m)
    list(risk = p, ase = ase, wald = freq_clip01(c(p - z * ase, p + z * ase)),
         cp = freq_cp_ci(x, m, alpha))
  }
  if (isTRUE(o$riskdiff)) {
    rd <- o$riskdiff_opts %||% list()
    column <- rd$column %||% 1L
    # only COLUMN=1 (the default) is supported; a COLUMN=2 table uses the
    # column 2 estimates and is not implemented yet, so fail loud
    if (!identical(as.integer(column), 1L)) abort_unsupported(paste0("RISKDIFF COLUMN=", column))
    if (isTRUE(rd$correct)) abort_unsupported("RISKDIFF option CORRECT")
    if (exact_rd) {
      if (any(abs(c(tb, n1, n2) - round(c(tb, n1, n2))) > 1e-9)) {
        abort_unsupported("EXACT RISKDIFF with non-integer weights (the exact test is enumerated over integer tables)")
      }
      if (any(tb == 0)) abort_unsupported("EXACT RISKDIFF with a zero cell (not supported)")
    }
    cols <- vector("list", 2L)
    for (j in 1:2) {
      x1 <- tb[1, j]
      x2 <- tb[2, j]
      r1 <- rate(x1, n1)
      r2 <- rate(x2, n2)
      rt <- rate(x1 + x2, n)
      d <- r1$risk - r2$risk
      ase_d <- sqrt(r1$risk * (1 - r1$risk) / n1 + r2$risk * (1 - r2$risk) / n2)
      cols[[j]] <- list(r1 = r1, r2 = r2, rt = rt, diff = d, ase_d = ase_d,
                        wald = c(d - z * ase_d, d + z * ase_d))
    }
    if (exact_rd) {
      ex <- freq_rd_exact_ci(tb[1, 1], tb[2, 1], n1, n2, alpha)
      cols[[1]]$exact <- ex
      # column 2 risks are the complements of the column 1 risks
      cols[[2]]$exact <- c(-ex[2], -ex[1])
    }
    cls <- rd$cl
    if (!is.null(cls)) {
      if (any(tb == 0)) {
        abort_unsupported("RISKDIFF CL= with a zero cell (the confidence-limit table is not supported for this table)")
      }
      if ("exact" %in% cls && !exact_rd) {
        abort_unsupported("RISKDIFF CL=EXACT without the EXACT RISKDIFF statement")
      }
      vals <- list()
      for (t in cls) {
        vals[[t]] <- switch(t,
          wald = cols[[1]]$wald,
          ac = freq_rd_ac_ci(tb[1, 1], tb[2, 1], n1, n2, alpha),
          ha = freq_rd_ha_ci(tb[1, 1], tb[2, 1], n1, n2, alpha),
          newcombe = freq_rd_newcombe_ci(tb[1, 1], tb[2, 1], n1, n2, alpha),
          exact = cols[[1]]$exact)
      }
      stat$rd_cl <- list(types = cls, vals = vals)
    }
    stat$rd <- list(alpha = alpha, column = column, cols = cols, exact = exact_rd,
                    col_disp = info[[vars[2]]]$disp, col_text = lv[[2]]$text,
                    cl = !is.null(cls))
  }
  if (isTRUE(o$relrisk)) {
    rel <- o$relrisk_opts %||% list()
    column <- rel$column %||% 1L
    # only COLUMN=1 (the default) is supported; a COLUMN=2 table uses the
    # column 2 relative risk and is not implemented yet, so fail loud
    if (!identical(as.integer(column), 1L)) abort_unsupported(paste0("RELRISK COLUMN=", column))
    a <- tb[1, 1]
    b <- tb[1, 2]
    cc <- tb[2, 1]
    dd <- tb[2, 2]
    or <- if (a > 0 && b > 0 && cc > 0 && dd > 0) a * dd / (b * cc) else NA_real_
    or_ci <- if (is.na(or)) c(NA_real_, NA_real_) else freq_or_wald_ci(a, b, cc, dd, alpha)
    rr1 <- if (a > 0 && cc > 0) (a / n1) / (cc / n2) else NA_real_
    rr2 <- if (b > 0 && dd > 0) (b / n1) / (dd / n2) else NA_real_
    rr1_ci <- if (is.na(rr1)) c(NA_real_, NA_real_) else freq_rr_wald_ci(a, cc, n1, n2, alpha)
    rr2_ci <- if (is.na(rr2)) c(NA_real_, NA_real_) else freq_rr_wald_ci(b, dd, n1, n2, alpha)
    st <- list(alpha = alpha, column = column, or = or, or_ci = or_ci, rr1 = rr1, rr1_ci = rr1_ci,
               rr2 = rr2, rr2_ci = rr2_ci, cl = rel$cl, exact = exact_rel,
               col_disp = info[[vars[2]]]$disp, col_text = lv[[2]]$text)
    if (!is.null(rel$cl)) {
      if ("exact" %in% rel$cl && !exact_rel) {
        abort_unsupported("RELRISK CL=EXACT without the EXACT RELRISK statement")
      }
      if (is.na(rr1)) {
        abort_unsupported("RELRISK CL= with a zero cell (confidence limits not supported for this table)")
      }
      vals <- list()
      for (t in rel$cl) {
        vals[[t]] <- switch(t,
          wald = rr1_ci,
          score = freq_rr_score_ci(a, cc, n1, n2, alpha),
          exact = freq_rr_exact_ci(a, cc, n1, n2, alpha))
      }
      st$rr <- rr1
      st$cl_vals <- vals
    }
    stat$rel <- st
  }
  stat
}

freq_request_stats <- function(tab, lv, vars, info, o, sp, nmiss) {
  k <- length(vars)
  want <- c(if (isTRUE(o$chisq) || "chisq" %in% sp$exact) "chisq",
            if (isTRUE(o$fisher) || "fisher" %in% sp$exact) "fisher",
            if (isTRUE(o$binomial) || "binomial" %in% sp$exact) "binomial",
            if (any(c(o$cmh, o$cmh1, o$cmh2) %in% TRUE)) "cmh",
            if (isTRUE(o$riskdiff)) "riskdiff", if (isTRUE(o$relrisk)) "relrisk")
  okw <- sp$output$kws
  if (length(okw) > 0) {
    if (any(okw %in% c("pchi", "lrchi", "ajchi", "mhchi", "chisq", "phi", "contgy", "cramv"))) want <- c(want, "chisq")
    if (any(okw %in% c("cmh", "cmh1", "cmh2", "cmhcor", "cmhrms", "cmhga"))) want <- c(want, "cmh")
    if ("binomial" %in% okw) want <- c(want, "binomial")
  }
  want <- unique(want)
  st <- list(n = sum(tab), nmiss = nmiss)
  if (length(want) == 0) return(st)
  if (any(as.numeric(tab) < 0)) abort_unsupported("statistics with negative weights")
  if (length(sp$by) > 0 && (k != 2L || nrow(tab) != 2L || ncol(tab) != 2L)) {
    # only the 2x2 CHISQ / FISHER listing per BY group is supported
    abort_unsupported("PROC FREQ statistics with BY groups (only CHISQ / FISHER for a 2x2 table are supported)")
  }
  if ("binomial" %in% want) {
    if (k != 1) abort_syntax("BINOMIAL is valid only for one-way tables")
    bo <- o$binomial_opts %||% list()
    l1 <- lv[[1]]
    cnt <- as.numeric(tab)
    is_m <- if (length(l1$values)) freq_is_missing(l1$values) else logical(length(cnt))
    keep <- !is_m
    cnt_nm <- cnt[keep]
    txt <- l1$text[keep]
    sel <- 1L
    if (!is.null(bo$level_index)) {
      if (bo$level_index < 1L || bo$level_index > length(txt)) {
        abort_runtime(paste0("The value ", bo$level_index, " of the LEVEL= option is not a level of the table variable."))
      }
      sel <- bo$level_index
    } else if (!is.null(bo$level)) {
      want_txt <- sas_rtrim(as.character(bo$level))
      hit <- which(sas_rtrim(txt) == want_txt)
      if (length(hit) == 0) abort_runtime(paste0("The value ", want_txt, " of the LEVEL= option is not a level of the table variable."))
      sel <- hit[1]
    }
    alpha <- o$alpha %||% 0.05
    # BINOMIAL(EXACT) asks for the exact (Clopper-Pearson) confidence limits,
    # as CL=EXACT; the exact test is the EXACT BINOMIAL statement [measured]
    if (isTRUE(bo$exact)) bo$cl <- unique(c(bo$cl, "exact"))
    if ("binomial" %in% sp$exact) {
      n_bin <- sum(cnt_nm)
      # the exact binomial test enumerates integer tables: a fractional WEIGHT
      # makes it undefined (and crashed the listing without this guard)
      if (any(abs(c(cnt_nm, n_bin) - round(c(cnt_nm, n_bin))) > 1e-9)) {
        abort_unsupported("EXACT BINOMIAL with non-integer weights (the exact test requires integer frequencies)")
      }
    }
    st <- c(st, freq_binomial(cnt_nm, alpha, bo$p0 %||% 0.5, sel, bo$cl, isTRUE(bo$correct)))
    st$bin_level <- txt[sel]
    st$bin_var <- vars[1]
    st$bin_cl <- bo$cl
    st$bin_exact <- "binomial" %in% sp$exact
  }
  if (any(c("chisq", "fisher") %in% want)) {
    if (k == 1) {
      if ("fisher" %in% want) abort_unsupported("FISHER for a one-way table")
      cnt <- as.numeric(tab)
      test <- freq_oneway_test_spec(o, cnt)
      if (!is.null(test)) {
        e <- test$expected
        x2 <- sum((cnt - e)^2 / e)
        df <- length(cnt) - 1L
        st <- c(st, list(pchi = x2, df_pchi = df, p_pchi = stats::pchisq(x2, df, lower.tail = FALSE),
                         oneway_small = any(e < 5), oneway_test = test$kind))
      } else {
        cnt <- cnt[cnt > 0]
        e <- sum(cnt) / length(cnt)
        x2 <- sum((cnt - e)^2 / e)
        st <- c(st, list(pchi = x2, df_pchi = length(cnt) - 1, p_pchi = stats::pchisq(x2, length(cnt) - 1, lower.tail = FALSE),
                         oneway_small = e < 5))
      }
      st$oneway_chi <- TRUE
    } else if (k > 2) {
      # per-stratum CHISQ / FISHER blocks are printed by freq_nway after each
      # stratum table; the multiway request itself has no pooled chi-square
    } else {
      if (!is.null(o$chisq_opts)) {
        abort_unsupported("CHISQ TESTP= / TESTF= for a two-way table (only the one-way goodness-of-fit test is supported)")
      }
      tb <- matrix(as.numeric(tab), nrow = dim(tab)[1])
      if (freq_table_score_na(lv)) {
        abort_unsupported("CHISQ with the MISSING option on a numeric table-score variable (the table scores are not supported)")
      }
      rs <- freq_scores(lv[[1]])
      cs <- freq_scores(lv[[2]])
      keep_r <- rowSums(tb) > 0
      keep_c <- colSums(tb) > 0
      if (sum(keep_r) < 2 || sum(keep_c) < 2) {
        # a table that structurally has a single row or column: no statistic is
        # computed and no statistics block is printed at all
        st$nostat_structural <- TRUE
        return(st)
      }
      # a level kept with WEIGHT / ZEROS whose row or column sum is zero: SAS
      # computes no statistics for the table
      if (!all(keep_r) || !all(keep_c)) {
        st$nostat <- TRUE
        return(st)
      }
      # FISHER is always accompanied by the chi-square statistics
      if ("fisher" %in% want) want <- c(want, "chisq")
      if ("chisq" %in% want) {
        cs2 <- freq_chisq(tb, rs, cs)
        mh <- freq_mhchi(tb[keep_r, keep_c, drop = FALSE], rs[keep_r], cs[keep_c])
        st <- c(st, list(df_pchi = cs2$df, pchi = cs2$pchi, p_pchi = cs2$p_pchi, lrchi = cs2$lrchi, df_lrchi = cs2$df,
                         p_lrchi = cs2$p_lrchi, mhchi = mh$mhchi, df_mhchi = 1, p_mhchi = mh$p_mhchi,
                         phi = cs2$phi, contgy = cs2$contgy, cramv = cs2$cramv, small = cs2$small))
        if (!is.null(cs2$ajchi)) st <- c(st, list(ajchi = cs2$ajchi, df_ajchi = 1, p_ajchi = cs2$p_ajchi))
        if ("chisq" %in% sp$exact) {
          if (any(abs(tb - round(tb)) > 1e-9)) {
            abort_unsupported("EXACT CHISQ with non-integer weights (the exact test is enumerated over integer tables)")
          }
          if (nrow(tb) != 2L || ncol(tb) != 2L) {
            abort_unsupported("EXACT CHISQ for a table that is not 2x2 (only the 2x2 exact enumeration is supported)")
          }
          st$exact_chisq <- freq_exact_chisq_2x2(tb[keep_r, keep_c, drop = FALSE])
        }
      }
      if ("fisher" %in% want || ("chisq" %in% want && nrow(tb) == 2 && ncol(tb) == 2)) {
        if (any(abs(tb - round(tb)) > 1e-9)) {
          if ("fisher" %in% sp$exact) abort_unsupported("EXACT FISHER with non-integer weights")
          if (nrow(tb) != 2L || ncol(tb) != 2L) {
            abort_unsupported("FISHER with non-integer weights for a table that is not 2x2 (only the 2x2 behaviour is supported)")
          }
          # Fisher's exact test is not computed for a 2x2 table of
          # fractional (non-integer) frequencies: the chi-square table is still
          # printed but has no Fisher block
        } else {
          st$fisher <- freq_fisher(tb[keep_r, keep_c, drop = FALSE])
        }
      }
    }
  }
  if ("cmh" %in% want) {
    if (k < 2) abort_syntax("CMH requires a two-way or n-way table")
    if (k == 2L) {
      abort_unsupported("CMH for a two-way table (only the multiway CMH summary listing is supported; use a multiway table request)")
    }
    if (any(abs(as.numeric(tab) - round(as.numeric(tab))) > 1e-9)) {
      abort_unsupported("CMH statistics with non-integer weights (only integer weights are supported)")
    }
    scores <- if (identical(o$scores, "rank")) "rank" else "table"
    if (identical(scores, "table") && freq_table_score_na(lv)) {
      abort_unsupported("CMH with the MISSING option on a numeric table-score variable (the table scores are not supported)")
    }
    st <- c(st, freq_cmh(tab, lv, scores))
    st$cmh_common <- freq_cmh_common(tab, o$alpha %||% 0.05)
  }
  if (any(c("riskdiff", "relrisk") %in% want)) {
    st <- c(st, freq_rr_rd_stats(tab, lv, vars, info, o, sp))
  }
  # development hook: when getOption("sasonr.freq_capture") is an environment,
  # the computed statistics are appended to its `stats` list
  cap <- getOption("sasonr.freq_capture")
  if (is.environment(cap)) cap$stats <- c(cap$stats, list(st))
  st
}

# The BINOMIAL block of a one-way table: proportion, confidence limits and the
# asymptotic (and optionally exact) test.  Column widths:
# the proportion block is 32 columns wide, 22 when CL= is given (the limits
# then go in their own table), 35 when the exact test adds its longer labels.
# BINOMIAL as ODS tables: Binomial (the proportion and its ASE),
# BinomialCLs (one row per confidence limit type) and BinomialTest (the test
# of H0, with the exact test when requested).  The listing block is drawn
# from the same statistics (freq_binomial_listing()) and goes with the first
# table; the other two add no lines.
freq_binomial_tables <- function(st, info) {
  tab <- function(x, name, label, role) {
    new_sasonr_ods_table(x, name = name, label = label, path = paste0("Freq.Table1.", name), proc = "Freq",
                         template = "base.freq.binomial", cols = ods_cols(names(x), role = role))
  }
  b1 <- tab(tibble::tibble(Name = c("_BIN_", "E_BIN"), Label = c("Proportion", "ASE"), Value = c(st$bin, st$e_bin)),
            "Binomial", "Binomial Proportion", c("label", "label", "stat"))
  attr(b1, "lines") <- freq_binomial_listing(st, info)
  # the limit types the listing shows: Wald and exact by default, the CL= list otherwise
  cl <- st$bin_cl
  types <- if (is.null(cl)) c(if (isTRUE(st$correct)) "wald_corrected" else "wald", "exact") else {
    tt <- if (identical(cl, "all")) c("wald", "wilson", "exact", "agresticoull", "jeffreys") else cl
    unique(unlist(lapply(tt, function(t) {
      if (t == "all") c("wald", "wilson", "exact", "agresticoull", "jeffreys")
      else if (t %in% c("wald", "wilson") && isTRUE(st$correct)) paste0(t, "_corrected") else t
    })))
  }
  types <- intersect(types, names(st$cl))
  cls <- tibble::tibble(Type = types, LowerCL = vapply(types, function(t) st$cl[[t]][1], 0, USE.NAMES = FALSE),
                        UpperCL = vapply(types, function(t) st$cl[[t]][2], 0, USE.NAMES = FALSE))
  b2 <- tab(cls, "BinomialCLs", "Binomial Proportion Confidence Limits", c("label", "stat", "stat"))
  use_c <- isTRUE(st$correct)
  tv <- c(st$e0_bin, if (use_c) st$zc_bin else st$z_bin, if (use_c) st$plc_bin else st$pl_bin, if (use_c) st$p2c_bin else st$p2_bin)
  tn <- c("E0_BIN", "Z_BIN", "PL_BIN", "P2_BIN")
  tl <- c("ASE under H0", "Z", "One-sided Pr", "Two-sided Pr > |Z|")
  if (isTRUE(st$bin_exact)) {
    tv <- c(tv, st$xpl_bin, st$xp2_bin)
    tn <- c(tn, "XPL_BIN", "XP2_BIN")
    tl <- c(tl, "Exact One-sided Pr", "Exact Two-sided Pr")
  }
  b3 <- tab(tibble::tibble(Name = tn, Label = tl, Value = tv), "BinomialTest", "Binomial Proportion Test", c("label", "label", "stat"))
  conf <- paste0(format((1 - st$alpha) * 100, trim = TRUE, scientific = FALSE), "%")
  attr(b1, "freq") <- list(level = conf, var = info[[st$bin_var]]$disp %||% st$bin_var, value = st$bin_level, default_cl = is.null(st$bin_cl),
                           wald = if (isTRUE(st$correct)) st$cl$wald_corrected else st$cl$wald, exact = st$cl$exact)
  attr(b2, "freq") <- list(level = conf, prop = st$bin, default_cl = is.null(st$bin_cl))
  attr(b3, "freq") <- list(p0 = st$p0, exact = isTRUE(st$bin_exact), corrected = use_c, default_cl = is.null(st$bin_cl))
  attr(b2, "lines") <- character()
  attr(b3, "lines") <- character()
  list(b1, b2, b3)
}

freq_binomial_listing <- function(st, info) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  conf_pct <- format((1 - st$alpha) * 100, trim = TRUE, scientific = FALSE)
  lv <- st$bin_level
  disp <- info[[st$bin_var]]$disp %||% st$bin_var
  centre <- function(txt, width) strrep(" ", max(0L, (width - nchar(txt)) %/% 2L))
  line <- function(lab, val, width) {
    n <- width - nchar(val)
    paste0(sprintf(paste0("%-", n, "s"), lab), val)
  }
  exact <- isTRUE(st$bin_exact)
  w_prop <- if (exact) 35L else if (!is.null(st$bin_cl)) 22L else 32L
  w_test <- if (exact) 35L else 32L
  out <- character()
  out <- c(out, paste0(centre("Binomial Proportion", w_prop), "Binomial Proportion"),
           paste0(centre(paste0(disp, " = ", lv), w_prop), disp, " = ", lv), "")
  out <- c(out, line(if (exact) "Proportion (P)" else "Proportion", f4(st$bin), w_prop),
           line("ASE", f4(st$e_bin), w_prop))
  cl <- st$bin_cl
  if (is.null(cl)) {
    # with CORRECT the Wald limits are the corrected ones [measured]
    wl <- if (isTRUE(st$correct)) st$cl$wald_corrected else st$cl$wald
    out <- c(out, line(paste0(conf_pct, "% Lower Conf Limit"), f4(wl[1]), w_prop),
             line(paste0(conf_pct, "% Upper Conf Limit"), f4(wl[2]), w_prop), "",
             "Exact Conf Limits",
             line(paste0(conf_pct, "% Lower Conf Limit"), f4(st$cl$exact[1]), w_prop),
             line(paste0(conf_pct, "% Upper Conf Limit"), f4(st$cl$exact[2]), w_prop), "")
  } else {
    types <- if (identical(cl, "all")) c("wald", "wilson", "exact", "agresticoull", "jeffreys") else cl
    types <- unique(vctrs::list_unchop(lapply(types, function(t) {
      if (t == "all") c("wald", "wilson", "exact", "agresticoull", "jeffreys")
      else if (t %in% c("wald", "wilson")) if (isTRUE(st$correct)) paste0(t, "_corrected") else t
      else t
    }), ptype = character(), name_spec = rlang::zap()))
    nm <- c(wald = "Wald", wilson = "Wilson", exact = "Clopper-Pearson (Exact)",
            agresticoull = "Agresti-Coull", jeffreys = "Jeffreys",
            wald_corrected = "Wald (Corrected)", wilson_corrected = "Wilson (Corrected)")
    bad <- setdiff(types, names(nm))
    if (length(bad)) abort_unsupported(paste0("BINOMIAL CL= ", toupper(bad[1])))
    rows <- sort(nm[types])
    l0 <- max(nchar(rows)) + 4L
    width <- l0 + 21L
    out <- c(out, paste0(centre("Confidence Limits for the Binomial Proportion", width),
                         "Confidence Limits for the Binomial Proportion"),
             paste0(centre(paste0("Proportion = ", f4(st$bin)), width), "Proportion = ", f4(st$bin)), "",
             paste0(sprintf(paste0("%-", l0, "s"), "Type"), sprintf("%21s", paste0(conf_pct, "% Confidence Limits"))), "")
    for (r in rows) {
      key <- names(nm)[match(r, nm)]
      lim <- st$cl[[key]]
      out <- c(out, paste0(sprintf(paste0("%-", l0, "s"), r), sprintf("%6s", f4(lim[1])), sprintf("%15s", f4(lim[2]))))
    }
    out <- c(out, "")
  }
  # centred over the 32 columns of the test table, rounded down: P= 0.5 -> 2
  # spaces, 0.25 / 0.125 / 0.95 -> 1, 0.1234 -> 0 (heading lengths 28..31)
  h0 <- paste0("Test of H0: Proportion = ", freq_num(st$p0))
  # longer headings (P= with five or more decimals) have no measured layout
  if (nchar(h0) > 31L) abort_unsupported(paste0("BINOMIAL P=", freq_num(st$p0), " (heading layout)"))
  out <- c(out, paste0(strrep(" ", max(0L, (32L - nchar(h0)) %/% 2L)), h0), "")
  use_c <- isTRUE(st$correct)
  zt <- if (use_c) st$zc_bin else st$z_bin
  pone <- if (use_c) st$plc_bin else st$pl_bin
  ptwo <- if (use_c) st$p2c_bin else st$p2_bin
  # Z = 0 reads "Pr <  Z"; only a positive Z reads "Pr >  Z"
  side <- if (zt > 0) ">" else "<"
  out <- c(out,
           line("ASE under H0", f4(st$e0_bin), w_test),
           line("Z", f4(zt), w_test),
           line(paste0("One-sided Pr ", side, "  Z"), pv(pone), w_test),
           line("Two-sided Pr > |Z|", pv(ptwo), w_test))
  if (use_c) {
    out <- c(out, "", if (is.null(cl)) c("The asymptotic confidence limits and test", "    include a continuity correction.") else
      "The asymptotic test includes a continuity correction.")
  }
  if (exact) {
    l0 <- max(nchar(c("One-sided Pr >=  P", "Two-sided = 2 * One-sided"))) + 4L
    side_exact <- if (st$bin >= st$p0) ">=" else "<="
    out <- c(out, "", "Exact Test",
             line(paste0("One-sided Pr ", side_exact, "  P"), pv(st$xpl_bin), l0 + 6L),
             line("Two-sided = 2 * One-sided", pv(st$xp2_bin), l0 + 6L))
  }
  c(out, "", freq_sample_size(st))
}

# The CMH summary of a multiway table request as segments: the headings
# ("The FREQ Procedure": the summary starts its own page), the CMH table, the
# CommonRelRisks and BreslowDayTest tables (stratified 2x2 tables), and
# "Total Sample Size = n".
freq_cmh_summary <- function(st, vars, info, o) {
  zlab <- paste0(format_pct_level(o$alpha %||% 0.05), "%")
  scores_txt <- if (identical(o$scores, "rank")) "Rank Scores" else "Table Scores"
  k <- length(vars)
  rowd <- info[[vars[k - 1L]]]$disp
  cold <- info[[vars[k]]]$disp
  strata_disp <- vapply(vars[seq_len(k - 2L)], function(v) info[[v]]$disp, "")
  tab <- function(x, name, label, tpl, role) {
    new_sasonr_ods_table(x, name = name, label = label, path = paste0("Freq.Table1.", name), proc = "Freq", template = tpl,
                         cols = ods_cols(names(x), role = role))
  }
  segs <- list(freq_lines_seg(c("The FREQ Procedure", "", paste("Summary Statistics for", rowd, "by", cold),
                                paste("Controlling for", paste(strata_disp, collapse = " ")),
                                paste0("Cochran-Mantel-Haenszel Statistics (Based on ", scores_txt, ")")), decor = TRUE))
  cm <- tibble::tibble(Statistic = 1:3, AltHyp = c("Nonzero Correlation", "Row Mean Scores Differ", "General Association"),
                       DF = c(st$df_cmhcor, st$df_cmhrms, st$df_cmhga), Value = c(st$cmhcor, st$cmhrms, st$cmhga),
                       Prob = c(st$p_cmhcor, st$p_cmhrms, st$p_cmhga))
  cmt <- tab(cm, "CMH", "Cochran-Mantel-Haenszel Statistics", "base.freq.cmh", c("label", "label", "stat", "stat", "stat"))
  attr(cmt, "freq") <- list(scores = scores_txt)
  segs <- c(segs, list(cmt))
  cc <- st$cmh_common
  if (!is.null(cc)) {
    m <- list(cc$ormh, cc$orl, cc$rr1, cc$rr1l, cc$rr2, cc$rr2l)
    rr <- tibble::tibble(Statistic = rep(c("Odds Ratio", "Relative Risk (Column 1)", "Relative Risk (Column 2)"), each = 2L),
                         Method = rep(c("Mantel-Haenszel", "Logit"), 3L), Value = vapply(m, `[`, 0, 1L),
                         LowerCL = vapply(m, `[`, 0, 2L), UpperCL = vapply(m, `[`, 0, 3L))
    rt <- tab(rr, "CommonRelRisks", "Common Relative Risks", "base.freq.commonrr", c("label", "label", "stat", "stat", "stat"))
    attr(rt, "freq") <- list(level = zlab)
    segs <- c(segs, list(rt))
    if (!is.null(cc$bd)) {
      bd <- tibble::tibble(Name = c("_BDCHI_", "DF_BDCHI", "P_BDCHI"), Label = c("Chi-Square", "DF", "Pr > ChiSq"),
                           Value = c(cc$bd$chisq, cc$bd$df, cc$bd$p))
      segs <- c(segs, list(tab(bd, "BreslowDayTest", "Breslow-Day Test", "base.freq.breslow", c("label", "label", "stat"))))
    }
  }
  c(segs, list(freq_lines_seg(c("", paste("Total Sample Size =", freq_num(st$n)), ""), decor = TRUE)))
}

# Listings of the multiway CMH summary tables.
freq_cmh_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) if (is.na(v)) "." else trimws(fmt_pvalue(v, 6L, 4L))
  c("Statistic Alternative Hypothesis DF Value Prob", strrep("-", 63),
    vapply(seq_len(nrow(tb)), function(i) paste(tb$Statistic[i], tb$AltHyp[i], sprintf("%.0f", tb$DF[i]), f4(tb$Value[i]), pv(tb$Prob[i])), ""))
}
freq_commonrr_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  c("", "Common Odds Ratio and Relative Risks", paste("Statistic Method Value", attr(tb, "freq", exact = TRUE)$level, "Confidence Limits"),
    strrep("-", 85),
    vapply(seq_len(nrow(tb)), function(i) paste(c(if (identical(tb$Method[i], "Mantel-Haenszel")) tb$Statistic[i], tb$Method[i],
                                                  f4(tb$Value[i]), f4(tb$LowerCL[i]), f4(tb$UpperCL[i])), collapse = " "), ""))
}
freq_breslow_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) if (is.na(v)) "." else trimws(fmt_pvalue(v, 6L, 4L))
  c("", "Breslow-Day Test for", "Homogeneity of Odds Ratios", strrep("-", 26),
    paste("Chi-Square", f4(tb$Value[1])), paste("DF", sprintf("%.0f", tb$Value[2])), paste("Pr > ChiSq", pv(tb$Value[3])))
}

# The statistics output of one table request: a list of segments, each a
# character vector of listing lines or an ODS table (ChiSq, FishersExact)
# (`pieces = TRUE`), or the listing lines.
freq_stats_listing <- function(st, tab, vars, info, o, sp, title = NULL, stratum_line = NULL, pieces = FALSE) {
  # BINOMIAL alone; with CHISQ the chi-square test comes first, whatever the
  # order of the options, and the sample size once at the end [measured]
  both <- !is.null(st$bin_var) && isTRUE(st$oneway_chi) && isTRUE(o$chisq)
  if (!is.null(st$bin_var) && !both) {
    return(if (pieces) freq_binomial_tables(st, info) else freq_binomial_listing(st, info))
  }
  segs <- list()
  out <- character()
  # the lines so far become a segment; `decor`: headings, warnings and the
  # sample size around the tables (no table content)
  flush <- function(decor = FALSE) {
    if (length(out)) segs[[length(segs) + 1L]] <<- freq_lines_seg(out, decor)
    out <<- character()
  }
  push <- function(tb) segs[[length(segs) + 1L]] <<- tb
  finish <- function() {
    flush()
    if (pieces) segs else unlist(lapply(segs, freq_seg_lines))
  }
  k <- length(vars)
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  # the multiway CMH summary is a listing block of its own (manual: "Cochran-
  # Mantel-Haenszel Statistics"); the per-stratum statistics are printed by
  # freq_nway right after each stratum table
  if (k > 2L) {
    if (!is.null(st$cmhcor)) segs <- freq_cmh_summary(st, vars, info, o)
    return(finish())
  }
  # RISKDIFF / RELRISK print their blocks after the test statistics and share a
  # single "Statistics for Table of ..." heading and a single Sample Size line.
  more <- !is.null(st$rd) || !is.null(st$rel)
  if (isTRUE(st$nostat_structural)) return(finish())
  header <- title %||% paste("Statistics for Table of", info[[vars[1]]]$disp, "by", info[[vars[2]]]$disp)
  hdr <- if (!is.null(stratum_line)) c(header, stratum_line) else header
  did <- FALSE
  if (isTRUE(st$oneway_chi) && isTRUE(o$chisq)) {
    test_line <- if (identical(st$oneway_test, "p")) {
      "for Specified Proportions"
    } else if (identical(st$oneway_test, "f")) {
      "for Specified Frequencies"
    } else {
      "for Equal Proportions"
    }
    out <- c(out, "   Chi-Square Test", test_line, "")
    flush(decor = TRUE)
    push(new_sasonr_ods_table(tibble::tibble(Name = c("_PCHI_", "DF_PCHI", "P_PCHI"), Label = c("Chi-Square", "DF", "Pr > ChiSq"),
                                             Value = c(st$pchi, st$df_pchi, st$p_pchi)),
                              name = "OneWayChiSq", label = "One-Way Chi-Square Test", path = "Freq.Table1.OneWayChiSq", proc = "Freq",
                              template = "base.freq.onewaychisq", cols = ods_cols(c("Name", "Label", "Value"), role = c("label", "label", "stat"))))
    attr(segs[[length(segs)]], "freq") <- list(test = test_line, small = isTRUE(st$oneway_small))
    out <- c(out, if (isTRUE(st$oneway_small)) c("WARNING: The table cells have expected counts less",
                                                 "         than 5. Chi-Square may not be a valid test.", ""),
             if (both) "" else freq_sample_size(st))
    flush(decor = TRUE)
    # the binomial block follows, closed by the sample size
    if (both) for (tb in freq_binomial_tables(st, info)) push(tb)
  } else if (isTRUE(st$nostat) && k == 2) {
    out <- c(out, hdr, "",
             "Row or column sum zero. No statistics computed for this table.", "", freq_sample_size(st))
    did <- TRUE
  } else if ((isTRUE(o$chisq) || isTRUE(o$fisher) || "fisher" %in% sp$exact) && k == 2 && !is.null(st$pchi)) {
    # FISHER also prints the chi-square table (it is computed with every 2-way test)
    rows <- list(c("Chi-Square", st$df_pchi, st$pchi, st$p_pchi), c("Likelihood Ratio Chi-Square", st$df_lrchi, st$lrchi, st$p_lrchi))
    if (!is.null(st$ajchi)) rows <- c(rows, list(c("Continuity Adj. Chi-Square", 1, st$ajchi, st$p_ajchi)))
    rows <- c(rows, list(c("Mantel-Haenszel Chi-Square", 1, st$mhchi, st$p_mhchi)))
    rows <- c(rows, list(c("Phi Coefficient", NA, st$phi, NA), c("Contingency Coefficient", NA, st$contgy, NA),
                         c("Cramer's V", NA, st$cramv, NA)))
    chi <- tibble::tibble(Statistic = vapply(rows, `[[`, "", 1L), DF = as.numeric(vapply(rows, `[[`, "", 2L)),
                          Value = as.numeric(vapply(rows, `[[`, "", 3L)), Prob = as.numeric(vapply(rows, `[[`, "", 4L)))
    out <- c(out, hdr, "")
    flush(decor = TRUE)
    push(new_sasonr_ods_table(chi, name = "ChiSq", label = "Chi-Square Tests", path = "Freq.Table1.ChiSq", proc = "Freq",
                              template = "base.freq.chisq", cols = ods_cols(names(chi), role = c("label", "stat", "stat", "stat"))))
    did <- TRUE
    if (isTRUE(st$small > 0) && !isTRUE(o$nowarn)) {
      attr(segs[[length(segs)]], "freq") <- list(warning = sprintf("WARNING: %.0f%% of the cells have expected counts less than 5. Chi-Square may not be a valid test.",
                                                                    st$small * 100))
      out <- c(out, sprintf(" WARNING: %.0f%% of the cells have expected counts less", st$small * 100),
               "          than 5. Chi-Square may not be a valid test.", "")
      flush(decor = TRUE)
    }
    if (!is.null(st$exact_chisq)) {
      # EXACT CHISQ: one block per chi-square test with its exact conditional
      # p-value
      ex <- st$exact_chisq
      blk <- function(lab, val, df, pval, pexact) {
        c(lab, strrep("-", 34),
          paste("Chi-Square", f4(val)),
          paste("DF", sprintf("%.0f", as.numeric(df))),
          paste("Asymptotic Pr > ChiSq", pv(pval)),
          paste("Exact Pr >= ChiSq", pv(pexact)))
      }
      out <- c(out,
               blk("Pearson Chi-Square Test", st$pchi, st$df_pchi, st$p_pchi, ex[["pearson"]]),
               blk("Likelihood Ratio Chi-Square Test", st$lrchi, st$df_lrchi, st$p_lrchi, ex[["lr"]]),
               blk("Mantel-Haenszel Chi-Square Test", st$mhchi, 1, st$p_mhchi, ex[["mh"]]))
    }
    if (is.null(st$fisher) && !more) {
      flush()
      out <- c(out, freq_sample_size(st))
      flush(decor = TRUE)
    }
  }
  if (!is.null(st$fisher) && k == 2) {
    fz <- st$fisher
    flush()
    fx <- if (!is.null(fz$xpl)) {
      tibble::tibble(Name = c("FISHER_F", "XPL_FISH", "XPR_FISH", "XPT_FISH", "XP2_FISH"),
                     Label = c("Cell (1,1) Frequency (F)", "Left-sided Pr <= F", "Right-sided Pr >= F", "Table Probability (P)", "Two-sided Pr <= P"),
                     Value = c(fz$fisher_f, fz$xpl, fz$xpr, fz$xpt, fz$xp2))
    } else {
      tibble::tibble(Name = c("XPT_FISH", "XP2_FISH"), Label = c("Table Probability (P)", "Pr <= P"), Value = c(fz$xpt, fz$xp2))
    }
    push(new_sasonr_ods_table(fx, name = "FishersExact", label = "Fisher's Exact Test", path = "Freq.Table1.FishersExact", proc = "Freq",
                              template = "base.freq.fisher", cols = ods_cols(names(fx), role = c("label", "label", "stat"))))
    if (!more) {
      out <- c(out, freq_sample_size(st))
      flush(decor = TRUE)
    }
  }
  if (!is.null(st[["bin"]]) && is.null(st$bin_var)) {
    a <- o$alpha %||% 0.05
    lvl <- paste0(format_pct_level(a), "%")
    lab <- info[[vars[1]]]
    out <- c(out, "Binomial Proportion", "", paste0("Proportion", formatC(f4(st$bin), width = 16L)),
             paste0("ASE", formatC(f4(st$e_bin), width = 23L)),
             paste0(lvl, " Lower Conf Limit", formatC(f4(st$l_bin), width = 9L - nchar(lvl) + 7L)),
             paste0(lvl, " Upper Conf Limit", formatC(f4(st$u_bin), width = 9L - nchar(lvl) + 7L)), "",
             "Exact Conf Limits", paste0(lvl, " Lower Conf Limit", formatC(f4(st$xl_bin), width = 9L - nchar(lvl) + 7L)),
             paste0(lvl, " Upper Conf Limit", formatC(f4(st$xu_bin), width = 9L - nchar(lvl) + 7L)), "",
             "Test of H0: Proportion = 0.5", "", paste0("ASE under H0", formatC(f4(st$e0_bin), width = 14L)),
             paste0("Z", formatC(f4(st$z_bin), width = 25L)),
             paste0(if (st$z_bin > 0) "One-sided Pr >  Z" else "One-sided Pr <  Z", formatC(pv(st$pl_bin), width = 9L)),
             paste0("Two-sided Pr > |Z|", formatC(pv(st$p2_bin), width = 8L)), "",
             paste0("Sample Size = ", freq_num(st$n)), "")
  }
  if (!is.null(st$cmhcor)) {
    out <- c(out, "Cochran-Mantel-Haenszel Statistics (Based on Table Scores)", "",
             "Statistic    Alternative Hypothesis    DF       Value      Prob", strrep("-", 62),
             paste0("    1        Nonzero Correlation", formatC("1", width = 10L), formatC(f4(st$cmhcor), width = 12L), formatC(pv(st$p_cmhcor), width = 10L)),
             paste0("    2        Row Mean Scores Differ", formatC(sprintf("%.0f", st$df_cmhrms), width = 7L), formatC(f4(st$cmhrms), width = 12L), formatC(pv(st$p_cmhrms), width = 10L)),
             paste0("    3        General Association", formatC(sprintf("%.0f", st$df_cmhga), width = 10L), formatC(f4(st$cmhga), width = 12L), formatC(pv(st$p_cmhga), width = 10L)), "")
  }
  if (!is.null(st$rd)) {
    if (!did) {
      out <- c(out, header, "")
      did <- TRUE
    }
    flush(decor = TRUE)
    for (tb in freq_riskdiff_tables(st)) push(tb)
  }
  if (!is.null(st$rel)) {
    if (!did) {
      out <- c(out, header, "")
      did <- TRUE
    }
    flush(decor = TRUE)
    push(freq_relrisk_table(st))
  }
  if (more) {
    flush()
    out <- c(out, freq_sample_size(st))
    flush(decor = TRUE)
  }
  finish()
}

freq_lines_seg <- function(lines, decor = FALSE) structure(lines, decor = decor)

# The listing lines of a segment of the statistics output.
freq_seg_lines <- function(seg) {
  if (!inherits(seg, "sasonr_ods_table")) return(as.character(seg))
  switch(attr(seg, "template", exact = TRUE),
    base.freq.chisq = freq_chisq_listing(seg),
    base.freq.fisher = freq_fisher_listing(seg),
    base.freq.cmh = freq_cmh_listing(seg),
    base.freq.commonrr = freq_commonrr_listing(seg),
    base.freq.breslow = freq_breslow_listing(seg),
    base.freq.relrisk = , base.freq.relriskcl = freq_relrisk_listing(seg),
    base.freq.riskdiff = , base.freq.riskdiffcl = freq_riskdiff_listing(seg),
    base.freq.binomial = attr(seg, "lines", exact = TRUE),
    base.freq.onewaychisq = freq_onewaychisq_listing(seg))
}

# Listing of a ChiSq table: the tests with DF and Prob, then the measures.
freq_chisq_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  body <- vapply(seq_len(nrow(tb)), function(i) {
    if (is.na(tb$DF[i])) return(paste0(formatC(tb$Statistic[i], width = -32L), formatC(f4(tb$Value[i]), width = 12L)))
    paste0(formatC(tb$Statistic[i], width = -27L), formatC(sprintf("%.0f", tb$DF[i]), width = 5L),
           formatC(f4(tb$Value[i]), width = 12L), formatC(pv(tb$Prob[i]), width = 10L))
  }, "")
  c(paste0(formatC("Statistic", width = -27L), "   DF       Value      Prob"), strrep("-", 54), body, "")
}

# Listing of a OneWayChiSq table.
freq_onewaychisq_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  c(paste0("Chi-Square", formatC(f4(tb$Value[1]), width = 10L)), paste0("DF", formatC(sprintf("%.0f", tb$Value[2]), width = 18L)),
    paste0("Pr > ChiSq", formatC(pv(tb$Value[3]), width = 10L)), "")
}

# Listing of a FishersExact table (2x2: one-sided and two-sided; larger
# tables: the table probability and Pr <= P).
freq_fisher_listing <- function(tb) {
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  line <- function(i) paste0(formatC(tb$Label[i], width = -26L),
                             formatC(if (identical(tb$Name[i], "FISHER_F")) freq_num(tb$Value[i]) else pv(tb$Value[i]), width = 8L))
  rows <- if (nrow(tb) == 5L) c(line(1), line(2), line(3), "", line(4), line(5)) else c(line(1), line(2))
  c("       Fisher's Exact Test", strrep("-", 34), rows, "")
}

# RISKDIFF as ODS tables: RiskDiffCol1 and RiskDiffCol2 (the row risks, the
# difference with its exact limits when requested) and, for the selected
# column, PdiffCLs (the confidence limits of the risk difference).
freq_riskdiff_tables <- function(st) {
  rd <- st$rd
  zlab <- paste0(format_pct_level(rd$alpha), "%")
  labs <- c(ac = "Agresti-Caffo", exact = "Exact (Score)", ha = "Hauck-Anderson", newcombe = "Newcombe", wald = "Wald")
  out <- list()
  for (j in 1:2) {
    cp <- rd$cols[[j]]
    r <- list(cp$r1, cp$r2, cp$rt)
    x <- tibble::tibble(Row = c("Row 1", "Row 2", "Total", "Difference"),
                        Risk = c(vapply(r, `[[`, 0, "risk"), cp$diff), ASE = c(vapply(r, `[[`, 0, "ase"), cp$ase_d),
                        LowerCL = c(vapply(r, function(z) z$wald[1], 0), cp$wald[1]),
                        UpperCL = c(vapply(r, function(z) z$wald[2], 0), cp$wald[2]),
                        ExactLowerCL = c(vapply(r, function(z) z$cp[1], 0), if (isTRUE(rd$exact)) cp$exact[1] else NA_real_),
                        ExactUpperCL = c(vapply(r, function(z) z$cp[2], 0), if (isTRUE(rd$exact)) cp$exact[2] else NA_real_))
    has_cl <- j == rd$column && !is.null(st$rd_cl)
    tb <- new_sasonr_ods_table(x, name = paste0("RiskDiffCol", j), label = paste0("Column ", j, " Risk Estimates"),
                               path = paste0("Freq.Table1.RiskDiffCol", j), proc = "Freq", template = "base.freq.riskdiff",
                               cols = ods_cols(names(x), role = c("label", rep("stat", 6L))))
    attr(tb, "freq") <- list(level = zlab, column = j, exact = isTRUE(rd$exact), has_cl = has_cl)
    out <- c(out, list(tb))
    if (has_cl) {
      types <- st$rd_cl$types
      types <- types[order(labs[types])]
      cl <- tibble::tibble(Type = unname(labs[types]), LowerCL = vapply(types, function(t) st$rd_cl$vals[[t]][1], 0, USE.NAMES = FALSE),
                           UpperCL = vapply(types, function(t) st$rd_cl$vals[[t]][2], 0, USE.NAMES = FALSE))
      ct <- new_sasonr_ods_table(cl, name = "PdiffCLs", label = "Confidence Limits for the Risk Difference", path = "Freq.Table1.PdiffCLs",
                                 proc = "Freq", template = "base.freq.riskdiffcl", cols = ods_cols(names(cl), role = c("label", "stat", "stat")))
      attr(ct, "freq") <- list(level = zlab, diff = cp$diff, column = paste0("Column ", j, " (", rd$col_disp, " = ", rd$col_text[j], ")"))
      out <- c(out, list(ct))
    }
  }
  out
}

# Listing of a RiskDiffCol table (and of the PdiffCLs table after it).
freq_riskdiff_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  a <- attr(tb, "freq", exact = TRUE)
  if (identical(attr(tb, "template", exact = TRUE), "base.freq.riskdiffcl")) {
    return(c("", "Confidence Limits for the Risk Difference", paste("Risk Difference =", f4(a$diff)), "",
             paste("Type", a$level, "Confidence Limits"), "",
             vapply(seq_len(nrow(tb)), function(i) paste(tb$Type[i], f4(tb$LowerCL[i]), f4(tb$UpperCL[i])), ""),
             "", a$column, ""))
  }
  row <- function(i) paste(tb$Row[i], f4(tb$Risk[i]), f4(tb$ASE[i]), f4(tb$LowerCL[i]), f4(tb$UpperCL[i]), f4(tb$ExactLowerCL[i]), f4(tb$ExactUpperCL[i]))
  dline <- paste("Difference", f4(tb$Risk[4]), f4(tb$ASE[4]), f4(tb$LowerCL[4]), f4(tb$UpperCL[4]))
  if (a$exact) dline <- paste(dline, f4(tb$ExactLowerCL[4]), f4(tb$ExactUpperCL[4]))
  out <- c(paste0("Column ", a$column, " Risk Estimates"), "", paste(a$level, "Exact", a$level),
           "Risk ASE Confidence Limits Confidence Limits", strrep("-", 77), row(1), row(2), row(3), "",
           dline, "", "Difference is (Row 1 - Row 2)",
           if (a$exact) "Exact confidence limits for the risk difference are based on a score statistic.")
  c(out, if (!a$has_cl) "")
}

# RELRISK: the RelativeRisks table (odds ratio and the column relative risks
# with their confidence limits) or, with RELRISK(CL=), the RelRiskCL table of
# the selected column.
freq_relrisk_table <- function(st) {
  rel <- st$rel
  zlab <- paste0(format_pct_level(rel$alpha), "%")
  if (is.null(rel$cl)) {
    x <- tibble::tibble(Statistic = c("Odds Ratio", "Relative Risk (Column 1)", "Relative Risk (Column 2)"),
                        Value = c(rel$or, rel$rr1, rel$rr2), LowerCL = c(rel$or_ci[1], rel$rr1_ci[1], rel$rr2_ci[1]),
                        UpperCL = c(rel$or_ci[2], rel$rr1_ci[2], rel$rr2_ci[2]))
    x <- x[!is.na(x$Value), ]
    tb <- new_sasonr_ods_table(x, name = "RelativeRisks", label = "Estimates of the Relative Risk", path = "Freq.Table1.RelativeRisks",
                               proc = "Freq", template = "base.freq.relrisk", cols = ods_cols(names(x), role = c("label", "stat", "stat", "stat")))
    attr(tb, "freq") <- list(level = zlab)
    return(tb)
  }
  labs <- c(exact = "Exact (Score)", score = "Score", wald = "Wald")
  types <- rel$cl
  types <- types[order(labs[types])]
  x <- tibble::tibble(Type = unname(labs[types]), LowerCL = vapply(types, function(t) rel$cl_vals[[t]][1], 0, USE.NAMES = FALSE),
                      UpperCL = vapply(types, function(t) rel$cl_vals[[t]][2], 0, USE.NAMES = FALSE))
  tb <- new_sasonr_ods_table(x, name = "RelRiskCL", label = "Confidence Limits for the Relative Risk", path = "Freq.Table1.RelRiskCL",
                             proc = "Freq", template = "base.freq.relriskcl", cols = ods_cols(names(x), role = c("label", "stat", "stat")))
  attr(tb, "freq") <- list(level = zlab, rr = rel$rr, column = paste0("Column ", rel$column, " (", rel$col_disp, " = ", rel$col_text[rel$column], ")"))
  tb
}

freq_relrisk_listing <- function(tb) {
  f4 <- function(v) if (is.na(v)) "." else display_fixed(v, 4L)
  a <- attr(tb, "freq", exact = TRUE)
  if (identical(attr(tb, "template", exact = TRUE), "base.freq.relrisk")) {
    rows <- vapply(seq_len(nrow(tb)), function(i) paste(tb$Statistic[i], f4(tb$Value[i]), f4(tb$LowerCL[i]), f4(tb$UpperCL[i])), "")
    out <- c("Odds Ratio and Relative Risks", "", paste("Statistic Value", a$level, "Confidence Limits"), strrep("-", 66), rows)
    if (length(rows) < 3L) out <- c(out, "", "One or more statistics not computed -- zero cell.")
    return(c(out, ""))
  }
  out <- c("Confidence Limits for the Relative Risk", paste("Relative Risk =", f4(a$rr)), "",
           paste("Type", a$level, "Confidence Limits"), "",
           vapply(seq_len(nrow(tb)), function(i) paste(tb$Type[i], f4(tb$LowerCL[i]), f4(tb$UpperCL[i])), ""))
  c(out, "", a$column, "")
}

# "Sample Size = n" after the statistics of a table, with the missing count
# and the WARNING when at least 20% of the data are missing.
freq_sample_size <- function(st) {
  out <- paste0("Sample Size = ", freq_num(st$n))
  if (isTRUE(st$nmiss > 0)) {
    out <- c(out, paste0("Frequency Missing = ", freq_num(st$nmiss)))
    pct <- st$nmiss / (st$n + st$nmiss) * 100
    if (pct >= 20) out <- c(out, sprintf("WARNING: %.0f%% of the data are missing.", pct))
  }
  c(out, "")
}

# OUTPUT OUT= data set: one row per table request (per BY group).
freq_output_data <- function(rows, sp) {
  kws <- sp$output$kws
  map <- list(
    n = list(c("n", "N", "Number of Nonmissing Observations")),
    nmiss = list(c("nmiss", "NMISS", "Number of Missing Observations")),
    pchi = list(c("pchi", "_PCHI_", "Chi-Square"), c("df_pchi", "DF_PCHI", "DF for Chi-Square"), c("p_pchi", "P_PCHI", "P-value for Chi-Square")),
    lrchi = list(c("lrchi", "_LRCHI_", "Likelihood Ratio Chi-Square"), c("df_lrchi", "DF_LRCHI", "DF for Likelihood Ratio Chi-Square"),
                 c("p_lrchi", "P_LRCHI", "P-value for Likelihood Ratio Chi-Square")),
    ajchi = list(c("ajchi", "_AJCHI_", "Continuity Adj. Chi-Square"), c("df_ajchi", "DF_AJCHI", "DF for Continuity Adj. Chi-Square"),
                 c("p_ajchi", "P_AJCHI", "P-value for Continuity Adj. Chi-Square")),
    mhchi = list(c("mhchi", "_MHCHI_", "Mantel-Haenszel Chi-Square"), c("df_mhchi", "DF_MHCHI", "DF for Mantel-Haenszel Chi-Square"),
                 c("p_mhchi", "P_MHCHI", "P-value for Mantel-Haenszel Chi-Square")),
    phi = list(c("phi", "_PHI_", "Phi Coefficient")),
    contgy = list(c("contgy", "_CONTGY_", "Contingency Coefficient")),
    cramv = list(c("cramv", "_CRAMV_", "Cramer's V")),
    cmhcor = list(c("cmhcor", "_CMHCOR_", "CMH Nonzero Correlation"), c("df_cmhcor", "DF_CMHCOR", "DF for CMH Nonzero Correlation"),
                  c("p_cmhcor", "P_CMHCOR", "P-value for CMH Nonzero Correlation")),
    cmhrms = list(c("cmhrms", "_CMHRMS_", "CMH Row Mean Scores Differ"), c("df_cmhrms", "DF_CMHRMS", "DF for CMH Row Mean Scores Differ"),
                  c("p_cmhrms", "P_CMHRMS", "P-value for CMH Row Mean Scores Differ")),
    cmhga = list(c("cmhga", "_CMHGA_", "CMH General Association"), c("df_cmhga", "DF_CMHGA", "DF for CMH General Association"),
                 c("p_cmhga", "P_CMHGA", "P-value for CMH General Association"))
  )
  exp <- vctrs::list_unchop(lapply(kws, function(k) switch(k,
    chisq = c("pchi", "lrchi", "ajchi", "mhchi", "phi", "contgy", "cramv"),
    cmh = c("cmhcor", "cmhrms", "cmhga"), cmh1 = "cmhcor", cmh2 = c("cmhcor", "cmhrms"),
    binomial = abort_unsupported("PROC FREQ OUTPUT BINOMIAL (output variable layout not supported)"),
    k)), ptype = character(), name_spec = rlang::zap())
  exp <- unique(exp)
  if (length(rows) > 1 && length(sp$by) == 0) abort_unsupported("OUTPUT statement with more than one table request")
  info <- sp$info
  cols <- lapply(sp$by, function(v) freq_col_of(v, info))
  stat_cols <- list()
  for (e in exp) {
    if (e == "ajchi" && all(vapply(rows, function(r) is.null(r$ajchi), TRUE))) next
    for (m in map[[e]]) stat_cols <- c(stat_cols, list(list(name = tolower(m[2]), disp = m[2], char = FALSE, len = 8L, label = m[3], key = m[1])))
  }
  cols <- c(cols, stat_cols)
  data <- lapply(cols, function(c) {
    if (!is.null(c$key)) return(vapply(rows, function(r) vctrs::vec_cast(r[[c$key]] %||% NA_real_, double()), 1))
    v <- lapply(rows, function(r) r[[c$name]])
    if (c$char) vapply(v, as.character, "") else vapply(v, as.numeric, 1)
  })
  names(data) <- vapply(cols, `[[`, "", "name")
  proc_out_options(tibble::as_tibble(data), proc_out_meta(cols), sp$output$ds_opts, sp$line)
}
