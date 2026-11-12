# PROC FREQ: parser, translation and run-time engine (frequency tables,
# OUT= data sets, OUTPUT statement statistics and the listing).

freq_table_opts <- c("list", "missing", "missprint", "nocum", "nopercent", "norow", "nocol", "nofreq", "sparse",
                     "outpct", "outcum", "chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "noprint", "nowarn",
                     "exact")
freq_output_kw <- c("n", "nmiss", "pchi", "lrchi", "ajchi", "mhchi", "chisq", "phi", "contgy", "cramv", "cmh",
                    "cmh1", "cmh2", "cmhcor", "cmhrms", "cmhga", "binomial", "bin")

# Statistics that have been checked against real SAS output.  Every other
# statistic request (TABLES options, EXACT, OUTPUT keywords) fails loud until
# it is verified; the computations below stay in place for that work.  The
# option `sasonr.freq_unverified_stats = TRUE` opens the gate for development
# and for the package's own reference-computation tests only.
# Verified (2026-11-11, freq_stats gold c01-c33): two-way and one-way CHISQ,
# FISHER (2x2 and RxC) and EXACT FISHER -- values within 1e-12 of SAS and the
# listing line for line.  BINOMIAL and CMH values agree too, but their
# listings (and CMH's common odds ratio / Breslow-Day tables) do not yet.
# The verified statistics are read from the PROC statistics registry
# (inst/registry/proc_stats.csv, rows with proc FREQ).
freq_stat_gate <- function(k, line) {
  k <- tolower(k)
  if (k == "bin") k <- "binomial"
  if (!identical(proc_stat_status("FREQ", k), "verified") && !isTRUE(getOption("sasonr.freq_unverified_stats"))) {
    abort_unsupported(paste0("PROC FREQ statistic ", toupper(k), " (not yet verified against SAS)"), line)
  }
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
          if (!k %in% c("fisher", "binomial", "bin")) abort_unsupported(paste0("EXACT ", toupper(k)), sline)
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
      abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC FREQ ",
                          "(ERROR 180-322: Statement is not valid or it is used out of proper order)"), sline)
    )
  })
  list(type = "proc", name = "freq", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

# BINOMIAL( options ): P=, LEVEL=, CL= (single value or a list), ALPHA=,
# CORRECT, EXACT.  Unsupported options fail loud rather than being ignored.
parse_freq_binomial_opts <- function(p, line) {
  opts <- list(p0 = NULL, level = NULL, cl = NULL, correct = FALSE, exact = FALSE, alpha = NULL)
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
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (k == "p") {
        opts$p0 <- as.numeric(parse_opt_value(p, "P", kl))
      } else if (k == "level") {
        # LEVEL=2 (a number) selects the second level of the table; LEVEL='x'
        # selects the level whose formatted value is x.
        if (tk_type(p) == "num") {
          opts$level_index <- as.integer(as.numeric(tk_val(p)))
          advance(p)
        } else {
          opts$level <- parse_opt_value(p, "LEVEL", kl)
        }
      } else if (k == "cl") {
        opts$cl <- value_list()
      } else if (k == "alpha") {
        opts$alpha <- as.numeric(parse_opt_value(p, "ALPHA", kl))
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
          o$alpha <- as.numeric(parse_opt_value(p, k, kl))
        } else if (k == "plots") {
          v <- tolower(parse_opt_value(p, k, kl))
          if (v != "none") abort_unsupported(paste0("TABLES option PLOTS=", toupper(v)), kl)
        } else {
          abort_unsupported(paste0("TABLES option ", toupper(k), "="), kl)
        }
        next
      }
      if (!k %in% freq_table_opts) abort_unsupported(paste0("TABLES option ", toupper(k)), kl)
      if (k %in% c("chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2", "exact")) freq_stat_gate(k, kl)
      advance(p)
      if (k == "binomial" && tk_type(p) == "lparen") o$binomial_opts <- parse_freq_binomial_opts(p, kl)
      o[[k]] <- TRUE
    }
  }
  expect_semi(p, "TABLES")
  list(stmt = "tables", reqs = reqs, opts = o, line = line)
}

# Translation -------------------------------------------------------------------

translate_proc_freq <- function(step, meta, last) {
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
  exact <- unique(unlist(lapply(of("exact"), `[[`, "kws")))
  tabs <- of("tables")
  requests <- list()
  for (s in tabs) {
    expanded <- list()
    for (r in s$reqs) {
      terms <- list(character())
      for (f in r) {
        vs <- proc_resolve(f, st, s$line)
        terms <- unlist(lapply(terms, function(a) lapply(vs, function(b) c(a, b))), recursive = FALSE)
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
    if (length(rq$vars) > 1 && any(c("binomial") %in% names(rq$opts))) abort_syntax("BINOMIAL is valid only for one-way tables", rq$line)
  }
  stat_opts <- c("chisq", "fisher", "binomial", "cmh", "cmh1", "cmh2")
  if (!is.null(by) && (length(exact) > 0 || any(vapply(requests, function(r) any(stat_opts %in% names(r$opts)), logical(1))))) {
    abort_unsupported("PROC FREQ statistics with BY groups (listing page layout not verified)", by$line)
  }
  outs <- of("output")
  if (length(outs) > 1) abort_unsupported("more than one OUTPUT statement in PROC FREQ", outs[[2]]$line)
  out_stmt <- if (length(outs) > 0) outs[[1]] else NULL
  if (!is.null(out_stmt) && is.null(out_stmt$out)) abort_unsupported("OUTPUT statement without OUT=", out_stmt$line)
  allv <- unique(c(unlist(lapply(requests, `[[`, "vars")), by$vars, w$var))
  spec <- list(
    line = step$line, data = inp$ds$name, print = !isTRUE(o$noprint), order = o$order %||% "internal",
    nlevels = isTRUE(o$nlevels), by = by$vars, by_desc = by$descending, weight = w$var, zeros = isTRUE(w$zeros),
    requests = requests, exact = exact, info = proc_var_info(st, allv),
    output = if (!is.null(out_stmt)) list(name = out_stmt$out$name, ds_opts = out_stmt$out$options, kws = out_stmt$kws)
  )
  header <- sprintf("# ---- PROC FREQ (SAS line %d)", as.integer(step$line))
  code <- c(
    header,
    paste0(".d <- ", proc_source_code(inp)),
    paste0(".r <- sas_proc_freq(.d, ", r_lit(spec), ")"),
    ".lst <- c(.lst, .titles_once(.titles, .r$listing))",
    ".meta <- c(.meta, sas_proc_store(work, .r$out))"
  )
  out_names <- c(unlist(lapply(requests, function(r) r$opts$out$name)), spec$output$name)
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

freq_num <- function(v) {
  if (is.na(v)) return(".")
  if (abs(v - round(v)) < 1e-9 * max(1, abs(v))) return(sprintf("%.0f", v))
  trimws(sas_fmt_best(v, 8L))
}
freq_pct <- function(v) if (is.na(v)) "." else sprintf("%.2f", display_round(v, 2L))

#' PROC FREQ engine
#' @param d Input data (after WHERE).
#' @param sp Specification built by the translator.
#' @export
#' @keywords internal
sas_proc_freq <- function(d, sp) {
  info <- sp$info
  groups <- if (!is.null(sp$by)) proc_by_groups(d, sp$by, sp$by_desc, sp$data, info) else list(seq_len(nrow(d)))
  lst <- character()
  titled <- FALSE
  outs <- list()
  out_rows <- list()
  stat_rows <- list()
  for (g in groups) {
    db <- d[g, , drop = FALSE]
    wt <- if (is.null(sp$weight)) rep(1, nrow(db)) else db[[sp$weight]]
    use <- !is.na(wt)
    if (!sp$zeros && !is.null(sp$weight)) use <- use & wt != 0
    db <- db[use, , drop = FALSE]
    wt <- wt[use]
    glst <- character()
    if (!is.null(sp$by) && length(g) > 0) glst <- c(glst, "", proc_by_line(d, g[1], sp$by, info), "")
    if (sp$nlevels) glst <- c(glst, freq_nlevels_table(db, sp))
    for (rq in sp$requests) {
      res <- freq_one_request(db, wt, rq, sp)
      glst <- c(glst, if (!isTRUE(rq$opts$noprint)) res$listing)
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
    if (sp$print && length(glst) > 0 && (sp$nlevels || any(!vapply(sp$requests, function(r) isTRUE(r$opts$noprint), TRUE)))) {
      if (!titled) {
        # the procedure title goes after the first BY line
        at <- if (!is.null(sp$by)) 3L else 0L
        glst <- append(glst, c("The FREQ Procedure", ""), after = at)
        titled <- TRUE
      }
      lst <- c(lst, glst)
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
    outs[[nm]] <- proc_out_options(data, proc_out_meta(cols), x$opts, x$line)
  }
  if (!is.null(sp$output)) outs[[sp$output$name]] <- freq_output_data(stat_rows, sp)
  list(listing = if (sp$print) lst else character(), out = outs)
}

freq_col_of <- function(v, info) {
  i <- info[[v]]
  list(name = v, disp = i$disp, char = i$char, len = i$len, fmt = i$fmt, informat = i$informat, label = i$label)
}

freq_nlevels_table <- function(db, sp) {
  vars <- unique(unlist(lapply(sp$requests, `[[`, "vars")))
  nl <- vapply(vars, function(v) length(unique(proc_value_text(db[[v]], sp$info[[v]]))), 1L)
  miss <- vapply(vars, function(v) length(unique(proc_value_text(db[[v]][freq_is_missing(db[[v]])], sp$info[[v]]))), 1L)
  if (any(miss > 0)) abort_unsupported("NLEVELS with missing levels (Missing Levels columns)")
  disp <- vapply(vars, function(v) sp$info[[v]]$disp, "")
  w <- max(8L, nchar(disp))
  c("Number of Variable Levels", "", paste0(formatC("Variable", width = -w), "      Levels"), strrep("-", w + 12L),
    paste0(formatC(disp, width = -w), formatC(as.character(nl), width = 12L)), "")
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
  if (missprint && length(vars) > 1) abort_unsupported("MISSPRINT for multi-way tables", rq$line)
  keep <- if (miss_opt || missprint) rep(TRUE, nrow(db)) else !anymiss
  dd <- db[keep, , drop = FALSE]
  ww <- wt[keep]
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
  res$stats <- freq_request_stats(tab, lv, vars, info, o, sp, nmiss)
  res$listing <- c(res$listing, freq_stats_listing(res$stats, tab, vars, info, o, sp))
  res
}

freq_value_cell <- function(lvl, j, info, v) lvl$text[j]

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
  pct <- cnt / tot * 100
  pct[is_m & missprint] <- NA
  cum <- cumsum(ifelse(is_m & missprint, 0, cnt))
  cpct <- cum / tot * 100
  show_pct <- !isTRUE(o$nopercent) && !neg
  show_cum <- !isTRUE(o$nocum)
  # listing
  vals <- l$text
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
  if (show_pct) {
    head <- paste0(head, "     Percent")
    top <- paste0(top, strrep(" ", 12))
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
    if (show_pct) s <- paste0(s, formatC(freq_pct(pct[j]), width = 11L), " ")
    if (show_cum) {
      mp <- is_m[j] && missprint
      if (!isTRUE(o$nofreq)) s <- paste0(s, formatC(if (mp) "." else freq_num(cum[j]), width = 13L), " ")
      if (show_pct) s <- paste0(s, formatC(if (mp) "." else freq_pct(cpct[j]), width = 12L), "  ")
    }
    sas_rtrim(s)
  }, "")
  lst <- c(if (show_cum) sas_rtrim(top), head, strrep("-", width), body)
  if (nmiss > 0) lst <- c(lst, "", paste0(strrep(" ", max(0L, (width - 21L) %/% 2L)), "Frequency Missing = ", freq_num(nmiss)))
  lst <- c(lst, "")
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
  list(listing = lst, out = od, out_cols = out_cols)
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
  show <- c(freq = !isTRUE(o$nofreq), pct = !isTRUE(o$nopercent), row = !isTRUE(o$norow), col = !isTRUE(o$nocol))
  labels <- c(freq = "Frequency", pct = "Percent", row = "Row Pct", col = "Col Pct")[show]
  rvals <- lr$text
  cvals <- lc$text
  R <- max(c(9L, nchar(labels), nchar(rvals) + 1L))
  C <- max(c(8L, nchar(cvals), 7L))
  lst <- c(title %||% paste("Table of", ri$disp, "by", ci$disp), stratum, "")
  # variable headings carry the label in parentheses; a row heading that does
  # not fit before the first column puts the column heading on the next line
  rh <- paste0(ri$disp, if (!is.null(ri$label)) paste0("(", ri$label, ")"))
  ch <- paste0(ci$disp, if (!is.null(ci$label)) paste0("(", ci$label, ")"))
  lst <- c(lst, if (nchar(rh) <= R) paste0(formatC(rh, width = -(R + 1L)), ch) else c(rh, paste0(strrep(" ", R + 1L), ch)))
  # a user-format label keeps its leading blanks in the column heading ('  50+ ')
  chv <- cvals
  if (!is.null(ci$fmt) && !is.null(user_fmt(ci$fmt$name, is.character(lc$values) || isTRUE(ci$fmt$char)))) {
    chv <- substr(sas_rtrim(sas_format(lc$values, ci$fmt)), 1L, C)
    chv[is.na(chv)] <- cvals[is.na(chv)]
  }
  chead <- paste(vapply(chv, function(x) formatC(x, width = if (proc_right_align(ci)) C else -C), ""), collapse = "|")
  for (j in seq_along(labels)) {
    lab <- formatC(labels[j], width = -R)
    lst <- c(lst, if (j < length(labels)) paste0(lab, "|") else paste0(lab, "|", chead, "|  Total"))
  }
  sep <- paste0(strrep("-", R), "+", strrep(paste0(strrep("-", C), "+"), nc))
  lst <- c(lst, sep)
  cell <- function(x) paste0(formatC(x, width = C - 1L), " ")
  for (r in seq_len(nr)) {
    rv <- if (!proc_right_align(ri)) formatC(rvals[r], width = -R) else paste0(formatC(rvals[r], width = R - 1L), " ")
    lines <- character()
    first <- TRUE
    for (s in names(labels)) {
      vals <- vapply(seq_len(nc), function(cc) {
        x <- tab[r, cc]
        switch(s, freq = freq_num(x), pct = freq_pct(x / tot * 100),
               row = if (rt_[r] == 0) "." else freq_pct(x / rt_[r] * 100),
               col = if (ct_[cc] == 0) "." else freq_pct(x / ct_[cc] * 100))
      }, "")
      totv <- switch(s, freq = freq_num(rt_[r]), pct = freq_pct(rt_[r] / tot * 100), "")
      lines <- c(lines, sas_rtrim(paste0(if (first) rv else strrep(" ", R), "|", paste(vapply(vals, cell, ""), collapse = "|"), "|",
                                         formatC(totv, width = 7L))))
      first <- FALSE
    }
    lst <- c(lst, lines, sep)
  }
  tl <- paste0(formatC("Total", width = -R), " ", paste(vapply(ct_, function(x) cell(freq_num(x)), ""), collapse = " "), " ",
               formatC(freq_num(tot), width = 7L))
  lst <- c(lst, sas_rtrim(tl))
  if (show[["pct"]]) {
    lst <- c(lst, sas_rtrim(paste0(strrep(" ", R), " ", paste(vapply(ct_, function(x) cell(freq_pct(x / tot * 100)), ""), collapse = " "),
                                   " ", formatC(if (tot > 0) freq_pct(100) else ".", width = 7L))))
  }
  if (nmiss > 0 && is.null(stratum)) lst <- c(lst, "", paste0("Frequency Missing = ", freq_num(nmiss)))
  if (R + 1L + (C + 1L) * nc + 7L > 132L) abort_unsupported("a crosstabulation wider than the line size (multi-panel tables)")
  lst <- c(lst, "")
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
  list(listing = lst, out = tibble::as_tibble(od), out_cols = out_cols)
}

freq_nway <- function(tab, obs, lv, vars, info, o, nmiss, sp) {
  k <- length(vars)
  dims <- dim(tab)
  tot <- sum(tab)
  combos <- as.matrix(expand.grid(lapply(rev(dims), seq_len)))[, k:1, drop = FALSE]
  combos <- combos[do.call(order, as.data.frame(combos)), , drop = FALSE]
  cnt <- tab[combos]
  nobs <- obs[combos]
  lst <- character()
  if (isTRUE(o$list)) {
    keep <- cnt != 0 | nobs > 0 | isTRUE(o$sparse)
    cmb <- combos[keep, , drop = FALSE]
    cc <- cnt[keep]
    texts <- lapply(seq_len(k), function(j) lv[[j]]$text[cmb[, j]])
    Ws <- vapply(seq_len(k), function(j) max(c(nchar(info[[vars[j]]]$disp), nchar(texts[[j]]))), 1L)
    rights <- vapply(vars, function(v) !info[[v]]$char, TRUE)
    padj <- function(x, j) formatC(x, width = if (rights[j]) Ws[j] else -Ws[j])
    show_pct <- !isTRUE(o$nopercent)
    show_cum <- !isTRUE(o$nocum)
    W <- sum(Ws) + 4L * (k - 1L)
    head <- paste(vapply(seq_len(k), function(j) padj(info[[vars[j]]]$disp, j), ""), collapse = "    ")
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
    cum <- cumsum(cc)
    body <- vapply(seq_along(cc), function(r) {
      s <- paste(vapply(seq_len(k), function(j) padj(texts[[j]][r], j), ""), collapse = "    ")
      s <- paste0(s, formatC(freq_num(cc[r]), width = 12L), " ")
      if (show_pct) s <- paste0(s, formatC(freq_pct(cc[r] / tot * 100), width = 11L), " ")
      if (show_cum) {
        s <- paste0(s, formatC(freq_num(cum[r]), width = 13L), " ")
        if (show_pct) s <- paste0(s, formatC(freq_pct(cum[r] / tot * 100), width = 12L), "  ")
      }
      sas_rtrim(s)
    }, "")
    lst <- c(if (show_cum) sas_rtrim(top), head, strrep("-", width), body)
    if (nmiss > 0) lst <- c(lst, "", paste0(strrep(" ", max(0L, (width - 21L) %/% 2L)), "Frequency Missing = ", freq_num(nmiss)))
    lst <- c(lst, "")
  } else {
    # one crosstabulation per stratum (combination of the leading variables)
    sdims <- dims[seq_len(k - 2L)]
    strata <- as.matrix(expand.grid(lapply(rev(sdims), seq_len)))[, (k - 2L):1, drop = FALSE]
    strata <- strata[do.call(order, as.data.frame(strata)), , drop = FALSE]
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
      lst <- c(lst, tw$listing)
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
  list(listing = lst, out = tibble::as_tibble(od), out_cols = out_cols)
}

# Statistics ---------------------------------------------------------------------------

# Table scores for the Mantel-Haenszel statistic: numeric values, else ranks.
freq_scores <- function(l) {
  v <- l$values
  if (is.numeric(v) && all(!is.na(v))) as.numeric(v) else seq_along(v)
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
  # continuity-corrected forms (SAS: 1/(2n) shift for Wald, the score interval
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

# Cochran-Mantel-Haenszel statistics over the strata of an n-way table
# (array with the row and column variables last).
freq_cmh <- function(tab, lv) {
  k <- length(dim(tab))
  dims <- dim(tab)
  R <- dims[k - 1L]
  C <- dims[k]
  m <- matrix(tab, ncol = R * C)
  if (k == 2) m <- matrix(as.numeric(tab), nrow = 1)
  a <- freq_scores(lv[[k - 1L]])
  b <- freq_scores(lv[[k]])
  G <- numeric(R * C)
  V <- matrix(0, R * C, R * C)
  for (h in seq_len(nrow(m))) {
    th <- matrix(m[h, ], nrow = R)
    nh <- sum(th)
    if (nh < 2) next
    pr <- rowSums(th) / nh
    pc <- colSums(th) / nh
    G <- G + as.numeric(th) - nh * as.numeric(outer(pr, pc))
    vr <- diag(pr, R) - outer(pr, pr)
    vc <- diag(pc, C) - outer(pc, pc)
    V <- V + nh^2 / (nh - 1) * kronecker(vc, vr)
  }
  q <- function(A) {
    g <- A %*% G
    va <- A %*% V %*% t(A)
    as.numeric(t(g) %*% solve(va, g))
  }
  Acor <- matrix(kronecker(b, a), nrow = 1)
  Arms <- kronecker(matrix(1, 1, C) %*% diag(b, C), cbind(diag(R - 1), 0))
  Arms <- kronecker(matrix(b, 1), cbind(diag(R - 1), 0))
  Aga <- kronecker(cbind(diag(C - 1), 0), cbind(diag(R - 1), 0))
  out <- list(cmhcor = q(Acor), df_cmhcor = 1)
  out$p_cmhcor <- stats::pchisq(out$cmhcor, 1, lower.tail = FALSE)
  out$cmhrms <- q(Arms)
  out$df_cmhrms <- R - 1
  out$p_cmhrms <- stats::pchisq(out$cmhrms, R - 1, lower.tail = FALSE)
  out$cmhga <- q(Aga)
  out$df_cmhga <- (R - 1) * (C - 1)
  out$p_cmhga <- stats::pchisq(out$cmhga, (R - 1) * (C - 1), lower.tail = FALSE)
  out
}

freq_request_stats <- function(tab, lv, vars, info, o, sp, nmiss) {
  k <- length(vars)
  want <- c(if (isTRUE(o$chisq)) "chisq", if (isTRUE(o$fisher) || "fisher" %in% sp$exact) "fisher",
            if (isTRUE(o$binomial) || "binomial" %in% sp$exact) "binomial",
            if (any(c(o$cmh, o$cmh1, o$cmh2) %in% TRUE)) "cmh")
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
    alpha <- bo$alpha %||% o$alpha %||% 0.05
    st <- c(st, freq_binomial(cnt_nm, alpha, bo$p0 %||% 0.5, sel, bo$cl, isTRUE(bo$correct)))
    st$bin_level <- txt[sel]
    st$bin_var <- vars[1]
    st$bin_cl <- bo$cl
    st$bin_exact <- isTRUE(bo$exact) || "binomial" %in% sp$exact
  }
  if (any(c("chisq", "fisher") %in% want)) {
    if (k == 1) {
      if ("fisher" %in% want) abort_unsupported("FISHER for a one-way table")
      cnt <- as.numeric(tab)
      cnt <- cnt[cnt > 0]
      e <- sum(cnt) / length(cnt)
      x2 <- sum((cnt - e)^2 / e)
      st <- c(st, list(pchi = x2, df_pchi = length(cnt) - 1, p_pchi = stats::pchisq(x2, length(cnt) - 1, lower.tail = FALSE),
                       oneway_small = e < 5))
      st$oneway_chi <- TRUE
    } else {
      if (k > 2) abort_unsupported("CHISQ / FISHER for n-way tables (per-stratum statistics)")
      tb <- matrix(as.numeric(tab), nrow = dim(tab)[1])
      rs <- freq_scores(lv[[1]])
      cs <- freq_scores(lv[[2]])
      keep_r <- rowSums(tb) > 0
      keep_c <- colSums(tb) > 0
      if (sum(keep_r) < 2 || sum(keep_c) < 2) abort_unsupported("CHISQ for a table with fewer than two nonzero rows or columns")
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
      }
      if ("fisher" %in% want || ("chisq" %in% want && nrow(tb) == 2 && ncol(tb) == 2)) {
        if (any(abs(tb - round(tb)) > 1e-9)) abort_unsupported("FISHER with non-integer weights")
        st$fisher <- freq_fisher(tb[keep_r, keep_c, drop = FALSE])
      }
    }
  }
  if ("cmh" %in% want) {
    if (k < 2) abort_syntax("CMH requires a two-way or n-way table")
    st <- c(st, freq_cmh(tab, lv))
  }
  # development hook: the conformance harness collects the computed statistics
  cap <- getOption("sasonr.freq_capture")
  if (is.environment(cap)) cap$stats <- c(cap$stats, list(st))
  st
}

# The BINOMIAL block of a one-way table: proportion, confidence limits and the
# asymptotic (and optionally exact) test.  Column widths are the ones SAS uses:
# the proportion block is 32 columns wide, 22 when CL= is given (the limits
# then go in their own table), 35 when the exact test adds its longer labels.
freq_binomial_listing <- function(st, info) {
  f4 <- function(v) if (is.na(v)) "." else sprintf("%.4f", display_round(v, 4L))
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
    out <- c(out, line(paste0(conf_pct, "% Lower Conf Limit"), f4(st$cl$wald[1]), w_prop),
             line(paste0(conf_pct, "% Upper Conf Limit"), f4(st$cl$wald[2]), w_prop), "",
             "Exact Conf Limits",
             line(paste0(conf_pct, "% Lower Conf Limit"), f4(st$cl$exact[1]), w_prop),
             line(paste0(conf_pct, "% Upper Conf Limit"), f4(st$cl$exact[2]), w_prop), "")
  } else {
    types <- if (identical(cl, "all")) c("wald", "wilson", "exact", "agresticoull", "jeffreys") else cl
    types <- unique(unlist(lapply(types, function(t) {
      if (t == "all") c("wald", "wilson", "exact", "agresticoull", "jeffreys")
      else if (t %in% c("wald", "wilson")) if (isTRUE(st$correct)) paste0(t, "_corrected") else t
      else t
    })))
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
  out <- c(out, paste0("  Test of H0: Proportion = ", freq_num(st$p0)), "")
  use_c <- isTRUE(st$correct) && !is.null(cl)
  zt <- if (use_c) st$zc_bin else st$z_bin
  pone <- if (use_c) st$plc_bin else st$pl_bin
  ptwo <- if (use_c) st$p2c_bin else st$p2_bin
  side <- if (zt >= 0) ">" else "<"
  out <- c(out,
           line("ASE under H0", f4(st$e0_bin), w_test),
           line("Z", f4(zt), w_test),
           line(paste0("One-sided Pr ", side, "  Z"), pv(pone), w_test),
           line("Two-sided Pr > |Z|", pv(ptwo), w_test))
  if (use_c) out <- c(out, "", "The asymptotic test includes a continuity correction.")
  if (exact) {
    l0 <- max(nchar(c("One-sided Pr >=  P", "Two-sided = 2 * One-sided"))) + 4L
    side_exact <- if (st$bin >= st$p0) ">=" else "<="
    out <- c(out, "", "Exact Test",
             line(paste0("One-sided Pr ", side_exact, "  P"), pv(st$xpl_bin), l0 + 6L),
             line("Two-sided = 2 * One-sided", pv(st$xp2_bin), l0 + 6L))
  }
  c(out, "", freq_sample_size(st))
}

freq_stats_listing <- function(st, tab, vars, info, o, sp) {
  if (!is.null(st$bin_var)) return(freq_binomial_listing(st, info))
  out <- character()
  k <- length(vars)
  f4 <- function(v) if (is.na(v)) "." else sprintf("%.4f", display_round(v, 4L))
  pv <- function(v) trimws(fmt_pvalue(v, 6L, 4L))
  if (isTRUE(st$oneway_chi) && isTRUE(o$chisq)) {
    out <- c(out, "   Chi-Square Test", "for Equal Proportions", "",
             paste0("Chi-Square", formatC(f4(st$pchi), width = 10L)), paste0("DF", formatC(sprintf("%.0f", st$df_pchi), width = 18L)),
             paste0("Pr > ChiSq", formatC(pv(st$p_pchi), width = 10L)), "",
             if (isTRUE(st$oneway_small)) c("WARNING: The table cells have expected counts less",
                                             "         than 5. Chi-Square may not be a valid test.", ""),
             freq_sample_size(st))
  } else if (isTRUE(st$nostat) && k == 2) {
    out <- c(out, paste("Statistics for Table of", info[[vars[1]]]$disp, "by", info[[vars[2]]]$disp), "",
             "Row or column sum zero. No statistics computed for this table.", "", freq_sample_size(st))
  } else if ((isTRUE(o$chisq) || isTRUE(o$fisher) || "fisher" %in% sp$exact) && k == 2 && !is.null(st$pchi)) {
    # FISHER also prints the chi-square table (SAS computes it with every 2-way test)
    rows <- list(c("Chi-Square", st$df_pchi, st$pchi, st$p_pchi), c("Likelihood Ratio Chi-Square", st$df_lrchi, st$lrchi, st$p_lrchi))
    if (!is.null(st$ajchi)) rows <- c(rows, list(c("Continuity Adj. Chi-Square", 1, st$ajchi, st$p_ajchi)))
    rows <- c(rows, list(c("Mantel-Haenszel Chi-Square", 1, st$mhchi, st$p_mhchi)))
    body <- vapply(rows, function(r) paste0(formatC(r[1], width = -27L), formatC(sprintf("%.0f", as.numeric(r[2])), width = 5L),
                                            formatC(f4(as.numeric(r[3])), width = 12L), formatC(pv(as.numeric(r[4])), width = 10L)), "")
    body <- c(body, vapply(list(c("Phi Coefficient", st$phi), c("Contingency Coefficient", st$contgy), c("Cramer's V", st$cramv)),
                           function(r) paste0(formatC(r[1], width = -32L), formatC(f4(as.numeric(r[2])), width = 12L)), ""))
    out <- c(out, paste("Statistics for Table of", info[[vars[1]]]$disp, "by", info[[vars[2]]]$disp), "",
             paste0(formatC("Statistic", width = -27L), "   DF       Value      Prob"), strrep("-", 54), body, "")
    if (isTRUE(st$small > 0) && !isTRUE(o$nowarn)) {
      out <- c(out, sprintf(" WARNING: %.0f%% of the cells have expected counts less", st$small * 100),
               "          than 5. Chi-Square may not be a valid test.", "")
    }
    if (is.null(st$fisher)) out <- c(out, freq_sample_size(st))
  }
  if (!is.null(st$fisher) && k == 2) {
    fz <- st$fisher
    if (!is.null(fz$xpl)) {
      out <- c(out, "       Fisher's Exact Test", strrep("-", 34),
               paste0(formatC("Cell (1,1) Frequency (F)", width = -26L), formatC(freq_num(fz$fisher_f), width = 8L)),
               paste0(formatC("Left-sided Pr <= F", width = -26L), formatC(pv(fz$xpl), width = 8L)),
               paste0(formatC("Right-sided Pr >= F", width = -26L), formatC(pv(fz$xpr), width = 8L)), "",
               paste0(formatC("Table Probability (P)", width = -26L), formatC(pv(fz$xpt), width = 8L)),
               paste0(formatC("Two-sided Pr <= P", width = -26L), formatC(pv(fz$xp2), width = 8L)), "",
               freq_sample_size(st))
    } else {
      out <- c(out, "       Fisher's Exact Test", strrep("-", 34),
               paste0(formatC("Table Probability (P)", width = -26L), formatC(pv(fz$xpt), width = 8L)),
               paste0(formatC("Pr <= P", width = -26L), formatC(pv(fz$xp2), width = 8L)), "",
               freq_sample_size(st))
    }
  }
  if (!is.null(st$bin)) {
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
  out
}

# "Sample Size = n" after the statistics of a table, with the missing count
# and the SAS warning when at least 20% of the data are missing.
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
  exp <- unlist(lapply(kws, function(k) switch(k,
    chisq = c("pchi", "lrchi", "ajchi", "mhchi", "phi", "contgy", "cramv"),
    cmh = c("cmhcor", "cmhrms", "cmhga"), cmh1 = "cmhcor", cmh2 = c("cmhcor", "cmhrms"),
    binomial = abort_unsupported("PROC FREQ OUTPUT BINOMIAL (output variable layout not verified)"),
    k)))
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
    if (!is.null(c$key)) return(vapply(rows, function(r) as.numeric(r[[c$key]] %||% NA_real_), 1))
    v <- lapply(rows, function(r) r[[c$name]])
    if (c$char) vapply(v, as.character, "") else vapply(v, as.numeric, 1)
  })
  names(data) <- vapply(cols, `[[`, "", "name")
  proc_out_options(tibble::as_tibble(data), proc_out_meta(cols), sp$output$ds_opts, sp$line)
}
