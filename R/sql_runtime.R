# PROC SQL run-time helpers: SQL-only expression functions, the DuckDB
# back end (join candidate pairs and grouped aggregation), and the SQL
# report (listing).

# Expression helpers ------------------------------------------------------------

# Equality key of a column: character values without trailing blanks;
# special missing values (.A-.Z, ._) as distinct sentinels below every number.
sql_norm_key <- function(x) {
  if (is.character(x)) return(sas_rt_rtrim(x))
  code <- sas_miss_code(x)
  sp <- !is.na(code) & code != 1L
  x[is.na(x)] <- NA_real_
  if (any(sp)) x[sp] <- -1.7e308 + (code[sp] + 1) * 1e300
  x
}

#' @export
sas_f_sqlcase <- function(...) {
  a <- list(...)
  k <- length(a)
  els <- a[[k]]
  if (any(lengths(a) == 0)) return(els[0])
  n <- max(lengths(a))
  out <- rep_len(els, n)
  done <- rep(FALSE, n)
  for (j in seq(1L, k - 1L, by = 2L)) {
    hit <- rep_len(sas_rt_true(a[[j]]), n) & !done
    if (any(hit)) out[hit] <- rep_len(a[[j + 1L]], n)[hit]
    done <- done | hit
  }
  out
}

#' @export
sas_f_sqlinlist <- function(x, ...) {
  vals <- list(...)
  res <- rep(FALSE, length(x))
  for (v in vals) res <- res | sas_cmp(x, rep_len(v, length(x))) == 0L
  as.numeric(res)
}

#' @export
sas_f_sqllike <- function(x, pattern, escape = NULL) {
  n <- max(length(x), length(pattern))
  x <- rep_len(sas_rt_rtrim(x), n)
  pattern <- rep_len(sas_rt_rtrim(pattern), n)
  esc <- if (is.null(escape)) "" else sas_rt_rtrim(escape)[1]
  pats <- unique(pattern)
  out <- logical(n)
  for (pt in pats) {
    cps <- strsplit(pt, "")[[1]]
    rx <- character()
    k <- 1L
    while (k <= length(cps)) {
      ch <- cps[k]
      if (nzchar(esc) && ch == esc && k < length(cps)) {
        rx <- c(rx, sql_rx_quote(cps[k + 1L]))
        k <- k + 2L
        next
      }
      rx <- c(rx, if (ch == "%") ".*" else if (ch == "_") "." else sql_rx_quote(ch))
      k <- k + 1L
    }
    re <- paste0("^", paste(rx, collapse = ""), "$")
    sel <- pattern == pt
    out[sel] <- grepl(re, x[sel], perl = TRUE)
  }
  as.numeric(out)
}

sql_rx_quote <- function(ch) if (grepl("[][{}()*+?.\\\\^$|]", ch)) paste0("\\", ch) else ch

#' @export
sas_f_sqlquant <- function(x, values, op, quant) {
  f <- switch(op, "=" = sas_rt_eq, "^=" = sas_rt_ne, "<" = sas_rt_lt, "<=" = sas_rt_le, ">" = sas_rt_gt, ">=" = sas_rt_ge)
  res <- if (quant == "all") rep(TRUE, length(x)) else rep(FALSE, length(x))
  for (v in values) {
    hit <- sas_rt_true(f(x, v))
    res <- if (quant == "all") res & hit else res | hit
  }
  as.numeric(res)
}

#' @export
sas_f_sqlmonotonic <- function() as.numeric(seq_len(get(".sql_n", envir = parent.frame())))

fn_table <- utils::modifyList(fn_table, list(
  sqlcase = fn_spec("num", 3, Inf), sqlinlist = fn_spec("num", 2, Inf), sqllike = fn_spec("num", 2, 3),
  sqlmonotonic = fn_spec("num", 0)
))

# DuckDB back end ---------------------------------------------------------------
# The connection belongs to the session (`SasSession$sql`): opened on first use,
# closed by SasSession$close() (end of a throw-away sas_run() session,
# sas_session_close() or garbage collection).

sql_duck <- function(eng) {
  sql <- eng$session$sql
  if (is.null(sql$con)) {
    drv <- suppressMessages(duckdb::duckdb(dbdir = ":memory:"))
    sql$drv <- drv
    sql$con <- DBI::dbConnect(drv)
    DBI::dbExecute(sql$con, "SET threads = 1")
  }
  sql$con
}

# `sql`: the SQL back-end environment of a session.
sql_duck_close <- function(sql) {
  if (!is.null(sql$con)) {
    try(DBI::dbDisconnect(sql$con), silent = TRUE)
    try(duckdb::duckdb_shutdown(sql$drv), silent = TRUE)
    sql$con <- NULL
    sql$drv <- NULL
  }
  invisible()
}

sql_duck_query <- function(eng, frames, sql) {
  con <- sql_duck(eng)
  for (nm in names(frames)) duckdb::duckdb_register(con, nm, frames[[nm]], overwrite = TRUE)
  on.exit(for (nm in names(frames)) try(duckdb::duckdb_unregister(con, nm), silent = TRUE))
  DBI::dbGetQuery(con, sql)
}

# Candidate row pairs of an equi-join: missing values match missing values,
# character keys compare without trailing blanks.
sql_duck_pairs <- function(eng, lkeys, rkeys) {
  nl <- if (length(lkeys)) length(lkeys[[1]]) else 0L
  nr <- if (length(rkeys)) length(rkeys[[1]]) else 0L
  if (nl == 0 || nr == 0) return(list(l = integer(), r = integer()))
  kn <- paste0("k", seq_along(lkeys))
  L <- stats::setNames(lapply(lkeys, sql_norm_key), kn)
  R <- stats::setNames(lapply(rkeys, sql_norm_key), kn)
  L <- data.frame(c(list(rid = seq_len(nl)), L), stringsAsFactors = FALSE)
  R <- data.frame(c(list(rid = seq_len(nr)), R), stringsAsFactors = FALSE)
  on <- paste(sprintf("l.%s IS NOT DISTINCT FROM r.%s", kn, kn), collapse = " AND ")
  res <- sql_duck_query(eng, list(sasonr_l = L, sasonr_r = R),
                        paste0("SELECT l.rid AS lr, r.rid AS rr FROM sasonr_l l JOIN sasonr_r r ON ", on, " ORDER BY lr, rr"))
  list(l = vctrs::vec_cast(res$lr, integer()), r = vctrs::vec_cast(res$rr, integer()))
}

sql_agg_sql <- function(fn, col, distinct) {
  d <- if (distinct) "DISTINCT " else ""
  switch(fn,
    count = , freq = , n = if (is.null(col)) "count(*)" else sprintf("count(%s%s)", d, col),
    sum = sprintf("sum(%s%s)", d, col),
    avg = , mean = sprintf("avg(%s%s)", d, col),
    min = sprintf("min(%s)", col),
    max = sprintf("max(%s)", col),
    nmiss = sprintf("count(*) - count(%s)", col),
    std = sprintf("stddev_samp(%s%s)", d, col),
    var = sprintf("var_samp(%s%s)", d, col),
    range = sprintf("max(%s) - min(%s)", col, col),
    uss = sprintf("sum(%s * %s)", col, col),
    css = sprintf("CASE WHEN count(%s) = 0 THEN NULL WHEN count(%s) = 1 THEN 0 ELSE var_samp(%s) * (count(%s) - 1) END", col, col, col, col),
    cv = sprintf("100 * stddev_samp(%s) / avg(%s)", col, col),
    stderr = sprintf("stddev_samp(%s) / sqrt(count(%s))", col, col),
    sumwgt = sprintf("CASE WHEN count(%s) = 0 THEN NULL ELSE count(%s) END", col, col),
    median = sprintf("median(%s)", col),
    abort_unsupported(paste0("summary function ", toupper(fn), "()"))
  )
}

# Grouped aggregation: `gid` group of each row (1..G), `aggs` list of
# list(fn, arg (vector or NULL for *), distinct). Returns a list of vectors of
# length G.  `order_known`: the rows are in the order in which they are read
# (see sql_select()); otherwise order-dependent statistics fail loud unless
# their value does not depend on the order.
sql_duck_aggregate <- function(eng, gid, G, aggs, order_known = TRUE, line = NULL) {
  if (length(aggs) == 0) return(list())
  cols <- list(g = vctrs::vec_cast(gid, integer()))
  exprs <- character()
  for (k in seq_along(aggs)) {
    a <- aggs[[k]]
    cn <- NULL
    if (!is.null(a$arg)) {
      cn <- paste0("a", k)
      v <- a$arg
      if (is.character(v)) {
        v <- sas_rt_rtrim(v)
        v[v == ""] <- NA_character_
        if (!a$fn %in% c("count", "freq", "n", "nmiss", "min", "max")) {
          abort_syntax(paste0("The ", toupper(a$fn), " summary function requires a numeric argument."))
        }
      } else {
        v[is.na(v)] <- NA_real_
      }
      cols[[cn]] <- v
    }
    exprs <- c(exprs, paste0(sql_agg_sql(a$fn, cn, isTRUE(a$distinct)), " AS v", k))
  }
  frame <- as.data.frame(cols, stringsAsFactors = FALSE)
  res <- sql_duck_query(eng, list(sasonr_g = frame),
                        paste0("SELECT g, ", paste(exprs, collapse = ", "), " FROM sasonr_g GROUP BY g"))
  out <- lapply(seq_along(aggs), function(k) {
    a <- aggs[[k]]
    sql_stat_gate(a$fn, line)
    # moments in R (stats_exact.R; DuckDB accumulates differently)
    if (a$fn %in% sql_moment_fns && is.numeric(a$arg)) {
      return(sql_r_moment(a$fn, as.numeric(a$arg), gid, G, order_known && !isTRUE(a$distinct), isTRUE(a$distinct), line))
    }
    v <- res[[paste0("v", k)]]
    full <- if (is.character(v)) rep(NA_character_, G) else rep(NA_real_, G)
    if (!is.character(v)) {
      # an overflow is not supported in PROC SQL: fail loud, never a silent missing value
      v <- stats_finite_check(as.numeric(v), "a PROC SQL summary function result")
    }
    full[res$g] <- v
    if (aggs[[k]]$fn %in% c("count", "freq", "n", "nmiss")) full[is.na(full)] <- 0
    if (a$fn == "median" && is.numeric(a$arg) && any(full %in% 0)) {
      negz <- tapply(a$arg %in% 0 & 1 / a$arg < 0, factor(gid, levels = seq_len(G)), any)
      if (any(negz & full %in% 0)) {
        abort_unsupported("PROC SQL MEDIAN() of 0 for data that contain both 0 and -0 (sign of the result not determined)", line)
      }
    }
    full
  })
  out
}

sql_moment_fns <- c("sum", "avg", "mean", "std", "var", "css", "cv", "stderr", "uss")

# PROC statistics registry name of a summary function
sql_stat_name <- function(fn) switch(fn, avg = "MEAN", count = , freq = "N", toupper(fn))

sql_stat_gate <- function(fn, line) proc_stat_check("SQL", sql_stat_name(fn), line)

sql_r_moment <- function(fn, v, gid, G, order_known = TRUE, distinct = FALSE, line = NULL) {
  out <- rep(NA_real_, G)
  idx <- split(seq_along(v), factor(gid, levels = seq_len(G)))
  for (g in seq_len(G)) {
    x <- v[idx[[g]]]
    x <- x[!is.na(x)]
    if (distinct) x <- sort(unique(x))
    if (length(x) == 0) next
    if (fn != "sum" && !stats_fma_range_ok(x)) {
      abort_unsupported(paste0("PROC SQL ", sql_stat_name(fn), "() of values beyond 1E140 or below 1E-140 in magnitude"), line)
    }
    if (!order_known && !stats_order_free(fn, x)) {
      abort_unsupported(paste0("PROC SQL ", sql_stat_name(fn), "() of rows without a defined order ",
                               "(GROUP BY on a data set not sorted by the group columns, a join, a view or DISTINCT): ",
                               "the result depends on the summation order"), line)
    }
    out[g] <- stats_sql_moment(fn, x)
  }
  # an overflowed SUM is missing (the sum of 1e308 and 1e308); other
  # overflows are not supported and fail loud
  if (fn == "sum") out[is.infinite(out)] <- NA_real_
  stats_finite_check(out, "a PROC SQL summary function result")
}

# Listing -------------------------------------------------------------------------

sql_col_width <- function(c) {
  if (!is.null(c$format)) {
    w <- c$format$w
    if (is.na(w)) w <- fmt_width(c$format, c$type == "char")
    if (!is.na(w)) return(vctrs::vec_cast(w, integer()))
  }
  if (c$type == "char") vctrs::vec_cast(c$len, integer()) else 8L
}

# A column header wider than its column is split in two at an underscore
# (kept on the first line) or a blank, choosing the split with the shortest
# longer line (the later one on ties); the column is widened to what remains.
sql_header_lines <- function(h, width) {
  n <- nchar(h)
  if (n <= width) return(list(lines = h, width = width))
  # a label with blanks wraps word by word at the width of the column or of
  # its longest word ("Number of Physical Observations" over 12: three
  # lines) [measured, DICTIONARY.TABLES labels]
  if (grepl(" ", h, fixed = TRUE) && !grepl("_", h, fixed = TRUE) && width > 0L) {
    w <- strsplit(h, " ", fixed = TRUE)[[1]]
    if (any(!nzchar(w))) abort_unsupported(paste0("the PROC SQL heading '", h, "' (several blanks in a row)"))
    lim <- max(width, nchar(w))
    lines <- character()
    cur <- ""
    for (x in w) {
      if (!nzchar(cur)) cur <- x else if (nchar(cur) + 1L + nchar(x) <= lim) cur <- paste(cur, x) else {
        lines <- c(lines, cur)
        cur <- x
      }
    }
    lines <- c(lines, cur)
    return(list(lines = lines, width = lim))
  }
  ch <- strsplit(h, "")[[1]]
  best <- NULL
  for (i in seq_len(n - 1L)) {
    # a leading underscore is not a split point
    if (i == 1L && ch[i] == "_") next
    if (ch[i] == "_") cand <- c(substr(h, 1L, i), substr(h, i + 1L, n))
    else if (ch[i] == " ") cand <- c(substr(h, 1L, i - 1L), substr(h, i + 1L, n))
    else next
    m <- max(nchar(cand))
    if (is.null(best) || m <= best$m) best <- list(m = m, lines = cand)
  }
  if (!is.null(best)) return(list(lines = best$lines, width = max(width, best$m)))
  list(lines = h, width = n)
}

#' PROC SQL report for a query result (as written to the listing)
#' @keywords internal
sas_sql_listing <- function(rel, titles = character(), number = FALSE) {
  cols <- rel$cols
  d <- rel$data
  hdrs <- vapply(cols, function(c) c$label %||% c$disp %||% "", "")
  widths <- vapply(cols, sql_col_width, 1L)
  is_num <- vapply(cols, function(c) c$type == "num", logical(1))
  # numeric headers are split; character columns are widened to the header
  hl <- Map(function(h, w, num) if (num) sql_header_lines(h, w) else list(lines = h, width = max(w, nchar(h))),
            hdrs, widths, is_num)
  widths <- vapply(hl, `[[`, 1L, "width")
  nlines <- max(c(1L, vapply(hl, function(x) length(x$lines), 1L)))
  hl <- lapply(hl, function(x) c(rep("", nlines - length(x$lines)), x$lines))
  cells <- lapply(seq_along(cols), function(k) {
    c <- cols[[k]]
    v <- d[[c$id]]
    if (c$type == "char") {
      s <- if (is.null(c$format)) v else sas_format(v, c$format)
      return(formatC(sas_rt_rtrim(s), width = -widths[k], flag = "-"))
    }
    s <- if (is.null(c$format)) sas_fmt_best(v, 8L) else sas_format(v, c$format)
    # the listing right-aligns missing values (w.d puts them in the units column)
    s[is.na(v)] <- trimws(s[is.na(v)])
    formatC(s, width = widths[k])
  })
  head <- lapply(seq_len(nlines), function(j) vapply(seq_along(cols), function(k) {
    h <- hl[[k]][j]
    if (is_num[k]) formatC(h, width = widths[k]) else formatC(h, width = -widths[k], flag = "-")
  }, ""))
  if (number) {
    rw <- max(6L, nchar(nrow(d)))
    head <- lapply(seq_len(nlines), function(j) c(formatC(if (j == nlines) "Row" else "", width = rw), head[[j]]))
    cells <- c(list(formatC(as.character(seq_len(nrow(d))), width = rw)), cells)
    widths <- c(rw, widths)
  }
  # a report wider than the line: the character columns are narrowed by a
  # rule not measured (fail loud)
  if (sum(widths) + 2L * (length(widths) - 1L) > rt_linesize()) {
    abort_unsupported("a PROC SQL report wider than LINESIZE (the narrowing of its columns is not implemented)")
  }
  # centring units: each TITLE line, the table
  top <- if (length(titles) > 0) lst_lines(c(titles, "")) else NULL
  out <- c(vapply(head, function(h) sas_rt_rtrim(paste(h, collapse = "  ")), ""),
           strrep("-", sum(widths) + 2L * (length(widths) - 1L)))
  if (nrow(d) > 0) out <- c(out, sas_rt_rtrim(rlang::exec(paste, !!!unname(cells), sep = "  ")))
  lst_c(top, lst_unit(c(out, "")))
}
