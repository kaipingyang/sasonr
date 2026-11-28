# PROC PRINT on the PROC registry (proc_registry.R).
#
# print_compute() returns one `sasonr_ods_table` per page (a BY group, or the
# whole listing without BY) with the printed columns as raw values; the
# listing template base.print lays them out.  Layout rule, per page and per
# column (measured on 15 listings of the reference output):
#   fw  widest entry of the column on the page (values, subtotal, grand total
#       on the last page, the BY label of a subtotal in the first column);
#   W   max(fw, widest heading line); columns are 4 blanks apart;
#   a formatted number's field is at least the format width;
#   the field starts at floor((W - fw) / 2) for a character column and a
#   formatted number, ceiling(...) for an unformatted number (and Obs) and
#   when the heading has more than one line; numbers are right-aligned in the
#   field, character values left-aligned; a one-line heading starts at
#   floor((W - width) / 2) over a character column, ceiling(...) over an
#   unformatted number, W - width (right-aligned) over a formatted number;
#   heading lines of several lines are centred with ceiling(...) and stacked
#   at the bottom [column positions measured on a grid of 120 columns,
#   print_layout].

print_compute <- function(df, spec) {
  # an empty data set prints nothing
  if (nrow(df) == 0) return(list(tables = list(), data = list(), events = character()))
  cols <- c(if (!is.null(spec$id)) list(spec$id), spec$cols)
  obs <- is.null(spec$id) && !isTRUE(spec$noobs)
  # the log after the not-sorted ERROR (observations read) is not measured
  groups <- tryCatch(proc_by_groups(df, spec$by, rep(FALSE, length(spec$by)), spec$data, spec$by_info),
                     sasonr_runtime_error = function(e) abort_unsupported(paste0("PROC PRINT BY on data that are not sorted (", conditionMessage(e), ")"), spec$line))
  sums <- spec$sums %||% character()
  # the text of each column over the whole data set, so that the decimals of
  # unformatted numbers line up across pages and with the sums
  sum_of <- function(x) {
    v <- x[is.na(sas_miss_code(x))]
    if (length(v) == 0) NA_real_ else Reduce(`+`, v)
  }
  sub_tot <- lapply(sums, function(v) vapply(groups, function(g) sum_of(df[[v]][g]), 0))
  grand <- lapply(sums, function(v) sum_of(df[[v]]))
  names(sub_tot) <- names(grand) <- sums
  n <- nrow(df)
  ng <- length(groups)
  text <- lapply(cols, function(s) {
    x <- df[[s$name]]
    extra <- if (s$name %in% sums) c(sub_tot[[s$name]], grand[[s$name]]) else NULL
    all <- if (s$char) {
      # character values are written left-justified (leading blanks dropped)
      sub("^ +", "", if (is.null(s$fmt)) sas_rt_rtrim(x) else sas_rt_rtrim(sas_format(x, s$fmt)))
    } else {
      y <- c(x, extra)
      # a formatted number keeps the blanks its format writes after the value
      if (is.null(s$fmt)) print_num_column(y) else sub("^ +", "", sas_format(y, s$fmt))
    }
    list(values = all[seq_len(n)], sub = if (!is.null(extra)) all[n + seq_len(ng)], grand = if (!is.null(extra)) all[n + ng + 1L],
         rounded = attr(all, "rounded", exact = TRUE), dec = attr(all, "dec", exact = TRUE),
         sfw = attr(all, "sfw", exact = TRUE), hdec = attr(all, "hdec", exact = TRUE))
  })
  names(text) <- vapply(cols, `[[`, "", "name")
  sub_rows <- length(sums) > 0 && length(spec$by) > 0
  tables <- lapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    x <- tibble::as_tibble(stats::setNames(lapply(cols, function(s) df[[s$name]][g]), names(text)))
    if (obs) x <- tibble::add_column(x, Obs = as.integer(df$.obs[g]), .before = 1L)
    by <- if (length(spec$by) > 0) list(values = lapply(stats::setNames(spec$by, spec$by), function(v) df[[v]][g[1]]),
                                        text = proc_by_line(df, g[1], spec$by, spec$by_info), group = k) else NULL
    tb <- new_sasonr_ods_table(x, name = "Print", label = "Data Set", path = "Print.Print", proc = "Print",
                            template = "base.print", by = by,
                            cols = ods_cols(names(x), role = "var"))
    attr(tb, "print") <- list(
      cols = c(if (obs) list(list(name = "Obs", head = spec$obs_label, char = FALSE)),
               lapply(cols, function(s) list(name = s$name, head = s$label %||% s$disp, label = !is.null(s$label), char = s$char,
                                             fw = if (!s$char && !is.null(s$fmt)) fmt_width(s$fmt, FALSE),
                                             # a number under a user-defined format is laid out as text
                                             label_fmt = !s$char && !is.null(s$fmt) && !is.null(user_fmt(s$fmt$name, FALSE))))),
      text = c(if (obs) list(list(values = as.character(df$.obs[g]))),
               lapply(names(text), function(nm) list(values = text[[nm]]$values[g], sub = text[[nm]]$sub[k],
                                                     grand = if (k == ng) text[[nm]]$grand, rounded = text[[nm]]$rounded,
                                                     dec = text[[nm]]$dec, sfw = text[[nm]]$sfw, hdec = text[[nm]]$hdec))),
      sums = sums, sub_label = if (sub_rows) spec$by_info[[spec$by]]$disp else NULL,
      last = k == ng, n = if (isTRUE(spec$n)) length(g) else NULL, split = spec$split, first = k == 1L,
      row_header = obs || !is.null(spec$id), id_n = spec$id_n %||% 0L
    )
    tb
  })
  list(tables = tables, data = list(), events = character())
}

# The heading lines of one column: a label split at the SPLIT= character, a
# label split at blanks when it is wider than the field (two words measured),
# a variable name split by print_header_lines().
print_heading <- function(c, fw, split) {
  h <- c$head
  if (isTRUE(c$label) && !is.null(split)) return(strsplit(h, split, fixed = TRUE)[[1]])
  if (isTRUE(c$label)) {
    if (nchar(h) <= fw) return(h)
    w <- strsplit(trimws(h), " +")[[1]]
    if (length(w) <= 2L) return(w)
    # (measured with 5 words) the narrowest width at which the words, put on
    # lines greedily, take at most three lines
    if (length(w) > 5L) abort_unsupported("a PROC PRINT label of more than five words without SPLIT=")
    for (wd in seq.int(max(nchar(w)), nchar(h))) {
      lines <- character()
      cur <- ""
      for (x in w) {
        if (!nzchar(cur)) cur <- x else if (nchar(cur) + 1L + nchar(x) <= wd) cur <- paste(cur, x) else {
          lines <- c(lines, cur)
          cur <- x
        }
      }
      lines <- c(lines, cur)
      if (length(lines) <= 3L) return(lines)
    }
    return(h)
  }
  if (c$name == "Obs" && is.null(c$label)) return(h)
  print_header_lines(h, fw)$lines
}

listing_base_print <- function(tb) {
  a <- attr(tb, "print", exact = TRUE)
  sums <- a$sums
  nc <- length(a$cols)
  label_first <- !is.null(a$sub_label)
  fws <- vapply(seq_len(nc), function(j) {
    t <- a$text[[j]]
    e <- c(t$values, if (a$cols[[j]]$name %in% sums) c(t$sub, t$grand), if (label_first && j == 1L) a$sub_label)
    # an unformatted number: one more position when a value is negative
    sw <- if (!is.null(t$sfw) && length(e)) max(nchar(sub("^-", "", e))) + any(startsWith(e, "-"), na.rm = TRUE) else 0L
    max(0L, nchar(e), a$cols[[j]]$fw, sw)
  }, 1L)
  heads <- lapply(seq_len(nc), function(j) print_heading(a$cols[[j]], fws[j], a$split))
  widths <- pmax(fws, vapply(heads, function(h) max(nchar(h)), 1L))
  # the gap between columns: 4, or the widest of 3, 2, 1 that fits the table
  # on the line (measured, print_wide); a table that does not fit with a gap of
  # 1 is split into column panels, which is not implemented
  gap <- print_fit_gap(widths)
  panels <- NULL
  if (is.na(gap)) {
    wide <- print_wide_plan(a, heads, fws, widths, attr(tb, "by", exact = TRUE))
    heads <- wide$heads
    widths <- wide$widths
    panels <- wide$panels
    gap <- wide$gap
  }
  # c: character, f: formatted number (the field is the format width), n: unformatted number or Obs
  rounded <- vapply(a$text, function(t) t$rounded %||% FALSE, NA)
  if (anyNA(rounded) && any(vapply(heads, function(h) max(nchar(h)), 1L)[is.na(rounded)] < fws[is.na(rounded)])) {
    abort_unsupported("the PROC PRINT heading of an unformatted numeric column of large magnitudes")
  }
  # an unformatted column rounded to decimals held at 2 is laid out like a formatted one
  kind <- vapply(seq_len(nc), function(j) {
    c <- a$cols[[j]]
    if (isTRUE(c$char) || isTRUE(c$label_fmt)) "c" else if (!is.null(c$fw) || isTRUE(rounded[j])) "f" else "n"
  }, "")
  off <- ifelse(kind == "n", ceiling((widths - fws) / 2), floor((widths - fws) / 2))
  # a heading narrower than a character column whose non-blank values are
  # uneven starts in the first position when the shortest value is more than
  # D(fw) narrower than the widest; D by the widest value fw = 5..16 (wider
  # columns of uneven values depend on the heading too: fail loud)
  # [measured on columns of two to five values, headings of 2 to 7]
  char_left <- vapply(seq_len(nc), function(j) {
    if (kind[j] != "c") return(FALSE)
    if (max(nchar(heads[[j]])) >= fws[j]) return(FALSE)
    v <- nchar(a$text[[j]]$values)
    v <- v[v > 0]
    if (length(v) < 2L) return(FALSE)
    if (min(v) == fws[j]) return(FALSE)
    d <- c(2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L)[fws[j] - 4L]
    if (fws[j] < 5L || fws[j] > 16L) {
      abort_unsupported(paste0("the PROC PRINT heading of a character column ", fws[j], " wide with values of several widths"))
    }
    fws[j] - min(v) > d
  }, TRUE)
  # a heading narrower than an unformatted numeric field [measured,
  # print_layout p01, p02, p08-p10]: right-aligned over uneven
  # values (the sums do not count): integers whose widths without the sign
  # differ by 2 or more, decimals whose widths differ by 3 or more; otherwise
  # centred rounding up, over integers without the sign position (hpos())
  num_head <- lapply(seq_len(nc), function(j) {
    t <- a$text[[j]]
    if (is.null(t$sfw) || kind[j] != "n") return(NULL)
    miss <- function(x) is.na(x) | grepl("^(\\.[A-Z_]?|[A-Z_])$", x)
    v <- t$values[!miss(t$values)]
    if (length(v) == 0L) return(NULL)
    dec <- isTRUE(t$hdec)
    w <- if (dec) nchar(v) else nchar(sub("^-", "", v))
    e <- c(t$values, t$sub, t$grand)
    e <- e[!miss(e)]
    list(right = max(w) - min(w) >= (if (dec) 3L else 2L),
         uw = if (dec) fws[j] else max(nchar(sub("^-", "", e))))
  })
  hpos <- function(j, h, gap) {
    nh <- num_head[[j]]
    if (is.null(nh) || max(nchar(heads[[j]])) >= fws[j]) return(ceiling(gap / 2))
    if (nh$right) return(gap)
    if (nchar(h) > nh$uw) return(ceiling(gap / 2))
    (fws[j] - nh$uw) + ceiling((nh$uw - nchar(h)) / 2)
  }
  # a missing value of an unformatted number sits in the decimal point column
  miss_pad <- vapply(a$text, function(t) as.integer(t$dec %||% 0L), 1L)
  cell <- function(j, s, field = TRUE) {
    if (is.na(s) || !nzchar(s)) return(strrep(" ", widths[j]))
    if (!field) return(formatC(s, width = -widths[j]))
    if (miss_pad[j] > 0L && grepl("^(\\.[A-Z_]?|[A-Z_])$", s)) s <- paste0(s, strrep(" ", miss_pad[j]))
    f <- formatC(s, width = if (kind[j] == "c") -fws[j] else fws[j])
    formatC(paste0(strrep(" ", off[j]), f), width = -widths[j])
  }
  # the cells of columns `js` joined by the gaps after each but the last
  line_of <- function(cells, gaps = rep(gap, length(cells) - 1L)) {
    sas_rt_rtrim(paste0(cells, c(strrep(" ", gaps), ""), collapse = ""))
  }
  head_of <- function(js, gaps) {
    nl <- max(vapply(heads[js], length, 1L))
    vapply(seq_len(nl), function(i) line_of(vapply(js, function(j) {
      h <- c(rep("", nl - length(heads[[j]])), heads[[j]])[i]
      if (!nzchar(h)) return(strrep(" ", widths[j]))
      gap <- widths[j] - nchar(h)
      # a formatted number: right-aligned, centred rounding down when its
      # heading is wider than the field
      pos <- if (kind[j] == "f") (if (max(nchar(heads[[j]])) > fws[j]) floor(gap / 2) else gap) else
        if (kind[j] == "n") hpos(j, h, gap) else if (char_left[j]) 0L else floor(gap / 2)
      formatC(paste0(strrep(" ", pos), h), width = -widths[j])
    }, ""), gaps), "")
  }
  nr <- length(a$text[[1]]$values)
  body_of <- function(js, gaps) {
    vapply(seq_len(nr), function(i) line_of(vapply(js, function(j) cell(j, a$text[[j]]$values[i]), ""), gaps), "")
  }
  if (!is.null(panels)) {
    # column panels, one after the other, a blank line between them
    lines <- unlist(lapply(seq_along(panels), function(k) {
      pn <- panels[[k]]
      c(if (k > 1L) "", head_of(pn$cols, pn$gaps), "", body_of(pn$cols, pn$gaps))
    }))
    return(lst_c(lst_lines(""), lst_unit(lines)))
  }
  all_gaps <- rep(gap, nc - 1L)
  head_lines <- head_of(seq_len(nc), all_gaps)
  body <- body_of(seq_len(nc), all_gaps)
  rule <- function(ch, first) line_of(vapply(seq_len(nc), function(j) {
    if (a$cols[[j]]$name %in% sums || (first && j == 1L)) strrep(ch, widths[j]) else strrep(" ", widths[j])
  }, ""))
  totals <- function(what, lab) line_of(vapply(seq_len(nc), function(j) {
    if (j == 1L && !is.null(lab)) return(cell(j, lab))
    if (a$cols[[j]]$name %in% sums) cell(j, a$text[[j]][[what]]) else strrep(" ", widths[j])
  }, ""))
  out <- character()
  if (length(sums) > 0) {
    if (label_first) out <- c(out, rule("-", TRUE), totals("sub", a$sub_label))
    if (a$last) out <- c(out, rule("=", FALSE), totals("grand", NULL))
  }
  by <- attr(tb, "by", exact = TRUE)
  top <- if (is.null(by)) lst_lines("") else lst_by_block(c(if (!a$first) c("", ""), ""), by$text)
  # the N line is centred on its own under CENTER [measured, center_listing]
  lst_c(top, lst_unit(c(head_lines, "", body, out)), if (!is.null(a$n)) lst_lines(c("", paste0("N = ", a$n))))
}

print_listing_tables <- function(tables) {
  do.call(lst_c, lapply(tables, listing_base_print))
}

# The gap between columns of widths `w`: 4, or the widest of 3, 2, 1 that fits
# the columns on the line (measured, print_wide); NA when none does.
print_fit_gap <- function(w) {
  g <- (4:1)[vapply(4:1, function(g) sum(w) + g * (length(w) - 1L) <= rt_linesize(), NA)]
  if (length(g)) g[1] else NA_integer_
}

# A PROC PRINT table that does not fit the line with a gap of 1 (measured,
# print_wide w01-w15, one observation each):
#   - variable names wider than their values (values one position wide): the
#     names are written down the columns, one character a line, and the gap is
#     chosen again for the narrower columns;
#   - otherwise column panels: as many as the columns need at a gap of 1, the
#     columns filled in order, ceiling(columns / panels) a panel (print_rtf wide_n6:
#     40 columns in 14, 14, 12), the ID
#     column at the start of every panel; each panel has its own gap, the ID
#     column keeps the gap of the first panel.
# Returns list(heads, widths, gap, panels = NULL or list(list(cols, gaps))).
# Everything else (more than one observation, BY groups, sums, N, the Obs
# column, columns of different widths, names of different lengths, names in
# panels wider than the values ...) is not measured: fail loud.
print_wide_plan <- function(a, heads, fws, widths, by) {
  nc <- length(a$cols)
  nr <- length(a$text[[1]]$values)
  no <- function(what) abort_unsupported(paste0("a PROC PRINT table wider than LINESIZE ", what))
  if (nr != 1L || !is.null(by) || length(a$sums) || !is.null(a$n)) {
    no("with more than one observation, BY groups, SUM or N (column panels measured on one observation)")
  }
  obs <- a$cols[[1]]$name == "Obs" && isTRUE(a$row_header) && identical(a$id_n, 0L)
  if (obs) no("with the Obs column (column panels measured with NOOBS or ID)")
  lab <- vapply(a$cols, function(c) isTRUE(c$label), NA)
  char <- vapply(a$cols, function(c) isTRUE(c$char), NA)
  nms <- vapply(a$cols, function(c) c$head, "")
  if (!any(lab) && all(char) && all(fws == 1L) && !isTRUE(a$row_header)) {
    # names down the columns
    if (length(unique(nchar(nms))) != 1L) no("with vertical headings of names of different lengths")
    heads <- lapply(nms, function(h) strsplit(h, "", fixed = TRUE)[[1]])
    widths <- rep(1L, nc)
    gap <- print_fit_gap(widths)
    if (is.na(gap)) no("with vertical headings that do not fit either")
    return(list(heads = heads, widths = widths, gap = gap, panels = NULL))
  }
  if (!(all(lab) || (!any(lab) && all(widths == fws)))) no("with names wider than their values in column panels")
  id <- isTRUE(a$row_header) && a$id_n == 1L
  if (isTRUE(a$row_header) && !id) no("with more than one ID variable")
  rest <- if (id) seq.int(2L, length.out = nc - 1L) else seq_len(nc)
  if (length(unique(widths[rest])) != 1L) no("with columns of different widths (column panels)")
  w <- widths[rest[1]]
  room <- rt_linesize() + 1L - (if (id) widths[1] + 1L else 0L)
  cap <- room %/% (w + 1L)
  if (cap < 1L) no("with a column wider than the line")
  np <- ceiling(length(rest) / cap)
  per <- ceiling(length(rest) / np)
  ends <- pmin(seq_len(np) * per, length(rest))
  size <- diff(c(0L, ends))
  if (any(size < 1L)) no("with an empty last column panel")
  panels <- lapply(seq_len(np), function(k) {
    cols <- c(if (id) 1L, rest[seq.int(ends[k] - size[k] + 1L, ends[k])])
    list(cols = cols, gap = print_fit_gap(widths[cols]))
  })
  g1 <- panels[[1]]$gap
  panels <- lapply(panels, function(pn) {
    if (is.na(pn$gap) || (id && pn$gap < g1)) no("with a later column panel narrower than the first")
    n <- length(pn$cols) - 1L
    pn$gaps <- if (id) c(g1, rep(pn$gap, n - 1L)) else rep(pn$gap, n)
    pn
  })
  list(heads = heads, widths = widths, gap = g1, panels = panels)
}
