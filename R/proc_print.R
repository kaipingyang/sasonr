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

print_compute <- function(d, spec) {
  # an empty data set prints nothing
  if (nrow(d) == 0) return(list(tables = list(), data = list(), events = character()))
  cols <- c(if (!is.null(spec$id)) list(spec$id), spec$cols)
  obs <- is.null(spec$id) && !isTRUE(spec$noobs)
  # the log after the not-sorted ERROR (observations read) is not measured
  groups <- tryCatch(proc_by_groups(d, spec$by, rep(FALSE, length(spec$by)), spec$data, spec$by_info),
                     sasonr_runtime_error = function(e) abort_unsupported(paste0("PROC PRINT BY on data that are not sorted (", conditionMessage(e), ")"), spec$line))
  sums <- spec$sums %||% character()
  # the text of each column over the whole data set, so that the decimals of
  # unformatted numbers line up across pages and with the sums
  sum_of <- function(x) {
    v <- x[is.na(sas_miss_code(x))]
    if (length(v) == 0) NA_real_ else Reduce(`+`, v)
  }
  sub_tot <- lapply(sums, function(v) vapply(groups, function(g) sum_of(d[[v]][g]), 0))
  grand <- lapply(sums, function(v) sum_of(d[[v]]))
  names(sub_tot) <- names(grand) <- sums
  n <- nrow(d)
  ng <- length(groups)
  text <- lapply(cols, function(s) {
    x <- d[[s$name]]
    extra <- if (s$name %in% sums) c(sub_tot[[s$name]], grand[[s$name]]) else NULL
    all <- if (s$char) {
      if (is.null(s$fmt)) sas_rtrim(x) else sas_rtrim(sas_format(x, s$fmt))
    } else {
      y <- c(x, extra)
      if (is.null(s$fmt)) print_num_column(y) else trimws(sas_format(y, s$fmt))
    }
    list(values = all[seq_len(n)], sub = if (!is.null(extra)) all[n + seq_len(ng)], grand = if (!is.null(extra)) all[n + ng + 1L],
         rounded = attr(all, "rounded", exact = TRUE))
  })
  names(text) <- vapply(cols, `[[`, "", "name")
  sub_rows <- length(sums) > 0 && length(spec$by) > 0
  tables <- lapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    x <- tibble::as_tibble(stats::setNames(lapply(cols, function(s) d[[s$name]][g]), names(text)))
    if (obs) x <- tibble::add_column(x, Obs = as.integer(d$.obs[g]), .before = 1L)
    by <- if (length(spec$by) > 0) list(values = lapply(stats::setNames(spec$by, spec$by), function(v) d[[v]][g[1]]),
                                        text = proc_by_line(d, g[1], spec$by, spec$by_info), group = k) else NULL
    tb <- new_sasonr_ods_table(x, name = "Print", label = "Data Set", path = "Print.Print", proc = "Print",
                            template = "base.print", by = by,
                            cols = ods_cols(names(x), role = "var"))
    attr(tb, "print") <- list(
      cols = c(if (obs) list(list(name = "Obs", head = spec$obs_label, char = FALSE)),
               lapply(cols, function(s) list(name = s$name, head = s$label %||% s$disp, label = !is.null(s$label), char = s$char,
                                             fw = if (!s$char && !is.null(s$fmt)) fmt_width(s$fmt, FALSE)))),
      text = c(if (obs) list(list(values = as.character(d$.obs[g]))),
               lapply(names(text), function(nm) list(values = text[[nm]]$values[g], sub = text[[nm]]$sub[k],
                                                     grand = if (k == ng) text[[nm]]$grand, rounded = text[[nm]]$rounded))),
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
    max(0L, nchar(e), a$cols[[j]]$fw)
  }, 1L)
  heads <- lapply(seq_len(nc), function(j) print_heading(a$cols[[j]], fws[j], a$split))
  widths <- pmax(fws, vapply(heads, function(h) max(nchar(h)), 1L))
  # wider than the line: SAS stacks the headings vertically or splits the
  # columns into panels; neither is implemented (the lines would be too long)
  if (sum(widths) + 4L * (nc - 1L) > rt_linesize()) {
    abort_unsupported("a PROC PRINT table wider than LINESIZE (vertical headings and column panels are not implemented)")
  }
  multi <- vapply(heads, length, 1L) > 1L
  # c: character, f: formatted number (the field is the format width), n: unformatted number or Obs
  rounded <- vapply(a$text, function(t) t$rounded %||% FALSE, NA)
  if (anyNA(rounded) && any(vapply(heads, function(h) max(nchar(h)), 1L)[is.na(rounded)] < fws[is.na(rounded)])) {
    abort_unsupported("the PROC PRINT heading of an unformatted numeric column of large magnitudes")
  }
  # an unformatted column rounded to decimals held at 2 is laid out like a formatted one
  kind <- vapply(seq_len(nc), function(j) {
    c <- a$cols[[j]]
    if (isTRUE(c$char)) "c" else if (!is.null(c$fw) || isTRUE(rounded[j])) "f" else "n"
  }, "")
  off <- ifelse(multi | kind == "n", ceiling((widths - fws) / 2), floor((widths - fws) / 2))
  # a one-line heading narrower than a character column whose non-blank
  # values are uneven starts in the first position: when the shortest value
  # is narrower than fw - floor(fw / 3) [measured with two values of widths
  # 6 to 20]; with more values the rule is not known when the shortest value
  # and the mean width disagree (fail loud)
  char_left <- vapply(seq_len(nc), function(j) {
    if (kind[j] != "c" || multi[j]) return(FALSE)
    if (max(nchar(heads[[j]])) >= fws[j]) return(FALSE)
    v <- nchar(a$text[[j]]$values)
    v <- v[v > 0]
    if (length(v) < 2L) return(FALSE)
    lim <- fws[j] - fws[j] %/% 3L
    by_min <- min(v) < lim
    by_mean <- mean(v) < (fws[j] + lim) / 2
    if (length(unique(v)) > 2L && by_min != by_mean) {
      abort_unsupported("the PROC PRINT heading of a character column with values of several widths")
    }
    by_min
  }, TRUE)
  cell <- function(j, s, field = TRUE) {
    if (is.na(s) || !nzchar(s)) return(strrep(" ", widths[j]))
    if (!field) return(formatC(s, width = -widths[j]))
    f <- formatC(s, width = if (a$cols[[j]]$char) -fws[j] else fws[j])
    formatC(paste0(strrep(" ", off[j]), f), width = -widths[j])
  }
  line_of <- function(cells) sas_rtrim(paste(cells, collapse = "    "))
  nl <- max(vapply(heads, length, 1L))
  head_lines <- vapply(seq_len(nl), function(i) {
    line_of(vapply(seq_len(nc), function(j) {
      h <- c(rep("", nl - length(heads[[j]])), heads[[j]])[i]
      if (!nzchar(h)) return(strrep(" ", widths[j]))
      gap <- widths[j] - nchar(h)
      pos <- if (multi[j] || kind[j] == "n") ceiling(gap / 2) else if (kind[j] == "f") gap else if (char_left[j]) 0L else floor(gap / 2)
      formatC(paste0(strrep(" ", pos), h), width = -widths[j])
    }, ""))
  }, "")
  nr <- length(a$text[[1]]$values)
  body <- vapply(seq_len(nr), function(i) line_of(vapply(seq_len(nc), function(j) cell(j, a$text[[j]]$values[i]), "")), "")
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
