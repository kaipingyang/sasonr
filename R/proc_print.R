# PROC PRINT on the PROC registry (proc_registry.R).
#
# print_compute() returns one `sasonr_ods_table` per page (a BY group, or the
# whole listing without BY) with the printed columns as raw values; the
# listing template base.print lays them out.  Layout rule, per page and per
# column (measured on 15 listings of the reference output):
#   fw  widest entry of the column on the page (values, subtotal, grand total
#       on the last page, the BY label of a subtotal in the first column);
#   W   max(fw, widest heading line); columns are 4 blanks apart;
#   the field starts at floor((W - fw) / 2), ceiling(...) when the heading of
#   the column has more than one line; numbers are right-aligned in the
#   field, character values left-aligned; heading lines are centred with
#   ceiling((W - width) / 2) and stacked at the bottom.

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
    list(values = all[seq_len(n)], sub = if (!is.null(extra)) all[n + seq_len(ng)], grand = if (!is.null(extra)) all[n + ng + 1L])
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
               lapply(cols, function(s) list(name = s$name, head = s$label %||% s$disp, label = !is.null(s$label), char = s$char))),
      text = c(if (obs) list(list(values = as.character(d$.obs[g]))),
               lapply(names(text), function(nm) list(values = text[[nm]]$values[g], sub = text[[nm]]$sub[k],
                                                     grand = if (k == ng) text[[nm]]$grand))),
      sums = sums, sub_label = if (sub_rows) spec$by_info[[spec$by]]$disp else NULL,
      last = k == ng, n = if (isTRUE(spec$n)) length(g) else NULL, split = spec$split, first = k == 1L
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
    if (length(w) > 2L) abort_unsupported("a PROC PRINT label of more than two words without SPLIT=")
    return(w)
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
    max(0L, nchar(e))
  }, 1L)
  heads <- lapply(seq_len(nc), function(j) print_heading(a$cols[[j]], fws[j], a$split))
  widths <- pmax(fws, vapply(heads, function(h) max(nchar(h)), 1L))
  multi <- vapply(heads, length, 1L) > 1L
  off <- ifelse(multi, ceiling((widths - fws) / 2), floor((widths - fws) / 2))
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
      formatC(paste0(strrep(" ", ceiling((widths[j] - nchar(h)) / 2)), h), width = -widths[j])
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
  top <- if (is.null(by)) "" else c(if (!a$first) c("", ""), "", by$text, "")
  c(top, head_lines, "", body, out, if (!is.null(a$n)) c("", paste0("N = ", a$n)))
}

print_listing_tables <- function(tables) {
  unlist(lapply(tables, listing_base_print), use.names = FALSE)
}
