# Listing template base.tabulate: the printed PROC TABULATE table.
#
# A table is drawn on a grid of logical rows (the column heading rows, then
# the body rows; a body row may be preceded by the heading row of the row
# class variables) and logical columns (the row-title columns, then one leaf
# column per data column).  Every cell of the table is a rectangle on that
# grid; tab_draw() writes the cells and their borders on a character canvas:
#   - outer frame: a line of `-` above and below, `|` at both ends of every
#     other line; the first heading row follows the top frame directly
#   - between two logical rows a rule line: `-` under every cell that ends
#     there, the text of a cell that continues
#   - a crossing on a rule line is `+` when rules meet from both sides and
#     the vertical border continues above and below, `-` when rules meet
#     from both sides and no border continues on one side, `|` when only one
#     side is a rule
# Layout:
#   - row-title area RTS = floor(LINESIZE / 4) including its two border
#     characters; k row-title columns share the inner width minus the k - 1
#     separators: floor(w / k) each, the last one the remainder
#   - a leaf column is as wide as its cell format; headings are centred (odd
#     remainder to the right) and wrapped at word boundaries, a word longer
#     than the column is cut after width - 1 characters followed by `-`
#   - the BOX= text fills the corner cell from the top, wrapped to the inner
#     RTS width; without column heading rows there is no corner and no BOX
#   - numbers right-aligned in the format width; an empty cell prints the
#     MISSTEXT, right-aligned when it fits, else cut to the column width
#   - leaves flow over panels of at most LINESIZE characters; each panel
#     repeats the row titles and the heading rows (cut to the panel's
#     leaves); a non-last panel is followed by a blank line and
#     `(Continued)`; each panel and each table starts a new page (three blank
#     lines under FORMDLIM=' ')
# Shapes outside these rules fail loud.

tab_page_break <- c("", "", "")

# The listing of the tables of one step.  Returns the lines with attribute
# `pages` (the number of pages written).
tab_listing_tables <- function(tables, ls) {
  out <- character()
  pages <- 0L
  for (tb in tables) {
    panels <- listing_base_tabulate(tb, ls)
    for (k in seq_along(panels)) {
      if (pages > 0L) out <- c(out, tab_page_break)
      out <- c(out, panels[[k]], if (k < length(panels)) c("", "(Continued)"))
      pages <- pages + 1L
    }
  }
  structure(out, pages = pages)
}

#' TITLE lines above the listing of a PROC TABULATE step
#'
#' The titles and a blank line go above the output of a step that prints one
#' page; titles above an output of several pages fail loud.
#' @param titles The TITLE lines.
#' @param r The result of `sas_rt_proc()`.
#' @export
#' @keywords internal
sas_tab_titles <- function(titles, r) {
  if (length(titles) > 0L && isTRUE(r$pages > 1L)) {
    abort_unsupported("TITLE lines above a PROC TABULATE output of more than one page")
  }
  sas_titles_once(titles, r$listing)
}

# The panels of one table: a list of character vectors (one per panel).
listing_base_tabulate <- function(tb, ls, ps = NULL) {
  fail <- function(what) abort_unsupported(paste0("the PROC TABULATE listing of ", what))
  cols <- attr(tb, "cols", exact = TRUE)
  header <- attr(tb, "header", exact = TRUE)
  rh <- attr(tb, "row_header", exact = TRUE)
  fmts <- attr(tb, "formats", exact = TRUE)
  box <- attr(tb, "box", exact = TRUE)
  misstext <- attr(tb, "misstext", exact = TRUE) %||% "."
  page <- attr(tb, "page", exact = TRUE)
  scols <- cols$name[cols$role == "stat"]
  nr <- nrow(tb)
  if (nr == 0L) fail("a table without body rows")
  if (!is.null(box) && !identical(box$kind, "text")) fail("BOX= with a variable")
  if (chr_any_wide(c(misstext, unlist(header$text), unlist(rh$text), box$value, page$text))) {
    fail("text with characters outside ASCII")
  }

  # leaf widths and cell texts
  widths <- vapply(seq_along(scols), function(j) {
    w <- unique(vapply(fmts[[j]], function(f) as.integer(fmt_width(f, FALSE)), 1L))
    if (length(w) != 1L || is.na(w)) fail("a column whose cell formats have different widths")
    w
  }, 1L)
  if (any(widths < 1L)) fail("a column of width 0")
  cell_text <- lapply(seq_along(scols), function(j) {
    x <- tb[[scols[j]]]
    vapply(seq_len(nr), function(i) {
      w <- widths[j]
      if (is.na(x[i])) {
        if (nchar(misstext) <= w) return(formatC(misstext, width = w))
        return(substr(misstext, 1L, w))
      }
      s <- sas_format(x[i], fmts[[j]][[i]])
      if (nchar(s) != w) fail("a cell value that is not as wide as its format")
      s
    }, "")
  })

  # row titles
  has_rows <- nrow(rh) > 0L
  k <- if (has_rows) max(rh$col + rh$ncol - 1L) else 0L
  rts <- if (has_rows) ls %/% 4L else 0L
  rt_w <- integer()
  if (has_rows) {
    inner <- rts - 2L - (k - 1L)
    rt_w <- c(rep(inner %/% k, k - 1L), inner - (inner %/% k) * (k - 1L))
    if (any(rt_w < 1L)) fail("more row-title columns than fit in the row-title area")
  }
  # body logical rows: the class-variable heading row goes before body row 1
  heads <- rh[rh$heading, ]
  levels <- rh[!rh$heading, ]
  if (nrow(heads) > 0L) {
    if (any(heads$row != 1L)) fail("a class-variable heading row inside the row dimension")
    if (!setequal(heads$col, seq_len(k)) || anyDuplicated(heads$col)) {
      fail("a class-variable heading row that does not cover every row-title column")
    }
  }
  head_row <- nrow(heads) > 0L

  # column heading cells: level, rowspan, leaf range
  hlev <- header$level
  hspan <- header$rowspan %||% rep(1L, nrow(header))
  hleaf <- lapply(header$columns, function(cc) match(cc, scols))
  for (l in hleaf) if (anyNA(l) || !identical(l, seq(min(l), max(l)))) fail("a heading over columns that are not adjacent")
  nh <- if (nrow(header)) max(hlev + hspan - 1L) else 0L

  # panels
  avail <- ls - (if (has_rows) rts else 1L)
  panels <- list()
  cur <- integer()
  used <- 0L
  for (j in seq_along(scols)) {
    if (widths[j] + 1L > avail) fail("a column wider than the line")
    if (used + widths[j] + 1L > avail) {
      panels <- c(panels, list(cur))
      cur <- integer()
      used <- 0L
    }
    cur <- c(cur, j)
    used <- used + widths[j] + 1L
  }
  panels <- c(panels, list(cur))
  if (length(panels) > 1L) {
    if (!has_rows) fail("several panels of a table without a row dimension")
    if (!is.null(box)) fail("several panels with a BOX= text")
    if (!is.null(page)) fail("several panels with a page dimension")
  }

  lapply(panels, function(pj) {
    np <- length(pj)
    cw <- c(rt_w, widths[pj])
    nc <- k + np
    cells <- list()
    add <- function(r0, r1, c0, c1, text, align, valign = "top") {
      cells[[length(cells) + 1L]] <<- list(r0 = r0, r1 = r1, c0 = c0, c1 = c1, text = text, align = align,
                                           valign = valign)
    }
    # heading rows 1..nh
    for (h in seq_len(nrow(header))) {
      l <- intersect(hleaf[[h]], pj)
      if (!length(l)) next
      c0 <- k + match(min(l), pj)
      c1 <- k + match(max(l), pj)
      add(hlev[h], hlev[h] + hspan[h] - 1L, c0, c1, header$text[[h]], "center", "bottom")
    }
    if (nh > 0L && has_rows) {
      add(1L, nh, 1L, k, if (is.null(box)) "" else box$value, "box")
    }
    # body rows
    off <- nh + head_row
    body <- function(i) off + i
    if (head_row) {
      for (z in seq_len(nrow(heads))) add(nh + 1L, nh + 1L, heads$col[z], heads$col[z], heads$text[[z]], "left")
    }
    for (z in seq_len(nrow(levels))) {
      add(body(levels$row[z]), body(levels$row[z] + levels$nrow[z] - 1L), levels$col[z],
          levels$col[z] + levels$ncol[z] - 1L, levels$text[[z]], "left")
    }
    for (i in seq_len(nr)) {
      top <- if (i == 1L && head_row) nh + 1L else body(i)
      for (q in seq_len(np)) add(top, body(i), k + q, k + q, cell_text[[pj[q]]][i], "raw", "bottom")
    }
    tab_draw(cells, off + nr, cw, fail, if (is.null(page)) NULL else paste(page$text, collapse = " "))
  })
}

# Lines of the text of one cell, `w` characters wide.
tab_cell_lines <- function(text, align, w, fail) {
  text <- paste(text, collapse = " ")
  switch(align,
    raw = text,
    left = {
      if (nchar(text) > w) fail("a row title longer than its column")
      formatC(text, width = -w)
    },
    box = {
      if (!nzchar(text)) return(strrep(" ", w))
      formatC(tab_wrap(text, w, fail, cut = FALSE), width = -w)
    },
    center = {
      vapply(tab_wrap(text, w, fail, cut = TRUE), function(s) {
        left <- (w - nchar(s)) %/% 2L
        paste0(strrep(" ", left), s, strrep(" ", w - nchar(s) - left))
      }, "", USE.NAMES = FALSE)
    }
  )
}

# Word wrap to `w` characters.  `cut`: a word longer than `w` is cut after
# w - 1 characters followed by `-` (column headings); otherwise it fails loud.
tab_wrap <- function(text, w, fail, cut) {
  if (!nzchar(text)) return("")
  if (grepl("  ", text, fixed = TRUE) || grepl("^ | $", text)) {
    if (nchar(text) > w) fail("a wrapped text with leading, trailing or repeated blanks")
    return(text)
  }
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  out <- character()
  cur <- ""
  for (wd in words) {
    while (nchar(wd) > w) {
      if (!cut || w < 2L) fail("a word longer than its column")
      if (nzchar(cur)) {
        out <- c(out, cur)
        cur <- ""
      }
      out <- c(out, paste0(substr(wd, 1L, w - 1L), "-"))
      wd <- substr(wd, w, nchar(wd))
    }
    if (!nzchar(cur)) {
      cur <- wd
    } else if (nchar(cur) + 1L + nchar(wd) <= w) {
      cur <- paste(cur, wd)
    } else {
      out <- c(out, cur)
      cur <- wd
    }
  }
  c(out, cur)
}

# Draw the cells (logical rows 1..nrow, columns of widths `cw`) on a canvas.
tab_draw <- function(cells, nrow, cw, fail, page_line = NULL) {
  nc <- length(cw)
  owner <- matrix(0L, nrow, nc)
  for (ci in seq_along(cells)) {
    c <- cells[[ci]]
    if (any(owner[c$r0:c$r1, c$c0:c$c1] != 0L)) fail("overlapping cells")
    owner[c$r0:c$r1, c$c0:c$c1] <- ci
  }
  if (any(owner == 0L)) fail("a grid position without a cell")
  xs <- cumsum(c(2L, cw[-nc] + 1L))
  xe <- xs + cw - 1L
  cell_w <- function(c) xe[c$c1] - xs[c$c0] + 1L
  lines <- lapply(cells, function(c) tab_cell_lines(c$text, c$align, cell_w(c), fail))
  # row heights: from the cells within one row; a cell over several rows
  # must fit the lines of its rows
  h <- rep(1L, nrow)
  for (ci in seq_along(cells)) {
    c <- cells[[ci]]
    if (c$r0 == c$r1) h[c$r0] <- max(h[c$r0], length(lines[[ci]]))
  }
  for (r in seq_len(nrow)) {
    n <- vapply(seq_along(cells), function(ci) {
      c <- cells[[ci]]
      if (c$r0 == r && c$r1 == r && !identical(c$align, "box")) length(lines[[ci]]) else NA_integer_
    }, 1L)
    n <- n[!is.na(n)]
    if (length(unique(n)) > 1L) fail("a heading row whose headings wrap to different numbers of lines")
  }
  ys <- cumsum(c(2L, h[-nrow] + 1L))
  ye <- ys + h - 1L
  W <- xe[nc] + 1L
  H <- ye[nrow] + 1L
  cv <- matrix(" ", H, W)
  for (ci in seq_along(cells)) {
    c <- cells[[ci]]
    y0 <- ys[c$r0]
    y1 <- ye[c$r1]
    ln <- lines[[ci]]
    if (length(ln) > y1 - y0 + 1L) fail("a cell text with more lines than its rows")
    if (length(ln) > 1L && c$r0 != c$r1 && !identical(c$align, "box")) fail("a wrapped heading over several heading rows")
    at <- if (identical(c$valign, "bottom")) y1 - length(ln) + 1L else y0
    for (q in seq_along(ln)) cv[at + q - 1L, xs[c$c0]:xe[c$c1]] <- strsplit(ln[q], "", fixed = TRUE)[[1]]
  }
  cv[c(1L, H), ] <- "-"
  cv[2:(H - 1L), c(1L, W)] <- "|"
  for (r in seq_len(nrow)) {
    for (q in seq_len(nc - 1L)) if (owner[r, q] != owner[r, q + 1L]) cv[ys[r]:ye[r], xe[q] + 1L] <- "|"
    if (r == nrow) next
    y <- ye[r] + 1L
    rule <- owner[r, ] != owner[r + 1L, ]
    for (q in seq_len(nc)) if (rule[q]) cv[y, xs[q]:xe[q]] <- "-"
    for (q in seq_len(nc - 1L)) {
      up <- owner[r, q] != owner[r, q + 1L]
      down <- owner[r + 1L, q] != owner[r + 1L, q + 1L]
      x <- xe[q] + 1L
      if (rule[q] && rule[q + 1L]) {
        cv[y, x] <- if (up && down) "+" else "-"
      } else if (rule[q] || rule[q + 1L] || up) {
        cv[y, x] <- "|"
      }
    }
  }
  c(page_line, apply(cv, 1L, paste, collapse = ""))
}
