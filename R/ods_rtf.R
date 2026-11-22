# ODS RTF: the RTF document of the tables written while the RTF destination
# is open (ODS RTF FILE= ... ODS RTF CLOSE), default style.
#
# Measured on the ODS RTF output of PROC PRINT (cases print_rtf):
# - one table per page of the procedure (BY group); the BY line is a bold
#   paragraph before its table; there are no page breaks (Word paginates
#   from \trhdr / \trkeep);
# - every row has \trkeep; the heading row has \trhdr; the paragraphs of the
#   heading row and of the second-to-last row have \keepn;
# - cell borders: top 15 on the heading row, bottom 5 (15 on the last row of
#   the table), left 15 on the first column and 5 on the others, right 15 on
#   the last column; heading cells bottom-aligned, others top-aligned;
#   background colour 17 for the heading row, the row-header column (Obs or
#   ID) and the sum rows, 8 otherwise;
# - text: heading, row-header column and sum rows bold 11 pt (\b\fs22), data
#   10 pt (\fs20); numbers and Obs right-aligned, character values left;
#   SPLIT= line breaks as {\line};
# - column width (twips): 125 plus the widest cell of the column, a cell's
#   text width being the sum of its character advances in 1/2048 twips
#   (regular 10 pt and bold 11 pt tables, rounded once); a heading wraps at
#   blanks, so its widest word counts; the first column adds 10.
# Characters without a measured advance fail loud.

rtf_char_widths <- local({
  tab <- NULL
  function() {
    if (is.null(tab)) {
      f <- system.file("rtf", "char_widths.csv", package = "sasonr")
      tab <<- utils::read.csv(f, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
    }
    tab
  }
})

# The text width of a cell (twips, without the cell padding).
rtf_text_width <- function(s, bold) {
  if (!nzchar(s)) return(0)
  w <- rtf_char_widths()
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  v <- if (bold) w$bold_num[match(ch, w$char)] else w$regular_num[match(ch, w$char)]
  if (anyNA(v)) {
    bad <- unique(ch[is.na(v)])
    abort_unsupported(paste0("the ODS RTF width of the character", if (length(bad) > 1) "s", " '",
                             paste(bad, collapse = "', '"), "' in ", if (bold) "bold" else "regular", " text"))
  }
  round(sum(v) / 2048)
}

rtf_heading_width <- function(lines) {
  words <- unlist(strsplit(lines, " +"))
  max(0, vapply(words, rtf_text_width, 0, bold = TRUE))
}

rtf_escape <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("{", "\\{", s, fixed = TRUE)
  s <- gsub("}", "\\}", s, fixed = TRUE)
  cp <- utf8ToInt(enc2utf8(s))
  if (all(cp < 128L)) return(s)
  paste(vapply(cp, function(u) {
    if (u < 128L) return(intToUtf8(u))
    if (u > 0xFFFFL) {
      u <- u - 0x10000L
      hi <- 0xD800L + bitwAnd(bitwShiftR(u, 10L), 0x3FFL)
      lo <- 0xDC00L + bitwAnd(u, 0x3FFL)
      return(paste0("\\u", hi - if (hi > 32767L) 65536L else 0L, ";\\u", lo - if (lo > 32767L) 65536L else 0L, ";"))
    }
    paste0("\\u", if (u > 32767L) u - 65536L else u, ";")
  }, ""), collapse = "")
}

rtf_prefix <- function(time = Sys.time()) {
  lt <- as.POSIXlt(time)
  colors <- c("0,0,0", "0,0,255", "0,255,255", "0,255,0", "255,0,255", "255,0,0", "255,255,0", "255,255,255",
              "0,0,128", "0,128,128", "0,128,0", "128,0,128", "128,0,0", "128,128,0", "128,128,128",
              "192,192,192", "187,187,187")
  ct <- vapply(strsplit(colors, ","), function(x) sprintf("\\red%s\\green%s\\blue%s;", x[1], x[2], x[3]), "")
  c("{\\rtf1\\ansi\\ansicpg1252\\uc1\\deff0\\deflang1033\\deflangfe1033",
    "{\\fonttbl", "{\\f1\\froman\\fprq2\\fcharset0 Times New Roman;}", "{\\f2\\fmodern\\fprq1\\fcharset0 Courier;}",
    "}{\\colortbl;", ct,
    "}{\\stylesheet{\\widctlpar\\adjustright\\fs20\\cgrid\\snext0 Normal;}{\\*\\cs10\\additive Default Paragraph Font;}",
    sprintf("}{\\info{\\title sasonr output}{\\author sasonr}{\\creatim\\yr%d\\mo%d\\dy%d\\hr%d\\min%d\\sec%d}}",
            lt$year + 1900L, lt$mon + 1L, lt$mday, lt$hour, lt$min, as.integer(lt$sec)),
    "\\widowctrl\\ftnbj\\aenddoc\\formshade\\viewkind1\\viewscale100\\pgbrdrhead\\pgbrdrfoot\\fet0\\paperw12240\\paperh15840\\margl360\\margr360\\margt360\\margb360",
    "\\sectd\\linex0\\endnhere\\headery720\\footery360\\marglsxn360\\margrsxn360\\margtsxn360\\margbsxn360",
    "{\\header\\pard\\plain\\qc{", "}}", "{\\footer\\pard\\plain\\qc{", "}}")
}

# One cell definition and its paragraph.
rtf_cell_def <- function(x, first, last, top, bottom, valign, bg, merge = "") {
  paste0(if (top) "\\clbrdrt\\brdrs\\brdrw15\\brdrcf1",
         "\\clbrdrb\\brdrs\\brdrw", bottom, "\\brdrcf1",
         "\\clbrdrl\\brdrs\\brdrw", if (first) 15 else 5, "\\brdrcf1",
         if (last) "\\clbrdrr\\brdrs\\brdrw15\\brdrcf1",
         merge, "\\cltxlrtb\\clvertal", valign, "\\clcbpat", bg, "\\clpadt60\\clpadft3\\clpadr60\\clpadfr3\\cellx", x)
}

rtf_cell_par <- function(text, keepn, bold, align) {
  paste0("\\pard\\plain\\intbl", if (keepn) "\\keepn", if (bold) "\\b", "\\sb60\\sa60\\q", align,
         "\\f1\\fs", if (bold) 22 else 20, "\\cf1{", text, "\\cell}")
}

# The rows of one PROC PRINT page (a table of template base.print).
rtf_print_table <- function(tb) {
  a <- attr(tb, "print", exact = TRUE)
  sums <- a$sums
  nc <- length(a$cols)
  # the row-header column: Obs or the ID variable
  role <- seq_len(nc) == 1L & isTRUE(a$row_header)
  heads <- lapply(a$cols, function(c) if (!is.null(a$split) && isTRUE(c$label)) strsplit(c$head, a$split, fixed = TRUE)[[1]] else c$head)
  sum_rows <- list()
  if (length(sums)) {
    if (!is.null(a$sub_label)) {
      sum_rows <- c(sum_rows, list(vapply(seq_len(nc), function(j) {
        if (j == 1L) a$sub_label else if (a$cols[[j]]$name %in% sums) a$text[[j]]$sub else ""
      }, "")))
    }
    if (isTRUE(a$last)) {
      sum_rows <- c(sum_rows, list(vapply(seq_len(nc), function(j) {
        if (a$cols[[j]]$name %in% sums) a$text[[j]]$grand %||% "" else ""
      }, "")))
    }
  }
  nr <- length(a$text[[1]]$values)
  # outside the measured rule: the N line (it widens a column)
  if (!is.null(a$n)) abort_unsupported("the ODS RTF output of PROC PRINT with the option N")
  widths <- vapply(seq_len(nc), function(j) {
    data_bold <- role[j]
    hw <- rtf_heading_width(heads[[j]])
    vals <- a$text[[j]]$values
    vw <- vapply(vals, rtf_text_width, 0, bold = data_bold)
    sums_j <- vapply(sum_rows, function(r) r[j], "")
    sw <- vapply(sums_j, rtf_text_width, 0, bold = TRUE)
    w <- max(hw, vw, sw, 0)
    125 + w + if (j == 1L) 10 else 0
  }, 0)
  xs <- cumsum(widths)
  align <- vapply(a$cols, function(c) if (isTRUE(c$char)) "l" else "r", "")
  n_row <- if (!is.null(a$n)) paste0("N = ", a$n)
  total <- 1L + nr + length(sum_rows) + if (!is.null(n_row)) 1L else 0L
  out <- character()
  row_k <- 0L
  emit_row <- function(texts, header = FALSE, bold_row = FALSE) {
    row_k <<- row_k + 1L
    last <- row_k == total
    keepn <- header || row_k == total - 1L
    defs <- vapply(seq_len(nc), function(j) {
      rtf_cell_def(xs[j], j == 1L, j == nc, header, if (last) 15 else 5, if (header) "b" else "t",
                   if (header || bold_row || role[j]) 17 else 8)
    }, "")
    pars <- vapply(seq_len(nc), function(j) {
      t <- if (header) paste(vapply(texts[[j]], rtf_escape, ""), collapse = "{\\line}\n") else rtf_escape(texts[[j]])
      rtf_cell_par(t, keepn, header || bold_row || role[j], align[j])
    }, "")
    out <<- c(out, paste0("\\trowd\\trkeep", if (header) "\\trhdr", "\\trql"), defs, pars, "{\\row}")
  }
  emit_row(heads, header = TRUE)
  for (i in seq_len(nr)) emit_row(lapply(a$text, function(t) t$values[i]))
  for (r in sum_rows) emit_row(as.list(r), bold_row = TRUE)
  if (!is.null(n_row)) {
    row_k <- row_k + 1L
    out <- c(out, "\\trowd\\trkeep\\trql", rtf_cell_def(xs[nc], TRUE, TRUE, FALSE, 15, "t", 8),
             rtf_cell_par(rtf_escape(n_row), FALSE, FALSE, "l"), "{\\row}")
  }
  out
}

#' Write the RTF document of the outputs collected while ODS RTF was open
#' @param outputs List of list(proc, titles, tables).
#' @param file Path of the document.
#' @noRd
rtf_write_document <- function(outputs, file) {
  body <- character()
  k <- 0L
  last_tb <- NULL
  last_o <- 0L
  for (o_id in seq_along(outputs)) {
    o <- outputs[[o_id]]
    if (length(o$titles) || length(o$footnotes)) abort_unsupported("TITLE or FOOTNOTE lines in an ODS RTF document")
    for (tb in o$tables) {
      tpl <- attr(tb, "template", exact = TRUE)
      if (!tpl %in% c("base.print", "base.tabulate")) abort_unsupported(paste0("the ODS RTF output of PROC ", toupper(o$proc)))
      by <- attr(tb, "by", exact = TRUE)
      if (k > 0L) {
        # PROC TABULATE pages: a new section; PROC PRINT BY groups: two
        # empty paragraphs; other sequences of tables are not measured
        prev <- last_tb
        if (identical(tpl, "base.tabulate") && identical(attr(prev, "table", exact = TRUE), attr(tb, "table", exact = TRUE)) &&
            identical(o_id, last_o)) {
          body <- c(body, "\\pard", "\\sect\\sectd\\linex0\\endnhere\\headery720\\footery360\\marglsxn360\\margrsxn360\\margtsxn360\\margbsxn360",
                    "{\\header\\pard\\plain\\qc{", "}}", "{\\footer\\pard\\plain\\qc{", "}}")
        } else if (identical(tpl, "base.print") && identical(attr(prev, "template", exact = TRUE), "base.print") && identical(o_id, last_o)) {
          body <- c(body, "\\pard\\par\\par")
        } else {
          abort_unsupported("an ODS RTF document with the tables of more than one step or TABLE statement")
        }
      }
      if (!is.null(by)) {
        body <- c(body, paste0("\\plain\\pard\\keepn\\chcbpat8\\i0\\b\\sb0\\sa0\\ql\\f1\\fs22\\cf1{", rtf_escape(by$text), "}"),
                  "{\\par}", "\\par")
      }
      body <- c(body, if (identical(tpl, "base.tabulate")) rtf_tabulate_table(tb, rtf_widths_override(tb)) else rtf_print_table(tb))
      k <- k + 1L
      last_tb <- tb
      last_o <- o_id
    }
  }
  doc <- c(rtf_prefix(), body, "\\pard}")
  if (!dir.exists(dirname(file))) abort_runtime(paste0("Physical file does not exist, ", file))
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(doc, con, useBytes = TRUE)
  invisible(file)
}

# The rows of one PROC TABULATE table (template base.tabulate), from the
# logical grid the listing and the HTML view share (tab_grid()): a cell over
# several leaf columns is one RTF cell whose \cellx is the right edge of its
# last column; a cell over several grid rows is \clvmgf on its first row and
# \clvmrg on the others [measured, cases tabulate_rtf].  Heading cells are
# centred (the corner cell vertically centred), row titles left and top,
# data right and bottom; a row has \keepn when it is a heading row, starts a
# vertical merge that goes on in the next row, or is the second-to-last row.
# The width of the data columns is not measured yet (fails loud).
rtf_tabulate_table <- function(tb, widths = NULL) {
  fail <- function(what) abort_unsupported(paste0("the PROC TABULATE ODS RTF output of ", what))
  g <- tab_grid_parts(tb, fail)
  if (any(g$head_at != 1L)) fail("a class-variable heading row inside the row dimension")
  page <- attr(tb, "page", exact = TRUE)
  if (!is.null(attr(tb, "by", exact = TRUE))) fail("BY groups")
  if (!is.null(g$box) && g$nh > 0L && g$has_rows) fail("a BOX= text")
  fmts <- attr(tb, "formats", exact = TRUE)
  cstat <- attr(tb, "cell_stats", exact = TRUE)
  fsrc <- attr(tb, "format_source", exact = TRUE)
  misstext <- attr(tb, "misstext", exact = TRUE) %||% "."
  data_text <- function(j, i) {
    v <- tb[[g$scols[j]]][i]
    if (is.na(v)) return(misstext)
    trimws(sas_format(v, tab_html_format(fmts[[j]][[i]], cstat[[j]][i], fsrc[[j]][i], fail)))
  }
  cells <- tab_grid(g, seq_along(g$scols), data_text, "")
  ncol <- g$k + length(g$scols)
  nrow <- g$nrow
  if (is.null(widths)) fail("the data columns (their width rule is not measured yet)")
  xs <- cumsum(widths)
  cover <- function(r, c) {
    for (cl in cells) if (cl$r0 <= r && r <= cl$r1 && cl$c0 <= c && c <= cl$c1) return(cl)
    NULL
  }
  keepn_row <- vapply(seq_len(nrow), function(r) {
    r <= g$nh || r == nrow - 1L || any(vapply(cells, function(cl) cl$r0 == r && cl$r1 > r && cl$r0 > g$nh, logical(1)))
  }, TRUE)
  out <- character()
  # the page text: a heading row of one cell over the table, bottom border
  # only; the table below it has no top border
  if (!is.null(page)) {
    out <- c(out, "\\trowd\\trkeep\\trhdr\\trql",
             paste0("\\clbrdrb\\brdrs\\brdrw15\\brdrcf1\\cltxlrtb\\clvertalb\\clcbpat8\\clpadt60\\clpadft3\\clpadr60\\clpadfr3\\cellx", xs[ncol]),
             rtf_cell_par(rtf_escape(paste(page$text, collapse = " ")), TRUE, TRUE, "l"), "{\\row}")
  }
  for (r in seq_len(nrow)) {
    header <- r <= g$nh
    defs <- character()
    pars <- character()
    c <- 1L
    while (c <= ncol) {
      cl <- cover(r, c)
      if (is.null(cl)) fail("a grid position without a cell")
      kind <- cl$align
      start <- cl$r0 == r
      merge <- if (cl$r1 > cl$r0) (if (start) "\\clvmgf" else "\\clvmrg") else ""
      is_head <- kind %in% c("center", "box")
      valign <- if (identical(kind, "box")) "c" else if (identical(kind, "left")) "t" else "b"
      bg <- if (identical(kind, "raw")) 8 else 17
      defs <- c(defs, paste0(
        if (cl$r0 == 1L && is.null(page)) "\\clbrdrt\\brdrs\\brdrw15\\brdrcf1",
        "\\clbrdrb\\brdrs\\brdrw", if (cl$r1 == nrow) 15 else 5, "\\brdrcf1",
        "\\clbrdrl\\brdrs\\brdrw", if (cl$c0 == 1L) 15 else 5, "\\brdrcf1",
        if (cl$c1 == ncol) "\\clbrdrr\\brdrs\\brdrw15\\brdrcf1",
        merge, "\\cltxlrtb\\clvertal", valign, "\\clcbpat", bg, "\\clpadt60\\clpadft3\\clpadr60\\clpadfr3\\cellx", xs[cl$c1]))
      if (!start) {
        pars <- c(pars, paste0("\\pard\\plain\\intbl", if (keepn_row[r]) "\\keepn", "{\\cell}"))
      } else {
        text <- rtf_escape(paste(cl$text, collapse = " "))
        bold <- !identical(kind, "raw")
        align <- if (is_head) "c" else if (identical(kind, "left")) "l" else "r"
        pars <- c(pars, rtf_cell_par(text, keepn_row[r], bold, align))
      }
      c <- cl$c1 + 1L
    }
    out <- c(out, paste0("\\trowd\\trkeep", if (header) "\\trhdr", "\\trql"), defs, pars, "{\\row}")
  }
  out
}

# Test hook: column widths given from outside (options(sasonr.rtf_widths =
# list(<widths>, ...)), one vector per table in order); NULL otherwise.
rtf_widths_override <- function(tb) {
  w <- getOption("sasonr.rtf_widths")
  if (is.null(w)) return(NULL)
  k <- getOption("sasonr.rtf_widths_k", 0L) + 1L
  options(sasonr.rtf_widths_k = k)
  w[[k]]
}
