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
#   text width being the sum of its character advances (regular 10 pt:
#   num / 2048; bold 11 pt: k / 2048 * 220, rounded once); a heading wraps at
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
  v <- if (bold) w$bold_k[match(ch, w$char)] else w$regular_num[match(ch, w$char)]
  if (anyNA(v)) {
    bad <- unique(ch[is.na(v)])
    abort_unsupported(paste0("the ODS RTF width of the character", if (length(bad) > 1) "s", " '",
                             paste(bad, collapse = "', '"), "' in ", if (bold) "bold" else "regular", " text"))
  }
  if (bold) round(sum(v) / 2048 * 220) else round(sum(v) / 2048)
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
  # outside the measured rule: a column whose width a bold digit sets (the
  # bold advances of digits are not exact yet), and the N line
  if (!is.null(a$n)) abort_unsupported("the ODS RTF output of PROC PRINT with the option N")
  widths <- vapply(seq_len(nc), function(j) {
    data_bold <- role[j]
    hw <- rtf_heading_width(heads[[j]])
    vals <- a$text[[j]]$values
    vw <- vapply(vals, rtf_text_width, 0, bold = data_bold)
    sums_j <- vapply(sum_rows, function(r) r[j], "")
    sw <- vapply(sums_j, rtf_text_width, 0, bold = TRUE)
    w <- max(hw, vw, sw, 0)
    bold_digits <- c(if (data_bold) vals[vw == w], sums_j[sw == w])
    if (w > hw && any(grepl("[0-9]", bold_digits))) {
      abort_unsupported("an ODS RTF column whose width a bold number sets")
    }
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
  for (o in outputs) {
    if (length(o$titles) || length(o$footnotes)) abort_unsupported("TITLE or FOOTNOTE lines in an ODS RTF document")
    for (tb in o$tables) {
      tpl <- attr(tb, "template", exact = TRUE)
      if (!identical(tpl, "base.print")) abort_unsupported(paste0("the ODS RTF output of PROC ", toupper(o$proc)))
      by <- attr(tb, "by", exact = TRUE)
      if (k > 0L) body <- c(body, "\\pard\\par\\par")
      if (!is.null(by)) {
        body <- c(body, paste0("\\plain\\pard\\keepn\\chcbpat8\\i0\\b\\sb0\\sa0\\ql\\f1\\fs22\\cf1{", rtf_escape(by$text), "}"),
                  "{\\par}", "\\par")
      }
      body <- c(body, rtf_print_table(tb))
      k <- k + 1L
    }
  }
  doc <- c(rtf_prefix(), body, "\\pard}")
  if (!dir.exists(dirname(file))) abort_runtime(paste0("Physical file does not exist, ", file))
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(doc, con, useBytes = TRUE)
  invisible(file)
}
