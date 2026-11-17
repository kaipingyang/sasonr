# HTML destination: the HTML view of ODS tables.
#
# render_html() turns one sas_ods_table into a <table class="sas-table">; the
# cell text comes from the display engine the listing uses (the `format` of
# each column, ods_cell_text()), so both destinations show the same text.
# sas_html() assembles the procedure outputs a result sent to the HTML
# destination (`ODS HTML;` / `ODS HTML CLOSE;`, open by default) into one
# document styled by inst/css/sasonr.css.

html_escape <- function(x) {
  x <- log_html_escape(x)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# The display text of value `v` of column `col` (one row of the `cols` tibble).
ods_cell_text <- function(v, col) {
  if (identical(col$role, "class")) return(proc_value_text(v, col$info[[1]]))
  f <- col$format[[1]]
  switch(f$engine %||% "",
    count = if (is.na(v)) "." else sprintf("%.0f", v),
    means = means_stat_text(f$stat, v, list(fw = f$fw, maxdec = f$maxdec, alpha = f$alpha,
                                           freq = if (isTRUE(f$freq)) TRUE)),
    rlang::abort(paste0("No display engine for column ", col$name), class = "sasonr_internal_error")
  )
}

# One <td>/<th>: `values` of length one is a plain cell, more values are
# stacked (one sas-stack block per value, top to bottom).
html_cell <- function(values, tag = "td", cls = character(), attrs = "") {
  values <- ifelse(nzchar(values), html_escape(values), "&nbsp;")
  body <- if (length(values) == 1L) values else paste0('<div class="sas-stack">', values, "</div>", collapse = "")
  cls <- c(cls, if (length(values) > 1L) "sas-stacked")
  paste0("<", tag, if (length(cls)) paste0(' class="', paste(cls, collapse = " "), '"'), attrs, ">", body, "</", tag, ">")
}

#' @noRd
render_html <- function(table) {
  validate_sas_ods_table(table)
  cols <- attr(table, "cols", exact = TRUE)
  header <- attr(table, "header", exact = TRUE)
  col_of <- function(nm) cols[match(nm, cols$name), ]
  leaf_level <- max(header$level)
  leaves <- header[header$level == leaf_level, ]
  scols <- cols[cols$role == "stat" & !is.na(cols$var), ]
  vars <- unique(scols$var)
  vinfo <- scols$info[match(vars, scols$var)]
  is_num <- function(h) {
    if (h$role == "stat") return(TRUE)
    if (h$role == "class") return(proc_right_align(col_of(h$columns[[1]])$info[[1]]))
    FALSE
  }
  head_rows <- vapply(sort(unique(header$level)), function(lv) {
    h <- header[header$level == lv, ]
    cells <- vapply(seq_len(nrow(h)), function(i) {
      hi <- h[i, ]
      sp <- hi$span
      html_cell(paste(hi$text[[1]], collapse = " "), "th",
                c("sas-header", if (lv == leaf_level && is_num(hi)) "sas-num"),
                paste0(' scope="', if (sp > 1L) "colgroup" else "col", '"', if (sp > 1L) paste0(' colspan="', sp, '"')))
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  prev <- NULL
  body_rows <- vapply(seq_len(nrow(table)), function(r) {
    ctext <- vapply(leaves$columns[leaves$role == "class"], function(nm) ods_cell_text(table[[nm]][r], col_of(nm)), "")
    same <- if (is.null(prev)) rep(FALSE, length(ctext)) else cumprod(ctext == prev) == 1
    prev <<- ctext
    k <- 0L
    cells <- vapply(seq_len(nrow(leaves)), function(i) {
      h <- leaves[i, ]
      switch(h$role,
        class = {
          k <<- k + 1L
          html_cell(if (same[k]) "" else ctext[k], "th", c("sas-rowheader", if (is_num(h)) "sas-num"), ' scope="row"')
        },
        var = html_cell(vapply(vinfo, `[[`, "", "disp"), "th", "sas-rowheader", ' scope="row"'),
        label = html_cell(vapply(vinfo, function(i) i$label %||% "", ""), "th", "sas-rowheader", ' scope="row"'),
        html_cell(vapply(h$columns[[1]], function(nm) ods_cell_text(table[[nm]][r], col_of(nm)), ""), "td",
                  c("sas-data", if (is_num(h)) "sas-num"))
      )
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  notes <- attr(table, "notes", exact = TRUE)
  c(paste0('<table class="sas-table" summary="Procedure ', html_escape(attr(table, "proc", exact = TRUE)), ": ",
           html_escape(attr(table, "label", exact = TRUE)), '">'),
    "<thead>", head_rows, "</thead>", "<tbody>", body_rows, "</tbody>", "</table>",
    if (length(notes)) paste0('<p class="sas-note-text">', html_escape(notes), "</p>"))
}

# One procedure output: the titles and the procedure title once, per BY group
# its byline and tables, then the footnotes.
render_html_output <- function(o) {
  out <- c('<div class="sas-proc">',
           if (length(o$titles)) paste0('<div class="sas-title">', html_escape(o$titles), "</div>"),
           paste0('<div class="sas-proctitle">The ', html_escape(toupper(o$proc)), " Procedure</div>"))
  group <- NULL
  for (tb in o$tables) {
    by <- attr(tb, "by", exact = TRUE)
    if (!is.null(by) && !identical(by$group, group)) {
      out <- c(out, paste0('<div class="sas-byline">', html_escape(by$text), "</div>"))
      group <- by$group
    }
    out <- c(out, render_html(tb))
  }
  c(out, if (length(o$footnotes)) paste0('<div class="sas-footnote">', html_escape(o$footnotes), "</div>"), "</div>")
}

#' The HTML document of a result
#'
#' The procedure output of a result as an HTML document: one table per ODS
#' table, under the titles, the procedure title and the BY lines.  Tables go to
#' the HTML destination while it is open: it is open when a program starts,
#' `ODS HTML CLOSE;` closes it and `ODS HTML;` opens it again (in a session the
#' state carries over to the next submission).  `ODS HTML FILE=` and the other
#' ODS HTML options are not supported; write the document with `file`.
#'
#' The style sheet is `system.file("css", "sasonr.css", package = "sasonr")`;
#' the document embeds it.
#' @param x A result of [sas_run()].
#' @param file `NULL`, or a path: the document is written there.
#' @param log `TRUE` adds the log after the tables (`sas_log(x, "html")`).
#' @return The document as one string (invisibly when `file` is given).
#' @examples
#' res <- sas_run("
#' data a;
#'   input grp $ x;
#'   datalines;
#' a 1
#' a 2
#' b 4
#' ;
#' run;
#' proc means data=a;
#'   class grp;
#'   var x;
#' run;
#' ")
#' doc <- sas_html(res)
#' substr(doc, 1, 60)
#' @export
sas_html <- function(x, file = NULL, log = FALSE) {
  if (!inherits(x, "sasonr_result")) rlang::abort("`x` must be a result of sas_run().")
  if (!is.null(file) && !(is.character(file) && length(file) == 1L && !is.na(file))) {
    rlang::abort("`file` must be NULL or one path.")
  }
  css <- readLines(system.file("css", "sasonr.css", package = "sasonr"), warn = FALSE)
  body <- unlist(lapply(attr(x, "html_output", exact = TRUE) %||% list(), render_html_output))
  doc <- paste(c("<!DOCTYPE html>", "<html>", "<head>", '<meta charset="utf-8">', "<title>sasonr output</title>",
                 "<style>", css, "</style>", "</head>", "<body>", '<div class="sasonr-output">', body, "</div>",
                 if (isTRUE(log)) sas_log(x, "html"), "</body>", "</html>"), collapse = "\n")
  if (is.null(file)) return(doc)
  writeLines(doc, file, useBytes = TRUE)
  invisible(doc)
}
