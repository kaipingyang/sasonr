# HTML destination: the HTML view of ODS tables.
#
# render_html() turns one sasonr_ods_table into a <table class="sas-table">; the
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

# HTML templates of the tables that are not drawn from their columns (the
# MEANS layout below): the template name of a table picks its renderer.
html_templates <- list(
  base.tabulate = function(tb) html_base_tabulate(tb),
  base.print = function(tb) abort_unsupported("the HTML output of PROC PRINT"),
  base.freq.oneway = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.crosstab = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.chisq = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.fisher = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.cmh = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.commonrr = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.breslow = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.relrisk = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.relriskcl = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.riskdiff = function(tb) abort_unsupported("the HTML output of PROC FREQ"),
  base.freq.riskdiffcl = function(tb) abort_unsupported("the HTML output of PROC FREQ")
)
# the templates without an HTML view (sas_html() fails loud on them)
html_undrawn <- c("base.print", "base.freq.oneway", "base.freq.crosstab", "base.freq.chisq", "base.freq.fisher", "base.freq.cmh",
                  "base.freq.commonrr", "base.freq.breslow", "base.freq.relrisk", "base.freq.relriskcl",
                  "base.freq.riskdiff", "base.freq.riskdiffcl", "base.freq.lines")

# A table laid out on a grid: `cells` is a list of rectangles list(r0, r1,
# c0, c1, text, kind) over logical rows 1..nrow, `kind` "header" (in
# <thead>, rows 1..nhead), "rowheader" or "data".  Each cell is written in
# the row where it starts, in column order, with rowspan/colspan.
render_html_grid <- function(cells, nrow, nhead, summary) {
  r0 <- vapply(cells, `[[`, 1L, "r0")
  c0 <- vapply(cells, `[[`, 1L, "c0")
  rows <- vapply(seq_len(nrow), function(r) {
    own <- cells[which(r0 == r)[order(c0[r0 == r])]]
    tds <- vapply(own, function(cl) {
      rs <- cl$r1 - cl$r0 + 1L
      cs <- cl$c1 - cl$c0 + 1L
      head <- identical(cl$kind, "header")
      scope <- if (head) (if (cs > 1L) "colgroup" else "col") else if (rs > 1L) "rowgroup" else "row"
      attrs <- paste0(if (!identical(cl$kind, "data")) paste0(' scope="', scope, '"'),
                      if (rs > 1L) paste0(' rowspan="', rs, '"'), if (cs > 1L) paste0(' colspan="', cs, '"'))
      switch(cl$kind,
        header = html_cell(cl$text, "th", "sas-header", attrs),
        rowheader = html_cell(cl$text, "th", "sas-rowheader", attrs),
        data = html_cell(cl$text, "td", c("sas-data", "sas-num"), attrs)
      )
    }, "")
    paste0("<tr>", paste(tds, collapse = ""), "</tr>")
  }, "")
  c(paste0('<table class="sas-table" summary="', html_escape(summary), '">'),
    "<thead>", rows[seq_len(nhead)], "</thead>", "<tbody>", rows[setdiff(seq_len(nrow), seq_len(nhead))], "</tbody>",
    "</table>")
}

#' @noRd
render_html <- function(table) {
  validate_sasonr_ods_table(table)
  tpl <- html_templates[[attr(table, "template", exact = TRUE) %||% ""]]
  if (!is.null(tpl)) return(tpl(table))
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

# Whether a procedure writes its title line ("The MEANS Procedure"): not
# when its PROC registry entry says so (PROC TABULATE).
html_proctitle <- function(proc) {
  key <- tolower(proc)
  if (key %in% names(proc_registry_alias)) key <- proc_registry_alias[[key]]
  !identical(proc_registry[[key]]$proctitle, FALSE)
}

# One procedure output: the titles and the procedure title once, per BY group
# its byline and tables, then the footnotes.
render_html_output <- function(o) {
  out <- c('<div class="sas-proc">',
           if (length(o$titles)) paste0('<div class="sas-title">', html_escape(o$titles), "</div>"),
           if (html_proctitle(o$proc)) paste0('<div class="sas-proctitle">The ', html_escape(toupper(o$proc)), " Procedure</div>"))
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
#' @param x A result of [sas_run()], or of a data-level PROC function
#'   ([sas_means_df()], [sas_tabulate_df()]).
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
sas_html <- function(x, file = NULL, log = FALSE) UseMethod("sas_html")

#' @export
sas_html.default <- function(x, file = NULL, log = FALSE) {
  rlang::abort("`x` must be a result of sas_run() or an ODS table of a data-level PROC function.", class = "sasonr_error")
}

#' @export
sas_html.sasonr_result <- function(x, file = NULL, log = FALSE) {
  outs <- attr(x, "html_output", exact = TRUE) %||% list()
  # a table the HTML destination received but that has no HTML view
  for (o in outs) {
    tpl <- vapply(o$tables, function(tb) attr(tb, "template", exact = TRUE) %||% "", "")
    if (any(tpl %in% html_undrawn)) abort_unsupported(paste0("the HTML output of PROC ", toupper(o$proc)), o$line)
  }
  body <- unlist(lapply(outs, render_html_output))
  html_document(body, if (isTRUE(log)) sas_log(x, "html"), file)
}

#' @export
sas_html.sasonr_ods_table <- function(x, file = NULL, log = FALSE) {
  sas_html(structure(list(x), class = "sasonr_ods_list", sasonr_log = attr(x, "sasonr_log", exact = TRUE)), file, log)
}

#' @export
sas_html.sasonr_ods_list <- function(x, file = NULL, log = FALSE) {
  tables <- unclass(x)
  attributes(tables) <- NULL
  proc <- if (length(tables)) attr(tables[[1]], "proc", exact = TRUE) else ""
  body <- render_html_output(list(proc = proc, titles = character(), footnotes = character(), tables = tables))
  html_document(body, if (isTRUE(log)) sas_log(x, "html"), file)
}

html_document <- function(body, log_html, file) {
  if (!is.null(file) && !(is.character(file) && length(file) == 1L && !is.na(file))) {
    rlang::abort("`file` must be NULL or one path.")
  }
  css <- readLines(system.file("css", "sasonr.css", package = "sasonr"), warn = FALSE)
  doc <- paste(c("<!DOCTYPE html>", "<html>", "<head>", '<meta charset="utf-8">', "<title>sasonr output</title>",
                 "<style>", css, "</style>", "</head>", "<body>", '<div class="sasonr-output">', body, "</div>",
                 log_html, "</body>", "</html>"), collapse = "\n")
  if (is.null(file)) return(doc)
  writeLines(doc, file, useBytes = TRUE)
  invisible(doc)
}
