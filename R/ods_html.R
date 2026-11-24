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
  # a line break inside a value ("\n") is a <br/>
  values <- ifelse(nzchar(values), gsub("\n", "<br/>", html_escape(values), fixed = TRUE), "&nbsp;")
  body <- if (length(values) == 1L) values else paste0('<div class="sas-stack">', values, "</div>", collapse = "")
  cls <- c(cls, if (length(values) > 1L) "sas-stacked")
  paste0("<", tag, if (length(cls)) paste0(' class="', paste(cls, collapse = " "), '"'), attrs, ">", body, "</", tag, ">")
}

# HTML templates of the tables that are not drawn from their columns (the
# MEANS layout below): the template name of a table picks its renderer.
html_templates <- list(
  base.tabulate = function(tb) html_base_tabulate(tb),
  base.print = function(tb) abort_unsupported("the HTML output of PROC PRINT"),
  base.freq.oneway = function(tb) html_freq_oneway(tb),
  base.freq.crosstab = function(tb) html_freq_crosstab(tb),
  base.freq.list = function(tb) html_freq_list(tb),
  base.freq.chisq = function(tb) html_freq_chisq(tb),
  base.freq.fisher = function(tb) html_freq_fisher(tb),
  base.freq.cmh = function(tb) html_freq_cmh(tb),
  base.freq.commonrr = function(tb) html_freq_commonrr(tb),
  base.freq.breslow = function(tb) html_freq_breslow(tb),
  base.freq.relrisk = function(tb) html_freq_relrisk(tb),
  base.freq.relriskcl = function(tb) html_freq_relriskcl(tb),
  base.freq.riskdiff = function(tb) html_freq_riskdiff(tb),
  base.freq.riskdiffcl = function(tb) html_freq_riskdiffcl(tb),
  base.freq.binomial = function(tb) html_freq_binomial(tb),
  base.freq.onewaychisq = function(tb) html_freq_onewaychisq(tb),
  base.freq.nlevels = function(tb) html_freq_nlevels(tb)
)
# the templates without an HTML view (sas_html() fails loud on them)
html_undrawn <- c("base.print", "base.freq.lines")

# PROC FREQ tables [measured, freq_html cases: one-way, crosstabulation,
# chi-square].  The other FREQ tables have no HTML view yet.
html_freq_table <- function(tb, head, body) {
  cells <- list()
  add <- function(r, c, text, kind, cs = 1L) cells[[length(cells) + 1L]] <<- list(r0 = r, r1 = r, c0 = c, c1 = c + cs - 1L, text = text, kind = kind)
  for (j in seq_along(head)) add(1L, j, head[[j]], "header")
  for (i in seq_along(body)) for (j in seq_along(body[[i]])) add(i + 1L, j, body[[i]][[j]], if (j == 1L) "rowheader" else "data")
  render_html_grid(cells, length(body) + 1L, 1L, paste0("Procedure Freq: ", attr(tb, "label", exact = TRUE)))
}

html_freq_oneway <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  if (!is.null(a$test)) abort_unsupported("the HTML output of a PROC FREQ one-way table with test frequencies or percentages")
  cols <- attr(tb, "cols", exact = TRUE)
  shown <- cols$name[!cols$hidden][-1]
  labs <- c(Frequency = "Frequency", Percent = "Percent", CumFrequency = "Cumulative Frequency", CumPercent = "Cumulative Percent")
  fmt <- function(nm, v) if (nm %in% c("Frequency", "CumFrequency")) (if (is.na(v)) "." else freq_num(v)) else freq_pct(v)
  body <- lapply(seq_len(nrow(tb)), function(i) c(a$text[i], vapply(shown, function(nm) fmt(nm, tb[[nm]][i]), "")))
  html_freq_table(tb, c(a$disp, unname(labs[shown])), body)
}

html_freq_crosstab <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  cols <- attr(tb, "cols", exact = TRUE)
  stat_cols <- c(freq = "Frequency", expected = "Expected", deviation = "Deviation", cellchi2 = "CellChiSquare",
                 pct = "Percent", row = "RowPercent", col = "ColPercent")
  shown <- names(stat_cols)[!cols$hidden[match(stat_cols, cols$name)]]
  if (any(c("expected", "deviation", "cellchi2") %in% shown)) {
    abort_unsupported("the HTML output of a PROC FREQ crosstabulation with EXPECTED, DEVIATION or CELLCHI2")
  }
  nr <- a$nr
  nc <- a$nc
  at <- function(r, cc, s) {
    i <- if (!is.na(r) && !is.na(cc)) (r - 1L) * (nc + 1L) + cc else if (!is.na(r)) r * (nc + 1L) else nr * (nc + 1L) + (if (is.na(cc)) nc + 1L else cc)
    tb[[stat_cols[[s]]]][i]
  }
  txt <- function(s, v) if (s == "freq") freq_num(v) else freq_pct(v)
  legend <- render_html_grid(list(list(r0 = 1L, r1 = 1L, c0 = 1L, c1 = 1L, text = unname(all_labels_freq[shown]), kind = "data")),
                             1L, 0L, paste0("Procedure Freq: ", attr(tb, "label", exact = TRUE)))
  rh <- paste0(a$ri$disp, if (!is.null(a$ri$label)) paste0("(", a$ri$label, ")"))
  ch <- paste0(a$ci$disp, if (!is.null(a$ci$label)) paste0("(", a$ci$label, ")"))
  # the table heading keeps its words together (no-break spaces); a stratum
  # of a multiway request adds its "Controlling for" row
  nbsp <- function(x) gsub(" ", "\u00a0", x, fixed = TRUE)
  sr <- if (is.null(a$stratum)) 0L else 1L
  cells <- c(list(list(r0 = 1L, r1 = 1L, c0 = 1L, c1 = nc + 2L, text = nbsp(a$title %||% paste("Table of", a$ri$disp, "by", a$ci$disp)),
                       kind = "header")),
             if (sr) list(list(r0 = 2L, r1 = 2L, c0 = 1L, c1 = nc + 2L, text = nbsp(a$stratum), kind = "header")),
             list(list(r0 = 2L + sr, r1 = 3L + sr, c0 = 1L, c1 = 1L, text = rh, kind = "header"),
                  list(r0 = 2L + sr, r1 = 2L + sr, c0 = 2L, c1 = nc + 2L, text = ch, kind = "header")))
  add <- function(r, c, text, kind) cells[[length(cells) + 1L]] <<- list(r0 = r, r1 = r, c0 = c, c1 = c, text = text, kind = kind)
  for (cc in seq_len(nc)) add(3L + sr, cc + 1L, a$ctext[cc], "header")
  add(3L + sr, nc + 2L, "Total", "header")
  tot_stats <- intersect(shown, c("freq", "pct"))
  for (r in seq_len(nr)) {
    add(3L + sr + r, 1L, a$rtext[r], "rowheader")
    for (cc in seq_len(nc)) add(3L + sr + r, cc + 1L, vapply(shown, function(s) txt(s, at(r, cc, s)), ""), "data")
    add(3L + sr + r, nc + 2L, vapply(shown, function(s) if (s %in% tot_stats) txt(s, at(r, NA, s)) else "", ""), "data")
  }
  add(4L + sr + nr, 1L, "Total", "rowheader")
  for (cc in seq_len(nc)) add(4L + sr + nr, cc + 1L, vapply(tot_stats, function(s) txt(s, at(NA, cc, s)), ""), "data")
  add(4L + sr + nr, nc + 2L, vapply(tot_stats, function(s) txt(s, at(NA, NA, s)), ""), "data")
  c(legend, render_html_grid(cells, 4L + sr + nr, 3L + sr, paste0("Procedure Freq: ", attr(tb, "label", exact = TRUE))))
}

html_freq_list <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  cols <- attr(tb, "cols", exact = TRUE)
  stats <- c(Frequency = "Frequency", Percent = "Percent", CumFrequency = "Cumulative Frequency", CumPercent = "Cumulative Percent")
  shown <- names(stats)[!cols$hidden[match(names(stats), cols$name)]]
  fmt <- function(nm, v) if (nm %in% c("Frequency", "CumFrequency")) freq_num(v) else freq_pct(v)
  body <- lapply(seq_len(nrow(tb)), function(i) c(vapply(seq_len(a$k), function(j) a$text[[j]][i], ""),
                                                  vapply(shown, function(nm) fmt(nm, tb[[nm]][i]), "")))
  html_freq_table(tb, c(a$disp, unname(stats[shown])), body)
}

# The title block before the statistics of a stratum of a multiway request.
html_freq_heading <- function(tb) {
  h <- attr(tb, "heading", exact = TRUE)
  if (is.null(h)) return(character())
  paste0('<div class="sas-proctitle">', paste(html_escape(h), collapse = "<br/>"), "</div>")
}

html_freq_chisq <- function(tb) {
  c(html_freq_heading(tb), html_freq_chisq_table(tb))
}
html_freq_chisq_table <- function(tb) {
  f4 <- function(v) if (is.na(v)) "" else display_fixed(v, 4L)
  pv <- function(v) if (is.na(v)) "" else trimws(fmt_pvalue(v, 6L, 4L))
  body <- lapply(seq_len(nrow(tb)), function(i) c(tb$Statistic[i], if (is.na(tb$DF[i])) "" else sprintf("%.0f", tb$DF[i]),
                                                  f4(tb$Value[i]), pv(tb$Prob[i])))
  out <- html_freq_table(tb, c("Statistic", "DF", "Value", "Prob"), body)
  warn <- attr(tb, "freq", exact = TRUE)$warning
  if (length(warn)) {
    n <- length(out)
    out <- c(out[seq_len(n - 2L)], paste0('<tr><td class="sas-data" colspan="4">', html_escape(warn), "</td></tr>"), out[(n - 1L):n])
  }
  out
}

# A FREQ statistics table as rows of cells: `head` and `body` are lists of
# rows, each a list of cells list(text, cs = column span); the first cell of
# a body row is a row header unless the row is one cell over all columns.
html_freq_rows <- function(summary, head, body) {
  cells <- list()
  put <- function(rows, r0, kind) {
    for (i in seq_along(rows)) {
      c0 <- 1L
      for (cl in rows[[i]]) {
        cs <- cl$cs %||% 1L
        k <- if (kind == "header") "header" else if (c0 == 1L && length(rows[[i]]) > 1L) "rowheader" else "data"
        cells[[length(cells) + 1L]] <<- list(r0 = r0 + i - 1L, r1 = r0 + i - 1L, c0 = c0, c1 = c0 + cs - 1L, text = cl$text, kind = k)
        c0 <- c0 + cs
      }
    }
  }
  put(head, 1L, "header")
  put(body, length(head) + 1L, "data")
  render_html_grid(cells, length(head) + length(body), length(head), paste0("Procedure Freq: ", summary))
}
hc <- function(text, cs = 1L) list(text = text, cs = cs)
html_f4 <- function(v) if (is.na(v)) "" else display_fixed(v, 4L)
html_pv <- function(v) if (is.na(v)) "" else trimws(fmt_pvalue(v, 6L, 4L))

html_freq_fisher <- function(tb) {
  c(html_freq_heading(tb), html_freq_fisher_table(tb))
}
html_freq_fisher_table <- function(tb) {
  if (nrow(tb) != 5L) abort_unsupported("the HTML output of Fisher's exact test for a table that is not 2x2")
  val <- function(i) if (identical(tb$Name[i], "FISHER_F")) freq_num(tb$Value[i]) else html_pv(tb$Value[i])
  rows <- lapply(1:5, function(i) list(hc(tb$Label[i]), hc(val(i))))
  html_freq_rows("Fisher's Exact Test", list(list(hc("Fisher's Exact Test", 2L))), c(rows[1:3], list(list(hc(""), hc(""))), rows[4:5]))
}

html_freq_riskdiff <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  if (a$exact) abort_unsupported("the HTML output of RISKDIFF with exact confidence limits")
  head <- list(list(hc(paste0("Column ", a$column, " Risk Estimates"), 7L)),
               list(hc(""), hc("Risk"), hc("ASE"), hc(paste(a$level, "Confidence Limits"), 2L), hc(paste("Exact", a$level, "Confidence Limits"), 2L)))
  body <- lapply(1:4, function(i) list(hc(tb$Row[i]), hc(html_f4(tb$Risk[i])), hc(html_f4(tb$ASE[i])), hc(html_f4(tb$LowerCL[i])),
                                       hc(html_f4(tb$UpperCL[i])), hc(html_f4(tb$ExactLowerCL[i])), hc(html_f4(tb$ExactUpperCL[i]))))
  html_freq_rows(paste0("Column ", a$column, " Risk Estimates"), head, c(body, list(list(hc("Difference is (Row 1 - Row 2)", 7L)))))
}

html_freq_relrisk <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  if (nrow(tb) < 3L) abort_unsupported("the HTML output of RELRISK with a zero cell")
  # (no-break spaces in this heading, as in the HTML destination)
  head <- list(list(hc("Odds Ratio and Relative Risks", 4L)),
               list(hc("Statistic"), hc("Value"), hc(paste0(a$level, "\u00a0Confidence\u00a0Limits"), 2L)))
  body <- lapply(seq_len(nrow(tb)), function(i) list(hc(tb$Statistic[i]), hc(html_f4(tb$Value[i])), hc(html_f4(tb$LowerCL[i])),
                                                     hc(html_f4(tb$UpperCL[i]))))
  html_freq_rows("Relative Risk Estimates", head, body)
}

# RELRISK(CL=) and RISKDIFF(CL=) confidence-limit tables [measured,
# freq_html f19, f20; the no-break spaces are where the HTML destination
# writes them].
html_freq_cl <- function(tb, summary, title, value) {
  a <- attr(tb, "freq", exact = TRUE)
  head <- list(list(hc(title, 3L)), list(hc(value, 3L)), list(hc("Type"), hc(paste0(a$level, "\u00a0Confidence\u00a0Limits"), 2L)))
  body <- lapply(seq_len(nrow(tb)), function(i) list(hc(tb$Type[i]), hc(html_f4(tb$LowerCL[i])), hc(html_f4(tb$UpperCL[i]))))
  html_freq_rows(summary, head, c(body, list(list(hc(gsub(" ", "\u00a0", a$column, fixed = TRUE), 3L)))))
}
html_freq_relriskcl <- function(tb) {
  html_freq_cl(tb, "Relative Risk Confidence Limits", "Confidence Limits for the\nRelative Risk",
               paste("Relative Risk =", html_f4(attr(tb, "freq", exact = TRUE)$rr)))
}
html_freq_riskdiffcl <- function(tb) {
  html_freq_cl(tb, "Risk Difference Confidence Limits", "Confidence Limits for the Risk\nDifference",
               gsub(" ", "\u00a0", paste("Risk Difference =", html_f4(attr(tb, "freq", exact = TRUE)$diff)), fixed = TRUE))
}

# BINOMIAL: three tables.  Without CL= the Wald and exact limits are rows of
# the Binomial table and BinomialCLs draws nothing; with CL= they are the
# BinomialCLs table.
html_freq_binomial <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  switch(attr(tb, "name", exact = TRUE),
    Binomial = {
      body <- list(list(hc("Proportion"), hc(html_f4(tb$Value[1]))), list(hc("ASE"), hc(html_f4(tb$Value[2]))))
      if (a$default_cl) {
        lim <- function(lab, v) list(hc(paste(a$level, lab)), hc(html_f4(v)))
        body <- c(body, list(lim("Lower Conf Limit", a$wald[1]), lim("Upper Conf Limit", a$wald[2]), list(hc(""), hc("")),
                             list(hc("Exact Conf Limits"), hc("")), lim("Lower Conf Limit", a$exact[1]), lim("Upper Conf Limit", a$exact[2])))
      }
      html_freq_rows("Binomial Proportion", list(list(hc("Binomial Proportion", 2L)), list(hc(paste(a$var, "=", a$value), 2L))), body)
    },
    BinomialCLs = {
      if (a$default_cl) return(character())
      nm <- c(wald = "Wald", wilson = "Wilson", exact = "Clopper-Pearson (Exact)", agresticoull = "Agresti-Coull",
              jeffreys = "Jeffreys", wald_corrected = "Wald (Corrected)", wilson_corrected = "Wilson (Corrected)")
      o <- order(nm[tb$Type])
      # the heading wraps to the width of the table: measured for Wilson
      # (wrapped) and Clopper-Pearson (one line) only
      title <- if (identical(tb$Type, "wilson")) "Confidence Limits for the\nBinomial Proportion" else
        if (identical(tb$Type, "exact")) "Confidence Limits for the Binomial Proportion" else
          abort_unsupported("the HTML output of the binomial confidence limits of these types")
      head <- list(list(hc(title, 3L)), list(hc(paste("Proportion =", html_f4(a$prop)), 3L)),
                   list(hc("Type"), hc(paste0(a$level, "\u00a0Confidence\u00a0Limits"), 2L)))
      body <- lapply(o, function(i) list(hc(unname(nm[tb$Type[i]])), hc(html_f4(tb$LowerCL[i])), hc(html_f4(tb$UpperCL[i]))))
      html_freq_rows("Binomial Confidence Limits", head, body)
    },
    BinomialTest = {
      if (a$exact) abort_unsupported("the HTML output of the exact binomial test")
      side <- if (tb$Value[2] > 0) ">" else "<"
      body <- list(list(hc("ASE under H0"), hc(html_f4(tb$Value[1]))), list(hc("Z"), hc(html_f4(tb$Value[2]))),
                   list(hc(paste0("One-sided Pr ", side, " Z")), hc(html_pv(tb$Value[3]))), list(hc("Two-sided Pr > |Z|"), hc(html_pv(tb$Value[4]))))
      # the continuity-correction footnote [measured, f23 / f24]
      if (a$corrected) {
        body <- c(body, list(list(hc(if (a$default_cl) "The asymptotic confidence limits and test\ninclude a continuity correction." else
          "The asymptotic test includes a continuity\ncorrection.", 2L))))
      }
      html_freq_rows("Binomial Proportion Test", list(list(hc(paste0("Test of H0: Proportion = ", freq_num(a$p0)), 2L))), body)
    })
}

# The multiway CMH summary tables [measured, freq_html f16].
html_freq_cmh <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  head <- list(list(hc(gsub(" ", "\u00a0", paste0("Cochran-Mantel-Haenszel Statistics (Based on ", a$scores, ")"), fixed = TRUE), 5L)),
               list(hc("Statistic"), hc("Alternative Hypothesis"), hc("DF"), hc("Value"), hc("Prob")))
  body <- lapply(seq_len(nrow(tb)), function(i) list(hc(as.character(tb$Statistic[i])), hc(tb$AltHyp[i]), hc(sprintf("%.0f", tb$DF[i])),
                                                     hc(html_f4(tb$Value[i])), hc(html_pv(tb$Prob[i]))))
  html_freq_rows("Cochran-Mantel-Haenszel Statistics", head, body)
}

html_freq_commonrr <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  head <- list(list(hc("Common Odds Ratio and Relative Risks", 5L)),
               list(hc("Statistic"), hc("Method"), hc("Value"), hc(paste0(a$level, "\u00a0Confidence\u00a0Limits"), 2L)))
  body <- lapply(seq_len(nrow(tb)), function(i) list(hc(if (identical(tb$Method[i], "Logit")) "" else tb$Statistic[i]), hc(tb$Method[i]),
                                                     hc(html_f4(tb$Value[i])), hc(html_f4(tb$LowerCL[i])), hc(html_f4(tb$UpperCL[i]))))
  html_freq_rows("Common Relative Risk Estimates", head, body)
}

html_freq_breslow <- function(tb) {
  body <- list(list(hc("Chi-Square"), hc(html_f4(tb$Value[1]))), list(hc("DF"), hc(sprintf("%.0f", tb$Value[2]))),
               list(hc("Pr > ChiSq"), hc(html_pv(tb$Value[3]))))
  html_freq_rows("Breslow-Day Test", list(list(hc("Breslow-Day Test for\nHomogeneity of Odds Ratios", 2L))), body)
}

html_freq_onewaychisq <- function(tb) {
  a <- attr(tb, "freq", exact = TRUE)
  if (a$small) abort_unsupported("the HTML output of the one-way chi-square test with small expected counts")
  body <- list(list(hc("Chi-Square"), hc(html_f4(tb$Value[1]))), list(hc("DF"), hc(sprintf("%.0f", tb$Value[2]))),
               list(hc("Pr > ChiSq"), hc(html_pv(tb$Value[3]))))
  html_freq_rows("One-Way Chi-Square Test", list(list(hc(paste0("Chi-Square Test\n", a$test), 2L))), body)
}

html_freq_nlevels <- function(tb) {
  body <- lapply(seq_len(nrow(tb)), function(i) list(hc(tb$TableVar[i]), hc(sprintf("%.0f", tb$NLevels[i]))))
  html_freq_rows("NLevels", list(list(hc("Number of Variable\nLevels", 2L)), list(hc("Variable"), hc("Levels"))), body)
}

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
