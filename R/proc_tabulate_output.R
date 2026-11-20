# PROC TABULATE: the output data set of the `Table` object.
#
# tab_output_data() is the one function that builds the columns of the
# TABULATE output data set: `ODS OUTPUT Table=` calls it, and OUT= is to call
# it as well.  It reads the cells of the tables (the `cells` attribute of
# tab_table_pages()), not the printed grid.
#
# Layout: the CLASS variables (name, length, label and format of the input
# variable; a format without a width gets the width the format uses), then
# _TYPE_ (character, one 0/1 per CLASS variable: the variable restricts the
# class-level combination), _PAGE_ (number of the page), _TABLE_ (number of
# the TABLE statement), then one numeric column per analysis variable and
# statistic: <var>_N, <var>_Mean, <var>_Sum, <var>_PctN_<0...0>_<var> (one 0
# per CLASS variable: the whole-table denominator), and N for the count of a
# cell without an analysis variable.  One observation per class-level
# combination: the cells of the same combination share it.  The combinations
# are ordered by type (the order the types first occur, row-major over the
# cells, which must also be the descending order of _TYPE_), then by page and
# by the order of the cells.  A CLASS variable outside the combination is
# blank (character) or missing (numeric).  Layouts outside this fail loud.

tab_output_stats <- c(n = "N", mean = "Mean", sum = "Sum", pctn = "PctN")

tab_output_data <- function(tables, sp, line) {
  no <- function(what) abort_unsupported(paste("ODS OUTPUT of the TABULATE Table with", what), line)
  if (length(sp$irs) != 1L) no("more than one TABLE statement")
  if (!length(tables)) no("no table")
  info <- sp$info
  cn <- names(sp$class)
  if (!length(cn)) no("no CLASS variable")
  if (!identical(cn, sp$ds_order)) no("CLASS variables in an order other than the order of the input data set")
  lv <- attr(tables[[1]], "levels", exact = TRUE)
  if (!setequal(names(lv), cn)) no("a CLASS variable that the table does not use")
  for (cl in sp$class) {
    if (cl$missing || cl$descending || !identical(cl$order, "internal")) no("the CLASS options MISSING, ORDER= or DESCENDING")
  }
  if (isTRUE(sp$label_stmt)) no("a LABEL statement")
  if (length(sp$keylabel) || !is.null(sp$format)) no("KEYLABEL or the PROC FORMAT= option")
  ir <- sp$irs[[1]]
  for (dn in names(ir$dims)) {
    for (n in tab_nodes(ir$dims[[dn]])) {
      if (!is.null(n$format)) no("F= in the TABLE statement")
      if (!is.na(n$label)) no("a label in the TABLE statement")
      if (dn != "col" && n$kind %in% c("stat", "var")) no("a statistic or analysis variable outside the column dimension")
    }
  }
  ncl <- length(cn)
  stat_col <- function(var, stat) {
    if (!stat %in% names(tab_output_stats)) no(paste0("the statistic ", toupper(stat)))
    if (is.null(var)) {
      if (stat != "n") no(paste0("the statistic ", toupper(stat), " without an analysis variable"))
      return("N")
    }
    d <- info[[var]]$disp
    if (stat == "pctn") paste0(d, "_PctN_", strrep("0", ncl), "_", d) else paste0(d, "_", tab_output_stats[[stat]])
  }

  # observations: one per (type, page, levels); cells in row-major order
  obs <- list()
  stat_names <- character()
  for (tb in tables) {
    pg <- attr(tb, "page_number", exact = TRUE)
    cells <- attr(tb, "cells", exact = TRUE)
    ord <- order(vapply(cells, `[[`, 1L, "row"), vapply(cells, `[[`, 1L, "col"))
    for (cell in cells[ord]) {
      key <- cell$key
      if (anyDuplicated(names(key))) no("a CLASS variable crossed with itself")
      if (cell$n == 0L) no("a cell without observations")
      type <- paste(ifelse(cn %in% names(key), "1", "0"), collapse = "")
      id <- paste(type, pg, paste(names(key), key, collapse = " "))
      sn <- stat_col(cell$var, cell$stat)
      if (!sn %in% stat_names) stat_names <- c(stat_names, sn)
      o <- obs[[id]] %||% list(type = type, page = pg, key = key, stats = list())
      if (!identical(names(o$key), names(key))) no("class-level combinations of one type nested in different orders")
      if (sn %in% names(o$stats)) no("the same statistic twice for one class-level combination")
      o$stats[[sn]] <- cell$value
      obs[[id]] <- o
    }
  }
  types <- unique(vapply(obs, `[[`, "", "type"))
  if (!identical(types, types[order(types, decreasing = TRUE, method = "radix")])) {
    no("class-level combinations whose types first occur out of descending _TYPE_ order")
  }
  if (!is.null(ir$dims$page) && length(types) > 1L) no("a page dimension and more than one type of class-level combination")
  obs <- unname(obs[order(match(vapply(obs, `[[`, "", "type"), types))])
  for (o in obs) if (!setequal(names(o$stats), stat_names)) no("statistics that differ between class-level combinations")
  all_names <- c(vapply(cn, function(v) info[[v]]$disp, ""), "_TYPE_", "_PAGE_", "_TABLE_", stat_names)
  if (anyDuplicated(tolower(all_names))) no(paste0("two columns named ", toupper(all_names[anyDuplicated(tolower(all_names))])))

  # columns
  nobs <- length(obs)
  cols <- list()
  data <- list()
  for (v in cn) {
    i <- info[[v]]
    f <- i$fmt
    if (!is.null(f) && is.na(f$w)) {
      uf <- user_fmt(f$name, i$char)
      if (is.null(uf)) no(paste0("the format ", toupper(f$text), " without a width on a CLASS variable"))
      f <- fmt_spec(f$name, uf$default_w, f$d, f$char)
    }
    cols[[length(cols) + 1L]] <- list(name = v, disp = i$disp, char = i$char, len = i$len, fmt = f, label = i$label)
    vals <- lv[[v]]
    col <- vctrs::vec_init(vals, nobs)
    if (i$char) col[] <- ""
    for (k in seq_len(nobs)) {
      key <- obs[[k]]$key
      if (v %in% names(key)) col[k] <- vals[key[[v]]]
    }
    data[[v]] <- col
  }
  cols[[length(cols) + 1L]] <- list(name = "_type_", disp = "_TYPE_", char = TRUE, len = ncl, label = "Type of Observation")
  data[["_type_"]] <- vapply(obs, `[[`, "", "type")
  cols[[length(cols) + 1L]] <- list(name = "_page_", disp = "_PAGE_", char = FALSE, len = 8L, label = "Page for Observation")
  data[["_page_"]] <- as.numeric(vapply(obs, `[[`, 1L, "page"))
  cols[[length(cols) + 1L]] <- list(name = "_table_", disp = "_TABLE_", char = FALSE, len = 8L, label = "Table for Observation")
  data[["_table_"]] <- rep(as.numeric(attr(tables[[1]], "table", exact = TRUE)), nobs)
  for (sn in stat_names) {
    cols[[length(cols) + 1L]] <- list(name = tolower(sn), disp = sn, char = FALSE, len = 8L)
    data[[tolower(sn)]] <- vapply(obs, function(o) as.numeric(o$stats[[sn]]), 0)
  }
  meta <- proc_out_meta(cols)
  data <- tibble::new_tibble(data, nrow = nobs)
  names(data) <- meta$vars
  list(data = data, meta = meta)
}
