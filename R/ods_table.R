# ODS tables: the output object of a procedure.
#
# A `sas_ods_table` is a tibble of raw values (numbers unformatted, character
# values as stored) with the SAS metadata of the table as attributes.  The
# listing (and later the HTML) renderers build their text from it, so every
# destination shows the same table.
#
#   name, label, path  ODS table name, label and path (`Summary`,
#                      "Summary statistics", `Means.Summary`; with BY groups
#                      `Means.ByGroup1.Summary`)
#   proc, template     producing procedure and the listing template id
#   by                 list(values = <named list of the BY values>,
#                      text = <byline>), NULL without BY
#   cols               tibble, one row per column of the table: name, label,
#                      format (display spec, list), hidden, role ("class",
#                      "stat", "var", "label"), var, stat, info (variable
#                      attributes, list)
#   header             tibble of header cells: level, text (list of heading
#                      parts), span, columns (list of the data columns under
#                      the cell), role
#   notes              lines that belong to the table

ods_table_roles <- c("class", "stat", "var", "label")

ods_cols <- function(name = character(), label = NA_character_, format = list(NULL), hidden = FALSE,
                     role = "stat", var = NA_character_, stat = NA_character_, info = list(NULL)) {
  n <- length(name)
  tibble::tibble(
    name = name, label = vctrs::vec_recycle(label, n), format = vctrs::vec_recycle(format, n),
    hidden = vctrs::vec_recycle(hidden, n), role = vctrs::vec_recycle(role, n),
    var = vctrs::vec_recycle(var, n), stat = vctrs::vec_recycle(stat, n), info = vctrs::vec_recycle(info, n)
  )
}

ods_header <- function(level = integer(), text = list(), span = integer(), columns = list(), role = character()) {
  tibble::tibble(level = as.integer(level), text = text, span = as.integer(span), columns = columns, role = role)
}

new_sas_ods_table <- function(x = tibble::tibble(), name = character(), label = character(), path = character(),
                              proc = character(), template = character(), by = NULL, cols = ods_cols(),
                              header = ods_header(), notes = character()) {
  stopifnot(is.data.frame(x))
  tibble::new_tibble(
    x, name = name, label = label, path = path, proc = proc, template = template, by = by,
    cols = cols, header = header, notes = notes, class = "sas_ods_table"
  )
}

validate_sas_ods_table <- function(x) {
  bad <- function(msg) rlang::abort(paste0("Invalid sas_ods_table: ", msg), class = "sasonr_internal_error")
  if (!inherits(x, "sas_ods_table") || !tibble::is_tibble(x)) bad("not a tibble of class sas_ods_table")
  for (a in c("name", "label", "path", "proc", "template")) {
    v <- attr(x, a, exact = TRUE)
    if (!is.character(v) || length(v) != 1L || is.na(v)) bad(paste0("`", a, "` must be one string"))
  }
  by <- attr(x, "by", exact = TRUE)
  if (!is.null(by) && !(is.list(by) && all(c("values", "text") %in% names(by)))) bad("`by` must be NULL or list(values, text)")
  cols <- attr(x, "cols", exact = TRUE)
  need <- c("name", "label", "format", "hidden", "role", "var", "stat", "info")
  if (!is.data.frame(cols) || !all(need %in% names(cols))) bad("`cols` must have the columns name, label, format, hidden, role, var, stat, info")
  if (!identical(cols$name, names(x))) bad("`cols$name` must list the table columns in order")
  if (!all(cols$role %in% ods_table_roles)) bad("unknown column role")
  header <- attr(x, "header", exact = TRUE)
  if (!is.data.frame(header) || !all(c("level", "text", "span", "columns", "role") %in% names(header))) {
    bad("`header` must have the columns level, text, span, columns, role")
  }
  hc <- unlist(header$columns, use.names = FALSE)
  if (!all(hc %in% names(x))) bad("`header` refers to a column that is not in the table")
  if (!is.character(attr(x, "notes", exact = TRUE))) bad("`notes` must be a character vector")
  invisible(x)
}

#' @export
print.sas_ods_table <- function(x, ...) {
  a <- function(nm) attr(x, nm, exact = TRUE)
  cat("<sas_ods_table> ", a("name"), " (", a("label"), ")\n", sep = "")
  cat("  path:     ", a("path"), "\n", sep = "")
  cat("  proc:     ", a("proc"), ", template ", a("template"), "\n", sep = "")
  if (!is.null(a("by"))) cat("  by:       ", a("by")$text, "\n", sep = "")
  cols <- a("cols")
  cat("  columns:\n")
  for (i in seq_len(nrow(cols))) {
    cat("    ", cols$name[i], " [", cols$role[i], if (isTRUE(cols$hidden[i])) ", hidden", "]",
        if (!is.na(cols$label[i])) paste0(" \"", cols$label[i], "\""), "\n", sep = "")
  }
  hd <- a("header")
  if (nrow(hd) > 0) {
    for (lv in sort(unique(hd$level))) {
      h <- hd[hd$level == lv, ]
      cat("  header ", lv, ": ", paste(vapply(h$text, paste, "", collapse = " "), collapse = " | "), "\n", sep = "")
    }
  }
  if (length(a("notes"))) cat("  notes:    ", paste(a("notes"), collapse = "; "), "\n", sep = "")
  cat("  rows:     ", nrow(x), "\n", sep = "")
  invisible(x)
}
