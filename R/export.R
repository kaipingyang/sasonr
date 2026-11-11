# Export WORK data sets the way SAS PROC EXPORT (DBMS=CSV) writes them, plus a
# table of variable attributes shaped like DICTIONARY.COLUMNS. Used by the
# conformance harness to compare data sets with real SAS.

csv_field <- function(x) {
  x[is.na(x)] <- ""
  q <- grepl("[,\"\n]", x, perl = TRUE)
  x[q] <- paste0("\"", gsub("\"", "\"\"", x[q], fixed = TRUE), "\"")
  x
}

fmt_attr_text <- function(f) {
  if (is.null(f)) return("")
  toupper(f$text)
}

# Values of one column as PROC EXPORT writes them: formatted values for
# variables with a format, BEST12. for other numbers, blank for missing.
export_values <- function(x, fmt) {
  if (is.character(x)) {
    out <- if (is.null(fmt)) x else sas_format(x, fmt)
    return(sas_rtrim(sub("^ +", "", out)))
  }
  out <- trimws(sas_format(x, fmt))
  out[is.na(x) & out == "."] <- ""
  out
}

#' Write WORK data sets as CSV files with an attribute table
#'
#' Writes `<MEMNAME>.csv` for each data set of a [sas_run()] result (column
#' names as stored, formatted values, blank missing values) and `__attr.csv`
#' with columns memname, varnum, name, type, length, format, informat, label.
#' @param res A `sasonr_result`.
#' @param dir Output directory (created if needed).
#' @examples
#' res <- sas_run("data a; x = 1.5; y = 'b'; run;")
#' out <- file.path(tempdir(), "work_csv")
#' sas_export_work(res, out)
#' readLines(file.path(out, "A.csv"))
#' @export
sas_export_work <- function(res, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  attr_rows <- list()
  for (nm in names(res$work)) {
    d <- res$work[[nm]]
    m <- res$meta[[nm]] %||% meta_from_df(d)
    vars <- names(d)
    disp <- vapply(vars, function(v) m$disp[[v]] %||% v, "")
    cols <- lapply(vars, function(v) csv_field(export_values(d[[v]], m$formats[[v]])))
    lines <- csv_field(disp)
    lines <- paste(lines, collapse = ",")
    if (nrow(d) > 0 && length(vars) > 0) lines <- c(lines, do.call(paste, c(cols, sep = ",")))
    # PROC EXPORT writes records of at most LRECL=32767 bytes; longer ones are cut
    long <- nchar(lines, type = "bytes") > 32767L
    if (any(long)) lines[long] <- vapply(lines[long], function(l) rawToChar(charToRaw(l)[1:32767]), "")
    writeLines(lines, file.path(dir, paste0(toupper(nm), ".csv")), useBytes = TRUE)
    for (k in seq_along(vars)) {
      v <- vars[k]
      char <- is.character(d[[v]])
      len <- m$len[[v]] %||% (if (char) max(1L, nchar(d[[v]])) else 8L)
      if (is.na(len)) len <- 8L
      attr_rows[[length(attr_rows) + 1L]] <- c(
        toupper(nm), k, disp[[k]], if (char) "char" else "num", len,
        fmt_attr_text(m$formats[[v]]), fmt_attr_text(m$informats[[v]]), m$labels[[v]] %||% ""
      )
    }
  }
  hdr <- "memname,varnum,name,type,length,format,informat,label"
  body <- vapply(attr_rows, function(r) paste(csv_field(r), collapse = ","), "")
  writeLines(c(hdr, body), file.path(dir, "__attr.csv"), useBytes = TRUE)
  if (length(res$unordered) > 0) writeLines(toupper(res$unordered), file.path(dir, "__unordered.txt"))
  invisible(dir)
}
