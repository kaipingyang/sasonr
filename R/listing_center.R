# OPTIONS CENTER for the listing.
#
# Under CENTER each centring unit of a procedure's output moves right by
# floor((LINESIZE - w) / 2), w being the length of its longest line (without
# trailing blanks): a TITLE line, the procedure title, a BY line and a heading
# line above a table are units of one line; a table is one unit (all its lines
# keep their relative position).  [measured, PROC PRINT, MEANS, FREQ,
# TABULATE, CONTENTS and SQL at LINESIZE= 64, 72, 80 and 132]
#
# The listing code of a procedure marks the units: lst_unit() makes its lines
# one unit, lst_lines() makes each line a unit of its own and lst_c()
# concatenates marked listings.  Output without marks cannot be centred: under
# CENTER it fails loud (engine_add_listing()).

lst_unit <- function(lines) {
  lines <- as.character(lines)
  structure(lines, units = rep(1L, length(lines)))
}

lst_lines <- function(lines) {
  lines <- as.character(lines)
  structure(lines, units = seq_along(lines))
}

lst_by_block <- function(blank_before, text) lst_c(lst_lines(blank_before), lst_by(text), lst_lines(""))

# A BY line: under CENTER it is written between dashes over the whole line.
lst_by <- function(text) structure(as.character(text), units = 1L, by = TRUE)

# Lines centred on their own with floor rounded up: (LINESIZE - w + 1) %/% 2
# (a PROC CONTENTS heading wider than its table).
lst_lines_up <- function(lines) structure(as.character(lines), units = seq_along(lines), up = rep(TRUE, length(lines)))

lst_c <- function(...) {
  parts <- list(...)
  out <- character()
  units <- integer()
  by <- logical()
  up <- logical()
  top <- 0L
  for (p in parts) {
    if (is.null(p) || !length(p)) next
    u <- attr(p, "units", exact = TRUE)
    if (is.null(u)) return(structure(unlist(lapply(parts, as.character)), units = NULL))
    out <- c(out, as.character(p))
    units <- c(units, u + top)
    by <- c(by, attr(p, "by", exact = TRUE) %||% rep(FALSE, length(p)))
    up <- c(up, attr(p, "up", exact = TRUE) %||% rep(FALSE, length(p)))
    top <- top + max(c(0L, u))
  }
  structure(out, units = units, by = by, up = up)
}

lst_marked <- function(lst) length(lst) == 0L || !is.null(attr(lst, "units", exact = TRUE))

lst_center <- function(lst, ls) {
  u <- attr(lst, "units", exact = TRUE)
  out <- as.character(lst)
  by <- attr(lst, "by", exact = TRUE) %||% rep(FALSE, length(out))
  for (i in which(by)) {
    t <- out[i]
    a <- (ls - nchar(t) - 2L) %/% 2L
    if (a < 1L) abort_unsupported("a BY line under OPTIONS CENTER as wide as the line")
    out[i] <- paste0(strrep("-", a), " ", t, " ", strrep("-", ls - nchar(t) - 2L - a))
  }
  for (k in unique(u)) {
    i <- which(u == k & nzchar(trimws(out)))
    if (!length(i)) next
    w <- max(nchar(sas_rtrim(out[i])))
    off <- max(0L, (ls - w + if (isTRUE((attr(lst, "up", exact = TRUE) %||% FALSE)[i[1]])) 1L else 0L) %/% 2L)
    if (off > 0L) out[i] <- paste0(strrep(" ", off), out[i])
  }
  out
}

#' Append the listing of a procedure to the step's listing, keeping the
#' centring units of both
#' @param ... Listings.
#' @export
#' @keywords internal
sas_lst_c <- function(...) lst_c(...)
