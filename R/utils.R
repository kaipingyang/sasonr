# Internal utilities: classed conditions and small code-generation helpers.

sasonr_abort <- function(msg, class, line = NULL) {
  if (!is.null(line) && length(line) == 1 && !is.na(line)) {
    msg <- paste0(msg, " (SAS line ", line, ")")
  }
  rlang::abort(msg, class = c(class, "sasonr_error"), sas_line = line)
}

abort_unsupported <- function(msg, line = NULL) {
  sasonr_abort(paste("Unsupported:", msg), "sasonr_unsupported", line)
}

abort_syntax <- function(msg, line = NULL) {
  sasonr_abort(paste("Syntax error:", msg), "sasonr_syntax_error", line)
}

abort_runtime <- function(msg, line = NULL) {
  sasonr_abort(paste("Runtime error:", msg), "sasonr_runtime_error", line)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Split "date9" into list(base = "date", width = 9L); no regex needed.
split_trailing_digits <- function(s) {
  cp <- utf8ToInt(s)
  is_digit <- cp >= 48L & cp <= 57L
  k <- length(cp)
  while (k > 0 && is_digit[k]) k <- k - 1L
  base <- if (k > 0) intToUtf8(cp[seq_len(k)]) else ""
  digits <- if (k < length(cp)) intToUtf8(cp[(k + 1L):length(cp)]) else ""
  list(base = base, width = if (nzchar(digits)) as.integer(digits) else NA_integer_)
}

# Expand a numbered variable range such as x1-x3.
expand_numbered_range <- function(from, to, line = NULL) {
  a <- split_trailing_digits(from)
  b <- split_trailing_digits(to)
  if (is.na(a$width) || is.na(b$width) || a$base != b$base || a$width > b$width) {
    abort_syntax(paste0("invalid variable range ", from, "-", to), line)
  }
  paste0(a$base, seq(a$width, b$width))
}

# R code helpers ----------------------------------------------------------

r_name <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", x, "`"))
}

r_str <- function(x) {
  vapply(x, function(s) encodeString(s, quote = "\""), character(1), USE.NAMES = FALSE)
}

r_num <- function(x) {
  if (is.na(x)) return("NA_real_")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  s <- format(x, digits = 15)
  if (as.numeric(s) != x) s <- sprintf("%.17g", x)
  s
}

r_chr_vec <- function(x) {
  if (length(x) == 0) return("character()")
  if (length(x) == 1) return(r_str(x))
  paste0("c(", paste(r_str(x), collapse = ", "), ")")
}

r_lit <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")

indent_lines <- function(x, n = 1L) {
  if (length(x) == 0) return(x)
  pad <- strrep("  ", n)
  ifelse(nzchar(x), paste0(pad, x), x)
}

`%|NA|%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else unname(x)
