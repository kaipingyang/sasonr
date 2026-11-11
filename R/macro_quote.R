# Macro quoting: quoted characters are replaced by private-use code points
# (U+E000...) while they travel through the macro layer, so they are neither
# macro triggers nor delimiters. They are restored when text leaves the macro
# layer (compiler-bound text, %PUT output, macro function arguments).

mq_plain <- c(";", ",", "'", "\"", "(", ")", "+", "-", "*", "/", "<", ">", "=",
              "^", "~", "|", "#", " ", "\t", "\n", "\u00ac", "&", "%")
mq_masked <- intToUtf8(0xE000 + seq_along(mq_plain) - 1L, multiple = TRUE)
mq_n_nontrigger <- length(mq_plain) - 2L # everything except & and %

mq_plain_cp <- utf8ToInt(paste(mq_plain, collapse = ""))
mq_masked_cp <- 0xE000L + seq_along(mq_plain) - 1L

mq_map <- function(x, from, to) {
  vapply(x, function(s) {
    cp <- utf8ToInt(s)
    k <- match(cp, from)
    if (all(is.na(k))) return(s)
    cp[!is.na(k)] <- to[k[!is.na(k)]]
    intToUtf8(cp)
  }, "", USE.NAMES = FALSE)
}

mq_mask <- function(x, triggers = TRUE) {
  k <- if (triggers) length(mq_plain) else mq_n_nontrigger
  mq_map(x, mq_plain_cp[seq_len(k)], mq_masked_cp[seq_len(k)])
}

mq_unmask <- function(x) mq_map(x, mq_masked_cp, mq_plain_cp)

mq_is_masked_cp <- function(c) c >= 0xE000 & c < 0xE000 + length(mq_plain)

# Strip unquoted leading/trailing blanks (%LET values, macro arguments).
mq_strip <- function(x) {
  cp <- utf8ToInt(x)
  ws <- cp %in% c(32L, 9L, 10L, 13L, 11L, 12L)
  if (all(ws)) return("")
  keep <- which(!ws)
  intToUtf8(cp[keep[1]:keep[length(keep)]])
}

# Split text on unquoted top-level commas (parentheses and quotes respected).
mq_split_commas <- function(x) {
  cp <- utf8ToInt(x)
  out <- character()
  depth <- 0L
  quote <- 0L
  start <- 1L
  for (k in seq_along(cp)) {
    c <- cp[k]
    if (quote != 0L) {
      if (c == quote) quote <- 0L
      next
    }
    if (c == 34L || c == 39L) {
      quote <- c
    } else if (c == 40L) {
      depth <- depth + 1L
    } else if (c == 41L) {
      depth <- max(0L, depth - 1L)
    } else if (c == 44L && depth == 0L) {
      out <- c(out, if (k > start) intToUtf8(cp[start:(k - 1L)]) else "")
      start <- k + 1L
    }
  }
  c(out, if (length(cp) >= start) intToUtf8(cp[start:length(cp)]) else "")
}
