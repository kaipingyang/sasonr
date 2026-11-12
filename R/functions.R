# DATA step function library (beyond the core set in runtime.R).
#
# Every function is vectorised over its arguments (columns in L1/L2 code,
# length-1 values in L3 code). Functions that can raise a SAS data error take
# `.at = c(line, column)` (passed by L3 code) and flag it via rt_error().

# Helpers ---------------------------------------------------------------------

recycle_n <- function(...) {
  args <- list(...)
  n <- if (any(lengths(args) == 0)) 0L else max(lengths(args))
  lapply(args, rep_len, n)
}

# Apply a scalar function row-wise over recycled arguments.
rowwise <- function(f, ..., type = character(1)) {
  a <- recycle_n(...)
  n <- length(a[[1]])
  vapply(seq_len(n), function(i) do.call(f, lapply(a, `[[`, i)), type, USE.NAMES = FALSE)
}

chars_of <- function(s) strsplit(s, "")[[1]]

num_args <- function(...) {
  args <- list(...)
  if (length(args) == 0) return(matrix(numeric(), 0, 0))
  args_matrix(args)
}

row_stat <- function(f, ...) {
  m <- num_args(...)
  apply(m, 1, function(r) {
    v <- r[!is.na(r)]
    f(v)
  })
}

# Character classes used by ANY*/NOT*/COMPRESS/FINDC/COUNTC modifiers.
class_chars <- function(mods) {
  m <- chars_of(tolower(mods))
  out <- character()
  up <- LETTERS
  lo <- letters
  dg <- as.character(0:9)
  punct <- chars_of("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
  if ("a" %in% m) out <- c(out, up, lo)
  if ("u" %in% m) out <- c(out, up)
  if ("l" %in% m) out <- c(out, lo)
  if ("d" %in% m) out <- c(out, dg)
  if ("s" %in% m) out <- c(out, " ", "\t", "\n", "\r", "\f", "\v")
  if ("h" %in% m) out <- c(out, "\t")
  if ("p" %in% m) out <- c(out, punct)
  if ("n" %in% m) out <- c(out, up, lo, dg, "_")
  if ("f" %in% m) out <- c(out, up, lo, "_")
  if ("x" %in% m) out <- c(out, dg, LETTERS[1:6], letters[1:6])
  if ("c" %in% m) out <- c(out, vapply(c(0:31, 127), function(k) rawToChar(as.raw(k)), "", USE.NAMES = FALSE)[-1])
  unique(out)
}

# Character functions -------------------------------------------------------

#' @export
sas_f_substrn <- function(x, pos, len = NULL) {
  x <- if (is.numeric(x)) strip_both(sas_fmt_best(x, 12L)) else x
  rowwise(function(s, p, l) {
    n <- nchar(s)
    if (is.na(p)) return("")
    if (!is.null(len) && is.na(l)) return("")
    p <- trunc(p)
    e <- if (is.null(len) || is.na(l)) n else p + trunc(l) - 1
    if (!is.null(len) && !is.na(l) && l <= 0) return("")
    a <- max(1, p)
    e <- min(e, n)
    if (e < a) "" else substr(s, a, e)
  }, x, pos, if (is.null(len)) NA_real_ else len)
}

#' @export
sas_f_char <- function(x, pos) {
  x <- as_sas_chr(x)
  rowwise(function(s, p) {
    if (is.na(p) || p < 1 || p > nchar(s)) " " else substr(s, p, p)
  }, x, trunc(pos))
}
#' @export
sas_f_first <- function(x) {
  x <- as_sas_chr(x)
  out <- substr(x, 1L, 1L)
  out[!nzchar(out)] <- " "
  out
}

#' @export
sas_f_compbl <- function(x) gsub(" {2,}", " ", as_sas_chr(x))

#' @export
sas_f_compress <- function(x, chars = NULL, mods = NULL) {
  x <- as_sas_chr(x)
  if (is.null(chars) && is.null(mods)) return(gsub(" ", "", x, fixed = TRUE))
  rowwise(function(s, cs, md) {
    md <- tolower(md)
    set <- c(chars_of(cs), class_chars(md))
    if (grepl("i", md, fixed = TRUE)) set <- unique(c(set, toupper(set), tolower(set)))
    if (grepl("t", md, fixed = TRUE)) s <- sas_rtrim(s)
    ch <- chars_of(s)
    keep <- if (grepl("k", md, fixed = TRUE)) ch %in% set else !ch %in% set
    paste(ch[keep], collapse = "")
  }, x, chars %||% "", mods %||% "")
}

find_pos <- function(s, sub, mods, start) {
  mods <- tolower(mods)
  if (grepl("t", mods, fixed = TRUE)) {
    s <- sas_rtrim(s)
    sub <- sas_rtrim(sub)
  }
  if (grepl("i", mods, fixed = TRUE)) {
    s <- tolower(s)
    sub <- tolower(sub)
  }
  if (!nzchar(sub)) return(0)
  n <- nchar(s)
  if (is.na(start)) start <- 1
  start <- trunc(start)
  if (start == 0) return(0)
  if (start > 0) {
    if (start > n) return(0)
    k <- regexpr(sub, substring(s, start), fixed = TRUE)
    return(if (k > 0) as.numeric(k + start - 1) else 0)
  }
  # negative start: search backward from position |start|
  st <- min(-start, n)
  pos <- gregexpr(sub, s, fixed = TRUE)[[1]]
  pos <- pos[pos > 0 & pos <= st]
  if (length(pos) == 0) 0 else as.numeric(max(pos))
}

#' @export
sas_f_find <- function(x, sub, a3 = NULL, a4 = NULL) {
  mods <- if (is.character(a3)) a3 else if (is.character(a4)) a4 else ""
  start <- if (is.numeric(a3)) a3 else if (is.numeric(a4)) a4 else 1
  rowwise(find_pos, as_sas_chr(x), as_sas_chr(sub), mods, start, type = numeric(1))
}

#' @export
sas_f_findc <- function(x, chars = "", a3 = NULL, a4 = NULL) {
  mods <- if (is.character(a3)) a3 else if (is.character(a4)) a4 else ""
  start <- if (is.numeric(a3)) a3 else if (is.numeric(a4)) a4 else NA_real_
  rowwise(function(s, cs, md, st) {
    md <- tolower(md)
    if (grepl("t", md, fixed = TRUE)) {
      s <- sas_rtrim(s)
      cs <- sas_rtrim(cs)
    }
    set <- c(chars_of(cs), class_chars(md))
    if (grepl("i", md, fixed = TRUE)) {
      set <- unique(c(toupper(set), tolower(set)))
    }
    ch <- chars_of(s)
    hit <- ch %in% set
    if (grepl("[kv]", md)) hit <- !hit
    n <- length(ch)
    if (is.na(st)) st <- if (grepl("b", md, fixed = TRUE)) -n else 1
    if (st > 0) {
      w <- which(hit & seq_len(n) >= st)
      if (length(w) == 0) 0 else as.numeric(w[1])
    } else {
      w <- which(hit & seq_len(n) <= -st)
      if (length(w) == 0) 0 else as.numeric(max(w))
    }
  }, as_sas_chr(x), chars, mods, start, type = numeric(1))
}

word_starts <- function(s, word, dlm) {
  ch <- chars_of(s)
  wd <- chars_of(word)
  n <- length(ch)
  k <- length(wd)
  if (k == 0) return(0)
  for (i in seq_len(max(0, n - k + 1))) {
    if (all(ch[i:(i + k - 1)] == wd)) {
      before <- i == 1 || ch[i - 1] %in% dlm
      after <- i + k - 1 == n || ch[i + k] %in% dlm
      if (before && after) return(i)
    }
  }
  0
}

#' @export
sas_f_indexw <- function(x, word, dlm = " ") {
  rowwise(function(s, w, d) as.numeric(word_starts(s, strip_both(w), chars_of(d))), as_sas_chr(x), word, dlm, type = numeric(1))
}

#' @export
sas_f_findw <- function(x, word, dlm = " !$%&()*+,-./;<^|", a4 = "", a5 = 1) {
  mods <- if (is.character(a4)) a4 else ""
  start <- if (is.numeric(a4)) a4 else a5
  rowwise(function(s, w, d, md, st) {
    md <- tolower(md)
    if (grepl("i", md, fixed = TRUE)) {
      s <- tolower(s)
      w <- tolower(w)
    }
    w <- strip_both(w)
    dl <- c(chars_of(d), class_chars(gsub("[eiktrb]", "", md)))
    ch <- chars_of(s)
    n <- length(ch)
    k <- nchar(w)
    if (k == 0) return(0)
    wd <- chars_of(w)
    st <- if (is.na(st)) 1 else trunc(st)
    idx <- seq_len(max(0, n - k + 1))
    if (st < 0) idx <- rev(idx[idx + k - 1 <= -st]) else idx <- idx[idx >= max(1, st)]
    for (i in idx) {
      if (all(ch[i:(i + k - 1)] == wd) && (i == 1 || ch[i - 1] %in% dl) && (i + k - 1 == n || ch[i + k] %in% dl)) {
        if (grepl("e", md, fixed = TRUE)) {
          before <- if (i > 1) ch[seq_len(i - 1)] else character()
          isd <- before %in% dl
          return(as.numeric(sum(!isd & c(TRUE, isd[-length(isd)])) + 1))
        }
        return(as.numeric(i))
      }
    }
    0
  }, as_sas_chr(x), word, dlm, mods, start, type = numeric(1))
}

#' @export
sas_f_verify <- function(x, ...) {
  set <- unique(unlist(lapply(list(...), function(a) chars_of(paste(a, collapse = "")))))
  rowwise(function(s) {
    w <- which(!chars_of(s) %in% set)
    if (length(w) == 0) 0 else as.numeric(w[1])
  }, as_sas_chr(x), type = numeric(1))
}

#' @export
sas_f_count <- function(x, sub, mods = "") {
  rowwise(function(s, b, md) {
    md <- tolower(md)
    if (grepl("t", md, fixed = TRUE)) {
      s <- sas_rtrim(s)
      b <- sas_rtrim(b)
    }
    if (grepl("i", md, fixed = TRUE)) {
      s <- tolower(s)
      b <- tolower(b)
    }
    if (!nzchar(b)) return(0)
    m <- gregexpr(b, s, fixed = TRUE)[[1]]
    as.numeric(sum(m > 0))
  }, as_sas_chr(x), sub, mods, type = numeric(1))
}

#' @export
sas_f_countc <- function(x, chars = "", mods = "") {
  rowwise(function(s, cs, md) {
    md <- tolower(md)
    if (grepl("t", md, fixed = TRUE)) {
      s <- sas_rtrim(s)
      cs <- sas_rtrim(cs)
    }
    set <- c(chars_of(cs), class_chars(md))
    if (grepl("i", md, fixed = TRUE)) set <- unique(c(toupper(set), tolower(set)))
    hit <- chars_of(s) %in% set
    # K and V count the characters that are not in the list
    if (grepl("[kv]", md)) hit <- !hit
    as.numeric(sum(hit))
  }, as_sas_chr(x), chars, mods, type = numeric(1))
}

scan_default_dlm <- " !$%&()*+,-./;<^|"

scan_words <- function(s, dlm, mods = "") {
  md <- tolower(mods)
  ch <- chars_of(if (grepl("m", md, fixed = TRUE)) sas_rtrim(s) else s)
  if (length(ch) == 0) return(if (grepl("m", md, fixed = TRUE)) "" else character())
  set <- c(chars_of(dlm), class_chars(gsub("[kmbqrot]", "", md)))
  if (grepl("i", md, fixed = TRUE)) set <- unique(c(toupper(set), tolower(set)))
  is_d <- ch %in% set
  if (grepl("k", md, fixed = TRUE)) is_d <- !is_d
  if (grepl("q", md, fixed = TRUE)) {
    # delimiters inside quoted substrings do not separate words
    q <- ""
    for (i in seq_along(ch)) {
      if (nzchar(q)) {
        is_d[i] <- FALSE
        if (ch[i] == q) q <- ""
      } else if (ch[i] %in% c("'", "\"")) {
        is_d[i] <- FALSE
        q <- ch[i]
      }
    }
  }
  if (grepl("m", md, fixed = TRUE)) {
    # every delimiter separates words: consecutive delimiters give empty words
    grp <- cumsum(is_d)
    return(vapply(0:sum(is_d), function(g) paste(ch[!is_d & grp == g], collapse = ""), ""))
  }
  r <- rle(is_d)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  vapply(which(!r$values), function(k) paste(ch[starts[k]:ends[k]], collapse = ""), "")
}

#' @export
sas_f_countw <- function(x, dlm = scan_default_dlm, mods = "") {
  dlm <- dlm %||% scan_default_dlm
  rowwise(function(s, d, md) as.numeric(length(scan_words(s, d, md))), as_sas_chr(x), dlm, mods, type = numeric(1))
}

#' @export
sas_f_scan <- function(x, n, dlm = scan_default_dlm, mods = "", .at = NULL) {
  dlm <- dlm %||% scan_default_dlm
  if (any(grepl("[^abcdfghiklmnpqsuwx ]", tolower(mods)))) abort_unsupported("SCAN modifiers O, R, T")
  # n = 0, missing or not an integer: invalid argument (blank result, _ERROR_)
  bad <- is.na(n) | n == 0 | n != trunc(n)
  if (any(bad)) note_invalid_arg("scan", 2L, .at, list(as_sas_chr(x), n), rep_len(bad, max(length(x), length(n))), numbered = TRUE, math = FALSE)
  rowwise(function(s, k, d, md) {
    if (is.na(k) || k == 0 || k != trunc(k)) return("")
    w <- scan_words(s, d, md)
    # B: count words from the end for a positive n (from the start for a negative n)
    if (grepl("b", tolower(md), fixed = TRUE)) k <- -k
    if (k < 0) k <- length(w) + k + 1
    if (k < 1 || k > length(w)) "" else w[k]
  }, as_sas_chr(x), n, dlm, mods)
}

#' @export
sas_f_transtrn <- function(x, from, to) {
  rowwise(function(s, f, t) if (!nzchar(f)) s else gsub(f, t, s, fixed = TRUE), as_sas_chr(x), as_sas_chr(from), as_sas_chr(to))
}

#' @export
sas_f_translate <- function(x, ...) {
  pairs <- list(...)
  x <- as_sas_chr(x)
  # an unpaired last to-argument is ignored (SAS 9.4)
  for (k in seq(1, length(pairs) - 1L, by = 2)) {
    to <- pairs[[k]]
    from <- pairs[[k + 1]]
    x <- rowwise(function(s, t, f) {
      fch <- chars_of(f)
      tch <- chars_of(t)
      if (length(tch) < length(fch)) tch <- c(tch, rep(" ", length(fch) - length(tch)))
      ch <- chars_of(s)
      idx <- match(ch, fch)
      ch[!is.na(idx)] <- tch[idx[!is.na(idx)]]
      paste(ch, collapse = "")
    }, x, to, from)
  }
  x
}

#' @export
sas_f_propcase <- function(x, dlm = " /-(.\t") {
  rowwise(function(s, d) {
    ch <- chars_of(tolower(s))
    if (length(ch) == 0) return(s)
    dl <- chars_of(d)
    up <- c(TRUE, ch[-length(ch)] %in% dl)
    ch[up] <- toupper(ch[up])
    paste(ch, collapse = "")
  }, as_sas_chr(x), dlm)
}

#' @export
# a zero-length value (from TRIMN, STRIP, ...) has length 0; variables and literals are at least 1 long
sas_f_lengthc <- function(x) as.numeric(nchar(as_sas_chr(x)))
#' @export
sas_f_lengthm <- function(x) sas_f_lengthc(x)

#' @export
sas_f_repeat <- function(x, n) {
  rowwise(function(s, k) {
    if (is.na(k) || k < 0) return("")
    if (!nzchar(s)) return("")
    times <- min(trunc(k), ceiling(32767 / nchar(s))) + 1
    substr(strrep(s, times), 1L, 32767L)
  }, as_sas_chr(x), n)
}

#' @export
sas_f_quote <- function(x, q = "\"") {
  rowwise(function(s, qq) {
    qq <- substr(qq, 1L, 1L)
    if (!qq %in% c("'", "\"")) qq <- "\""
    paste0(qq, gsub(qq, strrep(qq, 2), s, fixed = TRUE), qq)
  }, as_sas_chr(x), q)
}

#' @export
sas_f_dequote <- function(x) {
  rowwise(function(s) {
    t <- s
    if (!nzchar(t)) return(s)
    q <- substr(t, 1L, 1L)
    if (!q %in% c("'", "\"")) return(s)
    ch <- chars_of(t)
    out <- character()
    i <- 2L
    while (i <= length(ch)) {
      if (ch[i] == q) {
        if (i < length(ch) && ch[i + 1L] == q) {
          out <- c(out, q)
          i <- i + 2L
          next
        }
        break
      }
      out <- c(out, ch[i])
      i <- i + 1L
    }
    paste(out, collapse = "")
  }, as_sas_chr(x))
}

class_pos <- function(x, mods, negate, start) {
  set <- class_chars(mods)
  rowwise(function(s, st) {
    if (!is.na(st) && abs(st) >= 2^31) return(NA_real_)
    ch <- chars_of(s)
    hit <- ch %in% set
    if (negate) hit <- !hit
    n <- length(ch)
    if (is.na(st)) st <- 1
    # a start position that truncates to 0 finds nothing (SAS 9.4)
    st <- trunc(st)
    if (st == 0) return(0)
    if (st >= 0) {
      w <- which(hit & seq_len(n) >= max(st, 1))
      if (length(w) == 0) 0 else as.numeric(w[1])
    } else {
      w <- which(hit & seq_len(n) <= -st)
      if (length(w) == 0) 0 else as.numeric(max(w))
    }
  }, as_sas_chr(x), start, type = numeric(1))
}

#' @export
sas_f_anyalpha <- function(x, start = 1) class_pos(x, "a", FALSE, start)
#' @export
sas_f_anydigit <- function(x, start = 1) class_pos(x, "d", FALSE, start)
#' @export
sas_f_anyalnum <- function(x, start = 1) class_pos(x, "ad", FALSE, start)
#' @export
sas_f_anyspace <- function(x, start = 1) class_pos(x, "s", FALSE, start)
#' @export
sas_f_anypunct <- function(x, start = 1) class_pos(x, "p", FALSE, start)
#' @export
sas_f_anyupper <- function(x, start = 1) class_pos(x, "u", FALSE, start)
#' @export
sas_f_anylower <- function(x, start = 1) class_pos(x, "l", FALSE, start)
#' @export
sas_f_notalpha <- function(x, start = 1) class_pos(x, "a", TRUE, start)
#' @export
sas_f_notdigit <- function(x, start = 1) class_pos(x, "d", TRUE, start)
#' @export
sas_f_notalnum <- function(x, start = 1) class_pos(x, "ad", TRUE, start)
#' @export
sas_f_notspace <- function(x, start = 1) class_pos(x, "s", TRUE, start)
#' @export
sas_f_notupper <- function(x, start = 1) class_pos(x, "u", TRUE, start)
#' @export
sas_f_notlower <- function(x, start = 1) class_pos(x, "l", TRUE, start)
#' @export
sas_f_notpunct <- function(x, start = 1) class_pos(x, "p", TRUE, start)

#' @export
sas_f_byte <- function(n) {
  vapply(n, function(k) {
    if (is.na(k) || k < 0 || k > 255) return(" ")
    if (k == 0) return(" ")
    iconv(rawToChar(as.raw(k)), "latin1", "UTF-8")
  }, "", USE.NAMES = FALSE)
}
#' @export
sas_f_rank <- function(x) {
  vapply(as_sas_chr(x), function(s) {
    if (!nzchar(s)) return(32)
    as.numeric(utf8ToInt(substr(s, 1L, 1L))[1])
  }, 0, USE.NAMES = FALSE)
}

#' @export
sas_f_ifc <- function(cond, a, b, m = NULL) {
  ab <- recycle_n(cond, as_sas_chr(a), as_sas_chr(b), if (is.null(m)) "" else as_sas_chr(m))
  out <- ifelse(sas_true(ab[[1]]), ab[[2]], ab[[3]])
  if (!is.null(m)) out[is.na(ab[[1]])] <- ab[[4]][is.na(ab[[1]])]
  out
}
#' @export
sas_f_ifn <- function(cond, a, b, m = NULL) {
  ab <- recycle_n(cond, as.numeric(a), as.numeric(b), if (is.null(m)) NA_real_ else as.numeric(m))
  out <- ifelse(sas_true(ab[[1]]), ab[[2]], ab[[3]])
  if (!is.null(m)) out[is.na(ab[[1]])] <- ab[[4]][is.na(ab[[1]])]
  out
}

#' @export
sas_f_choosec <- function(k, ...) {
  vals <- lapply(list(...), as_sas_chr)
  ab <- do.call(recycle_n, c(list(k), vals))
  vapply(seq_along(ab[[1]]), function(i) {
    j <- floor(ab[[1]][i])
    m <- length(vals)
    if (is.na(j) || j == 0 || abs(j) > m) return("")
    if (j < 0) j <- m + j + 1
    ab[[j + 1]][i]
  }, "")
}
#' @export
sas_f_choosen <- function(k, ...) {
  m <- num_args(...)
  kk <- rep_len(k, nrow(m))
  vapply(seq_len(nrow(m)), function(i) {
    j <- floor(kk[i])
    if (is.na(j) || j == 0 || abs(j) > ncol(m)) return(NA_real_)
    if (j < 0) j <- ncol(m) + j + 1
    m[i, j]
  }, 0)
}

#' @export
sas_f_whichc <- function(x, ...) {
  vals <- lapply(list(...), as_sas_chr)
  ab <- do.call(recycle_n, c(list(as_sas_chr(x)), vals))
  vapply(seq_along(ab[[1]]), function(i) {
    v <- vapply(ab[-1], `[`, "", i)
    w <- which(sas_rtrim(v) == sas_rtrim(ab[[1]][i]))
    if (length(w) == 0) 0 else as.numeric(w[1])
  }, 0)
}
#' @export
sas_f_whichn <- function(x, ...) {
  m <- num_args(...)
  xx <- rep_len(x, nrow(m))
  vapply(seq_len(nrow(m)), function(i) {
    if (is.na(xx[i])) return(NA_real_)
    w <- which(m[i, ] == xx[i])
    if (length(w) == 0) 0 else as.numeric(w[1])
  }, 0)
}

#' @export
sas_f_compare <- function(a, b, mods = "") {
  rowwise(function(x, y, md) {
    md <- tolower(md)
    if (grepl("n", md, fixed = TRUE)) {
      unname_lit <- function(z) {
        t <- strip_both(z)
        if (grepl("^([\"']).*\\1[nN]$", t)) substr(t, 2L, nchar(t) - 2L) else z
      }
      x <- unname_lit(x)
      y <- unname_lit(y)
      md <- paste0(md, "i")
    }
    if (grepl("i", md, fixed = TRUE)) {
      x <- tolower(x)
      y <- tolower(y)
    }
    if (grepl("l", md, fixed = TRUE)) {
      x <- sub("^ +", "", x)
      y <- sub("^ +", "", y)
    }
    if (grepl(":", md, fixed = TRUE)) {
      m <- min(nchar(sas_rtrim(x)), nchar(sas_rtrim(y)))
      x <- substr(x, 1L, m)
      y <- substr(y, 1L, m)
    }
    n <- max(nchar(x), nchar(y))
    x <- formatC(x, width = -n)
    y <- formatC(y, width = -n)
    if (x == y) return(0)
    xc <- chars_of(x)
    yc <- chars_of(y)
    k <- which(xc != yc)[1]
    if (chr_cmp(xc[k], yc[k]) < 0) -k else k
  }, as_sas_chr(a), as_sas_chr(b), mods, type = numeric(1))
}

#' @export
sas_f_symget <- function(symtab, name, .at = NULL) {
  vapply(as_sas_chr(name), function(nm0) {
    # trailing blanks are ignored; a leading blank makes the name invalid (SAS: blank result,
    # NOTE "Invalid argument to function SYMGET(' mv ')", _ERROR_)
    nm <- sas_rtrim(nm0)
    if (!nzchar(nm)) {
      rt$stop_step <- TRUE
      rt_error("ERROR: The macro variable name is either all blank or missing.")
    }
    if (nchar(nm) > 32) {
      rt$stop_step <- TRUE
      rt_error(sprintf("ERROR: Symbolic variable name %s must be 32 or fewer characters long.", toupper(substr(nm, 1L, 33L))))
    }
    v <- if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm)) NULL else if (inherits(symtab, "sasonr_macro_session")) {
      mv_get(symtab, tolower(nm))
    } else {
      get0(tolower(nm), envir = symtab, inherits = FALSE)
    }
    if (is.null(v)) {
      note_invalid_arg("symget", NULL, .at, args = list(if (nzchar(nm)) nm0 else " "), math = FALSE, trim = FALSE)
      return("")
    }
    if (inherits(symtab, "sasonr_macro_session")) mq_unmask(v) else v
  }, "", USE.NAMES = FALSE)
}

# Regular expressions (PRX) ----------------------------------------------------

prx_compile <- function(re) {
  if (is.numeric(re)) {
    p <- rt$prx[[as.character(re)]]
    if (is.null(p)) abort_runtime("PRX regular-expression id was not created by PRXPARSE")
    return(p)
  }
  re <- strip_both(re)
  key <- paste0("re:", re)
  if (!is.null(rt$prx_cache[[key]])) return(rt$prx_cache[[key]])
  sub <- FALSE
  if (startsWith(re, "s/") || startsWith(re, "s|")) {
    sub <- TRUE
    re <- substring(re, 2L)
  }
  d <- substr(re, 1L, 1L)
  if (d %in% c(letters, LETTERS, as.character(0:9), " ", "\\") || !nzchar(d)) {
    abort_unsupported(paste0("regular expression without delimiters: ", re))
  }
  parts <- character()
  cur <- ""
  i <- 2L
  ch <- chars_of(re)
  while (i <= length(ch)) {
    if (ch[i] == "\\" && i < length(ch)) {
      cur <- paste0(cur, if (ch[i + 1L] == d) d else paste0("\\", ch[i + 1L]))
      i <- i + 2L
      next
    }
    if (ch[i] == d) {
      parts <- c(parts, cur)
      cur <- ""
      i <- i + 1L
      next
    }
    cur <- paste0(cur, ch[i])
    i <- i + 1L
  }
  flags <- cur
  if (length(parts) < (if (sub) 2L else 1L)) abort_syntax(paste0("invalid regular expression ", re))
  pat <- parts[1]
  # Perl constructs that SAS PRX does not accept (named groups) and invalid patterns
  if (grepl("\\(\\?P?<[A-Za-z]", pat, perl = TRUE)) return(NULL)
  ok <- tryCatch({
    regexpr(pat, "", perl = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(NULL)
  if (grepl("x", flags, fixed = TRUE)) pat <- paste0("(?x)", pat)
  if (grepl("s", flags, fixed = TRUE)) pat <- paste0("(?s)", pat)
  if (grepl("m", flags, fixed = TRUE)) pat <- paste0("(?m)", pat)
  out <- list(pattern = pat, icase = grepl("i", flags, fixed = TRUE), sub = sub,
              repl = if (sub) gsub("\\$([0-9])", "\\\\\\1", parts[2]) else NULL)
  if (is.null(rt$prx_cache)) rt$prx_cache <- list()
  rt$prx_cache[[key]] <- out
  out
}

# An invalid regular expression: SAS writes the Perl diagnostic (not
# reproduced), an ERROR naming the function and the invalid-argument NOTE.
prx_invalid_note <- function(fname, args, .at) {
  rt_error(sprintf("ERROR: The regular expression passed to the function %s contains a syntax error.", toupper(fname)))
  note_invalid_arg(fname, 1L, .at, args = args, numbered = TRUE)
}

#' @export
sas_f_prxparse <- function(re, .at = NULL) {
  vapply(re, function(r) {
    if (is.null(rt$prx)) rt$prx <- list()
    p <- prx_compile(r)
    if (is.null(p)) {
      prx_invalid_note("prxparse", list(r), .at)
      return(NA_real_)
    }
    id <- length(rt$prx) + 1
    rt$prx[[as.character(id)]] <- p
    id
  }, 0, USE.NAMES = FALSE)
}

#' @export
sas_f_prxmatch <- function(re, x, .at = NULL) {
  x <- as_sas_chr(x)
  ab <- recycle_n(if (is.numeric(re)) re else re, x)
  vapply(seq_along(ab[[2]]), function(i) {
    if (is.na(ab[[1]][i])) return(NA_real_)
    p <- prx_compile(ab[[1]][i])
    if (is.null(p)) {
      prx_invalid_note("prxmatch", list(ab[[1]][i], ab[[2]][i]), .at)
      return(NA_real_)
    }
    k <- regexpr(p$pattern, ab[[2]][i], perl = TRUE, ignore.case = p$icase)
    as.numeric(max(k, 0))
  }, 0)
}

#' @export
sas_f_prxchange <- function(re, times, x, .at = NULL) {
  x <- as_sas_chr(x)
  ab <- recycle_n(re, times, x)
  vapply(seq_along(ab[[3]]), function(i) {
    p <- prx_compile(ab[[1]][i])
    if (is.null(p)) {
      prx_invalid_note("prxchange", list(ab[[1]][i], ab[[2]][i], ab[[3]][i]), .at)
      return("")
    }
    if (!p$sub) abort_runtime("PRXCHANGE needs a substitution (s/.../.../)")
    n <- ab[[2]][i]
    s <- ab[[3]][i]
    if (is.na(n)) return("")
    if (n == -1 || n >= 2^31) return(gsub(p$pattern, p$repl, s, perl = TRUE, ignore.case = p$icase))
    if (n < 0) {
      # SAS 9.4: ERROR, blank result, the step continues (-1E300 and below: invalid argument, see fn_policy)
      if (n > -1e300) {
        rt_error(sprintf("ERROR: Argument 2 to the routine PRXCHANGE, the maximum number of replacements, must be nonnegative but the value is %s.",
                         trimws(sas_fmt_best(n, 12L))))
      }
      return("")
    }
    n <- trunc(n)
    if (n == 0) return(s)
    # replace the first n matches in one pass (later matches are searched in the original text)
    m <- gregexpr(p$pattern, s, perl = TRUE, ignore.case = p$icase)[[1]]
    if (m[1] < 0) return(s)
    k <- min(n, length(m))
    out <- ""
    last <- 1L
    for (j in seq_len(k)) {
      st <- m[j]
      len <- attr(m, "match.length")[j]
      piece <- substr(s, st, st + len - 1L)
      out <- paste0(out, substr(s, last, st - 1L), sub(p$pattern, p$repl, piece, perl = TRUE, ignore.case = p$icase))
      last <- st + len
    }
    paste0(out, substring(s, last))
  }, "")
}

# Numeric functions ------------------------------------------------------------

#' @export
sas_f_modz <- function(a, b, .at = NULL) {
  n <- max(length(a), length(b))
  a <- rep_len(as.numeric(a), n)
  b <- rep_len(as.numeric(b), n)
  zero <- !is.na(b) & b == 0
  if (any(zero & !is.na(a))) note_invalid_arg("modz", NULL, .at, list(a, b), zero & !is.na(a))
  mod_domain_check("MODZ", a, b, exact_only = FALSE)
  out <- mod_exact(a, b)
  out[zero] <- NA_real_
  out
}

# Remainder a - b * trunc(a / b) with one rounding (SAS 9.4 uses a fused
# multiply-add: tests/conformance/fn_probe/MOD, 594 of 600 values).
mod_exact <- function(a, b) {
  q <- trunc(a / b)
  out <- rep(NA_real_, length(a))
  ok <- !is.na(a) & !is.na(b) & b != 0
  out[ok] <- stats_fma(-b[ok], q[ok], a[ok])
  out
}

# Verified domain of MOD and MODZ (registry status partial).  MODZ: the
# arguments lie within 1E-140..1E140 in magnitude and a / b is not an integer
# unless the remainder is exactly 0 (SAS then returns 0 where the exact
# remainder is a few ulps).  MOD (`exact_only`) additionally needs arguments
# that are multiples of 2^-10 below 2^53 in magnitude (their remainder is exact,
# so SAS's fuzz cannot change it) or a remainder that the fuzz rule sets to 0.
mod_domain_check <- function(fname, a, b, exact_only, fuzz_zero = NULL) {
  ok <- !is.na(a) & !is.na(b) & b != 0
  if (!any(ok)) return(invisible())
  aa <- a[ok]
  bb <- b[ok]
  rng <- function(x) x == 0 | (abs(x) >= 1e-140 & abs(x) <= 1e140)
  bad <- !(rng(aa) & rng(bb))
  qq <- aa / bb
  r <- stats_fma(-bb, trunc(qq), aa)
  bad <- bad | (!bad & qq == trunc(qq) & r != 0)
  if (exact_only) {
    dy <- function(x) abs(x) < 2^53 & (x * 1024) == trunc(x * 1024)
    fz <- if (is.null(fuzz_zero)) FALSE else fuzz_zero[ok]
    bad <- bad | !(dy(aa) & dy(bb) | fz)
  }
  if (any(bad)) {
    i <- which(bad)[1]
    abort_unsupported(sprintf("%s(%s, %s): outside the verified domain (%s)", fname, format(aa[i], digits = 17),
                              format(bb[i], digits = 17),
                              if (exact_only) "arguments that are multiples of 2**-10 below 2**53, or a remainder of 0 after fuzzing" else
                                "magnitudes 1E-140..1E140; a / b not an integer unless the remainder is 0"))
  }
  invisible()
}

#' @export
sas_f_median <- function(...) row_stat(function(v) if (length(v) == 0) NA_real_ else stats::median(v), ...)
#' @export
sas_f_std <- function(...) row_stat(function(v) if (length(v) < 2) NA_real_ else stats::sd(v), ...)
#' @export
sas_f_var <- function(...) row_stat(function(v) if (length(v) < 2) NA_real_ else stats::var(v), ...)
#' @export
sas_f_stderr <- function(...) row_stat(function(v) {
  n <- length(v)
  if (n < 2) return(NA_real_)
  m <- sum(v) / n
  sqrt(sum((v - m)^2) / (n - 1) / n) # bit-identical to SAS 9.4 (not sd / sqrt(n))
}, ...)
#' @export
sas_f_cv <- function(...) row_stat(function(v) if (length(v) < 2 || mean(v) == 0) NA_real_ else stats::sd(v) / mean(v) * 100, ...)
#' @export
sas_f_range <- function(...) row_stat(function(v) if (length(v) == 0) NA_real_ else max(v) - min(v), ...)
#' @export
sas_f_css <- function(...) row_stat(function(v) if (length(v) == 0) NA_real_ else sum((v - mean(v))^2), ...)
#' @export
sas_f_uss <- function(...) row_stat(function(v) if (length(v) == 0) NA_real_ else sum(v^2), ...)
#' @export
sas_f_geomean <- function(...) row_stat(function(v) {
  if (length(v) == 0 || any(v < 0)) return(NA_real_)
  if (any(v == 0)) return(0)
  # sequential double sum of the logarithms (closest form found; GEOMEAN is approximate)
  exp(stats_seq_sum(log(v)) / length(v))
}, ...)
#' @export
sas_f_harmean <- function(...) row_stat(function(v) {
  if (length(v) == 0 || any(v < 0)) return(NA_real_)
  if (any(v == 0)) return(0) # SAS 9.4: a zero value gives 0
  # sequential double sum (tests/conformance/fn_probe/GEOMEAN: 300 of 300)
  length(v) / stats_seq_sum(1 / v)
}, ...)
#' @export
sas_f_skewness <- function(...) row_stat(function(v) {
  n <- length(v)
  if (n < 3) return(NA_real_)
  s <- stats::sd(v)
  if (s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((v - mean(v)) / s)^3)
}, ...)
#' @export
sas_f_kurtosis <- function(...) row_stat(function(v) {
  n <- length(v)
  if (n < 4) return(NA_real_)
  s <- stats::sd(v)
  if (s == 0) return(NA_real_)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((v - mean(v)) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
}, ...)

kth <- function(k, m, decreasing) {
  kk <- rep_len(k, nrow(m))
  vapply(seq_len(nrow(m)), function(i) {
    v <- sort(m[i, !is.na(m[i, ])], decreasing = decreasing)
    j <- trunc(kk[i])
    if (is.na(j) || j < 1 || j > length(v)) NA_real_ else v[j]
  }, 0)
}
#' @export
sas_f_largest <- function(k, ...) kth(k, num_args(...), TRUE)
#' @export
sas_f_smallest <- function(k, ...) kth(k, num_args(...), FALSE)
#' @export
sas_f_ordinal <- function(k, ...) {
  m <- num_args(...)
  kk <- rep_len(k, nrow(m))
  vapply(seq_len(nrow(m)), function(i) {
    codes <- sas_miss_code(m[i, ])
    o <- order(ifelse(is.na(codes), 99L, codes), ifelse(is.na(m[i, ]), 0, m[i, ]))
    j <- trunc(kk[i])
    if (is.na(j) || j < 1 || j > ncol(m)) NA_real_ else m[i, o[j]]
  }, 0)
}

#' @export
sas_f_cmiss <- function(...) {
  args <- list(...)
  n <- max(lengths(args))
  out <- numeric(n)
  for (a in args) out <- out + rep_len(as.numeric(sas_is_missing(a)), n)
  out
}

#' @export
sas_f_fact <- function(n, .at = NULL) {
  bad <- !is.na(n) & (n < 0 | n != trunc(n) | n > 170)
  out <- suppressWarnings(factorial(n))
  out[bad] <- NA_real_
  out
}
#' @export
sas_f_comb <- function(n, r) {
  out <- choose(n, r)
  out[is.na(n) | is.na(r) | r > n | r < 0 | n < 0 | n >= 2^53 | r >= 2^53 | is.infinite(out)] <- NA_real_
  out
}
#' @export
sas_f_perm <- function(n, r = NULL) if (is.null(r)) factorial(n) else factorial(n) / factorial(n - r)
#' @export
sas_f_gamma <- function(x, .at = NULL) {
  bad <- !is.na(x) & x <= 0 & x == trunc(x)
  out <- suppressWarnings(gamma(x))
  # negative arguments by the reflection formula (bit-identical to SAS 9.4 for GAMMA(-0.5))
  neg <- !is.na(x) & x < 0 & !bad
  if (any(neg)) out[neg] <- pi / (sin(pi * x[neg]) * suppressWarnings(gamma(1 - x[neg])))
  out[bad | is.infinite(out)] <- NA_real_
  out
}
#' @export
sas_f_lgamma <- function(x) lgamma(x)
#' @export
sas_f_beta <- function(a, b) beta(a, b)
#' @export
sas_f_gcd <- function(...) {
  m <- num_args(...)
  apply(m, 1, function(r) {
    if (anyNA(r) || any(abs(r) >= 2^53)) return(NA_real_)
    g <- function(a, b) if (b == 0) abs(a) else g(b, a %% b)
    Reduce(g, trunc(r))
  })
}
#' @export
sas_f_lcm <- function(...) {
  m <- num_args(...)
  apply(m, 1, function(r) {
    if (anyNA(r) || any(abs(r) >= 2^53)) return(NA_real_)
    g <- function(a, b) if (b == 0) abs(a) else g(b, a %% b)
    out <- Reduce(function(a, b) if (a == 0 || b == 0) 0 else abs(a) / g(a, b) * abs(b), trunc(r))
    # a result beyond 2^53 is missing, without a NOTE (SAS 9.4)
    if (out > 2^53) NA_real_ else out
  })
}
#' @export
sas_f_divide <- function(a, b) {
  ab <- recycle_n(as.numeric(a), as.numeric(b))
  out <- ab[[1]] / ab[[2]]
  # x/0 and overflow: .I (positive) or .M (negative); 0/0: .
  # division by zero: the sign of the numerator only (1/-0 is .I)
  z0 <- !is.na(ab[[2]]) & ab[[2]] == 0 & !is.na(ab[[1]]) & ab[[1]] != 0
  out[z0] <- ifelse(ab[[1]][z0] > 0, Inf, -Inf)
  out[!is.na(out) & out == Inf] <- sas_missing("I")
  out[!is.na(out) & out == -Inf] <- sas_missing("M")
  z <- !is.na(ab[[2]]) & ab[[2]] == 0
  out[z & !is.na(ab[[1]]) & ab[[1]] == 0] <- NA_real_
  # a missing numerator is passed through (special missing values kept); a missing denominator gives .
  na <- is.na(ab[[1]])
  out[na] <- ab[[1]][na]
  out[!na & is.na(ab[[2]])] <- ab[[2]][!na & is.na(ab[[2]])]
  out
}

trig <- function(f, name, dom = NULL) {
  force(f)
  force(dom)
  function(x, .at = NULL) {
    bad <- if (is.null(dom)) rep(FALSE, length(x)) else !is.na(x) & !do.call(dom, list(x))
    out <- suppressWarnings(f(x))
    out[bad] <- NA_real_
    out
  }
}
#' @export
sas_f_sin <- trig(sin, "sin")
#' @export
sas_f_cos <- trig(cos, "cos")
#' @export
sas_f_tan <- trig(tan, "tan")
#' @export
sas_f_arsin <- trig(asin, "arsin", function(x) abs(x) <= 1)
#' @export
sas_f_arcos <- trig(acos, "arcos", function(x) abs(x) <= 1)
#' @export
sas_f_atan <- trig(atan, "atan")
#' @export
sas_f_atan2 <- function(y, x) atan2(y, x)
#' @export
sas_f_sinh <- trig(sinh, "sinh")
#' @export
sas_f_cosh <- trig(cosh, "cosh")
#' @export
sas_f_tanh <- trig(tanh, "tanh")
#' @export
sas_f_logbeta <- function(a, b) lbeta(a, b)

#' @export
sas_f_constant <- function(name, p = NULL) {
  ab <- recycle_n(tolower(strip_both(as_sas_chr(name))), if (is.null(p)) NA_real_ else as.numeric(p))
  vapply(seq_along(ab[[1]]), function(i) {
    nm <- ab[[1]][i]
    pp <- ab[[2]][i]
    if (nm == "exactint" && !is.null(p) && !is.na(pp)) {
      # EXACTINT(n): largest integer stored exactly in n bytes (3 to 8)
      if (is.na(pp) || pp != trunc(pp) || pp < 2 || pp > 8) {
        # SAS 9.4 writes an ERROR, sets the result missing and continues (fn_policy counts the place)
        if (!is.na(pp)) {
          rt_error("ERROR: The second argument to the CONSTANT function with first argument of EXACTINT must be between 2 and 8.")
        }
        return(NA_real_)
      }
      return(2^(8 * pp - 11))
    }
    if (!is.null(p) && !is.na(pp) && nm %in% c("big", "e", "euler", "logbig", "logmaceps", "logsmall", "maceps", "pi",
                                              "small", "sqrtbig", "sqrtmaceps", "sqrtsmall")) {
      return(NA_real_) # a parameter for another constant: invalid (fn_policy)
    }
    switch(nm,
      pi = pi, e = exp(1), euler = 0.577215664901532860606512,
      big = .Machine$double.xmax, logbig = log(.Machine$double.xmax), sqrtbig = sqrt(.Machine$double.xmax),
      # LOGSMALL: SAS 9.4 value (hex C0862336F49C3C4F), not log(SMALL)
      small = .Machine$double.xmin, logsmall = -708.40183374461219, sqrtsmall = sqrt(.Machine$double.xmin),
      maceps = .Machine$double.eps, logmaceps = log(.Machine$double.eps), sqrtmaceps = sqrt(.Machine$double.eps),
      exactint = 2^53,
      if (nm %in% c("bigrecip", "golden", "logbigrecip", "logsmallrecip", "smallrecip")) {
        abort_unsupported(paste0("CONSTANT('", nm, "')"))
      } else {
        rt_error(paste0("ERROR: The first argument of the CONSTANT function must be one of the following: BIG, BIGRECIP, E, EULER, ",
                        "EXACTINT, GOLDEN, LOGBIG, LOGBIGRECIP, LOGMACEPS, LOGSMALL, LOGSMALLRECIP, MACEPS, PI, SMALL, ",
                        "SMALLRECIP, SQRTBIG, SQRTMACEPS, or SQRTSMALL."))
        NA_real_
      }
    )
  }, 0)
}

#' @export
sas_f_probnorm <- function(x) stats::pnorm(x)
#' @export
sas_f_probit <- function(p) {
  out <- stats::qnorm(p)
  out[!is.na(out) & out == 0] <- -0 # PROBIT(0.5) is -0 in SAS 9.4
  out
}
#' @export
sas_f_probt <- function(x, df, nc = 0) stats::pt(x, df, nc)
#' @export
sas_f_probchi <- function(x, df, nc = 0) stats::pchisq(x, df, nc)
#' @export
sas_f_probf <- function(x, n, d, nc = 0) {
  rowwise(function(x, n, d, nc) if (!is.na(nc) && nc > 0) pnf_series(x, n, d, nc) else stats::pf(x, n, d, nc),
          x, n, d, nc, type = numeric(1))
}

# Noncentral F distribution function as the Poisson mixture of regularized
# incomplete beta functions, summed outwards from the largest Poisson weight
# (R's pnbeta() stops at an absolute error of 1e-9).
pnf_series <- function(x, n, d, nc) {
  if (anyNA(c(x, n, d, nc)) || n <= 0 || d <= 0 || nc < 0) return(NA_real_)
  if (x <= 0) return(0)
  y <- n * x / (n * x + d)
  lam <- nc / 2
  term <- function(j) exp(stats::dpois(j, lam, log = TRUE) + stats::pbeta(y, n / 2 + j, d / 2, log.p = TRUE))
  j0 <- floor(lam)
  tot <- term(j0)
  j <- j0 + 1
  repeat {
    t <- term(j)
    tot <- tot + t
    if (t < 1e-20 * tot && stats::dpois(j, lam) < 1e-20) break
    j <- j + 1
  }
  j <- j0 - 1
  while (j >= 0) {
    t <- term(j)
    tot <- tot + t
    if (t < 1e-20 * tot) break
    j <- j - 1
  }
  min(tot, 1)
}

qnf_series <- function(p, n, d, nc) {
  if (anyNA(c(p, n, d, nc)) || p < 0 || p > 1) return(NA_real_)
  if (p == 0) return(0)
  if (p == 1) return(Inf)
  x0 <- suppressWarnings(stats::qf(p, n, d, nc))
  lo <- x0 * (1 - 1e-6)
  hi <- x0 * (1 + 1e-6)
  while (pnf_series(lo, n, d, nc) > p) lo <- lo / 2
  while (pnf_series(hi, n, d, nc) < p) hi <- hi * 2
  for (k in 1:200) {
    mid <- (lo + hi) / 2
    if (mid <= lo || mid >= hi) break
    if (pnf_series(mid, n, d, nc) < p) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}
#' @export
sas_f_tinv <- function(p, df, nc = 0) stats::qt(p, df, nc)
#' @export
sas_f_cinv <- function(p, df, nc = 0) stats::qchisq(p, df, nc)
#' @export
sas_f_finv <- function(p, n, d, nc = 0) {
  rowwise(function(p, n, d, nc) {
    if (anyNA(c(p, n, d, nc)) || n <= 0 || d <= 0 || nc < 0) return(NaN)
    if (nc > 0) qnf_series(p, n, d, nc) else suppressWarnings(stats::qf(p, n, d, nc))
  },
          p, n, d, nc, type = numeric(1))
}
#' @export
sas_f_probbnml <- function(p, n, m) stats::pbinom(m, n, p)
#' @export
sas_f_poisson <- function(m, n) stats::ppois(n, m)

# distribution names SAS 9.4 accepts (prefixes of these are not decided here: fail loud)
sas_dist_names <- c("bernoulli", "beta", "binomial", "cauchy", "chisquare", "exponential", "f", "gamma", "gaussian",
                    "geometric", "genpoisson", "hypergeometric", "igauss", "laplace", "logistic", "lognormal", "negb",
                    "negbinomial", "normal", "pareto", "poisson", "t", "tweedie", "uniform", "wald", "weibull")
sas_dist_known <- function(d) nzchar(d) && any(startsWith(sas_dist_names, d) | startsWith(d, sas_dist_names))

# Break a log message into lines of at most LINESIZE columns at blanks (SAS
# continues on lines indented by `indent`).
log_wrap <- function(text, indent = 6L, ls = rt$ls %||% 132L) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character()
  cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(cand) > ls && nzchar(cur)) {
      lines <- c(lines, cur)
      cur <- paste0(strrep(" ", indent), w)
    } else {
      cur <- cand
    }
  }
  c(lines, cur)
}

dist_fun <- function(kind, dist, x, args) {
  d <- tolower(strip_both(dist))
  a <- function(k, default) if (length(args) >= k) args[[k]] else default
  nc <- function(k) if (length(args) >= k) args[[k]] else NULL
  # a normal standard deviation (or another scale) that is not positive is invalid
  pos <- function(v) if (is.null(v)) v else ifelse(!is.na(v) & v <= 0, NaN, v)
  f <- switch(d,
    normal = , gauss = , gaussian = list(p = stats::pnorm, d = stats::dnorm, q = stats::qnorm, a = list(a(1, 0), pos(a(2, 1)))),
    t = list(p = stats::pt, d = stats::dt, q = stats::qt, a = list(a(1, NULL), nc(2))),
    chisq = , chisquare = list(p = stats::pchisq, d = stats::dchisq, q = stats::qchisq, a = list(a(1, NULL), nc(2))),
    f = list(p = stats::pf, d = stats::df, q = stats::qf, a = list(a(1, NULL), a(2, NULL), nc(3))),
    binomial = , binom = list(p = stats::pbinom, d = stats::dbinom, q = stats::qbinom, a = list(a(2, NULL), a(1, NULL)), discrete = TRUE),
    poisson = list(p = stats::ppois, d = stats::dpois, q = stats::qpois, a = list(a(1, NULL)), discrete = TRUE),
    uniform = list(p = stats::punif, d = stats::dunif, q = stats::qunif, a = list(a(1, 0), a(2, 1))),
    exponential = , expo = list(p = stats::pexp, d = stats::dexp, q = stats::qexp, a = list(1 / pos(a(1, 1)))),
    beta = list(p = stats::pbeta, d = stats::dbeta, q = stats::qbeta, a = list(a(1, NULL), a(2, NULL))),
    gamma = list(p = stats::pgamma, d = stats::dgamma, q = stats::qgamma, a = list(a(1, NULL), 1 / pos(a(2, 1)))),
    lognormal = list(p = stats::plnorm, d = stats::dlnorm, q = stats::qlnorm, a = list(a(1, 0), pos(a(2, 1)))),
    if (sas_dist_known(d)) abort_unsupported(paste0("distribution '", dist, "'")) else list(blank = TRUE)
  )
  if (isTRUE(f$blank)) {
    # not a SAS distribution: ERROR, missing result (the step continues; fn_policy counts the place)
    rt_error(log_wrap(sprintf(paste0("ERROR: The first argument of the %s function must be a character string with a value of ",
                                     "BERNOULLI, BETA, BINOMIAL, CAUCHY, CHISQUARE, EXPONENTIAL, F, GAMMA, GAUSSIAN, GEOMETRIC, ",
                                     "GENPOISSON, HYPERGEOMETRIC, IGAUSS, LAPLACE, LOGISTIC, LOGNORMAL, NEGB, NORMAL, PARETO, ",
                                     "POISSON, T, TWEEDIE, UNIFORM, WALD, or WEIBULL."), toupper(kind)), indent = 7L))
    return(rep(NaN, length(x)))
  }
  if (kind == "quantile" && d %in% c("t") && any(!is.na(x) & x == 0.5)) {
    abort_unsupported("QUANTILE('T', 0.5, ...) (SAS 9.4 returns a value near but not equal to 0)")
  }
  fa <- Filter(Negate(is.null), f$a)
  # SAS writes log(0) as -SQRT(BIG) in LOGPDF/LOGCDF/LOGSDF
  logz <- function(v) ifelse(!is.na(v) & v == -Inf, -sqrt(.Machine$double.xmax), v)
  out <- suppressWarnings(switch(kind,
    cdf = do.call(f$p, c(list(x), fa)),
    sdf = do.call(f$p, c(list(x), fa, lower.tail = FALSE)),
    pdf = do.call(f$d, c(list(x), fa)),
    quantile = do.call(f$q, c(list(x), fa)),
    logpdf = logz(do.call(f$d, c(list(x), fa, log = TRUE))),
    logcdf = logz(do.call(f$p, c(list(x), fa, log.p = TRUE))),
    logsdf = logz(do.call(f$p, c(list(x), fa, lower.tail = FALSE, log.p = TRUE)))
  ))
  # discrete distributions: p = 0 and p = 1 are valid quantiles; continuous ones: invalid
  if (kind == "quantile" && !isTRUE(f$discrete)) out[!is.na(x) & (x <= 0 | x >= 1)] <- NaN
  out
}
#' @export
sas_f_cdf <- function(dist, x, ...) dist_fun("cdf", dist[1], x, list(...))
#' @export
sas_f_sdf <- function(dist, x, ...) dist_fun("sdf", dist[1], x, list(...))
#' @export
sas_f_pdf <- function(dist, x, ...) dist_fun("pdf", dist[1], x, list(...))
#' @export
sas_f_quantile <- function(dist, x, ...) dist_fun("quantile", dist[1], x, list(...))
#' @export
sas_f_logpdf <- function(dist, x, ...) dist_fun("logpdf", dist[1], x, list(...))
#' @export
sas_f_logcdf <- function(dist, x, ...) dist_fun("logcdf", dist[1], x, list(...))
#' @export
sas_f_logsdf <- function(dist, x, ...) dist_fun("logsdf", dist[1], x, list(...))

# Dates and times -----------------------------------------------------------------

#' @export
sas_f_dhms <- function(d, h, m, s) d * 86400 + h * 3600 + m * 60 + s
#' @export
sas_f_datetime <- function() as.numeric(difftime(Sys.time(), as.POSIXct("1960-01-01", tz = "UTC"), units = "secs")) +
  as.numeric(format(Sys.time(), "%z")) / 100 * 3600
#' @export
sas_f_time <- function() {
  lt <- as.POSIXlt(Sys.time())
  lt$hour * 3600 + lt$min * 60 + floor(lt$sec)
}

#' @export
sas_f_juldate <- function(x) {
  p <- civil(x)
  # yyddd for years in the YEARCUTOFF window (1940-2039), else yyyyddd
  out <- ifelse(p$y >= 1940 & p$y < 2040, (p$y %% 100) * 1000 + p$yday, p$y * 1000 + p$yday)
  out[is.na(x)] <- NA_real_
  as.numeric(out)
}
#' @export
sas_f_juldate7 <- function(x) {
  p <- civil(x)
  out <- p$y * 1000 + p$yday
  out[is.na(x)] <- NA_real_
  as.numeric(out)
}
#' @export
sas_f_datejul <- function(j) {
  y <- floor(j / 1000)
  y <- ifelse(y < 100, ifelse(y < 40, 2000 + y, 1900 + y), y)
  from_ymd(y, 1, 1) + (j %% 1000) - 1
}

#' @export
sas_f_week <- function(x, desc = "u") {
  d <- tolower(substr(strip_both(desc), 1L, 1L))
  rowwise(function(v, dd) {
    if (is.na(v) || abs(v) > 7e6) return(NA_real_)
    p <- civil(v)
    jan1 <- from_ymd(p$y, 1, 1)
    wd1 <- civil(jan1)$wday # 1 = Sunday
    if (dd == "u") {
      first_sun <- jan1 + (8 - wd1) %% 7
      return(if (v < first_sun) 0 else floor((v - first_sun) / 7) + 1)
    }
    if (dd == "w") {
      first_mon <- jan1 + (9 - wd1) %% 7
      return(if (v < first_mon) 0 else floor((v - first_mon) / 7) + 1)
    }
    # ISO 8601 week number
    wd <- (civil(v)$wday + 5) %% 7 + 1 # Monday = 1
    thu <- v - wd + 4
    y <- civil(thu)$y
    floor((thu - from_ymd(y, 1, 1)) / 7) + 1
  }, x, d, type = numeric(1))
}

# Day-count bases of DATDIF and YRDIF (dates are truncated to integers).
# 30/360: a start on the last day of February or on the 31st counts as the
# 30th; an end on the 31st counts as the 30th when the start is the 30th or 31st.
day_count <- function(a, b, basis) {
  if (basis %in% c("ACT/ACT", "ACTUAL", "ACT/360", "ACT/365")) return(b - a)
  if (basis %in% c("30/360", "360")) {
    pa <- civil(a)
    pb <- civil(b)
    last_feb <- pa$m == 2 & pa$d == days_in_month(pa$y, 2)
    d1 <- ifelse(last_feb, 30, pmin(pa$d, 30))
    d2 <- ifelse(pb$d == 31 & d1 >= 30, 30, pb$d)
    return((pb$y - pa$y) * 360 + (pb$m - pa$m) * 30 + (d2 - d1))
  }
  NA_real_
}

#' @export
sas_f_datdif <- function(start, end, basis = "ACT/ACT") {
  rowwise(function(a, b, bs) {
    if (is.na(a) || is.na(b)) return(NA_real_)
    day_count(floor(a), floor(b), toupper(strip_both(bs)))
  }, start, end, basis, type = numeric(1))
}

# day of the year in a 365-day calendar (29 February counts as 28 February)
doy365 <- function(p) c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)[p$m] + pmin(p$d, ifelse(p$m == 2, 28, 31))

#' @export
sas_f_yrdif <- function(start, end, basis = "AGE") {
  rowwise(function(a, b, bs) {
    if (is.na(a) || is.na(b)) return(NA_real_)
    a <- floor(a)
    b <- floor(b)
    bs <- toupper(strip_both(bs))
    if (bs == "AGE") {
      # whole years plus the remaining days in a 365-day year (SAS 9.4); the
      # last bits of a fractional age are not reproduced (fn_probe/YRDIF), so
      # only whole years are in the verified domain
      sgn <- 1
      if (b < a) {
        t <- a
        a <- b
        b <- t
        sgn <- -1
      }
      pa <- civil(a)
      pb <- civil(b)
      da <- doy365(pa)
      db <- doy365(pb)
      yrs <- pb$y - pa$y - as.numeric(db < da)
      if ((db - da) %% 365 != 0) {
        abort_unsupported("YRDIF with basis 'AGE' for dates that are not a whole number of years apart (the fraction differs from SAS 9.4 in the last bits)")
      }
      return(sgn * (yrs + ((db - da) %% 365) / 365))
    }
    if (bs == "30/360" || bs == "360") return(day_count(a, b, "30/360") / 360)
    if (bs == "ACT/360") return((b - a) / 360)
    if (bs == "ACT/365") return((b - a) / 365)
    if (bs != "ACT/ACT" && bs != "ACTUAL") return(NA_real_)
    sgn <- 1
    if (b < a) {
      t <- a
      a <- b
      b <- t
      sgn <- -1
    }
    ya <- civil(a)$y
    yb <- civil(b)$y
    ylen <- function(y) from_ymd(y + 1, 1, 1) - from_ymd(y, 1, 1)
    if (ya == yb) return(sgn * (b - a) / ylen(ya))
    if (a != from_ymd(ya, 1, 1) || b != from_ymd(yb, 1, 1)) {
      abort_unsupported("YRDIF with basis 'ACT/ACT' for dates in different years that are not both 1 January (the sum of the year fractions differs from SAS 9.4 in the last bits)")
    }
    sgn * (yb - ya)
  }, start, end, basis, type = numeric(1))
}

# Intervals for INTCK / INTNX ----------------------------------------------------

# Parse an interval name: list(base, mult, shift, dt) where dt marks datetime
# (and time) intervals.
parse_interval <- function(x) {
  s <- tolower(strip_both(x))
  shift <- 1
  if (grepl(".", s, fixed = TRUE)) {
    shift <- as.numeric(sub("^.*\\.", "", s))
    s <- sub("\\..*$", "", s)
  }
  sp <- split_trailing_digits(s)
  base <- sp$base
  mult <- if (is.na(sp$width)) 1 else sp$width
  dt <- FALSE
  if (startsWith(base, "dt") && base != "dtday" || base == "dtday") {
    dt <- TRUE
    base <- substring(base, 3L)
  }
  if (base %in% c("hour", "minute", "second")) dt <- TRUE
  ok <- c("day", "week", "weekday", "tenday", "semimonth", "month", "qtr", "semiyear", "year", "hour", "minute", "second")
  if (!base %in% ok || is.na(mult) || mult < 1 || is.na(shift)) return(NULL)
  list(base = base, mult = mult, shift = shift, dt = dt)
}

interval_or_fail <- function(x, fname, .at) {
  iv <- parse_interval(x)
  nm <- sub("[0-9.]+$", "", tolower(strip_both(x)))
  if (is.null(iv) && nm %in% c("weekv", "yearv", "r445yr", "r454yr", "r544yr", "r445qtr", "r454qtr", "r544qtr",
                               "r445mon", "r454mon", "r544mon", "week1", "dtweekv", "dtyearv")) {
    abort_unsupported(paste0(toupper(fname), " interval '", strip_both(x), "'"))
  }
  if (is.null(iv)) return(NULL)
  if (iv$base %in% c("weekday", "tenday", "semimonth")) abort_unsupported(paste0(toupper(fname), " interval '", strip_both(x), "'"))
  # the shift index of YEAR/SEMIYEAR/QTR is in months and of WEEKn (n > 1) in weeks
  if (iv$shift != 1 && (iv$base %in% c("year", "semiyear", "qtr") || (iv$base == "week" && iv$mult > 1))) {
    abort_unsupported(paste0(toupper(fname), " interval '", strip_both(x), "' (shift index)"))
  }
  if (iv$base == "week") {
    # WEEK.d: weeks start on weekday d (1 = Sunday); the shift is in days
    iv$wshift <- iv$shift - 1
    iv$shift <- 1
  }
  iv
}

# SAS date values run from 1 January 1582 to 31 December 20000.
date_in_range <- function(v, iv) {
  lo <- -138061
  hi <- 6589335
  ok <- if (iv$dt) v >= lo * 86400 & v < (hi + 1) * 86400 else v >= lo & v <= hi
  isTRUE(ok)
}

# Index of the interval containing value v (calendar-based, before the multiplier).
interval_index <- function(v, iv) {
  if (iv$base %in% c("hour", "minute", "second")) {
    sec <- switch(iv$base, hour = 3600, minute = 60, second = 1)
    return(floor(v / sec))
  }
  d <- if (iv$dt) floor(v / 86400) else floor(v)
  switch(iv$base,
    day = d,
    week = floor((d + 5 - (iv$wshift %||% 0)) / 7), # weeks start on Sunday; 1960-01-01 was a Friday
    month = {
      p <- civil(d)
      (p$y - 1960) * 12 + p$m - 1
    },
    qtr = {
      p <- civil(d)
      (p$y - 1960) * 4 + (p$m - 1) %/% 3
    },
    semiyear = {
      p <- civil(d)
      (p$y - 1960) * 2 + (p$m - 1) %/% 6
    },
    year = civil(d)$y - 1960
  )
}

# First value (date or datetime) of interval index k.
interval_start <- function(k, iv) {
  if (iv$base %in% c("hour", "minute", "second")) {
    return(k * switch(iv$base, hour = 3600, minute = 60, second = 1))
  }
  d <- switch(iv$base,
    day = k,
    week = k * 7 - 5 + (iv$wshift %||% 0),
    month = from_ymd(1960 + k %/% 12, k %% 12 + 1, 1),
    qtr = from_ymd(1960 + k %/% 4, (k %% 4) * 3 + 1, 1),
    semiyear = from_ymd(1960 + k %/% 2, (k %% 2) * 6 + 1, 1),
    year = from_ymd(1960 + k, 1, 1)
  )
  if (iv$dt) d * 86400 else d
}

# Multiplier / shift: group base intervals into blocks of `mult` starting at `shift`.
block_index <- function(k, iv) floor((k - (iv$shift - 1)) / iv$mult)
block_first <- function(b, iv) b * iv$mult + (iv$shift - 1)

#' @export
sas_f_intck <- function(interval, from, to, method = "d", .at = NULL) {
  has_m <- !missing(method)
  rowwise(function(ivt, a, b, m) {
    iv <- interval_or_fail(ivt, "intck", .at)
    mm <- tolower(strip_both(m))
    ok_m <- nzchar(mm) && (startsWith("continuous", mm) || startsWith("discrete", mm))
    if (is.null(iv) || !ok_m ||
        (!is.na(a) && !date_in_range(a, iv)) || (!is.na(b) && !date_in_range(b, iv))) {
      note_invalid_arg("intck", NULL, .at, c(list(ivt, a, b), if (has_m) list(m)))
      return(NA_real_)
    }
    if (is.na(a) || is.na(b)) return(NA_real_)
    if (substr(mm, 1L, 1L) == "c") {
      if (iv$base %in% c("day", "hour", "minute", "second", "week")) {
        unit <- switch(iv$base, day = if (iv$dt) 86400 else 1, week = if (iv$dt) 7 * 86400 else 7,
                       hour = 3600, minute = 60, second = 1)
        return(trunc((b - a) / (unit * iv$mult)))
      }
      # continuous: whole intervals from a's position, rounded toward minus infinity
      n <- 0
      if (b >= a) {
        repeat {
          nxt <- sas_f_intnx(ivt, a, n + 1, "s")
          if (nxt > b || n > 1e6) break
          n <- n + 1
        }
      } else {
        repeat {
          n <- n - 1
          nxt <- sas_f_intnx(ivt, a, n, "s")
          if (nxt <= b || n < -1e6) break
        }
      }
      return(n)
    }
    ka <- block_index(interval_index(a, iv), iv)
    kb <- block_index(interval_index(b, iv), iv)
    kb - ka
  }, interval, from, to, method, type = numeric(1))
}

#' @export
sas_f_intnx <- function(interval, start, n, align = "b", .at = NULL) {
  has_al <- !missing(align)
  rowwise(function(ivt, s, k, al) {
    iv <- interval_or_fail(ivt, "intnx", .at)
    args <- c(list(ivt, s, k), if (has_al) list(al))
    if (is.null(iv)) {
      note_invalid_arg("intnx", 1L, .at, args, numbered = TRUE)
      return(NA_real_)
    }
    al <- tolower(strip_both(al))
    al <- c(b = "b", beginning = "b", m = "m", middle = "m", e = "e", end = "e", s = "s", same = "s", sameday = "s")[al]
    if (is.na(al)) {
      note_invalid_arg("intnx", 4L, .at, args, numbered = TRUE)
      return(NA_real_)
    }
    if (is.na(s) || is.na(k)) return(NA_real_)
    # a non-integer increment is rounded toward minus infinity
    k <- floor(k)
    if (!date_in_range(s, iv)) {
      note_invalid_arg("intnx", NULL, .at, args)
      return(NA_real_)
    }
    if (abs(k) > 1e7 || !date_in_range(interval_start(block_first(block_index(interval_index(s, iv), iv) + k, iv), iv), iv)) {
      note_invalid_arg("intnx", NULL, .at, args)
      return(NA_real_)
    }
    blk <- block_index(interval_index(s, iv), iv) + k
    first <- interval_start(block_first(blk, iv), iv)
    nxt <- interval_start(block_first(blk + 1, iv), iv)
    unit <- if (iv$dt || iv$base %in% c("hour", "minute", "second")) 1 else 1
    last <- nxt - unit
    switch(al,
      b = first,
      e = last,
      m = floor((first + last) / 2),
      s = {
        if (iv$base %in% c("day", "hour", "minute", "second")) return(first + (s - interval_start(interval_index(s, iv), iv)))
        if (iv$base == "week") return(first + (floor(if (iv$dt) s / 86400 else s) - floor(if (iv$dt) interval_start(interval_index(s, iv), iv) / 86400 else interval_start(interval_index(s, iv), iv))) * (if (iv$dt) 86400 else 1))
        d0 <- if (iv$dt) floor(s / 86400) else floor(s)
        p0 <- civil(d0)
        f0 <- if (iv$dt) floor(first / 86400) else first
        pf <- civil(f0)
        startblk <- interval_start(block_first(block_index(interval_index(s, iv), iv), iv), iv)
        ps <- civil(if (iv$dt) floor(startblk / 86400) else startblk)
        mon_off <- (p0$y - ps$y) * 12 + (p0$m - ps$m)
        ty <- pf$y + (pf$m - 1 + mon_off) %/% 12
        tm <- (pf$m - 1 + mon_off) %% 12 + 1
        dim <- days_in_month(ty, tm)
        out <- from_ymd(ty, tm, min(p0$d, dim))
        if (iv$dt) out * 86400 + (s - floor(s / 86400) * 86400) else out
      }
    )
  }, interval, start, n, align, type = numeric(1))
}

# Variable information (compile-time values are generated directly) ---------------

# Function catalogue additions -----------------------------------------------------

fn_table <- utils::modifyList(fn_table, list(
  substrn = fn_spec("char", 2, 3, "arg"), char = fn_spec("char", 2, 2, 1L), first = fn_spec("char", 1, 1, 1L),
  compbl = fn_spec("char", 1, 1, "arg"), compress = fn_spec("char", 1, 3, "arg"),
  find = fn_spec("num", 2, 4), findc = fn_spec("num", 2, 4), findw = fn_spec("num", 2, 5),
  indexw = fn_spec("num", 2, 3), verify = fn_spec("num", 2, Inf), count = fn_spec("num", 2, 3),
  countc = fn_spec("num", 2, 3), countw = fn_spec("num", 1, 3), scan = fn_spec("char", 2, 4, "arg"),
  transtrn = fn_spec("char", 3, 3, 200L), translate = fn_spec("char", 3, Inf, "arg"),
  propcase = fn_spec("char", 1, 2, "arg"), lengthc = fn_spec("num", 1), lengthm = fn_spec("num", 1),
  "repeat" = fn_spec("char", 2, 2, 200L), quote = fn_spec("char", 1, 2, 200L), dequote = fn_spec("char", 1, 1, "arg"),
  anyalpha = fn_spec("num", 1, 2), anydigit = fn_spec("num", 1, 2), anyalnum = fn_spec("num", 1, 2),
  anyspace = fn_spec("num", 1, 2), anypunct = fn_spec("num", 1, 2), anyupper = fn_spec("num", 1, 2),
  anylower = fn_spec("num", 1, 2), notalpha = fn_spec("num", 1, 2), notdigit = fn_spec("num", 1, 2),
  notalnum = fn_spec("num", 1, 2), notspace = fn_spec("num", 1, 2), notupper = fn_spec("num", 1, 2),
  notlower = fn_spec("num", 1, 2), notpunct = fn_spec("num", 1, 2),
  byte = fn_spec("char", 1, 1, 1L), rank = fn_spec("num", 1),
  ifc = fn_spec("char", 3, 4, 200L), ifn = fn_spec("num", 3, 4), choosec = fn_spec("char", 2, Inf, 200L),
  choosen = fn_spec("num", 2, Inf), whichc = fn_spec("num", 2, Inf), whichn = fn_spec("num", 2, Inf),
  compare = fn_spec("num", 2, 3), symget = fn_spec("char", 1, 1, 200L),
  prxparse = fn_spec("num", 1), prxmatch = fn_spec("num", 2), prxchange = fn_spec("char", 3, 3, 200L),
  rounde = fn_spec("num", 1, 2), roundz = fn_spec("num", 1, 2), intz = fn_spec("num", 1),
  floorz = fn_spec("num", 1), ceilz = fn_spec("num", 1), modz = fn_spec("num", 2), log2 = fn_spec("num", 1),
  median = fn_spec("num", 1, Inf), std = fn_spec("num", 1, Inf), var = fn_spec("num", 1, Inf),
  stderr = fn_spec("num", 1, Inf), cv = fn_spec("num", 1, Inf), range = fn_spec("num", 1, Inf),
  css = fn_spec("num", 1, Inf), uss = fn_spec("num", 1, Inf), geomean = fn_spec("num", 1, Inf),
  harmean = fn_spec("num", 1, Inf), skewness = fn_spec("num", 1, Inf), kurtosis = fn_spec("num", 1, Inf),
  largest = fn_spec("num", 2, Inf), smallest = fn_spec("num", 2, Inf), ordinal = fn_spec("num", 2, Inf),
  cmiss = fn_spec("num", 1, Inf), fact = fn_spec("num", 1), comb = fn_spec("num", 2), perm = fn_spec("num", 1, 2),
  gamma = fn_spec("num", 1), lgamma = fn_spec("num", 1), beta = fn_spec("num", 2), logbeta = fn_spec("num", 2),
  gcd = fn_spec("num", 1, Inf), lcm = fn_spec("num", 1, Inf), divide = fn_spec("num", 2),
  sin = fn_spec("num", 1), cos = fn_spec("num", 1), tan = fn_spec("num", 1), arsin = fn_spec("num", 1),
  arcos = fn_spec("num", 1), atan = fn_spec("num", 1), atan2 = fn_spec("num", 2), sinh = fn_spec("num", 1),
  cosh = fn_spec("num", 1), tanh = fn_spec("num", 1), constant = fn_spec("num", 1, 2),
  probnorm = fn_spec("num", 1), probit = fn_spec("num", 1), probt = fn_spec("num", 2, 3),
  probchi = fn_spec("num", 2, 3), probf = fn_spec("num", 3, 4), tinv = fn_spec("num", 2, 3),
  cinv = fn_spec("num", 2, 3), finv = fn_spec("num", 3, 4), probbnml = fn_spec("num", 3), poisson = fn_spec("num", 2),
  cdf = fn_spec("num", 2, 6), sdf = fn_spec("num", 2, 6), pdf = fn_spec("num", 2, 6), quantile = fn_spec("num", 2, 6),
  logpdf = fn_spec("num", 2, 6), logcdf = fn_spec("num", 2, 6), logsdf = fn_spec("num", 2, 6),
  dhms = fn_spec("num", 4), datetime = fn_spec("num", 0), time = fn_spec("num", 0),
  juldate = fn_spec("num", 1), juldate7 = fn_spec("num", 1), datejul = fn_spec("num", 1),
  week = fn_spec("num", 1, 2), datdif = fn_spec("num", 3), yrdif = fn_spec("num", 2, 3),
  intck = fn_spec("num", 3, 4), intnx = fn_spec("num", 3, 4),
  put = fn_spec("char", 1, 1, "fmt"), input = fn_spec("num", 1, 1),
  putn = fn_spec("char", 2, 4, 200L), putc = fn_spec("char", 2, 3, "arg"),
  inputn = fn_spec("num", 2, 4), inputc = fn_spec("char", 2, 3, 200L),
  vname = fn_spec("char", 1, 1, 200L), vlength = fn_spec("num", 1), vtype = fn_spec("char", 1, 1, 1L),
  vlabel = fn_spec("char", 1, 1, 200L), vformat = fn_spec("char", 1, 1, 200L), vformatn = fn_spec("char", 1, 1, 200L),
  vformatw = fn_spec("num", 1), vformatd = fn_spec("num", 1), vvalue = fn_spec("char", 1, 1, 200L),
  vinformat = fn_spec("char", 1, 1, 200L), lbound = fn_spec("num", 1, 2), hbound = fn_spec("num", 1, 2),
  missing = fn_spec("num", 1)
))

# Argument types of functions that convert mismatched arguments (see
# convert_fn_args()); verified against SAS in the function pilot.
fn_arg_types <- list(
  substr = c("char", "num", "num"), scan = c("char", "num", "char", "char"),
  compress = c("char", "char", "char"), index = c("char", "char"), round = c("num", "num"),
  intnx = c("char", "num", "num", "char"), intck = c("char", "num", "num", "char"), catx = "char"
)
# Functions whose maximum number of arguments is the SAS maximum (more is a
# compile-time error, ERROR 72-185, rather than an unimplemented option).
fn_max_exact <- c("substr", "scan", "compress", "index", "round", "intnx", "intck")

fn_table <- utils::modifyList(fn_table, list(cvt_n2c = fn_spec("char", 1, 1, 12L), cvt_c2n = fn_spec("num", 1, 1)))

#' @export
sas_f_cvt_n2c <- function(x) sas_fmt_best(x, 12L)

#' @export
sas_f_cvt_c2n <- function(x, .at = NULL) {
  s <- strip_both(x)
  ok <- grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", s)
  out <- rep(NA_real_, length(s))
  out[ok] <- as.numeric(s[ok])
  bad <- !ok & nzchar(s) & s != "."
  if (any(bad)) {
    rt_error(sprintf("NOTE: Invalid numeric data, '%s' , at line %d column %d.", s[which(bad)[1]],
                     as.integer(.at[1] %||% NA), as.integer(.at[2] %||% NA)))
  }
  out
}

# Functions whose arguments can be invalid (data error, _ERROR_ = 1).
err_fns <- c("substr", "sqrt", "exp", "log", "log10", "log2", "mod", "modz", "mdy", "input", "inputn", "fact",
             "gamma", "arsin", "arcos", "intck", "intnx", "symget", "scan", "round", "rounde", "roundz", "catx", "cat", "cats", "catt", "cvt_c2n",
             "prxparse", "prxmatch", "prxchange", "band", "bor", "bxor", "bnot", "blshift", "brshift")

# Functions that take a variable reference rather than a value.
var_info_fns <- c("vname", "vlength", "vtype", "vlabel", "vformat", "vformatn", "vformatw", "vformatd",
                  "vinformat", "vvalue")

#' CALL SORTN / SORTC: the values in ascending order (missing values first)
#' @export
#' @keywords internal
sas_sort_values <- function(vals) {
  v <- unlist(vals)
  if (is.character(v)) return(as.list(v[order(sas_rtrim(v), method = "radix")]))
  codes <- sas_miss_code(v)
  as.list(v[order(ifelse(is.na(codes), 99L, codes), ifelse(is.na(v), 0, v), method = "radix")])
}

# More functions ------------------------------------------------------------------------

# Bitwise functions on unsigned 32-bit integers (outside 0..2^32-1: missing).
bit32_op <- function(a, b, f, name, .at = NULL) {
  ab <- recycle_n(as.numeric(a), as.numeric(b))
  ab <- lapply(ab, trunc)
  ok <- !is.na(ab[[1]]) & !is.na(ab[[2]]) & ab[[1]] >= 0 & ab[[2]] >= 0 & ab[[1]] < 2^32 & ab[[2]] < 2^32
  out <- rep(NA_real_, length(ok))
  hi <- function(x) as.integer(floor(x / 65536))
  lo <- function(x) as.integer(x %% 65536)
  x <- ab[[1]][ok]
  y <- ab[[2]][ok]
  out[ok] <- f(hi(x), hi(y)) * 65536 + f(lo(x), lo(y))
  out
}
#' @export
sas_f_band <- function(a, b, .at = NULL) bit32_op(a, b, bitwAnd, "band", .at)
#' @export
sas_f_bor <- function(a, b, .at = NULL) bit32_op(a, b, bitwOr, "bor", .at)
#' @export
sas_f_bxor <- function(a, b, .at = NULL) bit32_op(a, b, bitwXor, "bxor", .at)
#' @export
sas_f_bnot <- function(a, .at = NULL) bit32_op(a, 2^32 - 1, bitwXor, "bnot", .at)
bit_shift <- function(a, n, f, name, .at) {
  a <- trunc(a)
  n <- trunc(n)
  bad <- !is.na(a) & !is.na(n) & (a < 0 | a >= 2^32 | n < 0 | n > 31)
  out <- f(a, n)
  out[bad | is.na(a) | is.na(n)] <- NA_real_
  out
}
#' @export
sas_f_blshift <- function(a, n, .at = NULL) bit_shift(a, n, function(a, n) (a * 2^n) %% 2^32, "blshift", .at)
#' @export
sas_f_brshift <- function(a, n, .at = NULL) bit_shift(a, n, function(a, n) floor(a / 2^n) + 0, "brshift", .at)

#' @export
sas_f_collate <- function(start, end = NULL, len = NULL) {
  has_end <- !is.null(end)
  rowwise(function(s, e, l) {
    # missing or invalid arguments: blank (and _ERROR_, see fn_policy)
    if (is.na(s) || s < 0) return("")
    if (has_end) {
      if (is.na(e) || e < s) return("")
      last <- e
    } else if (!is.na(l)) {
      if (l <= 0) return("")
      last <- s + l - 1
    } else {
      last <- 255
    }
    codes <- seq(trunc(s), min(255, trunc(last)))
    codes <- codes[codes > 0]
    paste(vapply(codes, function(k) iconv(rawToChar(as.raw(k)), "latin1", "UTF-8"), ""), collapse = "")
  }, start, if (is.null(end)) NA_real_ else end, if (is.null(len)) NA_real_ else len)
}

#' @export
sas_f_nvalid <- function(x, opt = "v7") {
  rowwise(function(s, o) {
    s <- sas_rtrim(s)
    o <- tolower(strip_both(o))
    if (!nzchar(o)) o <- "v7"
    if (o == "any") return(as.numeric(nchar(s) >= 1 && nchar(s) <= 32))
    if (o == "nliteral" && grepl("^(['\"]).*\\1[nN]$", s)) return(1)
    # UPCASE: a V7 name in upper case; other option values are invalid (fn_policy)
    if (o == "upcase") return(as.numeric(grepl("^[A-Z_][A-Z0-9_]{0,31}$", s)))
    if (!o %in% c("v7", "nliteral")) return(NA_real_)
    as.numeric(grepl("^[A-Za-z_][A-Za-z0-9_]{0,31}$", s))
  }, as_sas_chr(x), opt, type = numeric(1))
}

#' @export
sas_f_pctl <- function(p, ...) {
  m <- num_args(...)
  pp <- rep_len(p, nrow(m))
  vapply(seq_len(nrow(m)), function(i) {
    v <- sort(m[i, !is.na(m[i, ])])
    n <- length(v)
    q <- pp[i] / 100
    if (n == 0 || is.na(q) || q < 0 || q > 1) return(NA_real_)
    np <- n * q
    j <- floor(np)
    if (np - j > 1e-12) return(v[min(n, j + 1)])
    if (j == 0) return(v[1])
    if (j >= n) return(v[n])
    (v[j] + v[j + 1]) / 2
  }, 0)
}

fn_table <- utils::modifyList(fn_table, list(
  band = fn_spec("num", 2), bor = fn_spec("num", 2), bxor = fn_spec("num", 2), bnot = fn_spec("num", 1),
  blshift = fn_spec("num", 2), brshift = fn_spec("num", 2), collate = fn_spec("char", 1, 3, 200L),
  nvalid = fn_spec("num", 1, 2), pctl = fn_spec("num", 2, Inf)
))
