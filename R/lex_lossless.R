# Lossless lexer: the token table behind sas_lex(lossless = TRUE).
#
# The same state machine as sas_lex() (R/lex.R), with the characters sas_lex()
# skips kept as rows of their own: `ws` (a run of blanks, tabs and other
# spaces), `newline` (one line feed, with the carriage return before it) and
# `comment` (`/* ... */`, `%* ... ;` and statement `* ... ;` comments, the
# semicolon included). Every other row is the sas_lex() token with the same
# kind, value, line and column; tokens_semantic() gives that stream back.
#
# Contract: paste0(tok$text, collapse = "") is the source exactly, where the
# source is `paste(code, collapse = "\n")` converted with enc2utf8() (a latin1
# string comes back as the same characters in UTF-8).
#
# Positions. `line` and `col` count characters with carriage returns removed,
# as sas_lex() does; `end_line`/`end_col` are the position of the last
# character (for a zero-width row, the column before `col`). A carriage return
# belongs to the row that follows it, so `\r\n` is one `newline` row; one at the
# very end of the source joins the last row.
#
# Inputs sas_lex() rejects still give a complete table; the first such error is
# kept in the attribute "lex_error" and tokens_semantic() signals it:
# - an unterminated `/*` comment: a `comment` row up to the end;
# - an unterminated quoted string: an `error` row up to the end;
# - an unexpected character, a hexadecimal constant with too many digits: an
#   `error` row for those characters;
# - a semicolon inside DATALINES data: the line stays a `datalines` row.
# The text after `DATALINES;` on the statement line, which sas_lex() ignores, is
# a `ws` row when blank and a `comment` row otherwise.

trivia_kinds <- c("ws", "newline", "comment")

sas_tokens <- function(code) {
  code <- paste(code, collapse = "\n")
  orig <- utf8ToInt(enc2utf8(code))
  keep <- orig != 13L
  cp <- orig[keep]
  opos <- which(keep) # original index of each stripped position
  n <- length(cp)

  empty <- function() {
    tibble::new_tibble(list(
      id = integer(), kind = character(), text = character(), value = character(),
      line = integer(), col = integer(), end_line = integer(), end_col = integer()
    ), nrow = 0L)
  }
  if (length(orig) == 0L) return(empty())
  if (n == 0L) {
    # only carriage returns
    return(tibble::new_tibble(list(
      id = 1L, kind = "ws", text = intToUtf8(orig), value = "", line = 1L, col = 1L,
      end_line = 1L, end_col = 0L
    ), nrow = 1L))
  }

  is_nl <- cp == 10L
  line_of <- cumsum(c(1L, is_nl))[seq_len(n)]
  last_nl <- cummax(ifelse(is_nl, seq_len(n), 0L))
  col_of <- seq_len(n) - c(0L, last_nl)[seq_len(n)]

  at <- function(k) if (k >= 1L && k <= n) cp[k] else -1L
  is_alpha <- function(c) (c >= 65L & c <= 90L) | (c >= 97L & c <= 122L) | c == 95L
  is_digit <- function(c) c >= 48L & c <= 57L
  is_name_char <- function(c) is_alpha(c) | is_digit(c)
  is_space <- function(c) c %in% c(32L, 9L, 10L, 11L, 12L, 160L)
  substr_cp <- function(a, b) if (b < a) "" else intToUtf8(cp[a:b])

  cap <- 256L
  kinds <- character(cap)
  values <- character(cap)
  pos_v <- integer(cap) # position for line/col (as sas_lex())
  a_v <- integer(cap) # first stripped position of the row
  b_v <- integer(cap) # last stripped position (a - 1 for a zero-width row)
  ntok <- 0L
  last_kind <- NA_character_ # last non-trivia row
  last_value <- NA_character_
  stmt_start <- TRUE
  lex_error <- NULL

  push <- function(kind, value, pos, a, b) {
    if (ntok == cap) {
      cap <<- cap * 2L
      length(kinds) <<- cap
      length(values) <<- cap
      length(pos_v) <<- cap
      length(a_v) <<- cap
      length(b_v) <<- cap
    }
    ntok <<- ntok + 1L
    kinds[ntok] <<- kind
    values[ntok] <<- value
    pos_v[ntok] <<- pos
    a_v[ntok] <<- a
    b_v[ntok] <<- b
  }
  # a token of the sas_lex() stream
  emit <- function(type, value, pos, a = pos, b = a - 1L) {
    push(type, value, pos, a, b)
    last_kind <<- type
    last_value <<- value
    stmt_start <<- type %in% c("semi", "datalines_end")
  }
  # whitespace a..b: runs of spaces as `ws`, each line feed as `newline`
  emit_space <- function(a, b) {
    j <- a
    while (j <= b) {
      if (cp[j] == 10L) {
        push("newline", "\n", j, j, j)
        j <- j + 1L
        next
      }
      k <- j
      while (k < b && cp[k + 1L] != 10L) k <- k + 1L
      push("ws", substr_cp(j, k), j, j, k)
      j <- k + 1L
    }
  }
  emit_comment <- function(a, b) push("comment", substr_cp(a, b), a, a, b)
  # the first error sas_lex() would raise, kept for tokens_semantic()
  record <- function(expr) {
    if (is.null(lex_error)) lex_error <<- tryCatch(expr, error = function(e) e)
  }

  value_position <- function() {
    if (is.na(last_kind)) return(TRUE)
    t <- last_kind
    if (t == "name") return(tolower(last_value) %in% value_expecting_kw)
    !t %in% c("num", "str", "str_macro", "date", "time", "datetime", "namelit",
              "hexstr", "missing", "rparen", "rbrace", "dot", "macro_var")
  }

  close_quote <- function(j) {
    q <- cp[j]
    k <- j + 1L
    while (k <= n) {
      if (cp[k] == q) {
        if (at(k + 1L) == q) {
          k <- k + 2L
          next
        }
        return(k)
      }
      k <- k + 1L
    }
    NA_integer_
  }

  skip_comment_to_semi <- function(j, what, quotes = TRUE) {
    while (j <= n) {
      c <- cp[j]
      if (quotes && (c == 39L || c == 34L)) {
        k <- close_quote(j)
        if (is.na(k)) {
          cli::cli_warn(
            c("Unmatched quote inside a {what} (line {line_of[j]}).",
              "i" = "The rest of the program is swallowed."),
            class = "sasonr_unmatched_quote_in_comment"
          )
          return(n + 1L)
        }
        j <- k + 1L
        next
      }
      if (c == 59L) return(j + 1L)
      j <- j + 1L
    }
    n + 1L
  }

  lex_datalines <- function(j, four) {
    # rest of the statement line (ignored by sas_lex())
    e <- j
    while (e <= n && cp[e] != 10L) e <- e + 1L
    if (e > j) {
      if (all(is_space(cp[j:(e - 1L)]))) emit_space(j, e - 1L) else emit_comment(j, e - 1L)
    }
    if (e <= n) push("newline", "\n", e, e, e)
    j <- e + 1L
    repeat {
      if (j > n) {
        emit("datalines_end", "", n, n + 1L, n)
        return(n + 1L)
      }
      e <- j
      while (e <= n && cp[e] != 10L) e <- e + 1L
      line_txt <- substr_cp(j, e - 1L)
      line_cp <- if (e > j) cp[j:(e - 1L)] else integer()
      if (four) {
        if (length(line_cp) >= 4L && all(line_cp[1:4] == 59L)) {
          emit("datalines_end", "", j, j, j + 3L)
          return(j + 4L)
        }
      } else {
        nb <- which(!is_space(line_cp))
        if (length(nb) > 0 && line_cp[nb[1]] == 59L) {
          emit("datalines_end", "", j, j, j + nb[1] - 1L)
          return(j + nb[1])
        }
        if (any(line_cp == 59L)) {
          record(abort_unsupported("a semicolon inside DATALINES data; use DATALINES4", line_of[j]))
        }
      }
      emit("datalines", line_txt, j, j, e - 1L)
      if (e <= n) push("newline", "\n", e, e, e)
      j <- e + 1L
    }
  }

  i <- 1L
  while (i <= n) {
    c <- cp[i]
    nx <- at(i + 1L)

    if (is_space(c)) {
      k <- i
      while (k < n && is_space(cp[k + 1L])) k <- k + 1L
      emit_space(i, k)
      i <- k + 1L
      next
    }
    # Block comment
    if (c == 47L && nx == 42L) {
      k <- i + 2L
      while (k < n && !(cp[k] == 42L && cp[k + 1L] == 47L)) k <- k + 1L
      if (k >= n) {
        record(abort_syntax("unterminated /* comment", line_of[i]))
        emit_comment(i, n)
        i <- n + 1L
        next
      }
      emit_comment(i, k + 1L)
      i <- k + 2L
      next
    }
    # Macro comment %* ... ;
    if (c == 37L && nx == 42L) {
      k <- skip_comment_to_semi(i + 2L, "%* macro comment")
      emit_comment(i, k - 1L)
      i <- k
      next
    }
    # Statement comment * ... ; (only at statement start)
    if (c == 42L && stmt_start) {
      k <- skip_comment_to_semi(i + 1L, "* comment statement", quotes = FALSE)
      emit_comment(i, k - 1L)
      i <- k
      next
    }
    # Quoted literals with optional type suffix
    if (c == 39L || c == 34L) {
      k <- close_quote(i)
      if (is.na(k)) {
        record(abort_syntax("unterminated quoted string", line_of[i]))
        push("error", substr_cp(i, n), i, i, n)
        i <- n + 1L
        next
      }
      q <- intToUtf8(c)
      body <- substr_cp(i + 1L, k - 1L)
      body <- gsub(strrep(q, 2), q, body, fixed = TRUE)
      s <- k + 1L
      while (s <= n && is_name_char(cp[s])) s <- s + 1L
      suffix <- tolower(substr_cp(k + 1L, s - 1L))
      type <- switch(suffix,
        d = "date", t = "time", dt = "datetime", n = "namelit", x = "hexstr",
        b = "bitstr", NA_character_
      )
      if (is.na(type)) {
        type <- "str"
        s <- k + 1L
        if (c == 34L) {
          inner <- if (k - 1L >= i + 1L) cp[(i + 1L):(k - 1L)] else integer()
          trig <- which(inner %in% c(37L, 38L))
          if (any(is_alpha(inner[trig + 1L]) | inner[trig + 1L] %in% 38L, na.rm = TRUE)) {
            type <- "str_macro"
          }
        }
      }
      emit(type, body, i, i, s - 1L)
      i <- s
      next
    }
    # Names (keywords are names too)
    if (is_alpha(c)) {
      was_stmt_start <- stmt_start
      k <- i
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      word <- substr_cp(i, k - 1L)
      emit("name", word, i, i, k - 1L)
      i <- k
      low <- tolower(word)
      if (was_stmt_start && low %in% datalines_words) {
        k <- i
        while (k <= n && is_space(cp[k])) k <- k + 1L
        if (at(k) == 59L) {
          if (k > i) emit_space(i, k - 1L)
          emit("semi", ";", k, k, k)
          i <- lex_datalines(k + 1L, four = endsWith(low, "4"))
        }
      }
      next
    }
    # Hexadecimal numeric constants
    if (is_digit(c)) {
      is_hex <- function(c) is_digit(c) | (c >= 65L & c <= 70L) | (c >= 97L & c <= 102L)
      k <- i
      while (k <= n && is_hex(cp[k])) k <- k + 1L
      if (at(k) %in% c(88L, 120L) && !is_name_char(at(k + 1L))) {
        hx <- substr_cp(i, k - 1L)
        if (nchar(hx) > 13L) {
          record(abort_unsupported(paste0("hexadecimal constant ", hx, "x with more than 13 digits"), line_of[i]))
          push("error", substr_cp(i, k), i, i, k)
        } else {
          v <- Reduce(function(a, d) a * 16 + d, strtoi(strsplit(hx, "")[[1]], 16L), 0)
          emit("num", format(v, scientific = FALSE), i, i, k)
        }
        i <- k + 1L
        next
      }
    }
    # Numbers
    if (is_digit(c) || (c == 46L && is_digit(nx))) {
      k <- i
      while (k <= n && is_digit(cp[k])) k <- k + 1L
      if (at(k) == 46L) {
        k <- k + 1L
        while (k <= n && is_digit(cp[k])) k <- k + 1L
      }
      if (at(k) %in% c(69L, 101L)) {
        e1 <- at(k + 1L)
        skip <- if (is_digit(e1)) 1L else if (e1 %in% c(43L, 45L) && is_digit(at(k + 2L))) 2L else 0L
        if (skip > 0L) {
          k <- k + skip
          while (k <= n && is_digit(cp[k])) k <- k + 1L
        }
      }
      emit("num", substr_cp(i, k - 1L), i, i, k - 1L)
      i <- k
      next
    }
    # `.`: missing value, special missing, or a dot
    if (c == 46L) {
      if (value_position()) {
        if ((is_alpha(nx)) && !is_name_char(at(i + 2L))) {
          emit("missing", paste0(".", toupper(intToUtf8(nx))), i, i, i + 1L)
          i <- i + 2L
        } else {
          emit("missing", ".", i, i, i)
          i <- i + 1L
        }
      } else {
        emit("dot", ".", i, i, i)
        i <- i + 1L
      }
      next
    }
    # Macro tokens
    if (c == 38L && (is_alpha(nx) || nx == 38L)) {
      k <- i
      while (k <= n && cp[k] == 38L) k <- k + 1L
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      emit("macro_var", substr_cp(i, k - 1L), i, i, k - 1L)
      i <- k
      next
    }
    if (c == 37L && is_alpha(nx)) {
      k <- i + 1L
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      emit("macro", substr_cp(i, k - 1L), i, i, k - 1L)
      i <- k
      next
    }
    # Punctuation
    punct <- switch(as.character(c),
      "59" = "semi", "40" = "lparen", "41" = "rparen", "44" = "comma",
      "123" = "lbrace", "91" = "lbrace", "125" = "rbrace", "93" = "rbrace",
      NA_character_
    )
    if (!is.na(punct)) {
      emit(punct, intToUtf8(c), i, i, i)
      i <- i + 1L
      next
    }
    # Operators (normalised spelling in `value`, source spelling in `text`)
    op <- NA_character_
    len <- 1L
    if (c == 42L) {
      if (nx == 42L) { op <- "**"; len <- 2L } else op <- "*"
    } else if (c %in% c(124L, 33L, 166L)) {
      if (nx == c) { op <- "||"; len <- 2L } else op <- "|"
    } else if (c == 60L) {
      if (nx == 61L) { op <- "<="; len <- 2L } else if (nx == 62L) { op <- "<>"; len <- 2L } else op <- "<"
    } else if (c == 62L) {
      if (nx == 61L) { op <- ">="; len <- 2L } else if (nx == 60L) { op <- "><"; len <- 2L } else op <- ">"
    } else if (c == 61L) {
      op <- "="
    } else if (c %in% c(94L, 126L, 172L)) {
      if (nx == 61L) { op <- "^="; len <- 2L } else op <- "^"
    } else if (c == 38L) {
      op <- "&"
    } else if (c %in% c(43L, 45L, 47L, 36L, 64L, 35L, 58L, 63L)) {
      op <- intToUtf8(c)
    }
    if (is.na(op)) {
      record(abort_syntax(paste0("unexpected character '", intToUtf8(c), "'"), line_of[i]))
      push("error", intToUtf8(c), i, i, i)
      i <- i + 1L
      next
    }
    if (op %in% c("=", "^=", "<", "<=", ">", ">=") && at(i + len) == 58L) {
      op <- paste0(op, ":")
      len <- len + 1L
    }
    emit("op", op, i, i, i + len - 1L)
    i <- i + len
  }

  s <- seq_len(ntok)
  kinds <- kinds[s]; values <- values[s]; pos_v <- pos_v[s]; a_v <- a_v[s]; b_v <- b_v[s]
  pos_c <- pmin(pos_v, n)
  # source text: each non-empty row runs from the character after the previous
  # non-empty row to its own last character (carriage returns join the row
  # after them); the last row runs to the end of the source
  wide <- which(b_v >= a_v)
  st <- c(1L, opos[b_v[wide[-length(wide)]]] + 1L)
  en <- c(st[-1] - 1L, length(orig))
  text <- character(ntok)
  text[wide] <- vapply(seq_along(wide), function(w) intToUtf8(orig[st[w]:en[w]]), "")
  values[kinds %in% trivia_kinds] <- text[kinds %in% trivia_kinds]
  bz <- b_v >= 1L & b_v >= a_v
  end_line <- ifelse(bz, line_of[pmax(b_v, 1L)], line_of[pos_c])
  end_col <- ifelse(bz, col_of[pmax(b_v, 1L)], col_of[pos_c] - 1L)

  out <- tibble::new_tibble(list(
    id = s, kind = kinds, text = text, value = values,
    line = line_of[pos_c], col = col_of[pos_c],
    end_line = as.integer(end_line), end_col = as.integer(end_col)
  ), nrow = ntok)
  if (!is.null(lex_error)) attr(out, "lex_error") <- lex_error
  out
}

# The sas_lex() token stream of a lossless table: the rows that are not
# whitespace, line feeds or comments, as columns `type`, `value`, `line`, `col`.
# A table holding an input sas_lex() rejects signals the same error.
tokens_semantic <- function(tok) {
  err <- attr(tok, "lex_error")
  if (!is.null(err)) stop(err)
  k <- !tok$kind %in% trivia_kinds
  tibble::new_tibble(list(
    type = tok$kind[k], value = tok$value[k], line = tok$line[k], col = tok$col[k]
  ), nrow = sum(k))
}
