# Lexer: a hand-written state machine over code points.
#
# Regular expressions are never used to recognise statements; the lexer only
# classifies characters to build single tokens. Keywords are not reserved: every
# word is emitted as a `name` token and the parser decides its role by context.

datalines_words <- c("datalines", "cards", "lines", "datalines4", "cards4", "lines4")

# After these keywords a value is expected, so `.a` is a special missing value.
value_expecting_kw <- c(
  "if", "then", "else", "and", "or", "not", "until", "while", "to", "by",
  "eq", "ne", "lt", "le", "gt", "ge", "in", "min", "max", "contains", "like",
  "when", "return"
)

#' Tokenize SAS source code
#'
#' @param text SAS source (character; multiple elements are joined by newlines).
#' @return A tibble with columns `type`, `value`, `line`, `col`.
#' @examples
#' sas_lex("x = substr(name, 1, 3); /* comment */")
#' @export
sas_lex <- function(text) {
  text <- paste(text, collapse = "\n")
  cp <- utf8ToInt(enc2utf8(text))
  cp <- cp[cp != 13L] # normalise CRLF
  n <- length(cp)
  # tibble::new_tibble(): the same object as tibble::tibble(), without its
  # per-call column checks (the lexer runs once per statement chunk)
  if (n == 0) {
    return(tibble::new_tibble(list(type = character(), value = character(), line = integer(), col = integer()), nrow = 0L))
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
  types <- character(cap)
  values <- character(cap)
  lines <- integer(cap)
  cols <- integer(cap)
  ntok <- 0L
  stmt_start <- TRUE

  emit <- function(type, value, pos) {
    if (ntok == cap) {
      cap <<- cap * 2L
      length(types) <<- cap
      length(values) <<- cap
      length(lines) <<- cap
      length(cols) <<- cap
    }
    ntok <<- ntok + 1L
    types[ntok] <<- type
    values[ntok] <<- value
    lines[ntok] <<- if (pos <= n) line_of[pos] else line_of[n]
    cols[ntok] <<- if (pos <= n) col_of[pos] else col_of[n]
    stmt_start <<- type %in% c("semi", "datalines_end")
  }

  value_position <- function() {
    if (ntok == 0L) return(TRUE)
    t <- types[ntok]
    if (t == "name") return(tolower(values[ntok]) %in% value_expecting_kw)
    !t %in% c("num", "str", "str_macro", "date", "time", "datetime", "namelit",
              "hexstr", "missing", "rparen", "rbrace", "dot", "macro_var")
  }

  # Find the end of a quoted literal starting at quote position j.
  # Returns the position of the closing quote, or NA when unterminated.
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

  # Skip a `%*...;` comment (quotes honoured, as in SAS: an unmatched quote
  # swallows the rest of the program) or a `*...;` statement comment, where
  # real SAS 9.4 does not treat quotes specially (corpus case dist_functions).
  skip_comment_to_semi <- function(j, what, quotes = TRUE) {
    while (j <= n) {
      c <- cp[j]
      if (quotes && (c == 39L || c == 34L)) {
        k <- close_quote(j)
        if (is.na(k)) {
          cli::cli_warn(
            c("Unmatched quote inside a {what} (line {line_of[j]}).",
              "i" = "As in SAS, the rest of the program is swallowed."),
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
    # j: position just after the `;` of the DATALINES statement.
    while (j <= n && cp[j] != 10L) j <- j + 1L # rest of the statement line is ignored
    j <- j + 1L
    repeat {
      if (j > n) {
        emit("datalines_end", "", n)
        return(n + 1L)
      }
      e <- j
      while (e <= n && cp[e] != 10L) e <- e + 1L
      line_txt <- substr_cp(j, e - 1L)
      line_cp <- if (e > j) cp[j:(e - 1L)] else integer()
      if (four) {
        if (length(line_cp) >= 4L && all(line_cp[1:4] == 59L)) {
          emit("datalines_end", "", j)
          return(j + 4L)
        }
      } else {
        nb <- which(!is_space(line_cp))
        if (length(nb) > 0 && line_cp[nb[1]] == 59L) {
          emit("datalines_end", "", j)
          return(j + nb[1])
        }
        if (any(line_cp == 59L)) {
          abort_unsupported("a semicolon inside DATALINES data; use DATALINES4", line_of[j])
        }
      }
      emit("datalines", line_txt, j)
      j <- e + 1L
    }
  }

  i <- 1L
  while (i <= n) {
    c <- cp[i]
    nx <- at(i + 1L)

    if (is_space(c)) {
      i <- i + 1L
      next
    }
    # Block comment
    if (c == 47L && nx == 42L) {
      k <- i + 2L
      while (k < n && !(cp[k] == 42L && cp[k + 1L] == 47L)) k <- k + 1L
      if (k >= n) abort_syntax("unterminated /* comment", line_of[i])
      i <- k + 2L
      next
    }
    # Macro comment %* ... ;
    if (c == 37L && nx == 42L) {
      i <- skip_comment_to_semi(i + 2L, "%* macro comment")
      next
    }
    # Statement comment * ... ; (only at statement start)
    if (c == 42L && stmt_start) {
      i <- skip_comment_to_semi(i + 1L, "* comment statement", quotes = FALSE)
      next
    }
    # Quoted literals with optional type suffix
    if (c == 39L || c == 34L) {
      k <- close_quote(i)
      if (is.na(k)) abort_syntax("unterminated quoted string", line_of[i])
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
      emit(type, body, i)
      i <- s
      next
    }
    # Names (keywords are names too)
    if (is_alpha(c)) {
      was_stmt_start <- stmt_start
      k <- i
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      word <- substr_cp(i, k - 1L)
      emit("name", word, i)
      i <- k
      low <- tolower(word)
      if (was_stmt_start && low %in% datalines_words) {
        k <- i
        while (k <= n && is_space(cp[k])) k <- k + 1L
        if (at(k) == 59L) {
          emit("semi", ";", k)
          i <- lex_datalines(k + 1L, four = endsWith(low, "4"))
        }
      }
      next
    }
    # Hexadecimal numeric constants: a digit, hexadecimal digits, then X (0FX = 15)
    if (is_digit(c)) {
      is_hex <- function(c) is_digit(c) | (c >= 65L & c <= 70L) | (c >= 97L & c <= 102L)
      k <- i
      while (k <= n && is_hex(cp[k])) k <- k + 1L
      if (at(k) %in% c(88L, 120L) && !is_name_char(at(k + 1L))) {
        hx <- substr_cp(i, k - 1L)
        if (nchar(hx) > 13L) abort_unsupported(paste0("hexadecimal constant ", hx, "x with more than 13 digits"), line_of[i])
        v <- Reduce(function(a, d) a * 16 + d, strtoi(strsplit(hx, "")[[1]], 16L), 0)
        emit("num", format(v, scientific = FALSE), i)
        i <- k + 1L
        next
      }
    }
    # Numbers: 12, 1.5, .5, 1e3, 10.
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
      emit("num", substr_cp(i, k - 1L), i)
      i <- k
      next
    }
    # `.`: missing value, special missing, or a dot (first.x, formats)
    if (c == 46L) {
      if (value_position()) {
        if ((is_alpha(nx)) && !is_name_char(at(i + 2L))) {
          emit("missing", paste0(".", toupper(intToUtf8(nx))), i)
          i <- i + 2L
        } else {
          emit("missing", ".", i)
          i <- i + 1L
        }
      } else {
        emit("dot", ".", i)
        i <- i + 1L
      }
      next
    }
    # Macro tokens (not expanded in this version)
    if (c == 38L && (is_alpha(nx) || nx == 38L)) {
      k <- i
      while (k <= n && cp[k] == 38L) k <- k + 1L
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      emit("macro_var", substr_cp(i, k - 1L), i)
      i <- k
      next
    }
    if (c == 37L && is_alpha(nx)) {
      k <- i + 1L
      while (k <= n && is_name_char(cp[k])) k <- k + 1L
      emit("macro", substr_cp(i, k - 1L), i)
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
      emit(punct, intToUtf8(c), i)
      i <- i + 1L
      next
    }
    # Operators (normalised spelling)
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
      abort_syntax(paste0("unexpected character '", intToUtf8(c), "'"), line_of[i])
    }
    if (op %in% c("=", "^=", "<", "<=", ">", ">=") && at(i + len) == 58L) {
      op <- paste0(op, ":")
      len <- len + 1L
    }
    emit("op", op, i)
    i <- i + len
  }

  tibble::new_tibble(list(
    type = types[seq_len(ntok)], value = values[seq_len(ntok)],
    line = lines[seq_len(ntok)], col = cols[seq_len(ntok)]
  ), nrow = ntok)
}
