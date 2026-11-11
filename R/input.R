# INPUT statement engine: INFILE / DATALINES records, pointer controls,
# list / column / formatted input and the MISSOVER / TRUNCOVER / FLOWOVER /
# DSD rules.
#
# An input stream (`inf`) is an environment holding the records and the
# current position. sas_input_stmt() executes one INPUT statement against it
# and returns the updated PDV; reaching the end of the data before the first
# value of a statement stops the step (sasonr_stop), reaching it in the middle
# is a LOST CARD (data error, then stop).

#' Open an input stream
#' @param lines Records.
#' @param line_no SAS log line numbers of the records (in-stream data) or
#'   record numbers (external files).
#' @param opts list(dlm, dsd, mode ("flowover", "missover", "truncover",
#'   "stopover"), firstobs, obs, pad, lines (N), end_text).
#' @param instream `TRUE` for DATALINES/CARDS (records are padded to 80).
#' @export
#' @keywords internal
sas_infile_open <- function(lines, line_no = seq_along(lines), opts = list(), instream = TRUE) {
  inf <- new.env(parent = emptyenv())
  first <- opts$firstobs %||% 1
  last <- min(opts$obs %||% length(lines), length(lines))
  keep <- if (last >= first) seq.int(first, last) else integer()
  inf$term_no <- if (length(line_no) > 0) line_no[length(line_no)] + 1L else NA_integer_
  inf$term_text <- opts$end_text %||% ";"
  inf$instream <- instream
  lines <- lines[keep]
  line_no <- line_no[keep]
  if (!is.null(opts$ls)) lines <- substr(lines, 1L, opts$ls)
  if (instream || isTRUE(opts$pad)) {
    w <- max(80L, if (instream) 80L * ceiling(max(c(1L, nchar(lines))) / 80L) else opts$lrecl %||% 256L)
    lines <- paste0(lines, strrep(" ", pmax(0L, w - nchar(lines))))
  }
  inf$recs <- lines
  inf$raw <- sas_rtrim(lines)
  inf$line_no <- line_no
  inf$nrec <- length(lines)
  inf$dsd <- isTRUE(opts$dsd)
  dlm <- opts$dlm
  if (is.null(dlm)) dlm <- if (inf$dsd) "," else NULL
  inf$dlm <- if (is.null(dlm)) c(" ", "\t") else strsplit(dlm, "")[[1]]
  inf$blank_dlm <- is.null(opts$dlm) && !inf$dsd
  inf$mode <- opts$mode %||% "flowover"
  inf$N <- max(1L, as.integer(opts$lines %||% 1L))
  inf$r <- 0L
  inf$base <- 1L
  inf$c <- 1L
  inf$chars <- character()
  inf$len <- 0L
  inf$hold <- ""
  inf$ruled <- FALSE
  inf$flow_notes <- 0L
  inf
}

load_record <- function(inf, r) {
  inf$r <- r
  if (r > inf$nrec) {
    inf$chars <- character()
    inf$len <- 0L
  } else {
    inf$chars <- strsplit(inf$recs[r], "")[[1]]
    inf$len <- length(inf$chars)
  }
  inf$c <- 1L
}

lost_card <- function(inf) {
  rt_error("NOTE: LOST CARD.")
  load_record(inf, inf$nrec + 1L)
  inf$hold <- ""
  sas_stop_step()
}

# Move to the next record inside a statement (`/`, FLOWOVER).
next_record <- function(inf) {
  if (inf$r + 1L > inf$nrec) {
    # nothing read yet by this statement: a normal end of the data
    if (!isTRUE(inf$started)) {
      inf$eof <- TRUE
      sas_stop_step()
    }
    lost_card(inf)
  }
  load_record(inf, inf$r + 1L)
  inf$got <- TRUE
}

input_begin <- function(inf) {
  inf$got <- FALSE
  inf$started <- FALSE
  inf$var_seen <- FALSE
  if (inf$hold != "") {
    inf$got <- TRUE
    return(invisible())
  }
  nxt <- max(inf$r, if (inf$r > 0L) inf$base + inf$N - 1L else 0L) + 1L
  if (nxt > inf$nrec) {
    inf$eof <- TRUE
    sas_stop_step()
  }
  inf$base <- nxt
  load_record(inf, nxt)
}

field_text <- function(inf, a, b) {
  if (b < a) return("")
  paste(inf$chars[a:b], collapse = "")
}

# Reached the end of the record while values are still needed: returns TRUE
# when the caller should give up (missing value), FALSE after moving on.
end_of_record <- function(inf) {
  switch(inf$mode,
    missover = , truncover = TRUE,
    stopover = abort_runtime("INPUT statement exceeded record length. INFILE OPTION STOPOVER specified"),
    {
      # written once, at the end of the step (after LOST CARD and the like)
      rt$flow_note <- TRUE
      inf$flow_notes <- inf$flow_notes + 1L
      next_record(inf)
      FALSE
    }
  )
}

read_list_text <- function(inf, item) {
  if (inf$dsd) return(read_dsd_text(inf, item))
  dl <- inf$dlm
  repeat {
    c <- inf$c
    n <- inf$len
    while (c <= n && inf$chars[c] %in% dl) c <- c + 1L
    if (c <= n) break
    inf$c <- c
    if (end_of_record(inf)) return(NULL)
  }
  start <- c
  if (isTRUE(item$amp)) {
    # `&`: the value ends at two consecutive blanks (or a delimiter other than blank)
    while (c <= n && !(inf$chars[c] == " " && (c == n || inf$chars[c + 1L] == " ")) &&
           !(inf$chars[c] %in% setdiff(dl, " "))) c <- c + 1L
  } else {
    while (c <= n && !inf$chars[c] %in% dl) c <- c + 1L
  }
  inf$c <- c + 1L
  list(text = field_text(inf, start, c - 1L), c1 = start, c2 = c - 1L)
}

read_dsd_text <- function(inf, item) {
  dl <- inf$dlm
  repeat {
    if (inf$c <= inf$len + 1L) break
    if (end_of_record(inf)) return(NULL)
  }
  c <- inf$c
  n <- inf$len
  if (c == n + 1L) {
    inf$c <- n + 2L
    return(list(text = "", c1 = c, c2 = c))
  }
  if (!" " %in% dl) while (c <= n && inf$chars[c] == " ") c <- c + 1L
  start <- c
  if (c <= n && inf$chars[c] %in% c("\"", "'")) {
    q <- inf$chars[c]
    buf <- character()
    c <- c + 1L
    while (c <= n) {
      if (inf$chars[c] == q) {
        if (c < n && inf$chars[c + 1L] == q) {
          buf <- c(buf, q)
          c <- c + 2L
          next
        }
        break
      }
      buf <- c(buf, inf$chars[c])
      c <- c + 1L
    }
    text <- paste(buf, collapse = "")
    if (isTRUE(item$tilde)) text <- paste0(q, text, q)
    while (c <= n && !inf$chars[c] %in% dl) c <- c + 1L
  } else {
    while (c <= n && !inf$chars[c] %in% dl) c <- c + 1L
    text <- field_text(inf, start, c - 1L)
  }
  inf$c <- if (c <= n) c + 1L else n + 2L
  list(text = text, c1 = start, c2 = max(start, c - 1L))
}

# A fixed field [a, b] of the current record (column / formatted input).
read_fixed_text <- function(inf, a, b) {
  if (b > inf$len && !inf$instream) {
    if (inf$mode == "truncover") return(list(text = field_text(inf, a, inf$len), c1 = a, c2 = b))
    if (inf$mode == "missover") return(NULL)
    if (inf$mode == "flowover" && a > inf$len) {
      end_of_record(inf)
      w <- b - a
      return(list(text = field_text(inf, 1L, min(1L + w, inf$len)), c1 = 1L, c2 = 1L + w, moved = TRUE))
    }
    return(list(text = field_text(inf, a, min(b, inf$len)), c1 = a, c2 = b))
  }
  list(text = field_text(inf, a, min(b, inf$len)), c1 = a, c2 = b)
}

invalid_data <- function(inf, item, c1, c2) {
  if (identical(item$qq, "??")) return(invisible())
  note <- if (identical(item$qq, "?")) NULL else {
    sprintf("NOTE: Invalid data for %s in line %d %d-%d.", item$disp %||% item$name,
            as.integer(inf$line_no[inf$r]), as.integer(c1), as.integer(c2))
  }
  rt_error(note)
}

store_value <- function(pdv, item, text, inf, c1, c2, mode) {
  fmt <- item$fmt
  if (item$char) {
    if (!is.null(fmt) && !is.null(user_fmt(fmt$name, TRUE, "invalue"))) {
      r <- sas_informat_read(text, fmt)
      if (!r$ok) invalid_data(inf, item, c1, c2)
      pdv[[item$name]] <- sas_char(r$v, item$len)
      return(pdv)
    }
    keep <- mode == "formatted" && !is.null(fmt) && fmt$name == "char"
    if (mode %in% c("list", "modified") || is.null(fmt)) {
      v <- sub("^ +", "", text)
      if (strip_both(v) == ".") v <- ""
      if (!is.null(fmt) && fmt$name == "upcase") v <- toupper(v)
    } else {
      v <- read_char_field(text, fmt, keep)
    }
    pdv[[item$name]] <- sas_char(v, item$len)
    return(pdv)
  }
  f <- fmt %||% fmt_spec("", NA_integer_, item$cd %||% NA_integer_)
  r <- sas_informat_read(text, f)
  if (!r$ok) invalid_data(inf, item, c1, c2)
  pdv[[item$name]] <- as.numeric(r$v)
  pdv
}

ptr_value <- function(v, pdv) {
  if (is.function(v)) v <- v(pdv)
  if (is.character(v)) return(v)
  if (length(v) != 1 || is.na(v)) abort_runtime("invalid INPUT pointer value")
  as.integer(trunc(v))
}

#' Execute one INPUT statement
#' @param spec list(items, hold): see the generated code.
#' @export
#' @keywords internal
sas_input_stmt <- function(inf, pdv, spec) {
  inf$pdv <- NULL
  inf$pdv_partial <- NULL
  input_begin(inf)
  # a LOST CARD stops the step: keep the values already read for the PDV dump
  withCallingHandlers(pdv <- input_items(inf, pdv, spec), sasonr_stop = function(e) inf$pdv <- inf$pdv_partial %||% pdv)
  inf$hold <- spec$hold %||% ""
  pdv
}

input_items <- function(inf, pdv, spec) {
  for (it in spec$items) {
    # once a value has been read, running out of records is a LOST CARD
    if (isTRUE(inf$var_seen)) inf$started <- TRUE
    switch(it$k,
      col = {
        k <- ptr_value(it$v, pdv)
        if (k < 1 && is.function(it$v)) k <- 1L
        if (k < 1) abort_runtime("The specified value for the '@' INPUT/PUT statement option must be within the range 1<=N<=9.0071993E15")
        inf$c <- k
      },
      col_str = {
        s <- ptr_value(it$v, pdv)
        rec <- paste(inf$chars, collapse = "")
        pos <- regexpr(s, substring(rec, inf$c), fixed = TRUE)
        inf$c <- if (pos > 0) inf$c + pos - 1L + nchar(s) else inf$len + 2L
      },
      rel = inf$c <- max(1L, inf$c + ptr_value(it$v, pdv)),
      line = {
        target <- inf$base + ptr_value(it$v, pdv) - 1L
        if (target < inf$r - inf$N + 1L) {
          abort_runtime(sprintf("Old line %d wanted but SAS is at line %d", as.integer(inf$line_no[target]), as.integer(inf$line_no[inf$r])))
        }
        if (target > inf$nrec) lost_card(inf)
        load_record(inf, target)
      },
      "next" = next_record(inf),
      var = {
        inf$var_seen <- TRUE
        if (it$mode %in% c("list", "modified")) {
          f <- read_list_text(inf, it)
          if (is.null(f)) {
            pdv[[it$name]] <- if (it$char) sas_char("", it$len) else NA_real_
            next
          }
          if (it$mode == "modified" && !is.null(it$fmt) && !it$char) {
            pdv <- store_value(pdv, it, f$text, inf, f$c1, f$c2, "formatted")
          } else {
            pdv <- store_value(pdv, it, f$text, inf, f$c1, f$c2, it$mode)
          }
        } else if (it$mode == "column") {
          f <- read_fixed_text(inf, it$from, it$to)
          inf$c <- (if (isTRUE(f$moved)) f$c2 else it$to) + 1L
          if (is.null(f)) {
            pdv[[it$name]] <- if (it$char) sas_char("", it$len) else NA_real_
            next
          }
          pdv <- store_value(pdv, it, f$text, inf, f$c1, f$c2, "column")
        } else {
          w <- infmt_default_w(it$fmt)
          if (is.na(w)) w <- it$len
          f <- read_fixed_text(inf, inf$c, inf$c + w - 1L)
          if (is.null(f)) {
            inf$c <- inf$c + w
            pdv[[it$name]] <- if (it$char) sas_char("", it$len) else NA_real_
            next
          }
          inf$c <- f$c1 + w
          pdv <- store_value(pdv, it, f$text, inf, f$c1, f$c2, "formatted")
        }
      }
    )
    inf$pdv_partial <- pdv
  }
  pdv
}

#' End of a DATA step iteration: release a record held by a trailing `@`
#' @export
#' @keywords internal
sas_input_iter_end <- function(inf) {
  if (identical(inf$hold, "@")) inf$hold <- ""
  invisible()
}

#' Is the current record the last one? (INFILE END=)
#' @export
#' @keywords internal
sas_input_at_end <- function(inf) {
  # in-stream data are read unbuffered: SAS never sets END= to 1 for DATALINES
  if (inf$instream) return(0)
  as.numeric(inf$r >= inf$nrec)
}

input_buffer_lines <- function(inf) {
  if (inf$r < 1L) return(character())
  out <- character()
  if (!inf$ruled) {
    inf$ruled <- TRUE
    out <- paste0("RULE:", strrep(" ", if (inf$instream) 6L else 5L),
                  paste0(rep(c("----+----"), 10), c(1:9, 0), collapse = ""))
  }
  rs <- seq.int(inf$base, max(inf$base, inf$r))
  if (inf$N <= 1L) rs <- inf$r
  for (r in rs) {
    if (r > inf$nrec) {
      out <- c(out, formatC(as.character(inf$term_no), width = -10L, flag = "-") |> paste0(" ", inf$term_text))
    } else {
      out <- c(out, paste0(formatC(as.character(inf$line_no[r]), width = -10L), " ", inf$raw[r]))
    }
  }
  out
}

#' Read all observations of a step whose only reader is one INPUT statement
#'
#' Used by vectorised (L1/L2) code: each iteration executes the INPUT
#' statement once; the variables it reads start missing in every iteration.
#' @export
#' @keywords internal
sas_read_input <- function(inf, spec, end_var = NULL) {
  vars <- list()
  for (it in spec$items) {
    if (it$k == "var" && is.null(vars[[it$name]])) {
      vars[[it$name]] <- if (it$char) sas_char("", it$len) else NA_real_
    }
  }
  rows <- list()
  ends <- numeric()
  repeat {
    ok <- tryCatch({
      rec <- sas_input_stmt(inf, vars, spec)
      TRUE
    }, sasonr_stop = function(e) FALSE)
    if (!ok) break
    rows[[length(rows) + 1L]] <- rec
    ends <- c(ends, sas_input_at_end(inf))
    sas_input_iter_end(inf)
  }
  cols <- lapply(names(vars), function(v) {
    proto <- vars[[v]]
    if (length(rows) == 0) return(proto[0])
    vapply(rows, function(r) r[[v]], FUN.VALUE = proto, USE.NAMES = FALSE)
  })
  names(cols) <- names(vars)
  out <- tibble::as_tibble(cols)
  if (!is.null(end_var)) out[[end_var]] <- ends
  out
}

input_var_names <- function(r) {
  unique(vapply(Filter(function(it) it$k == "var", r$items), `[[`, "", "name"))
}

#' Read an external text file for INFILE (path relative to the working directory)
#' @export
#' @keywords internal
sas_read_file <- function(path) {
  if (!file.exists(path)) abort_runtime(paste0("Physical file does not exist, ", path))
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  sub("\r$", "", lines)
}
