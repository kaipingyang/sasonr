# Views of the log (log.R): the events as a table, and the plain, ANSI and
# HTML renderers with the optional source echo.

# Where the step boundaries fall in a chunk of program text (`text`, from
# program line `line`): the last line of each step, at least its last
# statement and at most the line before the next step starts (the line
# itself when both share it).  For a DATA step with DATALINES the end is its
# DATALINES line; attribute "tail": for each such step the lines after that
# end that still belong to it, c(from, to) (empty when from > to; NULL for
# the other steps).
log_step_ends <- function(steps, text, line) {
  if (!length(steps)) return(integer())
  last <- log_chunk_last(text, line)
  starts <- vapply(steps, function(s) vctrs::vec_cast(s$line %||% line, integer()), 1L)
  maxl <- vapply(steps, log_max_line, 1L)
  nxt <- c(pmax(starts[-1] - 1L, starts[-length(starts)]), last)
  ends <- vctrs::vec_cast(pmin(last, pmax(starts, maxl, nxt)), integer())
  # a DATA step with DATALINES runs when its DATALINES statement has been
  # read: the data lines are not echoed, the lines after them come later
  dl <- lapply(steps, log_step_datalines)
  has <- !vapply(dl, is.null, logical(1))
  full <- ends
  ends[has] <- vapply(dl[has], function(d) vctrs::vec_cast(d$line, integer()), 1L)
  attr(ends, "tail") <- lapply(seq_along(ends), function(k) if (has[k]) c(ends[k] + 1L, full[k]))
  ends
}

# The DATALINES statement of a DATA step (NULL: none).
log_step_datalines <- function(step) {
  if (!identical(step$type, "data")) return(NULL)
  for (s in step$stmts) if (is.list(s) && identical(s$stmt, "datalines")) return(s)
  NULL
}

# The last program line of a chunk of text that starts at line `line`
# (trailing blank lines excluded).
log_chunk_last <- function(text, line) {
  vctrs::vec_cast(line + nchar(gsub("[^\n]", "", sub("\\s+$", "", text))), integer())
}

# The largest `line` field in a parsed step.
log_max_line <- function(x) {
  m <- NA_integer_
  walk <- function(y) {
    if (!is.list(y)) return()
    l <- y[["line"]]
    if (is.numeric(l) && length(l) == 1L && !is.na(l)) m <<- max(m, vctrs::vec_cast(l, integer()), na.rm = TRUE)
    for (z in y) if (is.list(z)) walk(z)
  }
  walk(x)
  m
}

log_step_label <- function(step) {
  switch(step$type,
    data = "DATA step",
    sql_begin = "PROC SQL",
    proc = paste("PROC", toupper(step$origin %||% step$name)),
    toupper(step$type)
  )
}

# The events of a submission as a tibble (the `log_events` field of a
# result): one row per event.  `programs`: the program of each submission
# (log_program()) named by its number, kept as an attribute for the source
# echo; `sub`: the submission of events that do not record one.
log_events_table <- function(events, programs, sub = 1L) {
  col <- function(f, type) vapply(events, function(ev) ev[[f]] %||% NA, type)
  out <- tibble::tibble(
    seq = col("seq", integer(1)),
    submission = vapply(events, function(ev) vctrs::vec_cast(ev$sub %||% sub, integer()), integer(1)),
    step = col("step", integer(1)),
    phase = col("phase", character(1)),
    kind = col("kind", character(1)),
    id = col("id", character(1)),
    args = lapply(events, `[[`, "args"),
    at = lapply(events, `[[`, "at"),
    places = lapply(events, `[[`, "places"),
    text = lapply(events, `[[`, "text"),
    source_line = vapply(events, function(ev) vctrs::vec_cast(ev$src %||% 0L, integer()), integer(1))
  )
  attr(out, "programs") <- programs
  out
}

# The events table of the submission of an engine (before log_flush()).
log_result_events <- function(eng) {
  sub <- length(eng$session$log_programs) + 1L
  log_events_table(eng$events, stats::setNames(list(eng$program), sub), sub)
}

# The events table of a session (every submission).
log_session_events <- function(s) {
  progs <- s$log_programs %||% list()
  names(progs) <- as.character(seq_along(progs))
  log_events_table(s$events, progs)
}

# The items of a view: the event lines and, with `echo`, the source echo as
# with their numbers (docs: ?sas_log): the program lines read before each event,
# numbered in an 11-column field and wrapped at LINESIZE (log_echo_line()),
# as long as OPTIONS SOURCE is in effect and except the data lines after
# DATALINES; under a line with an ERROR whose column is known the underscore
# marker and the error number; a blank line after a numbered ERROR message
# (log_error_blank()) and before what a step writes when it runs (the "echo"
# events, log_boundary(); before the numbered ERRORs of the step's last line,
# log_step_end_at()).
# One item: list(type = "src", "marker" or the event kind, step, sub, lines).
# `compact`: the echo in the compact layout (log_compact_items()).
log_items <- function(ev, echo, compact = FALSE) {
  progs <- attr(ev, "programs") %||% list()
  items <- list()
  subs <- unique(c(ev$submission, if (echo) as.integer(names(progs))))
  for (sub in subs[!is.na(subs)]) {
    e <- ev[ev$submission == sub, , drop = FALSE]
    p <- progs[[as.character(sub)]]
    # an echo that is not implemented leaves the echo of the submission out
    # (the messages and PUT output stay, after a NOTE that says why)
    why <- if (echo) p$echo_omit
    one <- if (echo && is.null(why)) {
      tryCatch(log_sub_items(e, p, sub, TRUE), sasonr_echo_unknown = function(c) {
        why <<- conditionMessage(c)
        NULL
      })
    }
    if (!is.null(why)) {
      one <- c(list(list(type = "note", step = 0L, sub = sub, lines = log_render("sasonr.echo_omitted", list(reason = why)))),
               log_sub_items(e, p, sub, FALSE))
    } else if (!echo) {
      one <- log_sub_items(e, p, sub, FALSE)
    }
    items <- c(items, one)
  }
  if (compact) log_compact_items(items) else items
}

# The compact layout of the echo (sas_log(compact = TRUE)): the line number
# left-justified in as many columns as the largest line number of the view
# needs plus one, then the text; a continuation line starts with "!" in those
# columns. The lines break where they do in the default layout and the marker
# lines lose the same columns, so they stay under their tokens.
log_compact_items <- function(items) {
  type <- vapply(items, `[[`, "", "type")
  if (!any(type == "src")) return(items)
  head <- function(l) substr(l, 1L, 11L)
  src <- unlist(lapply(items[type == "src"], `[[`, "lines"))
  w <- max(nchar(sub(" .*$", "", head(src)))) + 1L
  for (k in which(type == "src")) {
    l <- items[[k]]$lines
    num <- sub(" .*$", "", head(l))
    num[grepl("! $", head(l))] <- "!"
    items[[k]]$lines <- paste0(formatC(num, width = -w), substring(l, 12L))
  }
  for (k in which(type == "marker")) items[[k]]$lines <- substring(items[[k]]$lines, 12L - w)
  items
}

# An echo layout that is not implemented (`reason`): log_items() leaves the
# echo of the submission out.
log_echo_unknown <- function(reason) {
  rlang::abort(reason, class = "sasonr_echo_unknown")
}

# The items of submission `sub` (its events `e`, its program `p`).
log_sub_items <- function(e, p, sub, echo) {
  items <- list()
  add <- function(type, step, sub, lines) items[[length(items) + 1L]] <<- list(type = type, step = step, sub = sub, lines = lines)
  {
    lines <- if (echo && !is.null(p)) p$lines else character()
    marks <- if (length(lines)) log_markers(e, lines) else list()
    done <- 0L
    # the lines after the data lines of a DATALINES step stay in that step
    st <- p$steps
    tails <- if (is.data.frame(st) && !is.null(st$tail_from)) st[!is.na(st$tail_from), , drop = FALSE]
    line_steps <- function(n, step) {
      s <- rep(step, length(n))
      for (k in seq_len(if (is.null(tails)) 0L else nrow(tails))) {
        s[tails$tail_from[k] <= n & n <= tails$tail_to[k]] <- tails$step[k]
      }
      s
    }
    # per program line, computed once: OPTIONS SOURCE in effect and LINESIZE
    all_n <- seq_along(lines)
    src_on <- log_source_on_all(p$source, all_n)
    ls_all <- log_ls_at_all(p$ls, all_n)
    # one item per run of lines of the same step; a marker ends a run
    echo_to <- function(upto, step) {
      upto <- min(upto, length(lines))
      if (done >= upto) return(invisible())
      n <- (done + 1L):upto
      done <<- upto
      n <- n[!(n %in% p$skip) & src_on[n]]
      if (!length(n)) return(invisible())
      s <- line_steps(n, step)
      mk <- as.character(n) %in% names(marks)
      grp <- cumsum(c(TRUE, s[-1L] != s[-length(s)] | mk[-length(mk)]))
      for (g in unique(grp)) {
        w <- grp == g
        idx <- n[w]
        add("src", s[w][1L], sub, log_echo_lines(idx, lines[idx], ls_all[idx]))
        m <- marks[[as.character(idx[length(idx)])]]
        if (!is.null(m)) add("marker", s[w][1L], sub, m)
      }
    }
    for (i in seq_len(nrow(e))) {
      if (e$kind[i] == "echo" && !echo) next
      n0 <- length(items)
      echo_to(e$source_line[i], e$step[i])
      # the step ends on the line of its numbered messages (log_step_end_at())
      k <- if (e$kind[i] == "echo" && length(items) == n0) log_step_end_at(items, e$step[i])
      if (!is.null(k)) {
        blank <- list(type = "echo", step = e$step[i], sub = sub, lines = "")
        for (m in rev(k)) {
          if (m == length(items) || items[[m + 1L]]$type != "echo") items <- append(items, list(blank), after = m)
        }
        items <- append(items, list(blank), after = k[1L] - 1L)
        next
      }
      if (echo && e$kind[i] == "warning" && log_warning_numbered(e$text[[i]])) {
        log_echo_unknown("a numbered WARNING")
      }
      add(e$kind[i], e$step[i], sub, e$text[[i]])
      if (echo && e$kind[i] == "error" && log_error_blank(e$text[[i]], st$label[st$step == e$step[i]])) {
        add("echo", e$step[i], sub, "")
      }
    }
    echo_to(length(lines), if (nrow(e)) e$step[nrow(e)] else 0L)
  }
  items
}

# The step's lines end on the line of its last numbered ERRORs (`items` so
# far, `step`: the items after the step's last echoed line or its marker are
# ERRORs of the step and the blank lines after them): the blank line that
# ends the step's lines comes before the first numbered ERROR (the ERRORs
# without a number stay above it) and every numbered ERROR is followed by a
# blank line, whatever the procedure.  The positions of those numbered ERRORs,
# or NULL: the blank line comes at the end, as usual.  What is not known
# leaves the echo out (log_echo_unknown()): such ERRORs after no echoed line
# of the step, a NOTE or WARNING among them, an ERROR without a number after
# the last numbered one.
log_step_end_at <- function(items, step) {
  j <- length(items)
  while (j >= 1L && identical(items[[j]]$step, step) && items[[j]]$type %in% c("error", "echo", "note", "warning")) j <- j - 1L
  tail <- seq_len(length(items) - j) + j
  num <- tail[vapply(items[tail], function(it) it$type == "error" && log_error_numbered(it$lines), logical(1))]
  if (!length(num)) return(NULL)
  ok <- j >= 1L && identical(items[[j]]$step, step) && items[[j]]$type %in% c("src", "marker") &&
    all(vapply(items[tail], `[[`, "", "type") %in% c("error", "echo")) &&
    all(vapply(items[tail[tail > max(num)]], `[[`, "", "type") == "echo")
  if (!ok) log_echo_unknown("the blank lines around numbered ERRORs at the end of a step")
  num
}

log_error_numbered <- function(text) length(text) > 0L && grepl("^ERROR [0-9]+-[0-9]+:", text[1])
log_warning_numbered <- function(text) length(text) > 0L && grepl("^WARNING [0-9]+-[0-9]+:", text[1])

# Whether the source echo writes a blank line after an ERROR message (`text`,
# in a step labelled `label`; none: open code): after a numbered ERROR
# ("ERROR 22-322: ..."), except in the steps of the procedures that write
# none; never after an ERROR without a number ("ERROR: File WORK.X.DATA does
# not exist."). The blank line before what the step writes when it runs is
# a different one (log_boundary(), log_step_end_at()).
log_error_blank <- function(text, label) {
  log_error_numbered(text) && !any(label %in% log_error_no_blank)
}

# The procedures whose numbered ERRORs are not followed by a blank line when
# more of the step's lines follow. The DATA step, global statements and PROC
# SQL, FORMAT, PRINT, TABULATE, DELETE, DATASETS, IMPORT and EXPORT write one.
# (A numbered WARNING, "WARNING 1-322: ...", leaves the echo out.)
log_error_no_blank <- paste("PROC", c("MEANS", "SUMMARY", "UNIVARIATE", "TRANSPOSE", "COMPARE",
                                      "SORT", "FREQ", "RANK", "APPEND", "CONTENTS"))

# The echo of program line `n`: the line number left-justified in 11
# columns, then the text without trailing blanks; a longer line than `ls`
# continues on lines that start with the number in 9 columns and "! ".  A
# line breaks after `ls` characters when a blank follows (the blank starts
# the next line), else at its last blank (dropped).
log_echo_line <- function(n, text, ls) {
  full <- paste0(formatC(n, width = -11), sub("[[:space:]]+$", "", text))
  out <- character()
  while (nchar(full) > ls) {
    if (substr(full, ls + 1L, ls + 1L) == " ") {
      cut <- ls
      skip <- 0L
    } else {
      sp <- gregexpr(" ", substr(full, 12L, ls), fixed = TRUE)[[1]]
      if (sp[1] > 0) {
        cut <- 11L + sp[length(sp)] - 1L
        skip <- 1L
      } else {
        cut <- ls
        skip <- 0L
      }
    }
    out <- c(out, substr(full, 1L, cut))
    rest <- paste0(formatC(n, width = -9), "! ", substring(full, cut + 1L + skip))
    # every turn moves at least one character of the text to `out`
    if (nchar(rest) >= nchar(full)) rlang::abort("log_echo_line(): the line does not get shorter.", .internal = TRUE)
    full <- rest
  }
  c(out, full)
}

# The echo of program lines `n` (vectorised log_echo_line(): lines that fit in
# `ls` need no wrapping).
log_echo_lines <- function(n, text, ls) {
  full <- paste0(formatC(n, width = -11), sub("[[:space:]]+$", "", text))
  out <- as.list(full)
  for (j in which(nchar(full) > ls)) out[[j]] <- log_echo_line(n[j], text[j], ls[j])
  unlist(out)
}

# log_source_on() and log_ls_at() for every line of `n` at once: the last
# setting, in the order the settings were recorded, on a line before n.  The
# lines of the settings need not increase: text from a macro variable counts
# its lines from the line it is used on (`%let t = %str(...); &t`).
log_setting_at <- function(line, value, n, default) {
  out <- rep(default, length(n))
  for (i in seq_along(line)) out[n > line[i]] <- value[i]
  out
}

log_source_on_all <- function(src, n) {
  if (is.null(src) || !nrow(src)) return(rep(TRUE, length(n)))
  log_setting_at(src$line, src$on, n, TRUE)
}

log_ls_at_all <- function(ls, n) {
  if (is.null(ls) || !nrow(ls)) return(rep(132L, length(n)))
  log_setting_at(ls$line, ls$ls, n, ls$ls[1])
}

# OPTIONS SOURCE / NOSOURCE in effect for program line `n`.
log_source_on <- function(src, n) {
  if (is.null(src) || !nrow(src)) return(TRUE)
  before <- src$on[src$line < n]
  !length(before) || before[length(before)]
}

# The LINESIZE in effect for program line `n`.
log_ls_at <- function(ls, n) {
  if (is.null(ls) || !nrow(ls)) return(132L)
  v <- ls$ls[ls$line < n]
  if (length(v)) v[length(v)] else ls$ls[1]
}

# The marker lines of the ERROR events with a known column and token, by
# program line: underscores under the token and the error number below them.
# A marker is drawn only where the program line holds the token at that
# column (case-insensitive): an ERROR in code generated by a macro has no
# place in the program lines.  (The events do not record whether their code
# was generated by a macro, so a generated line whose program line holds the
# same token at that column would still get a marker.)
log_markers <- function(e, lines) {
  err <- e[e$kind == "error", , drop = FALSE]
  by_line <- list()
  for (i in seq_len(nrow(err))) {
    at <- err$at[[i]]
    tok <- err$args[[i]]$token
    if (length(at) < 2L || anyNA(at[1:2]) || !is.character(tok) || length(tok) != 1L || !nzchar(tok)) next
    ln <- vctrs::vec_cast(at[1], integer())
    col <- vctrs::vec_cast(at[2], integer())
    if (ln < 1L || ln > length(lines) || !identical(toupper(substr(lines[ln], col, col + nchar(tok) - 1L)), toupper(tok))) next
    code <- regmatches(err$text[[i]][1], regexec("^ERROR ([0-9]+)-[0-9]+:", err$text[[i]][1]))[[1]]
    len <- nchar(tok)
    k <- as.character(ln)
    by_line[[k]] <- vctrs::vec_rbind(by_line[[k]], data.frame(col = col, len = len,
                                                   code = if (length(code) == 2L) code[2] else ""))
  }
  lapply(by_line, function(m) {
    m <- m[order(m$col), , drop = FALSE]
    out <- character()
    u <- cl <- ""
    for (j in seq_len(nrow(m))) {
      pos <- 11L + m$col[j] - 1L
      # several ERRORs at the same token: one underline, the numbers stacked
      # under it (22 above 76)
      if (j > 1L && m$col[j] == m$col[j - 1L] && m$len[j] == m$len[j - 1L] && nzchar(m$code[j]) &&
          nzchar(cl[length(cl)])) {
        cl <- c(cl, paste0(strrep(" ", pos), m$code[j]))
        next
      }
      if (nchar(u) > pos || nchar(cl[length(cl)]) > pos || length(cl) > 1L) {
        out <- c(out, u, cl[nzchar(cl)])
        u <- cl <- ""
      }
      u <- paste0(u, strrep(" ", pos - nchar(u)), strrep("_", m$len[j]))
      if (nzchar(m$code[j])) cl[1L] <- paste0(cl[1L], strrep(" ", pos - nchar(cl[1L])), m$code[j])
    }
    c(out, u, cl[nzchar(cl)])
  })
}

# The counts of ERROR and WARNING lines ("2 ERRORs, 1 WARNING"): the lines of
# the ERROR and WARNING messages that start with "ERROR" or "WARNING" (a
# message of two ERROR lines counts 2; continuation lines and PUT output that
# starts with "ERROR:" are not counted).
log_summary_counts <- function(ev) {
  n <- function(kind, head) {
    sum(vapply(ev$text[ev$kind == kind], function(l) sum(startsWith(l, head)), integer(1)))
  }
  c(error = n("error", "ERROR"), warning = n("warning", "WARNING"))
}

log_summary_text <- function(counts, style = identity) {
  word <- function(k, w) paste0(counts[[k]], " ", w, if (counts[[k]] != 1L) "s")
  parts <- c(if (counts[["error"]] > 0) style("error", word("error", "ERROR")),
             if (counts[["warning"]] > 0) style("warning", word("warning", "WARNING")))
  if (!length(parts)) "No ERRORs or WARNINGs" else paste(parts, collapse = ", ")
}

log_view_plain <- function(ev, echo, compact = FALSE) {
  unlist(lapply(log_items(ev, echo, compact), `[[`, "lines")) %||% character()
}

log_ansi_style <- function(type, x) {
  switch(type,
    error = , marker = cli::col_red(x),
    warning = cli::col_green(x),
    note = cli::col_blue(x),
    x
  )
}

log_view_ansi <- function(ev, echo, compact = FALSE) {
  items <- log_items(ev, echo, compact)
  head <- cli::style_bold(log_summary_text(log_summary_counts(ev), function(k, x) log_ansi_style(k, x)))
  body <- unlist(lapply(items, function(it) as.character(log_ansi_style(it$type, it$lines))))
  c(as.character(head), body %||% character())
}

log_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

log_html_class <- c(note = "sas-note", warning = "sas-warning", error = "sas-error", put = "sas-put",
                    echo = "sas-src", src = "sas-src", marker = "sas-error sas-marker")

# One <details> block per step of each submission; the CSS classes are those
# of inst/css/sasonr.css.
log_view_html <- function(ev, echo, compact = FALSE) {
  items <- log_items(ev, echo, compact)
  progs <- attr(ev, "programs") %||% list()
  counts <- log_summary_counts(ev)
  cls <- c(error = "sas-error", warning = "sas-warning")
  out <- c('<div class="sas-log">',
           paste0('<p class="sas-log-summary">',
                  log_summary_text(counts, function(k, x) paste0('<span class="', cls[[k]], '">', x, "</span>")), "</p>"))
  key <- vapply(items, function(it) paste(it$sub, it$step), "")
  grp <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
  for (g in unique(grp)) {
    its <- items[grp == g]
    sub <- its[[1]]$sub
    step <- its[[1]]$step
    st <- progs[[as.character(sub)]]$steps
    hit <- if (!is.null(st)) which(st$step == step) else integer()
    label <- if (length(hit)) sprintf("%s (line %d)", st$label[hit[1]], st$line[hit[1]]) else "Open code"
    if (length(progs) > 1L) label <- paste0("Submission ", sub, ": ", label)
    body <- unlist(lapply(its, function(it) {
      paste0('<span class="', log_html_class[[it$type]], '">', log_html_escape(it$lines), "</span>")
    }))
    out <- c(out, paste0('<details class="sas-step" open><summary>', log_html_escape(label), "</summary>"),
             paste0('<pre class="sas-log-text">', paste(body, collapse = "\n"), "</pre>"), "</details>")
  }
  paste(c(out, "</div>"), collapse = "\n")
}

#' The log of a result or a session
#'
#' The log as plain text, as text coloured for the terminal, or as HTML.
#'
#' * `"plain"`: the SAS-style log text, one element per line: the lines of
#'   `x$log` (for a session: the log of all submissions).
#' * `"ansi"`: the same lines coloured with cli (ERROR red, WARNING green,
#'   NOTE blue, PUT output unstyled), under a summary line that counts the
#'   ERROR and WARNING lines of the messages (PUT output that starts with
#'   `ERROR:` is not counted). [print()] of a result uses this view in a
#'   terminal with colours; `options(sasonr.log_style = "plain")` turns it off.
#' * `"html"`: one string, a `<div class="sas-log">` with a `<details>` block
#'   per step and the lines in `<span>` elements of the classes `sas-note`,
#'   `sas-warning`, `sas-error`, `sas-put` and `sas-src` (source lines). The
#'   style sheet is `system.file("css", "sasonr.css", package = "sasonr")`;
#'   the HTML has no inline styles.
#'
#' Every view shows the program lines with their line numbers (`echo = FALSE`: the
#' messages and PUT output only): each line with its number left-justified in
#' 11 columns, a line longer than `LINESIZE` continued on lines that start
#' with `!`; a compile-time ERROR right after its line, with an underscore
#' marker and the error number under the line when the column is known; a
#' blank line after a numbered ERROR message (`ERROR 22-322: ...`, except in
#' PROC MEANS, SUMMARY, UNIVARIATE, TRANSPOSE, COMPARE, SORT, FREQ, RANK, APPEND
#' and CONTENTS when more of the step's lines follow); a blank line between a
#' step's lines and what the step writes when it runs (when the step ends on
#' the line of its numbered ERRORs, this blank line comes before them and each
#' of them is followed by a blank line). The data lines after `DATALINES` / `CARDS`
#' are not echoed. The program's `OPTIONS SOURCE` and `OPTIONS NOSOURCE` turn
#' the echo on and off.
#'
#' Limits of the echo: it shows the lines of the program as written, numbered
#' from 1 in each submission (a session does not continue the numbering);
#' code generated by macros (MPRINT lines) and `%INCLUDE` lines are not shown;
#' the NOTEs written while a step is compiled follow the step's lines; the
#' statements of a PROC SQL step that share a line are not split onto `!`
#' lines; a line on which `RUN;` is followed by more text is echoed whole
#' (in SAS the rest follows the step's messages); lines with multibyte
#' characters wrap by characters; the timing NOTEs of SAS are not written.
#' When the echo cannot be shown exactly, the echo of that submission is left
#' out and the log starts with a NOTE that says why; the run itself goes on:
#' a numbered ERROR on a line with a macro reference (a macro variable, call,
#' statement or comment), a compile-time ERROR on a line that `OPTIONS
#' NOSOURCE` hides, `OPTIONS SOURCE` / `NOSOURCE` with more text on its line,
#' a numbered WARNING. For now a NOTE place (`(Line):(Column)`, `at line N
#' column M`) on a line with a macro reference fails loud.
#'
#' `compact = TRUE` is a narrower layout of the echo for display: the line
#' number takes as many columns as the largest line number of the view needs,
#' plus one blank; a continuation line starts with `!` in those columns. The
#' lines break where they do in the default layout, and the underscore
#' markers and error numbers under a line move with its text. The messages
#' are unchanged (the places in a NOTE are program lines and columns).
#'
#' @param x A result of [sas_run()] or a session from [sas_session()].
#' @param session The former name of `x` (a session), still accepted; with
#'   `session`, `x` is left out (or is the format, given by position).
#' @param format `"plain"`, `"ansi"` or `"html"`.
#' @param echo Show the program lines (`TRUE`, the default) or only the
#'   messages and PUT output (`FALSE`).
#' @param compact The compact layout of the echo (`TRUE`) or the default
#'   layout with the line numbers in 11 columns (`FALSE`, the default).
#' @param ... Not used; the arguments after it are given by name.
#' @return `"plain"` and `"ansi"`: a character vector of class `sasonr_log`,
#'   one element per line; it prints as the log text, one line per element.
#'   `as.character()` gives the plain character vector; `[` keeps the class.
#'   `"html"`: a character string.
#' @seealso The events behind every view are the `log_events` field of a
#'   result (a tibble: `seq`, `submission`, `step`, `phase`, `kind`, `id`,
#'   `args`, `at`, `places`, `text`, `source_line`).
#' @examples
#' res <- sas_run("data a;\n  x = 1;\n  put x=;\nrun;")
#' sas_log(res)
#' sas_log(res, echo = FALSE)
#' sas_log(res, compact = TRUE)
#' cat(sas_log(res, "html"))
#' res$log_events[, c("step", "kind", "id")]
#' @export
sas_log <- function(x, format = c("plain", "ansi", "html"), echo = log_echo_default, ...,
                    compact = FALSE, session) {
  rlang::check_dots_empty()
  # `session =`: the name of the first argument before `x`; then `x` is
  # missing or the format given by position (`sas_log(session = s, "html")`)
  if (!missing(session)) {
    if (!missing(x)) {
      if (!missing(format) || !rlang::is_string(x)) {
        rlang::abort("Give the result or session as `x` or as `session`, not both.", class = "sasonr_error")
      }
      format <- x
    }
    x <- session
  }
  format <- rlang::arg_match(format, c("plain", "ansi", "html"))
  if (!rlang::is_bool(echo)) rlang::abort("`echo` must be `TRUE` or `FALSE`.", class = "sasonr_error")
  if (!rlang::is_bool(compact)) rlang::abort("`compact` must be `TRUE` or `FALSE`.", class = "sasonr_error")
  sas_log_view(x, format, echo, compact)
}

#' The log view of an object (the S3 generic behind sas_log())
#'
#' Methods: a result of [sas_run()] and a session (the log events of the
#' submissions), an ODS table or list of tables of a data-level PROC function
#' ([sas_means_df()]: the WARNINGs and NOTEs of the computation).
#' @param x The object.
#' @param format,echo,compact As in [sas_log()].
#' @return As [sas_log()].
#' @keywords internal
#' @export
sas_log_view <- function(x, format, echo, compact) UseMethod("sas_log_view")

#' @export
sas_log_view.default <- function(x, format, echo, compact) {
  rlang::abort("`x` must be a result of `sas_run()`, a session from `sas_session()` or a result of a data-level PROC function.",
               class = "sasonr_error")
}

log_view_events <- function(ev, format, echo, compact) {
  switch(format,
    plain = new_sasonr_log(log_view_plain(ev, echo, compact)),
    ansi = new_sasonr_log(log_view_ansi(ev, echo, compact)),
    html = log_view_html(ev, echo, compact)
  )
}

#' @export
sas_log_view.sasonr_result <- function(x, format, echo, compact) {
  ev <- x$log_events %||% rlang::abort("`x` has no log events (a result of an older version of sasonr).", class = "sasonr_error")
  log_view_events(ev, format, echo, compact)
}

#' @export
sas_log_view.SasSession <- function(x, format, echo, compact) log_view_events(log_session_events(x), format, echo, compact)

# a data-level result has no program: its log is the messages of the computation
log_view_lines <- function(x, format) {
  lines <- attr(x, "sasonr_log", exact = TRUE) %||% character()
  if (format == "html") return(paste0('<pre class="sas-log">', paste(html_escape(lines), collapse = "\n"), "</pre>"))
  new_sasonr_log(lines)
}

#' @export
sas_log_view.sasonr_ods_table <- function(x, format, echo, compact) log_view_lines(x, format)

#' @export
sas_log_view.sasonr_ods_list <- function(x, format, echo, compact) log_view_lines(x, format)

# The lines of a log view (sas_log()) with the class that prints them as text.
new_sasonr_log <- function(x) structure(x, class = "sasonr_log")

#' @export
print.sasonr_log <- function(x, ...) {
  cat(paste0(x, "\n"), sep = "")
  invisible(x)
}

#' @export
`[.sasonr_log` <- function(x, i) new_sasonr_log(NextMethod())
