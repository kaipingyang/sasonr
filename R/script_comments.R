# SAS comments in the script of a translation (sas_script()).
#
# Comments come from the lossless token table of the program (sas_tokens()):
# block comments `/* ... */` and statement comments `* ... ;`.  Macro comments
# `%* ... ;` are left out, and so are comments inside macro definitions and in
# code that a macro generates (that text is kept as SAS text in the script).
#
# Attachment (script_comment_attach(), a pure function of the tokens and the
# start positions of the steps):
# - a comment that starts on the line where a statement ends, after its
#   semicolon, is a trailing comment of that statement;
# - any other comment is a leading comment of the next statement;
# - a statement belongs to the last step that starts at or before it; the
#   comments of the first statement of a step belong to the step;
# - comments after the last statement follow the last step.
#
# Placement (script_comments()): step comments go above the step verb.  A
# comment of a later statement in a DATA step goes above the line of the
# generated code that carries the SAS position of the statement (the
# `.at = c(line, col)` arguments of L3 statements and mutate() arguments);
# when no such line exists it goes above the step and records why.  The tidy
# form has no `.at` arguments: the node keeps the line of the step code of each
# line of its tidy code (`tidy_map`), and the comment goes above that line.  The
# comments change only the text: the code parses to the same objects.

# The comments of `tok` with the statement each one belongs to.  `starts`:
# data frame with `line` and `col` of each step, in program order.  Returns a
# list of records: `text` (the comment as written), `line`, `end_line`,
# `step` (index into `starts`, NA after the last statement), `first` (the
# statement is the first statement of its step), `position` ("leading" or
# "trailing") and `stmt` (line, col, end_line, end_col of the statement).
script_comment_attach <- function(tok, starts) {
  if (!nrow(tok) || !nrow(starts)) return(list())
  is_cm <- tok$kind == "comment" & grepl("^(/\\*|\\*)", tok$text)
  sem <- which(!tok$kind %in% trivia_kinds)
  if (!any(is_cm)) return(list())
  # statements: runs of non-trivia rows ending at a semicolon
  semi <- tok$kind[sem] == "semi"
  sid <- cumsum(c(1L, utils::head(semi, -1L)))
  n_st <- max(c(0L, sid))
  st_first <- sem[match(seq_len(n_st), sid)]
  st_last <- sem[length(sid) + 1L - match(seq_len(n_st), rev(sid))]
  st <- data.frame(line = tok$line[st_first], col = tok$col[st_first],
                   end_line = tok$end_line[st_last], end_col = tok$end_col[st_last])
  # the step of each statement: the last one starting at or before it
  key <- function(l, c) l * 1e6 + c
  sk <- key(starts$line, starts$col)
  st$step <- vapply(key(st$line, st$col), function(k) max(c(1L, which(sk <= k))), 1L)
  st$first <- !duplicated(st$step)
  out <- list()
  for (i in which(is_cm)) {
    prev <- sem[sem < i]
    prev <- if (length(prev)) prev[length(prev)] else NA_integer_
    nxt <- sem[sem > i]
    trailing <- !is.na(prev) && tok$kind[prev] == "semi" && tok$end_line[prev] == tok$line[i]
    s <- if (trailing) sid[match(prev, sem)] else if (length(nxt)) sid[match(nxt[1], sem)] else NA_integer_
    out <- c(out, list(list(
      text = tok$text[i], line = tok$line[i], end_line = tok$end_line[i],
      step = if (is.na(s)) NA_integer_ else st$step[s], first = !is.na(s) && st$first[s],
      position = if (trailing) "trailing" else "leading",
      stmt = if (is.na(s)) NULL else unlist(st[s, c("line", "col", "end_line", "end_col")])
    )))
  }
  out
}

# The lines of a comment as R comment lines (without the indentation).
script_comment_lines <- function(text) {
  x <- if (startsWith(text, "/*")) sub("\\*/$", "", substring(text, 3L)) else sub(";$", "", substring(text, 2L))
  lines <- sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", strsplit(x, "\n", fixed = TRUE)[[1]]))
  if (!length(lines)) lines <- ""
  while (length(lines) > 1L && !nzchar(lines[1])) lines <- lines[-1L]
  while (length(lines) > 1L && !nzchar(lines[length(lines)])) lines <- lines[-length(lines)]
  ifelse(nzchar(lines), paste0("# ", lines), "#")
}

# The line of `code` (character, one element per line) above which a comment
# of the SAS statement at `stmt` (line, col, end_line, end_col) goes: the
# start of the innermost statement or mutate() argument that holds the first
# `.at = c(line, col)` inside the statement.  NA: none.
script_body_anchor <- function(code, stmt) {
  m <- regmatches(code, gregexpr("\\.at = c\\([0-9]+L, [0-9]+L\\)", code))
  hit <- NA_integer_
  for (k in seq_along(m)) {
    for (a in m[[k]]) {
      v <- as.integer(regmatches(a, gregexpr("[0-9]+", a))[[1]])
      p <- v[1] * 1e6 + v[2]
      if (p >= stmt[["line"]] * 1e6 + stmt[["col"]] && p <= stmt[["end_line"]] * 1e6 + stmt[["end_col"]]) {
        hit <- k
        break
      }
    }
    if (!is.na(hit)) break
  }
  if (is.na(hit)) return(NA_integer_)
  ex <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(NA_integer_)
  pd <- utils::getParseData(ex)
  pd <- pd[order(pd$line1, pd$col1), ]
  brace <- unique(pd$parent[pd$token == "'{'"])
  ex_rows <- pd[pd$token == "expr" & (pd$parent == 0L | pd$parent %in% brace), ]
  cand <- data.frame(start = ex_rows$line1, end = ex_rows$line2)
  # named arguments of mutate()
  for (r in which(pd$token == "SYMBOL_SUB")) {
    par <- pd$parent[r]
    sib <- pd[pd$parent == par, ]
    fn_expr <- sib$id[sib$token == "expr"][1]
    fn_names <- pd$text[pd$parent %in% fn_expr & pd$token == "SYMBOL_FUNCTION_CALL"]
    if (!any(fn_names %in% c("mutate", "transmute"))) next
    after <- sib[sib$line1 > pd$line1[r] | (sib$line1 == pd$line1[r] & sib$col1 > pd$col1[r]), ]
    val <- after[after$token == "expr", ][1, ]
    if (!is.na(val$line2)) cand <- rbind(cand, data.frame(start = pd$line1[r], end = val$line2))
  }
  cand <- cand[cand$start <= hit & cand$end >= hit, ]
  if (!nrow(cand)) return(NA_integer_)
  max(cand$start)
}

# Insert comment lines into `lines` above the lines `at` (one entry per
# comment); NULL when the result does not parse to the same objects.
script_insert_comments <- function(lines, at, cms) {
  before <- tryCatch(parse(text = lines, keep.source = FALSE), error = function(e) NULL)
  if (is.null(before)) return(NULL)
  out <- character()
  for (k in seq_along(lines)) {
    for (j in which(at == k)) {
      ind <- sub("[^ ].*$", "", lines[k])
      out <- c(out, paste0(ind, script_comment_lines(cms[[j]]$text)))
    }
    out <- c(out, lines[k])
  }
  after <- tryCatch(parse(text = out, keep.source = FALSE), error = function(e) NULL)
  if (is.null(after) || !identical(as.list(before), as.list(after))) return(NULL)
  out
}

# Fill the `comments` field of the script nodes `ir` (with `pos`: line and col
# of each node) from the program text `src`, and put the comments into the
# `text` (and `tidy`) of the nodes.
script_comments <- function(ir, pos, src) {
  if (!length(src) || !length(ir)) return(ir)
  tok <- tryCatch(sas_tokens(src), error = function(e) NULL)
  if (is.null(tok) || !is.null(attr(tok, "lex_error"))) return(ir)
  starts <- data.frame(line = vapply(pos, `[[`, 1L, "line"), col = vapply(pos, `[[`, 1L, "col"))
  recs <- script_comment_attach(tok, starts)
  for (r in recs) {
    k <- if (is.na(r$step)) length(ir) else r$step
    nd <- ir[[k]]
    lines <- range(nd$sas_lines)
    # macro and deferred text keeps its comments as SAS text
    if (identical(nd$verb, "deferred") && r$line >= lines[1] && r$line <= lines[2]) next
    # comments in code a macro generated
    if (length(nd$macro_stack) && !r$first) next
    place <- if (is.na(r$step)) "after" else if (r$first) "step" else "body"
    why <- NULL
    if (identical(place, "body") && !(identical(nd$verb, "data") && !length(nd$macro_stack))) {
      place <- "step"
      why <- "the statement runs inside the step"
    }
    ir[[k]]$comments <- c(nd$comments, list(list(
      text = r$text, lines = script_comment_lines(r$text), line = r$line, end_line = r$end_line,
      position = r$position, stmt_line = if (is.null(r$stmt)) NA_integer_ else as.integer(r$stmt[["line"]]),
      stmt = r$stmt, place = place, why = why
    )))
  }
  for (k in seq_along(ir)) if (length(ir[[k]]$comments)) ir[[k]] <- script_comments_render(ir[[k]])
  ir
}

# Put the comments of a node into its text: body comments at their code
# line (a DATA step), the others above (or, after the last statement, below).
script_comments_render <- function(nd) {
  cms <- nd$comments
  body <- which(vapply(cms, function(c) identical(c$place, "body"), TRUE))
  if (length(body)) {
    tl <- strsplit(nd$text, "\n", fixed = TRUE)[[1]]
    open <- 1L
    close <- grep("^\\}, sas = ", tl)[1]
    code <- if (!is.na(close)) substring(tl[(open + 1L):(close - 1L)], 3L) else character()
    at <- vapply(cms[body], function(c) script_body_anchor(code, c$stmt), 1L)
    for (j in which(is.na(at))) {
      cms[[body[j]]]$place <- "step"
      cms[[body[j]]]$why <- "no line of the generated code carries the SAS position of the statement"
    }
    ok <- !is.na(at)
    new_text <- if (any(ok)) script_insert_comments(tl, ifelse(ok, at + 1L, NA_integer_), cms[body]) else NULL
    tidy <- nd$tidy
    new_tidy <- if (!is.null(new_text) && !is.null(tidy)) {
      # the lines of the tidy code map to lines of the step code (tidy_map)
      tdl <- strsplit(tidy, "\n", fixed = TRUE)[[1]]
      map <- nd$tidy_map %||% seq_along(tdl)
      at_t <- match(at, map)
      if (length(map) == length(tdl) && !any(ok & is.na(at_t))) {
        script_insert_comments(tdl, ifelse(ok, at_t, NA_integer_), cms[body])
      }
    }
    if (any(ok) && (is.null(new_text) || (!is.null(tidy) && is.null(new_tidy)))) {
      for (j in which(ok)) {
        cms[[body[j]]]$place <- "step"
        cms[[body[j]]]$why <- "the comment does not fit between the lines of the generated code"
      }
    } else if (any(ok)) {
      nd$text <- paste(new_text, collapse = "\n")
      if (!is.null(tidy)) nd$tidy <- paste(new_tidy, collapse = "\n")
    }
  }
  above <- unlist(lapply(cms, function(c) if (identical(c$place, "step")) c$lines))
  below <- unlist(lapply(cms, function(c) if (identical(c$place, "after")) c$lines))
  nd$text <- paste(c(above, nd$text, below), collapse = "\n")
  if (!is.null(nd$tidy) && length(below)) nd$tidy <- paste(c(nd$tidy, below), collapse = "\n")
  nd$comments <- cms
  nd
}
