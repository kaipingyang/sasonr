# The translation as an R script: sas_script() and the step verbs.
#
# The engine records one node per SAS step while it translates (sas_translate())
# or runs (sas_run(), the trace of that run): the SAS text of the step (a slice
# of the expanded program text, from the source span sas_parse() gives each
# step), its SAS lines and, for DATA and PROC steps, the translation.
# script_ir() turns the nodes into a `sasonr_script`: a list of step nodes, each
# with the verb call that runs the step (`lang`), its text (`text`), the SAS
# text and lines, the macro call chain (`macro_stack`, empty for now), the
# %INCLUDE origin (`include`, NULL), `kind` ("static", "deferred" or "traced")
# and `comments` (the SAS comments of the step, script_comments.R).  format() renders the nodes as a script that
# runs after library(sasonr).
#
# The verbs (sas_data_step(), sas_proc_sort() ...) take a session first, run
# one step in it as sas_run() does (the engine of run.R, one submission per
# verb) and return the session invisibly.  sas_data_step() runs the code of
# its function instead of the translation of its SAS text; the SAS text gives
# the step's variable attributes, its log and the row-by-row re-run.

# ---- recording (engine side) -------------------------------------------------

# Record step `step` of the chunk `text` (starting at program line `line`);
# returns the index of the node.
script_record <- function(eng, step, text, line, tr = NULL) {
  stack <- lapply(eng$session$macro$frames, function(fr) fr$name)
  node <- list(type = step$type, name = step$name, kind = step$kind, line = step$line,
               col = if (is.null(step$span)) 1L else as.integer(step$span[["col"]]),
               sas = script_slice(text, line, step$span), tr = tr, macro_stack = stack)
  if (eng$execute && !length(stack) && !is.null(node$sas)) node <- script_inner_macro(eng, node)
  eng$script <- c(eng$script, list(node))
  if (step$type %in% c("sql", "proc")) eng$sql_seen <- TRUE
  if (identical(step$type, "sql_begin")) eng$sql_begin_line <- step$line
  length(eng$script)
}

# A trace: a step with macro statements inside it (%LET, %PUT in a PROC
# DATASETS step ...) is kept as its program lines, run through the macro
# processor, so that what the macro statements write and set is kept.
script_inner_macro <- function(eng, node) {
  last <- node$line + mp_count_nl(node$sas)
  src <- eng$program$lines
  if (last > length(src) || !any(seq.int(node$line, last) %in% eng$session$macro$macro_lines)) return(node)
  orig <- paste(src[node$line:last], collapse = "\n")
  tok <- tryCatch(sas_lex(orig), error = function(e) NULL)
  if (is.null(tok) || !any(tok$type == "macro")) return(node)
  # the step must start and end on its lines alone
  if (!identical(tok$line[1], 1L) || !identical(tok$type[nrow(tok)], "semi")) return(node)
  eng$script_upto <- max(eng$script_upto %||% 0L, last)
  list(type = "deferred", line = node$line, sas = sub("[[:space:]]+$", "", orig), macro_stack = list(),
       why = "macro statements inside the step", traced = TRUE)
}

# The program lines before a chunk (at `line`) that no chunk covered: open
# code with only macro statements (%LET, %PUT, macro definitions and calls
# that generate no step).  They become a node that runs them through the
# macro processor, in program order.  A chunk that a macro generated covers
# the line of the invocation only.
script_open_code <- function(eng, text, line, end = FALSE) {
  from <- (eng$script_upto %||% 0L) + 1L
  src <- eng$program$lines
  # a chunk can start with comments and with the lines of macro statements
  # that the macro processor consumed: it begins at its first token
  first <- line
  if (!end) {
    ct <- tryCatch(sas_lex(text), error = function(e) NULL)
    # a chunk of comments only covers no line
    if (!is.null(ct) && !nrow(ct)) return(invisible())
    if (!is.null(ct)) first <- line + ct$line[1] - 1L
  }
  if (first - 1L >= from && from <= length(src)) {
    lines <- src[from:min(first - 1L, length(src))]
    tok <- tryCatch(sas_lex(paste(lines, collapse = "\n")), error = function(e) NULL)
    if (is.null(tok) || nrow(tok) > 0L) {
      skip <- if (is.null(tok)) 0L else tok$line[1] - 1L
      eng$script <- c(eng$script, list(list(type = "macro", line = from + skip, macro_stack = list(),
                                            sas = sub("[[:space:]]+$", "", paste(lines[(skip + 1L):length(lines)], collapse = "\n")))))
    }
  }
  if (end) return(invisible())
  inside <- length(eng$session$macro$frames) > 0
  # the line of the macro invocation whose steps are being generated
  eng$script_inv <- if (inside) eng$script_inv %||% line else NULL
  last <- if (inside) line else line + mp_count_nl(text)
  eng$script_upto <- max(eng$script_upto %||% 0L, last)
  invisible()
}

# The text of `lines` from (l1, c1) to (l2, c2), the end exclusive.
script_cut <- function(lines, l1, c1, l2, c2) {
  if (l1 == l2) return(substr(lines[l1], c1, c2 - 1L))
  mid <- if (l2 - l1 > 1L) lines[(l1 + 1L):(l2 - 1L)] else character()
  paste(c(substr(lines[l1], c1, nchar(lines[l1])), mid, substr(lines[l2], 1L, c2 - 1L)), collapse = "\n")
}

# The text of a step from its source span (sas_parse()), without trailing
# blanks and line breaks; NULL for a statement covered by the span of an
# earlier one (the statements of a PROC SQL block in one chunk).
script_slice <- function(text, line, span) {
  if (is.null(span)) return(NULL)
  lines <- strsplit(paste0(strrep("\n", max(0L, line - 1L)), text), "\n", fixed = TRUE)[[1]]
  l2 <- span[["end_line"]]
  c2 <- span[["end_col"]]
  if (is.na(l2)) {
    l2 <- length(lines)
    c2 <- nchar(lines[l2]) + 1L
  }
  sub("[[:space:]]+$", "", script_cut(lines, span[["line"]], span[["col"]], l2, c2))
}

# Whether the program can make later code depend on run-time values: CALL
# SYMPUT(X/N) / CALL EXECUTE / SYMGET / RESOLVE, PROC SQL INTO:, %SYSFUNC /
# %SYSCALL, or the automatic macro variables the steps set (&SYS..., &SQL...).
script_dynamic <- function(code) {
  tok <- tryCatch(sas_lex(code), error = function(e) NULL)
  if (is.null(tok)) return(TRUE)
  v <- tolower(tok$value)
  any(tok$type == "name" & v %in% c("symput", "symputx", "symputn", "execute", "symget", "symgetn", "resolve", "into")) ||
    any(tok$type == "macro" & v %in% c("%sysfunc", "%qsysfunc", "%syscall")) ||
    any(tok$type == "macro_var" & grepl("^&+(sys|sql)", v))
}

# sas_translate(): in a program that can depend on run-time values
# (script_dynamic()), the first chunk the macro processor changed and
# everything after it are kept as SAS text (one deferred node); when macro
# definitions or global macro variables exist by then, the whole program is
# (the script has no macro state).  TRUE: the chunk is not translated.
script_defer <- function(eng, text, line, why = NULL) {
  if (isTRUE(eng$deferred)) return(TRUE)
  ses <- eng$session$macro
  if (is.null(why)) {
    if (!isTRUE(eng$dynamic)) return(FALSE)
    span <- seq.int(line, line + mp_count_nl(text))
    if (!any(span %in% ses$macro_lines)) return(FALSE)
  }
  eng$deferred <- TRUE
  # inside an open PROC SQL block: from its PROC SQL statement
  from <- if (isTRUE(eng$sql_open) && !is.null(eng$sql_begin_line)) min(line, eng$sql_begin_line) else line
  src <- eng$program$lines
  # open code before the chunk runs first; the steps that the same macro
  # invocation generated before are in the deferred text again
  script_open_code(eng, text, line, end = TRUE)
  eng$script <- Filter(function(n) n$line < from, eng$script)
  why <- why %||% "the macro code from here on depends on values known only at run time"
  eng$script <- c(eng$script, list(list(type = "deferred", line = from, why = why,
                                        sas = sub("[[:space:]]+$", "", paste(src[from:length(src)], collapse = "\n")))))
  TRUE
}

script_defer_rest <- function(eng, e) {
  if (isTRUE(eng$deferred)) return(invisible())
  if (!isTRUE(eng$dynamic)) rlang::cnd_signal(e)
  line <- if (!is.null(eng$script_inv)) eng$script_inv else (eng$script_upto %||% 0L) + 1L
  if (line > length(eng$program$lines)) rlang::cnd_signal(e)
  script_defer(eng, "", line, why = "the macro code from here on needs values known only at run time")
  invisible()
}

# sas_translate() after a PROC SQL or PROC step: a step that cannot be
# translated before that step ran (the layout of its output is known only at
# run time) is kept as SAS text with the rest of the program.
script_exec_after_sql <- function(eng, text, line) {
  n_script <- length(eng$script)
  n_steps <- length(eng$steps)
  ok <- tryCatch({
    engine_exec_steps(eng, text, line)
    TRUE
  }, sasonr_unsupported = function(e) rlang::cnd_signal(e), sasonr_error = function(e) FALSE)
  if (!ok) {
    eng$script <- eng$script[seq_len(n_script)]
    eng$steps <- eng$steps[seq_len(n_steps)]
    script_defer(eng, text, line, why = "the data sets made by an earlier PROC step are known only at run time")
  }
  invisible()
}

# The recorded nodes as kept in a translation; sas_script() builds the IR
# from them (a run never fails because of its script).
script_raw <- function(nodes, mode, inputs, src = character()) {
  structure(list(nodes = nodes, mode = mode, inputs = inputs, src = src), class = "sasonr_script_raw")
}

# ---- the IR ------------------------------------------------------------------

script_ir <- function(nodes, mode, inputs, src = character()) {
  out <- list()
  pos <- list()
  k <- 1L
  while (k <= length(nodes)) {
    nd <- nodes[[k]]
    if (identical(nd$type, "sql_begin") || identical(nd$type, "sql")) {
      # a PROC SQL block: its statements up to QUIT (or the next step)
      parts <- character()
      first <- nd$line
      repeat {
        parts <- c(parts, nodes[[k]]$sas)
        last <- nodes[[k]]$line
        end <- identical(nodes[[k]]$type, "sql_end")
        k <- k + 1L
        if (end || k > length(nodes) || !nodes[[k]]$type %in% c("sql", "sql_end")) break
      }
      out <- c(out, list(script_node_sql(paste(parts, collapse = "\n"), first, last, mode)))
      pos <- c(pos, list(list(line = as.integer(first), col = nd$col %||% 1L)))
      next
    }
    node <- script_node(nd, mode)
    node$macro_stack <- nd$macro_stack %||% list()
    out <- c(out, list(node))
    # a step a macro generated starts at the invocation
    pos <- c(pos, list(list(line = as.integer(nd$line), col = if (length(node$macro_stack)) 1L else nd$col %||% 1L)))
    k <- k + 1L
  }
  out <- script_comments(out, pos, src)
  structure(out, class = "sasonr_script", mode = mode, inputs = inputs, tidy = FALSE)
}

new_script_node <- function(verb, lang, text, sas, lines, kind, outputs = character(), why = NULL, tidy = NULL,
                            macro_stack = list()) {
  structure(list(verb = verb, lang = lang, text = text, sas = sas, sas_lines = as.integer(lines),
                 macro_stack = macro_stack, include = NULL, kind = kind, comments = list(),
                 outputs = outputs, why = why, tidy = tidy),
            class = "sasonr_script_step")
}

script_lines_of <- function(line, sas) {
  seq.int(line, length.out = length(strsplit(sas, "\n", fixed = TRUE)[[1]]) %|% 1L)
}

`%|%` <- function(a, b) if (length(a) == 0L || a == 0L) b else a

script_node <- function(nd, mode) {
  kind <- if (identical(mode, "trace")) "traced" else "static"
  line <- as.integer(nd$line)
  stext <- nd$sas %||% ""
  lines <- script_lines_of(line, stext)
  if (identical(nd$type, "macro")) {
    lang <- rlang::call2("sas_deferred", quote(s), stext, line = line)
    text <- paste0("s |> sas_deferred(", r_raw(stext), ", line = ", line, "L)")
    return(new_script_node("deferred", lang, text, stext, lines, "deferred", why = "macro statements in open code"))
  }
  if (identical(nd$type, "deferred")) {
    lang <- rlang::call2("sas_deferred", quote(s), stext, line = line)
    text <- paste0("# ", nd$why, ": run by the sasonr engine\n",
                   "s |> sas_deferred(", r_raw(stext), ", line = ", line, "L)")
    return(new_script_node("deferred", lang, text, stext, lines, if (isTRUE(nd$traced)) kind else "deferred", why = nd$why))
  }
  if (identical(nd$type, "global")) {
    st <- script_stmts(stext)
    key <- if (length(st)) st[[1]]$key else ""
    if (identical(key, "options")) {
      lang <- rlang::call2("sas_options", quote(s), st[[1]]$rest, line = line)
      text <- paste0("s |> sas_options(", r_str(st[[1]]$rest), ", line = ", line, "L)")
    } else if (length(st) == 1L && grepl("^title([1-9]|10)?$", key)) {
      n <- suppressWarnings(as.integer(sub("^title", "", key)))
      if (is.na(n)) n <- 1L
      lang <- rlang::call2("sas_title", quote(s), st[[1]]$rest, n = n, line = line)
      text <- paste0("s |> sas_title(", r_str(st[[1]]$rest), if (n != 1L) paste0(", n = ", n, "L"), ", line = ", line, "L)")
    } else {
      lang <- rlang::call2("sas_global", quote(s), stext, line = line)
      text <- paste0("s |> sas_global(", r_raw(stext), ", line = ", line, "L)")
    }
    return(new_script_node("global", lang, text, stext, lines, kind, why = "a global statement"))
  }
  tr <- nd$tr
  outs <- tr$outputs %||% character()
  if (identical(nd$type, "data")) {
    level <- if (isTRUE(nd$rerun)) "L3" else NULL
    code <- paste(tr$code, collapse = "\n")
    fn <- rlang::call2("function", as.pairlist(alist(work = )), rlang::call2("{", !!!as.list(cg_exec(tr$exprs))))
    lang <- rlang::call2("sas_data_step", quote(s), outs, fn, sas = stext, line = line, level = level)
    text <- paste0("s |> sas_data_step(", r_chr(outs), ", \\(work) {\n",
                   paste0("  ", strsplit(code, "\n", fixed = TRUE)[[1]], collapse = "\n"), "\n}, sas = ", r_raw(stext),
                   ", line = ", line, "L", if (!is.null(level)) ", level = \"L3\"", ")")
    why <- script_why_data(tr, level)
    td <- if (is.null(why)) script_tidy_code(code) else NULL
    if (is.null(why) && is.null(td)) why <- "the step reads or writes WORK in a way the tidy form does not cover"
    node <- new_script_node("data", lang, text, stext, lines, kind, outs, why, td$code)
    node$tidy_map <- td$map
    return(node)
  }
  name <- nd$name %||% ""
  if (identical(nd$type, "proc") && name %in% c("sort", "means", "freq", "transpose", "print", "tabulate")) {
    st <- script_stmts(stext)
    head <- st[[1]]
    opts <- sub("^[[:alnum:]_]+[[:space:]]*", "", head$rest)
    body <- Filter(function(x) !x$key %in% c("run", "quit"), st[-1])
    verb <- paste0("sas_proc_", name)
    args <- stats::setNames(lapply(body, `[[`, "rest"), vapply(body, `[[`, "", "key"))
    lang <- rlang::call2(verb, quote(s), opts, !!!args, line = line)
    text <- paste0("s |> ", verb, "(", paste(c(r_str(opts), vapply(seq_along(args), function(i) {
      paste0(names(args)[i], " = ", r_str(args[[i]]))
    }, ""), paste0("line = ", line, "L")), collapse = ", "), ")")
    # (the sort order, which later steps check, is kept in the session)
    return(new_script_node(verb, lang, text, stext, lines, kind, outs, paste0("PROC ", toupper(name), " runs in the session")))
  }
  lang <- rlang::call2("sas_proc", quote(s), stext, line = line)
  text <- paste0("s |> sas_proc(", r_raw(stext), ", line = ", line, "L)")
  new_script_node("proc", lang, text, stext, lines, kind, outs, paste0("PROC ", toupper(name), " runs in the session"))
}

script_node_sql <- function(stext, first, last, mode) {
  st <- script_stmts(stext)
  opts <- if (length(st) && identical(st[[1]]$key, "proc")) sub("^sql[[:space:]]*", "", st[[1]]$rest) else ""
  body <- st[if (length(st) && identical(st[[1]]$key, "proc")) -1L else seq_along(st)]
  body <- Filter(function(x) !identical(x$key, "quit"), body)
  sql <- paste(vapply(body, function(x) paste0(x$text, ";"), ""), collapse = "\n")
  args <- list(quote(s), sql)
  if (nzchar(opts)) args$options <- opts
  args$line <- as.integer(first)
  lang <- rlang::call2("sas_proc_sql", !!!args)
  text <- paste0("s |> sas_proc_sql(", r_raw(sql), if (nzchar(opts)) paste0(", options = ", r_str(opts)),
                 ", line = ", first, "L)")
  new_script_node("sas_proc_sql", lang, text, stext, seq.int(first, max(first, last)),
                  if (identical(mode, "trace")) "traced" else "static", why = "PROC SQL runs in the session")
}

# Why a DATA step needs the session in the tidy form (NULL: it does not).
script_why_data <- function(tr, level) {
  nm <- all.names(as.call(c(quote(`{`), as.list(cg_exec(tr$exprs)))))
  # MERGE has only a row-by-row form
  if ("sas_merge_read" %in% nm) return("a MERGE step (MERGE runs row by row, L3)")
  if (!is.null(level) || !tr$level %in% c("L1", "L2")) return("a row-by-row (L3) step")
  if (any(c(".log", ".lst") %in% nm)) return("the step writes to the log or the listing (PUT, INFILE ...)")
  if (any(c(".symtab", ".titles", ".meta") %in% nm)) return("the step uses macro variables, titles or run-time attributes")
  NULL
}

# The tidy form of the code of a step: WORK reads and writes become R
# variables (`sas_ds(work, "a", keep = ...)` -> `a |> sas_ds(keep = ...)`),
# what only serves the log goes (sas_note_read() calls, the `.at = c(line,
# col)` positions of the log NOTEs), dplyr verbs lose their prefix (so that
# tidylog can report them) and the step pipeline `.d <- src |> ...` with its
# output `out <- .d |> select(...)` becomes one pipeline.  Returns `code` and
# `map` (for each line of the tidy code, its line in `code`), NULL when the
# result still refers to WORK or does not parse.
script_tidy_code <- function(code) {
  str <- "\"(?:[^\"\\\\]|\\\\.)*\""
  x <- gsub(paste0(" [|]> sas_note_read\\(", str, "(?:, where = ", str, ")?\\)"), "", code, perl = TRUE)
  x <- gsub(", \\.at = c\\(-?[0-9]+L, (?:-?[0-9]+L|NA(?:_integer_)?)\\)", "", x, perl = TRUE)
  nm <- "([A-Za-z_][A-Za-z0-9_]*)"
  # replace each match of `pattern` by f(<its first group>)
  sub_fn <- function(pattern, x, f) {
    m <- gregexpr(pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(h) {
      vapply(h, function(one) f(regmatches(one, regexec(pattern, one, perl = TRUE))[[1]][2]), "", USE.NAMES = FALSE)
    })
    x
  }
  x <- sub_fn(paste0("sas_ds\\(work, \"", nm, "\"\\)"), x, r_name)
  x <- sub_fn(paste0("sas_ds\\(work, \"", nm, "\", "), x, function(n) paste0(r_name(n), " |> sas_ds("))
  x <- sub_fn(paste0("(?m)^work\\$`?", nm, "`? <- "), x, function(n) paste0(r_name(n), " <- "))
  verbs <- c("mutate", "select", "filter", "arrange", "rename", "summarise", "summarize", "group_by", "ungroup",
             "distinct", "transmute", "relocate", "left_join", "right_join", "inner_join", "full_join",
             "anti_join", "semi_join", "count", "slice")
  x <- gsub(paste0("dplyr::(", paste(verbs, collapse = "|"), ")\\("), "\\1(", x)
  ex <- tryCatch(parse(text = x, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex) || any(c("work", ".log", ".lst", ".meta", ".symtab", ".titles") %in% all.names(ex))) return(NULL)
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  script_tidy_merge(lines, ex)
}

# One pipeline for `.d <- src |> stages` followed by `out <- .d |> select()`
# when `.d` is used nowhere else (no PUT reads it).
script_tidy_merge <- function(lines, ex) {
  keep <- list(code = paste(lines, collapse = "\n"), map = seq_along(lines))
  nm <- all.names(as.call(c(quote(`{`), as.list(ex))))
  if (sum(nm == ".d") != 2L) return(keep)
  pd <- utils::getParseData(parse(text = lines, keep.source = TRUE))
  top <- pd[pd$parent == 0L & pd$token == "expr", ]
  top <- top[order(top$line1), ]
  a <- which(startsWith(lines[top$line1], ".d <- "))
  if (length(a) != 1L || a == nrow(top)) return(keep)
  b1 <- top$line1[a + 1L]
  m <- regmatches(lines[b1], regexec("^([^ ]+) <- \\.d( [|]>)?$", lines[b1]))[[1]]
  if (!length(m) || b1 != top$line2[a] + 1L) return(keep)
  a1 <- top$line1[a]
  a2 <- top$line2[a]
  lines[a1] <- paste0(m[2], " <- ", substring(lines[a1], 7L))
  if (nzchar(m[3])) lines[a2] <- paste0(lines[a2], " |>")
  out <- lines[-b1]
  if (is.null(tryCatch(parse(text = out, keep.source = FALSE), error = function(e) NULL))) return(keep)
  list(code = paste(out, collapse = "\n"), map = seq_along(lines)[-b1])
}

# The statements of SAS text (split at the semicolon tokens): `key` (the
# first word, lower case), `rest` (the text after it) and `text`.
script_stmts <- function(stext) {
  tok <- sas_lex(stext)
  lines <- strsplit(stext, "\n", fixed = TRUE)[[1]]
  if (!length(lines)) lines <- ""
  out <- list()
  start <- 1L
  for (i in seq_len(nrow(tok))) {
    if (!identical(tok$type[i], "semi")) next
    if (i > start) {
      text <- script_cut(lines, tok$line[start], tok$col[start], tok$line[i], tok$col[i])
      key <- tolower(tok$value[start])
      rest <- if (i > start + 1L) script_cut(lines, tok$line[start + 1L], tok$col[start + 1L], tok$line[i], tok$col[i]) else ""
      out <- c(out, list(list(key = key, rest = sub("[[:space:]]+$", "", rest), text = sub("[[:space:]]+$", "", text))))
    }
    start <- i + 1L
  }
  out
}

# R literals: a string, a character vector, a raw string for SAS text.
r_str <- function(x) encodeString(x, quote = "\"")
r_chr <- function(x) {
  if (length(x) == 0L) return("character()")
  if (length(x) == 1L) r_str(x) else paste0("c(", paste(vapply(x, r_str, ""), collapse = ", "), ")")
}
r_raw <- function(x) {
  d <- ""
  while (grepl(paste0(")", d, "\""), x, fixed = TRUE)) d <- paste0(d, "-")
  paste0("r\"", d, "(", x, ")", d, "\"")
}

# ---- rendering ---------------------------------------------------------------

#' The translation of a SAS program as an R script
#'
#' `sas_script()` turns the result of [sas_translate()] (the translation of
#' the program, before it runs) or of [sas_run()] (the trace of that run: the
#' code that ran) into a script: one call of a step verb per SAS step
#' ([sas_data_step()], [sas_proc_sort()], [sas_proc_means()],
#' [sas_proc_sql()], [sas_proc()], [sas_options()], [sas_title()],
#' [sas_global()], [sas_deferred()]), each running its step in a session
#' created by [sas_session()]. Printing the object writes the script;
#' `as.character()` gives its lines.
#'
#' Each DATA step is written as the R code sasonr generated for it, unchanged,
#' inside `sas_data_step()`. A PROC SORT or PROC MEANS step shows only the
#' options and statements the program gave; other PROC steps and PROC SQL keep
#' their SAS text. In a translation, macro code whose result depends on values
#' known only at run time (CALL SYMPUT, PROC SQL INTO:, `%SYSFUNC`, automatic
#' macro variables) is kept as SAS text in `sas_deferred()`, which runs it
#' through the macro processor; a trace has no such steps, but it is valid for
#' the data of that run only.
#'
#' SAS comments (`/* ... */` and `* ... ;`) are kept as R comment lines: a
#' comment on the line where a statement ends belongs to that statement, any
#' other comment to the next statement. Comments of the first statement of a
#' step go above the step verb; comments of a later statement of a DATA step
#' go above the line of the generated code for that statement, or above the
#' step when there is no such line. Macro comments (`%* ... ;`) and comments
#' in macro definitions or in code a macro generates are not copied.
#'
#' With `tidy = TRUE` the script has no session where it does not need one:
#' DATA steps that run vectorised (levels L1 and L2) read and
#' write R variables (`adsl <- dm |> mutate(...)`), and the script starts with
#' `library(dplyr)` and `library(tidylog)` (tidylog then reports what each verb
#' did; see [sasonr_tidy_log()]). The other steps (row loops, PUT or INFILE,
#' macro variables, user formats, PROC steps, PROC SQL, global statements) run in
#' a session block with a comment that says why; the data sets go into the
#' session with [sas_session_add()] and come back with
#' `sas_work(s, trim = FALSE)`. The values are the same, but data sets made by
#' the tidy steps carry no labels or formats into later session steps.
#'
#' @param x A `sasonr_translation` ([sas_translate()]) or a `sasonr_result`
#'   ([sas_run()]).
#' @param tidy `FALSE` (one session for the whole script) or `TRUE` (see
#'   Details).
#' @return A `sasonr_script`: a list of step nodes, each with `verb`, `lang`
#'   (the call that runs the step), `text` (its R text), `sas` (the SAS text),
#'   `sas_lines` (its SAS lines), `macro_stack` (empty list), `include` (NULL),
#'   `kind` (`"static"`, `"deferred"` or `"traced"`) and `comments` (the SAS
#'   comments of the step: `lines` (the R comment lines), `line`, `position`
#'   (`"leading"` or `"trailing"`) and `place` (`"step"`: above the step verb,
#'   `"body"`: above the line of the generated code of the statement,
#'   `"after"`: below the last step)).
#' @examples
#' tr <- sas_translate("
#' data b;
#'   set a;
#'   y = x * 2;
#' run;
#' proc sort data=b; by descending y; run;
#' ", work = list(a = data.frame(x = c(1, 3, 2))))
#' sas_script(tr)
#' sas_script(tr, tidy = TRUE)
#' @export
sas_script <- function(x, tidy = FALSE) {
  tr <- if (inherits(x, "sasonr_result")) x$translation else x
  if (!inherits(tr, "sasonr_translation") || is.null(tr$script)) {
    rlang::abort("`x` must be the result of `sas_translate()` or `sas_run()`.", class = "sasonr_error")
  }
  raw <- tr$script
  ir <- script_ir(raw$nodes, raw$mode, raw$inputs, raw$src %||% character())
  attr(ir, "tidy") <- isTRUE(tidy)
  ir
}

#' @export
format.sasonr_script <- function(x, ...) {
  mode <- attr(x, "mode")
  inputs <- attr(x, "inputs") %||% character()
  head <- if (identical(mode, "trace")) {
    c("# Trace of this run of the SAS program, written by sasonr: the code that ran.",
      "# It is valid for the data of this run (macro code was resolved at run time).")
  } else {
    "# Translation of the SAS program, written by sasonr."
  }
  wl <- paste0("list(", paste0(inputs, " = ", inputs, collapse = ", "), ")")
  if (!isTRUE(attr(x, "tidy"))) {
    body <- vapply(x, `[[`, "", "text")
    out <- c(head, "library(sasonr)", "",
             paste0("s <- sas_session(", if (length(inputs)) paste0("work = ", wl), ")"), "", paste0(body, "\n"))
    return(unlist(strsplit(paste(out, collapse = "\n"), "\n", fixed = TRUE)))
  }
  out <- c(head, "# Tidy form: steps that need a sasonr session run in a session block.",
           "library(sasonr)", "library(dplyr)", "library(tidylog)", "")
  for (nm in inputs) {
    rn <- r_name(nm)
    out <- c(out, paste0(rn, " <- sas_work(sas_session(work = list(", rn, " = ", rn, ")), trim = FALSE)$", rn,
                         "  # SAS character values"))
  }
  fresh <- inputs
  has_s <- FALSE
  k <- 1L
  fmt <- FALSE
  while (k <= length(x)) {
    nd <- x[[k]]
    # user formats live in the session: later DATA steps stay there
    if (identical(nd$verb, "proc") && grepl("^proc[[:space:]]+format\\b", tolower(nd$sas))) fmt <- TRUE
    if (fmt) x[[k]]$tidy <- NULL
    if (fmt && is.null(nd$why)) x[[k]]$why <- "the step can use user formats of the session"
    nd <- x[[k]]
    if (!is.null(nd$tidy)) {
      above <- unlist(lapply(nd$comments, function(c) if (identical(c$place, "step")) c$lines))
      out <- c(out, above, paste0("# ", script_tidy_title(nd)), nd$tidy, "")
      fresh <- union(fresh, nd$outputs)
        k <- k + 1L
      next
    }
    block <- list()
    while (k <= length(x) && (is.null(x[[k]]$tidy) || fmt)) {
      if (identical(x[[k]]$verb, "proc") && grepl("^proc[[:space:]]+format\\b", tolower(x[[k]]$sas))) fmt <- TRUE
      if (fmt && !is.null(x[[k]]$tidy)) x[[k]]$why <- "the step can use user formats of the session"
      block <- c(block, list(x[[k]]))
      k <- k + 1L
    }
    why <- unique(vapply(block, function(b) b$why %||% "it runs in the session", ""))
    out <- c(out, paste0("# In a sasonr session: ", paste(why, collapse = "; ")))
    push <- if (length(fresh)) paste0("list(", paste0(r_name(fresh), " = ", r_name(fresh), collapse = ", "), ")") else NULL
    # the session is `.s` here: a data set can be called `s`
    if (!has_s) {
      out <- c(out, paste0(".s <- sas_session(", if (!is.null(push)) paste0("work = ", push), ")"))
      has_s <- TRUE
    } else if (!is.null(push)) {
      out <- c(out, paste0("sas_session_add(.s, ", push, ")"))
    }
    out <- c(out, gsub("(^|\n)s [|]> ", "\\1.s |> ", vapply(block, `[[`, "", "text")),
             "list2env(sas_work(.s, trim = FALSE), environment())", "")
    fresh <- character()
  }
  unlist(strsplit(paste(out, collapse = "\n"), "\n", fixed = TRUE))
}

script_tidy_title <- function(nd) {
  first <- strsplit(nd$sas, "\n", fixed = TRUE)[[1]][1]
  paste0(substr(first, 1L, 70L), "  (SAS line ", nd$sas_lines[1], ")")
}

#' @export
print.sasonr_script <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
as.character.sasonr_script <- function(x, ...) format(x)

#' @export
`[.sasonr_script` <- function(x, i) {
  structure(unclass(x)[i], class = "sasonr_script", mode = attr(x, "mode"), inputs = attr(x, "inputs"),
            tidy = attr(x, "tidy"))
}

# ---- the step verbs ------------------------------------------------------------

# Submit the program text to `session` as one submission: with `macro = FALSE` straight
# to the executor (the text is already macro-expanded), else through the
# macro processor.  `override`: the code a DATA step runs (sas_data_step()).
script_submit <- function(session, text, line = 1L, macro = FALSE, override = NULL, level = "auto") {
  check_session(session)
  if (!rlang::is_string(text)) rlang::abort("The SAS text must be a single string.", class = "sasonr_error")
  line <- vctrs::vec_cast(line, integer())
  session_enter(session)
  on.exit(session_leave(session), add = TRUE)
  eng <- new_engine(session, level, execute = TRUE)
  on_error <- getOption("sasonr.on_error", "sas")[1]
  eng$on_error <- rlang::arg_match(on_error, c("sas", "abort"))
  eng$status <- "ok"
  eng$override <- override
  old <- .active$session
  on.exit(sas_rt_active(old), add = TRUE)
  sas_rt_active(session)
  on.exit(log_approx_close(eng), add = TRUE)
  on.exit(engine_flush(eng), add = TRUE)
  full <- paste0(strrep("\n", line - 1L), text)
  log_program(eng, full)
  # the source echo starts at the step's first line
  if (line > 1L) log_source_skip(eng, seq_len(line - 1L))
  if (macro) {
    sink <- code_sink(session$macro)
    tryCatch({
      mp_scan(mp_src(full, line_of = TRUE), session$macro, sink, mode = "code")
      sink$finish()
    }, sasonr_syntax_error = function(e) macro_error_unsupported(e),
    sasonr_runtime_error = function(e) macro_error_unsupported(e))
  } else {
    eng$chunk_no <- 1L
    session$rt$macro_lines <- integer()
    engine_exec(eng, text, line)
  }
  log_approx_close(eng)
  invisible(session)
}

#' Run one SAS step in a session
#'
#' The verbs of a script written by [sas_script()]. Each takes a session from
#' [sas_session()], runs one step in it as [sas_run()] does (the log, the
#' listing, WORK and the macro variables of the session change), and returns
#' the session invisibly, so the calls can be chained with `|>`.
#'
#' * `sas_data_step()` runs the R code of `fn` (the code sasonr generated for
#'   the DATA step `sas`) in place of the translation of `sas`; the SAS text
#'   gives the variable attributes of the output data sets and the log. When
#'   vectorised code meets a data error the step is run again row by row from
#'   its SAS text, as in [sas_run()].
#' * `sas_proc_sort()`, `sas_proc_means()`, `sas_proc_freq()`,
#'   `sas_proc_transpose()`, `sas_proc_print()`, `sas_proc_tabulate()`: the
#'   options of the PROC statement and one argument per statement
#'   (`by = "usubjid"`, `output = "out=m mean="`, `table = "grp, x*sum"`), as
#'   the program wrote them; a statement given twice is two arguments with
#'   the same name.
#' * `sas_proc_sql()`: the SQL statements of a PROC SQL step.
#' * `sas_proc()`: any PROC step as SAS text.
#' * `sas_options()`, `sas_title()`, `sas_global()`: global statements.
#' * `sas_deferred()`: SAS text with macro code, run through the macro
#'   processor (code that depends on values known only at run time).
#'
#' @param session A session from [sas_session()].
#' @param data The output data sets of the DATA step (for the reader; the SAS
#'   text names them).
#' @param fn A function of `work` whose body is the code of the step.
#' @param sas SAS text of the step.
#' @param line The SAS line the step starts on (line numbers in the log).
#' @param level `NULL` (the level of the translation) or `"L3"` (the step ran
#'   row by row).
#' @param options Options of the PROC (or OPTIONS) statement as SAS text.
#' @param ... Statements of the PROC step: `name = "text"`.
#' @param sql SQL statements, each ending with a semicolon.
#' @param title The text of a TITLE statement (quoted SAS strings).
#' @param n The title line (`TITLEn`).
#' @return The session, invisibly.
#' @examples
#' s <- sas_session(work = list(a = data.frame(x = c(2, 1, 3))))
#' s |> sas_proc_sort("data=a out=b", by = "descending x")
#' s |> sas_proc_means("data=b noprint", var = "x", output = "out=m mean=xm")
#' sas_work(s)$m
#' @name sas_step_verbs
NULL

#' @rdname sas_step_verbs
#' @export
sas_data_step <- function(session, data, fn, sas, line = 1L, level = NULL) {
  if (!is.function(fn)) rlang::abort("`fn` must be a function of `work`.", class = "sasonr_error")
  if (!is.character(data)) rlang::abort("`data` must be a character vector.", class = "sasonr_error")
  b <- body(fn)
  exprs <- if (is.call(b) && identical(b[[1]], as.name("{"))) as.list(b)[-1] else list(b)
  level <- if (is.null(level)) "auto" else rlang::arg_match(level, "L3")
  script_submit(session, sas, line, override = exprs, level = level)
}

script_proc_text <- function(name, options, stmts) {
  if (length(stmts) && (is.null(names(stmts)) || any(!nzchar(names(stmts))))) {
    rlang::abort("Every statement must be named (`by = \"x\"`).", class = "sasonr_error")
  }
  body <- if (length(stmts)) paste0("  ", names(stmts), " ", unlist(stmts), ";\n", collapse = "") else ""
  paste0("proc ", name, if (nzchar(options)) " ", options, ";\n", body, "run;")
}

#' @rdname sas_step_verbs
#' @export
sas_proc_sort <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("sort", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_means <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("means", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_freq <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("freq", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_transpose <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("transpose", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_print <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("print", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_tabulate <- function(session, options = "", ..., line = 1L) {
  script_submit(session, script_proc_text("tabulate", options, list(...)), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc_sql <- function(session, sql, options = "", line = 1L) {
  script_submit(session, paste0("proc sql", if (nzchar(options)) " ", options, ";\n", sql, "\nquit;"), line)
}

#' @rdname sas_step_verbs
#' @export
sas_proc <- function(session, sas, line = 1L) script_submit(session, sas, line)

#' @rdname sas_step_verbs
#' @export
sas_options <- function(session, options, line = 1L) {
  script_submit(session, paste0("options ", options, ";"), line)
}

#' @rdname sas_step_verbs
#' @export
sas_title <- function(session, title = "", n = 1L, line = 1L) {
  script_submit(session, paste0("title", if (n != 1L) n, if (nzchar(title)) " ", title, ";"), line)
}

#' @rdname sas_step_verbs
#' @export
sas_global <- function(session, sas, line = 1L) script_submit(session, sas, line)

#' @rdname sas_step_verbs
#' @export
sas_deferred <- function(session, sas, line = 1L) script_submit(session, sas, line, macro = TRUE)

#' Show tidylog messages for an expression
#'
#' Runs `expr` with `options(tidylog.display = list(message))`, so that the
#' dplyr verbs of a script from `sas_script(tidy = TRUE)` report what they did
#' as messages, and restores the option afterwards.
#' @param expr Code to run.
#' @return The value of `expr`.
#' @examples
#' sasonr_tidy_log(1 + 1)
#' @export
sasonr_tidy_log <- function(expr) {
  old <- options(tidylog.display = list(message))
  on.exit(options(old), add = TRUE)
  expr
}
