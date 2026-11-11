# Macro statements, %IF / %DO control flow, macro definition and invocation.

mp_percent <- function(nm, raw, src, ses, sink, mode, in_dq) {
  line <- src_line(src)
  if (nm %in% mac_builtin_functions) {
    res <- mp_function(nm, src, ses, mode)
    if (nm == "unquote" && grepl("[&%]", res)) {
      mp_scan(mp_src(res), ses, sink, mode = mode)
    } else {
      sink$add(res)
    }
    return(invisible())
  }
  def <- ses$macros[[nm]]
  if (!is.null(def)) return(mp_call(def, src, ses, sink))
  switch(nm,
    let = mp_let(src, ses),
    put = mp_put_stmt(src, ses),
    global = ,
    local = mp_scope_stmt(nm, src, ses),
    symdel = {
      r <- mp_scan_text(src, ses, stop = list(chars = 59L, nodepth = TRUE))
      mp_expect_semi(src, "%SYMDEL")
      for (v in strsplit(trimws(mq_unmask(r$text)), "[[:space:]]+")[[1]]) {
        if (nzchar(v) && exists(tolower(v), envir = ses$globals, inherits = FALSE)) rm(list = tolower(v), envir = ses$globals)
      }
    },
    macro = mp_define(src, ses),
    "if" = mp_if(src, ses, sink, mode),
    do = mp_do(src, ses, sink, mode),
    mend = abort_syntax("%MEND without a matching %MACRO", line),
    end = abort_syntax("%END without a matching %DO", line),
    "else" = abort_syntax("%ELSE without a matching %IF-%THEN", line),
    then = abort_syntax("%THEN without a matching %IF", line),
    to = ,
    by = ,
    "while" = ,
    until = abort_syntax(paste0("%", toupper(nm), " outside a %DO statement"), line),
    {
      if (nm %in% mac_reserved_unsupported) abort_unsupported(paste0("macro statement %", toupper(nm)), line)
      if (in_dq || mode == "macro") {
        mac_warn(paste0("Apparent invocation of macro ", toupper(nm), " not resolved."))
        sink$add(paste0("%", raw))
      } else {
        abort_unsupported(paste0("macro %", toupper(nm), " is not defined (autocall library not available)"), line)
      }
    }
  )
  invisible()
}

mp_expect_semi <- function(src, what) {
  if (src_at(src) != 59L) abort_syntax(paste("missing semicolon after", what), src_line(src))
  src$i <- src$i + 1L
}

mp_let <- function(src, ses) {
  line <- src_line(src)
  r <- mp_scan_text(src, ses, stop = list(chars = c(61L, 59L), nodepth = TRUE))
  if (src_at(src) != 61L) abort_syntax("expected '=' in %LET statement", line)
  name <- trimws(mq_unmask(r$text))
  src$i <- src$i + 1L
  v <- mp_scan_text(src, ses, stop = list(chars = 59L, nodepth = TRUE))
  mp_expect_semi(src, "%LET")
  if (!mv_valid_name(tolower(name))) abort_syntax(paste0("Invalid symbolic variable name ", toupper(name), " in %LET"), line)
  mv_let(ses, name, mq_strip(v$text))
}

mp_put_stmt <- function(src, ses) {
  line <- src_line(src)
  k <- src_skip_ws(src)
  w <- tolower(src_word(src, k))
  if (src_at(src, k) == 95L && w %in% c("_user_", "_all_", "_global_", "_local_", "_automatic_")) {
    abort_unsupported(paste0("%PUT ", toupper(w)), line)
  }
  r <- mp_scan_text(src, ses, stop = list(chars = 59L, nodepth = TRUE), put_eq = TRUE)
  mp_expect_semi(src, "%PUT")
  txt <- mq_unmask(r$text)
  txt <- chartr("\n\t", "  ", txt)
  txt <- sub("^ +", "", txt)
  mac_put(ses, sas_rtrim(txt))
}

mp_scope_stmt <- function(nm, src, ses) {
  line <- src_line(src)
  r <- mp_scan_text(src, ses, stop = list(chars = 59L, nodepth = TRUE))
  mp_expect_semi(src, paste0("%", toupper(nm)))
  txt <- trimws(mq_unmask(r$text))
  if (grepl("/", txt, fixed = TRUE)) abort_unsupported(paste0("%", toupper(nm), " options"), line)
  names <- tolower(strsplit(txt, "[[:space:]]+")[[1]])
  names <- names[nzchar(names)]
  for (v in names) if (!mv_valid_name(v)) abort_syntax(paste0("Invalid macro variable name ", toupper(v)), line)
  if (nm == "local") {
    if (length(ses$frames) == 0) abort_syntax("%LOCAL is not valid in open code", line)
    tab <- mv_current(ses)
  } else {
    tab <- ses$globals
  }
  for (v in names) if (!exists(v, envir = tab, inherits = FALSE)) assign(v, "", envir = tab)
}

# Raw (unexpanded) scanning helpers ---------------------------------------------

# Position of the `)` matching the `(` at k (quotes and comments respected).
raw_match_paren <- function(src, k) {
  depth <- 0L
  n <- src$n
  while (k <= n) {
    c <- src$cp[k]
    if (c == 37L && src_at(src, k + 1L) %in% c(40L, 41L, 39L, 34L, 37L)) {
      k <- k + 2L
      next
    }
    if (c == 39L || c == 34L) {
      j <- k + 1L
      while (j <= n && src$cp[j] != c) j <- j + 1L
      k <- j + 1L
      next
    }
    if (c == 40L) depth <- depth + 1L
    if (c == 41L) {
      depth <- depth - 1L
      if (depth == 0L) return(k)
    }
    k <- k + 1L
  }
  NA_integer_
}

# Skip a quoted literal or comment starting at k; returns the next position or
# NA when k does not start one.
raw_skip_opaque <- function(src, k) {
  c <- src$cp[k]
  n <- src$n
  if (c == 39L || c == 34L) {
    j <- k + 1L
    while (j <= n && src$cp[j] != c) j <- j + 1L
    return(j + 1L)
  }
  if (c == 47L && src_at(src, k + 1L) == 42L) {
    j <- k + 2L
    while (j < n && !(src$cp[j] == 42L && src$cp[j + 1L] == 47L)) j <- j + 1L
    return(j + 2L)
  }
  if (c == 37L && src_at(src, k + 1L) == 42L) return(mp_skip_macro_comment(src, k + 2L))
  NA_integer_
}

# Find the %END matching an already-opened %DO; body starts at k.
# Returns list(end = position of `%` of %END, after = position after `%END;`).
raw_find_end <- function(src, k, open = "do", close = "end") {
  depth <- 1L
  n <- src$n
  while (k <= n) {
    nx <- raw_skip_opaque(src, k)
    if (!is.na(nx)) {
      k <- nx
      next
    }
    if (src$cp[k] == 37L && is_alpha_cp(src_at(src, k + 1L))) {
      w <- tolower(src_word(src, k + 1L))
      if (w == open) depth <- depth + 1L
      if (w == close) {
        depth <- depth - 1L
        if (depth == 0L) {
          after <- k + 1L + nchar(w)
          j <- src_skip_ws(src, after)
          if (close == "mend") {
            if (is_alpha_cp(src_at(src, j))) j <- src_skip_ws(src, j + nchar(src_word(src, j)))
          }
          if (src_at(src, j) == 59L) after <- j + 1L
          return(list(end = k, after = after))
        }
      }
      k <- k + 1L + nchar(w)
      next
    }
    k <- k + 1L
  }
  NULL
}

# End of a %THEN/%ELSE statement action: the first top-level `;` (inclusive),
# or just before a top-level %ELSE.
raw_find_stmt_end <- function(src, k) {
  depth <- 0L
  n <- src$n
  while (k <= n) {
    nx <- raw_skip_opaque(src, k)
    if (!is.na(nx)) {
      k <- nx
      next
    }
    c <- src$cp[k]
    if (c == 40L) depth <- depth + 1L
    if (c == 41L) depth <- max(0L, depth - 1L)
    if (c == 59L && depth == 0L) return(list(last = k, after = k + 1L))
    if (c == 37L && depth == 0L && tolower(src_word(src, k + 1L)) == "else") return(list(last = k - 1L, after = k))
    k <- k + 1L
  }
  list(last = n, after = n + 1L)
}

# %IF ---------------------------------------------------------------------------

mp_action <- function(src) {
  j <- src_skip_ws(src)
  if (src_at(src, j) == 37L && tolower(src_word(src, j + 1L)) == "do") {
    k <- j + 3L
    k2 <- src_skip_ws(src, k)
    e <- raw_find_end(src, k)
    if (is.null(e)) abort_syntax("%DO without a matching %END", src_line(src, j))
    src$i <- e$after
    if (src_at(src, k2) == 59L) return(list(kind = "block", a = k2 + 1L, b = e$end - 1L))
    return(list(kind = "do", a = j, b = e$after - 1L))
  }
  e <- raw_find_stmt_end(src, j)
  src$i <- e$after
  list(kind = "text", a = j, b = e$last)
}

mp_if <- function(src, ses, sink, mode) {
  line <- src_line(src)
  r <- mp_scan_text(src, ses, stop = list(kw = "then"))
  if (r$reason != "kw") abort_syntax("%IF without %THEN", line)
  src$i <- src$i + 5L
  cond <- mac_eval_bool(r$text)
  then_act <- mp_action(src)
  else_act <- NULL
  j <- src_skip_ws(src)
  if (src_at(src, j) == 37L && tolower(src_word(src, j + 1L)) == "else" && !is_name_cp(src_at(src, j + 5L))) {
    src$i <- j + 5L
    else_act <- mp_action(src)
  }
  if (length(ses$frames) == 0) {
    kinds <- c(then_act$kind, else_act$kind)
    if (any(kinds != "block")) abort_syntax("%IF-%THEN/%ELSE in open code requires %DO-%END blocks", line)
  }
  act <- if (cond) then_act else else_act
  if (!is.null(act) && act$kind == "text" && src_at(src, act$b) == 59L) {
    # The `;` ends the %IF statement; it is generated text only when the
    # action itself is a macro statement (`%then %put x;`).
    first <- if (src_at(src, act$a) == 37L) tolower(src_word(src, act$a + 1L)) else ""
    if (!first %in% c("let", "put", "global", "local", "symdel", "if", "do", "goto", "return",
                      "macro", "include", "syscall", "abort", "sysexec")) {
      act$b <- act$b - 1L
    }
  }
  if (!is.null(act)) mp_scan(mp_sub(src, act$a, act$b), ses, sink, mode = mode)
  invisible()
}

# %DO ---------------------------------------------------------------------------

mac_max_iter <- 1e5

mp_do <- function(src, ses, sink, mode) {
  line <- src_line(src)
  j <- src_skip_ws(src)
  if (src_at(src, j) == 59L) {
    e <- raw_find_end(src, j + 1L)
    if (is.null(e)) abort_syntax("%DO without a matching %END", line)
    src$i <- e$after
    if (length(ses$frames) == 0) abort_syntax("%DO is not valid in open code", line)
    mp_scan(mp_sub(src, j + 1L, e$end - 1L), ses, sink, mode = mode)
    return(invisible())
  }
  if (length(ses$frames) == 0) abort_syntax("The %DO statement is not valid in open code", line)
  w <- if (src_at(src, j) == 37L) tolower(src_word(src, j + 1L)) else ""
  if (w %in% c("while", "until")) {
    k <- src_skip_ws(src, j + 1L + nchar(w))
    if (src_at(src, k) != 40L) abort_syntax(paste0("expected '(' after %", toupper(w)), line)
    rp <- raw_match_paren(src, k)
    if (is.na(rp)) abort_syntax(paste0("unclosed %", toupper(w), " condition"), line)
    cond_src <- function() mp_sub(src, k + 1L, rp - 1L)
    src$i <- src_skip_ws(src, rp + 1L)
    mp_expect_semi(src, "%DO")
    body_a <- src$i
    e <- raw_find_end(src, body_a)
    if (is.null(e)) abort_syntax("%DO without a matching %END", line)
    src$i <- e$after
    test <- function() mac_eval_bool(mp_scan_text(cond_src(), ses)$text, paste0("%", toupper(w), " condition"))
    for (iter in seq_len(mac_max_iter + 1)) {
      if (iter > mac_max_iter) abort_runtime("%DO loop exceeded 100000 iterations", line)
      if (w == "while" && !test()) break
      mp_scan(mp_sub(src, body_a, e$end - 1L), ses, sink, mode = mode)
      if (w == "until" && test()) break
    }
    return(invisible())
  }
  # %DO var = from %TO to [%BY by];
  r <- mp_scan_text(src, ses, stop = list(chars = c(61L, 59L), nodepth = TRUE))
  if (src_at(src) != 61L) abort_syntax("invalid %DO statement", line)
  var <- tolower(trimws(mq_unmask(r$text)))
  if (!mv_valid_name(var)) abort_syntax(paste0("invalid %DO index variable ", toupper(var)), line)
  src$i <- src$i + 1L
  r <- mp_scan_text(src, ses, stop = list(kw = "to", chars = 59L, nodepth = TRUE))
  if (r$reason != "kw") abort_syntax("%DO without %TO", line)
  from <- mac_eval_int(r$text, "%DO start value")
  src$i <- src$i + 3L
  r <- mp_scan_text(src, ses, stop = list(kw = "by", chars = 59L, nodepth = TRUE))
  to <- mac_eval_int(r$text, "%DO %TO value")
  by <- 1
  if (r$reason == "kw") {
    src$i <- src$i + 3L
    r <- mp_scan_text(src, ses, stop = list(chars = 59L, nodepth = TRUE))
    by <- mac_eval_int(r$text, "%DO %BY value")
  }
  mp_expect_semi(src, "%DO")
  if (is.na(by) || by == 0) abort_runtime("%DO %BY value is zero", line)
  body_a <- src$i
  e <- raw_find_end(src, body_a)
  if (is.null(e)) abort_syntax("%DO without a matching %END", line)
  src$i <- e$after
  mv_let(ses, var, mac_num_text(from))
  for (iter in seq_len(mac_max_iter + 1)) {
    if (iter > mac_max_iter) abort_runtime("%DO loop exceeded 100000 iterations", line)
    cur <- mac_eval_int(mv_get(ses, var) %||% "", "%DO index")
    if ((by > 0 && cur > to) || (by < 0 && cur < to)) break
    mp_scan(mp_sub(src, body_a, e$end - 1L), ses, sink, mode = mode)
    cur <- mac_eval_int(mv_get(ses, var) %||% "", "%DO index")
    mv_let(ses, var, mac_num_text(cur + by))
  }
  invisible()
}

# %MACRO ------------------------------------------------------------------------

mp_define <- function(src, ses) {
  line <- src_line(src)
  k <- src_skip_ws(src)
  name <- tolower(src_word(src, k))
  if (!mv_valid_name(name)) abort_syntax("invalid macro name in %MACRO", line)
  if (name %in% c(mac_builtin_functions, "let", "put", "if", "do", "end", "macro", "mend", "global", "local")) {
    abort_syntax(paste0("macro name ", toupper(name), " is reserved"), line)
  }
  k <- src_skip_ws(src, k + nchar(name))
  params <- list()
  if (src_at(src, k) == 40L) {
    rp <- raw_match_paren(src, k)
    if (is.na(rp)) abort_syntax("unclosed %MACRO parameter list", line)
    ptxt <- src_text(src, k + 1L, rp - 1L)
    if (nzchar(trimws(ptxt))) {
      for (p in mq_split_commas(ptxt)) {
        eq <- regexpr("=", p, fixed = TRUE)
        pname <- tolower(trimws(if (eq > 0) substr(p, 1L, eq - 1L) else p))
        if (!mv_valid_name(pname)) abort_syntax(paste0("invalid macro parameter ", p), line)
        params[[length(params) + 1L]] <- list(
          name = pname, keyword = eq > 0,
          default = if (eq > 0) trimws(substr(p, eq + 1L, nchar(p))) else ""
        )
      }
    }
    k <- src_skip_ws(src, rp + 1L)
  }
  parmbuff <- FALSE
  if (src_at(src, k) == 47L) {
    e <- k + 1L
    while (e <= src$n && src$cp[e] != 59L) {
      nx <- raw_skip_opaque(src, e)
      e <- if (is.na(nx)) e + 1L else nx
    }
    opts <- src_text(src, k + 1L, e - 1L)
    words <- tolower(strsplit(trimws(gsub("=", " = ", opts, fixed = TRUE)), "[[:space:]]+")[[1]])
    w <- 1L
    while (w <= length(words)) {
      o <- words[w]
      if (o %in% c("parmbuff", "pbuff")) {
        parmbuff <- TRUE
      } else if (o == "des" && identical(words[w + 1L], "=")) {
        w <- w + 2L # quoted description, ignored
        while (w <= length(words) && !grepl("['\"]$", words[w])) w <- w + 1L
      } else if (nzchar(o)) {
        abort_unsupported(paste0("%MACRO option ", toupper(o)), line)
      }
      w <- w + 1L
    }
    k <- e
  }
  if (src_at(src, k) != 59L) abort_syntax("missing semicolon after %MACRO statement", line)
  body_a <- k + 1L
  e <- raw_find_end(src, body_a, open = "macro", close = "mend")
  if (is.null(e)) abort_syntax(paste0("%MACRO ", toupper(name), " has no matching %MEND"), line)
  src$i <- e$after
  ses$macros[[name]] <- list(
    name = name, params = params, parmbuff = parmbuff,
    # Line breaks are not part of generated text; leading/trailing blanks of the body are dropped.
    body = mq_strip(chartr("\n\t", "  ", src_text(src, body_a, e$end - 1L)))
  )
  invisible()
}

# Macro invocation ----------------------------------------------------------------

mp_call <- function(def, src, ses, sink) {
  line <- src_line(src)
  j <- src_skip_ws(src)
  args <- NULL
  buff <- ""
  if (src_at(src, j) == 40L) {
    src$i <- j + 1L
    r <- mp_scan_text(src, ses, stop = list(chars = 41L))
    if (r$reason != "char") abort_syntax(paste0("unclosed argument list in call to %", toupper(def$name)), line)
    src$i <- src$i + 1L
    buff <- paste0("(", r$text, ")")
    args <- mq_split_commas(r$text)
    if (length(args) == 1 && !nzchar(trimws(mq_unmask(args)))) args <- character()
  }
  pnames <- vapply(def$params, `[[`, "", "name")
  pos_names <- pnames[!vapply(def$params, `[[`, TRUE, "keyword")]
  vals <- list()
  for (p in def$params) {
    vals[[p$name]] <- if (p$keyword) mq_strip(mp_resolve(p$default, ses)) else ""
  }
  k_pos <- 0L
  seen_kw <- FALSE
  for (a in args) {
    cp <- utf8ToInt(a)
    s <- 1L
    while (s <= length(cp) && is_ws_cp(cp[s])) s <- s + 1L
    e <- s
    while (e <= length(cp) && is_name_cp(cp[e])) e <- e + 1L
    e2 <- e
    while (e2 <= length(cp) && is_ws_cp(cp[e2])) e2 <- e2 + 1L
    kw <- if (e > s && e2 <= length(cp) && cp[e2] == 61L) tolower(intToUtf8(cp[s:(e - 1L)])) else NA_character_
    if (!is.na(kw) && kw %in% pnames) {
      vals[[kw]] <- mq_strip(if (e2 < length(cp)) intToUtf8(cp[(e2 + 1L):length(cp)]) else "")
      seen_kw <- TRUE
      next
    }
    if (!is.na(kw) && !def$parmbuff) {
      abort_syntax(paste0("Keyword parameter ", toupper(kw), " was not defined with macro ", toupper(def$name)), line)
    }
    k_pos <- k_pos + 1L
    if (k_pos > length(pos_names)) {
      if (def$parmbuff) next
      abort_syntax(paste0("More positional parameters found than defined for macro ", toupper(def$name)), line)
    }
    if (seen_kw) abort_syntax("Positional parameters must precede keyword parameters", line)
    vals[[pos_names[k_pos]]] <- mq_strip(a)
  }
  if (length(ses$frames) >= 400) abort_runtime("macro calls nest more than 400 levels deep", line)
  frame <- list(name = def$name, vars = new.env(parent = emptyenv()))
  for (nm in names(vals)) assign(nm, vals[[nm]], envir = frame$vars)
  if (def$parmbuff) assign("syspbuff", buff, envir = frame$vars)
  ses$frames[[length(ses$frames) + 1L]] <- frame
  old_name <- get("sysmacroname", envir = ses$globals)
  assign("sysindex", as.character(as.integer(get("sysindex", envir = ses$globals)) + 1L), envir = ses$globals)
  assign("sysmacroname", toupper(def$name), envir = ses$globals)
  depth <- length(ses$frames)
  on.exit({
    ses$frames <- ses$frames[seq_len(depth - 1L)]
    assign("sysmacroname", old_name, envir = ses$globals)
  })
  body <- mp_src(def$body)
  mp_scan(body, ses, sink, mode = "code")
  invisible()
}
