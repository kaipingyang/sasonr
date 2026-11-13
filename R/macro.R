# Macro processor (plan.md section 3, step 2).
#
# Text-level, like the SAS word scanner feeding the macro processor:
#   * the program text is scanned character by character; `&name` and `%name`
#     triggers are resolved/executed, everything else is copied;
#   * single-quoted text bound for the compiler is opaque, double-quoted text
#     is resolved; `/* */` comments are copied untouched, `%*` comments dropped;
#   * DATALINES data is copied raw;
#   * macro definitions are stored as text and rescanned at every call;
#     %IF / %DO bodies are rescanned when (and each time) they execute.
#
# The macro layer is a pull-style generator of complete steps: compiler-bound
# text accumulates in a "code sink" that cuts it at step boundaries (RUN; /
# QUIT; / the end of a DATALINES block / the next DATA or PROC statement) and
# hands each complete step to the executor *before* scanning continues. So a
# DATA step generated inside a macro body runs before the rest of the body is
# expanded, and CALL SYMPUT(X) results are visible to the macro text after it.

mac_reserved_unsupported <- c(
  "include", "goto", "return", "abort", "label", "syscall", "sysexec", "window",
  "display", "input", "copy", "sysrput", "syslput", "sysget", "sysprod", "sysmacdelete",
  "verify", "compstor", "qcmpres", "sysmexecdepth", "sysmexecname", "symdel_all", "qsysfunc_"
)

mac_builtin_functions <- c(
  "eval", "sysevalf", "str", "nrstr", "quote", "nrquote", "bquote", "nrbquote", "superq",
  "unquote", "upcase", "qupcase", "lowcase", "qlowcase", "scan", "qscan", "substr",
  "qsubstr", "index", "length", "sysfunc", "qsysfunc", "left", "qleft", "trim", "qtrim",
  "cmpres", "symexist", "symglobl", "symlocal", "datatyp", "sysmacexist"
)

# Session ------------------------------------------------------------------

new_macro_session <- function(exec = NULL) {
  ses <- new.env(parent = emptyenv())
  ses$globals <- new.env(parent = emptyenv())
  ses$frames <- list()
  ses$macros <- new.env(parent = emptyenv())
  ses$log <- character()
  ses$exec <- exec
  ses$line <- 1L
  now <- Sys.time()
  auto <- list(
    sysdate = toupper(format(now, "%d%b%y")), sysdate9 = toupper(format(now, "%d%b%Y")),
    sysday = format(now, "%A"), systime = format(now, "%H:%M"), sysver = "9.4",
    sysvlong = "9.04.01M7P080520", sysscp = "LIN X64", sysscpl = "Linux", syserr = "0",
    syscc = "0", sysrc = "0", sysmacroname = "", sysindex = "0", sysncpu = "1"
  )
  for (nm in names(auto)) assign(nm, auto[[nm]], envir = ses$globals)
  class(ses) <- c("sasonr_macro_session", class(ses))
  ses
}

mv_valid_name <- function(name) {
  cp <- utf8ToInt(name)
  length(cp) > 0 && length(cp) <= 32 && is_alpha_cp(cp[1]) && all(is_name_cp(cp))
}

mv_find <- function(ses, name) {
  for (k in rev(seq_along(ses$frames))) {
    if (exists(name, envir = ses$frames[[k]]$vars, inherits = FALSE)) return(ses$frames[[k]]$vars)
  }
  if (exists(name, envir = ses$globals, inherits = FALSE)) return(ses$globals)
  NULL
}

mv_get <- function(ses, name) {
  name <- tolower(name)
  e <- mv_find(ses, name)
  if (is.null(e)) NULL else get(name, envir = e, inherits = FALSE)
}

mv_current <- function(ses) {
  k <- length(ses$frames)
  if (k == 0) ses$globals else ses$frames[[k]]$vars
}

mv_let <- function(ses, name, value) {
  name <- tolower(name)
  if (!mv_valid_name(name)) abort_syntax(paste0("Invalid symbolic variable name ", toupper(name)))
  e <- mv_find(ses, name) %||% mv_current(ses)
  assign(name, value, envir = e)
  invisible()
}

# CALL SYMPUT / SYMPUTX placement (SAS Macro Language Reference, "Scopes of
# macro variables with CALL SYMPUT"): an existing variable is updated where it
# lives; otherwise it is created in the most local nonempty symbol table.
mv_symput <- function(ses, name, value, scope = NULL) {
  name <- tolower(name)
  if (!mv_valid_name(name)) abort_runtime(paste0("Symbolic variable name ", toupper(name), " is invalid"))
  scope <- toupper(scope %||% "")
  target <- NULL
  if (scope == "G") {
    target <- ses$globals
  } else if (scope == "L") {
    target <- mv_current(ses)
  } else {
    target <- mv_find(ses, name)
    if (is.null(target)) {
      target <- ses$globals
      for (k in rev(seq_along(ses$frames))) {
        if (length(ls(ses$frames[[k]]$vars, all.names = TRUE)) > 0) {
          target <- ses$frames[[k]]$vars
          break
        }
      }
    }
  }
  assign(name, value, envir = target)
  invisible()
}

mac_put <- function(ses, line) {
  ses$log <- c(ses$log, line)
  if (is.function(ses$on_put)) ses$on_put(line)
  invisible()
}

# A macro-time ERROR that does not stop processing (written to the log).
mac_error_note <- function(msg) {
  rt <- rt_diag()
  if (is.function(rt$on_error)) rt$on_error(msg)
  invisible()
}

mac_warn <- function(msg, class = "sasonr_macro_warning") {
  rt <- rt_diag()
  if (is.function(rt$on_warn)) rt$on_warn(msg)
  cli::cli_warn(msg, class = class)
}

# Code-point helpers ---------------------------------------------------------

is_alpha_cp <- function(c) (c >= 65L & c <= 90L) | (c >= 97L & c <= 122L) | c == 95L
is_name_cp <- function(c) is_alpha_cp(c) | (c >= 48L & c <= 57L)
is_ws_cp <- function(c) c %in% c(32L, 9L, 10L, 11L, 12L, 13L, 160L)

# A source: code points plus a cursor (an environment, so it is shared).
mp_src <- function(text, line_of = NULL) {
  src <- new.env(parent = emptyenv())
  src$cp <- utf8ToInt(enc2utf8(text))
  src$cp <- src$cp[src$cp != 13L]
  src$n <- length(src$cp)
  src$i <- 1L
  src$line_of <- line_of
  if (isTRUE(line_of)) src$line_of <- cumsum(c(1L, src$cp == 10L))[seq_len(src$n)]
  src
}

mp_sub <- function(src, a, b) {
  s <- new.env(parent = emptyenv())
  if (b < a) {
    s$cp <- integer()
  } else {
    s$cp <- src$cp[a:b]
  }
  s$n <- length(s$cp)
  s$i <- 1L
  s$line_of <- if (!is.null(src$line_of) && b >= a) src$line_of[a:b] else NULL
  s
}

src_at <- function(src, k = src$i) if (k >= 1L && k <= src$n) src$cp[k] else -1L
src_text <- function(src, a, b) if (b < a) "" else intToUtf8(src$cp[a:b])
src_skip_ws <- function(src, k = src$i) {
  while (k <= src$n && is_ws_cp(src$cp[k])) k <- k + 1L
  k
}
src_word <- function(src, k) {
  j <- k
  while (j <= src$n && is_name_cp(src$cp[j])) j <- j + 1L
  src_text(src, k, j - 1L)
}
src_line <- function(src, k = src$i) {
  if (is.null(src$line_of) || src$n == 0) return(NULL)
  src$line_of[min(max(k, 1L), src$n)]
}

# Sinks ------------------------------------------------------------------------

str_sink <- function() {
  s <- new.env(parent = emptyenv())
  s$parts <- character()
  s$compiler <- FALSE
  s$add <- function(x) s$parts[length(s$parts) + 1L] <- x
  s$comment <- s$add
  s
}

sink_text <- function(s) paste(s$parts, collapse = "")

datalines_stmt_words <- c("datalines", "cards", "lines", "datalines4", "cards4", "lines4")

# The compiler-bound sink: tracks statements (outside quotes and comments) and
# cuts complete steps out of the text.
code_sink <- function(ses) {
  s <- new.env(parent = emptyenv())
  s$compiler <- TRUE
  s$acc <- ""
  s$state <- "code"
  s$qchar <- 0L
  s$qret <- "code"
  s$stmt <- integer()
  s$stmt_begin <- NA_integer_
  s$stmt_line <- 1L
  s$chunk_line <- NA_integer_
  s$dl <- FALSE
  s$dl4 <- FALSE
  s$sql <- FALSE

  flush_upto <- function(p) {
    text <- substr(s$acc, 1L, p)
    s$acc <- substr(s$acc, p + 1L, nchar(s$acc))
    line <- s$chunk_line
    s$chunk_line <- if (nzchar(trimws(s$acc))) s$stmt_line else NA_integer_
    if (nzchar(trimws(text))) ses$exec(text, if (is.na(line)) 1L else line)
  }

  stmt_end <- function(pos) {
    txt <- tolower(trimws(intToUtf8(s$stmt)))
    cp <- utf8ToInt(txt)
    k <- 1L
    while (k <= length(cp) && is_name_cp(cp[k])) k <- k + 1L
    first <- substr(txt, 1L, k - 1L)
    rest <- trimws(substr(txt, k, nchar(txt)))
    begin <- s$stmt_begin
    s$stmt <- integer()
    s$stmt_begin <- NA_integer_
    if (first %in% c("run", "quit") && rest %in% c("", "cancel")) {
      if (first == "quit") s$sql <- FALSE
      flush_upto(pos)
      return(pos)
    } else if (first %in% datalines_stmt_words && rest == "" && !s$sql) {
      s$dl <- TRUE
      s$dl4 <- endsWith(first, "4")
    } else if (first %in% c("data", "proc") && !startsWith(rest, "=")) {
      shift <- 0L
      if (!is.na(begin) && begin > 1L) {
        flush_upto(begin - 1L)
        shift <- begin - 1L
      }
      # PROC SQL executes each statement as soon as it is complete
      s$sql <- first == "proc" && first_word(rest) == "sql"
      if (s$sql) {
        flush_upto(pos - shift)
        return(pos)
      }
      return(shift)
    } else if (s$sql) {
      flush_upto(pos)
      return(pos)
    }
    0L
  }

  s$add <- function(text) {
    text <- mq_unmask(text)
    if (!nzchar(text)) return(invisible())
    if (is.na(s$chunk_line) && nzchar(trimws(text))) s$chunk_line <- ses$line
    base <- nchar(s$acc)
    s$acc <- paste0(s$acc, text)
    if (s$dl) return(invisible())
    cps <- utf8ToInt(text)
    cur_line <- ses$line
    for (k in seq_along(cps)) {
      c <- cps[k]
      st <- s$state
      if (k > 1L && cps[k - 1L] == 10L) cur_line <- cur_line + 1L
      if (st == "code") {
        if (is_ws_cp(c)) {
          if (length(s$stmt) > 0) s$stmt[length(s$stmt) + 1L] <- 32L
        } else if (c == 59L) {
          base <- base - stmt_end(base + k)
        } else if (length(s$stmt) == 0 && c == 42L) {
          s$state <- "star"
        } else {
          if (length(s$stmt) == 0) {
            s$stmt_begin <- base + k
            s$stmt_line <- cur_line
          }
          s$stmt[length(s$stmt) + 1L] <- c
          if (c == 34L || c == 39L) {
            s$state <- "q"
            s$qchar <- c
            s$qret <- "code"
          }
        }
      } else if (st == "q") {
        if (s$qret == "code") s$stmt[length(s$stmt) + 1L] <- c
        if (c == s$qchar) s$state <- s$qret
      } else if (st == "star") {
        if (c == 59L) s$state <- "code"
      }
    }
    invisible()
  }
  s$at_stmt_start <- function() s$state == "code" && length(s$stmt) == 0 && !s$dl
  s$comment <- function(text) {
    if (is.na(s$chunk_line)) s$chunk_line <- ses$line
    s$acc <- paste0(s$acc, text)
  }
  s$end_datalines <- function() {
    s$dl <- FALSE
    s$stmt <- integer()
    s$stmt_begin <- NA_integer_
    flush_upto(nchar(s$acc))
  }
  s$finish <- function() {
    if (nzchar(trimws(s$acc))) flush_upto(nchar(s$acc))
  }
  s
}

# Scanner ---------------------------------------------------------------------

# Scan `src` from its cursor, resolving macro triggers into `sink`.
#   mode     "code": compiler-bound text, single quotes hide triggers;
#            "macro": macro statement text; single quotes also hide triggers
#            (SAS: `%let q='AT&t';` stores `'AT&t'` unresolved);
#            "macro_bq": %BQUOTE / %NRBQUOTE arguments, single quotes only group.
#   stop     list(chars = code points, kw = %keywords, nodepth = TRUE/FALSE):
#            return (without consuming) at a top-level stop character or a
#            `%keyword`; parentheses nest unless `nodepth`.
#   mask_lit mask literal special characters (%STR / %NRSTR arguments).
#   triggers resolve & and % triggers (FALSE for %NRSTR).
#   put_eq   `&=name` is written as NAME=value (%PUT).
# Returns "eof", "char" or "kw"; the cursor is left at the stop token.
mp_scan <- function(src, ses, sink, mode = "code", stop = NULL, mask_lit = FALSE,
                    triggers = TRUE, put_eq = FALSE) {
  cp <- src$cp
  n <- src$n
  at <- function(k) if (k >= 1L && k <= n) cp[k] else -1L
  depth <- 0L
  star <- FALSE
  quote <- 0L
  i <- src$i
  lit <- i
  emit_lit <- function(to) {
    if (to >= lit) {
      txt <- intToUtf8(cp[lit:to])
      if (mask_lit) txt <- mq_mask(txt, triggers = !triggers)
      if (!is.null(src$line_of)) ses$line <- src$line_of[lit]
      sink$add(txt)
    }
  }
  stop_chars <- stop$chars
  stop_kw <- stop$kw
  nodepth <- isTRUE(stop$nodepth)
  is_trigger <- function(k) {
    c <- cp[k]
    nx <- at(k + 1L)
    if (c == 38L) return(is_alpha_cp(nx) || nx == 38L || (put_eq && nx == 61L && is_alpha_cp(at(k + 2L))))
    c == 37L && is_alpha_cp(nx)
  }

  while (i <= n) {
    c <- cp[i]
    # %STR escapes: %( %) %' %" %%
    if (mask_lit && c == 37L && at(i + 1L) %in% c(40L, 41L, 39L, 34L, 37L)) {
      emit_lit(i - 1L)
      sink$add(mq_mask(intToUtf8(cp[i + 1L])))
      i <- i + 2L
      lit <- i
      next
    }
    if (quote != 0L) {
      if (c == quote) {
        quote <- 0L
        i <- i + 1L
        next
      }
      if (triggers && (quote == 34L || mode == "macro_bq") && is_trigger(i)) {
        emit_lit(i - 1L)
        src$i <- i
        mp_trigger(src, ses, sink, mode, in_dq = quote == 34L, put_eq = put_eq)
        i <- src$i
        lit <- i
        next
      }
      i <- i + 1L
      next
    }
    if (!is.null(stop_chars) && c %in% stop_chars && (nodepth || depth == 0L)) {
      emit_lit(i - 1L)
      src$i <- i
      return("char")
    }
    if (c == 40L) {
      depth <- depth + 1L
    } else if (c == 41L) {
      depth <- max(0L, depth - 1L)
    } else if (c == 42L && !star && isTRUE(sink$compiler)) {
      emit_lit(i - 1L)
      lit <- i
      star <- sink$at_stmt_start()
    } else if ((c == 39L || c == 34L) && !star) {
      quote <- c
    } else if (c == 47L && at(i + 1L) == 42L) {
      emit_lit(i - 1L)
      k <- i + 2L
      while (k < n && !(cp[k] == 42L && cp[k + 1L] == 47L)) k <- k + 1L
      if (k >= n) abort_syntax("unterminated /* comment", src_line(src, i))
      if (mode == "code") sink$comment(intToUtf8(cp[i:(k + 1L)]))
      i <- k + 2L
      lit <- i
      next
    } else if (c == 37L && at(i + 1L) == 42L) {
      emit_lit(i - 1L)
      i <- mp_skip_macro_comment(src, i + 2L)
      lit <- i
      next
    } else if (c == 59L && isTRUE(sink$compiler)) {
      star <- FALSE
      emit_lit(i)
      i <- i + 1L
      lit <- i
      if (isTRUE(sink$dl)) {
        i <- mp_copy_datalines(src, i, ses, sink)
        lit <- i
      }
      next
    } else if (triggers && (c == 38L || c == 37L) && is_trigger(i)) {
      if (c == 37L && !is.null(stop_kw)) {
        w <- tolower(src_word(src, i + 1L))
        if (w %in% stop_kw) {
          emit_lit(i - 1L)
          src$i <- i
          return("kw")
        }
      }
      emit_lit(i - 1L)
      src$i <- i
      mp_trigger(src, ses, sink, mode, in_dq = FALSE, put_eq = put_eq)
      i <- src$i
      lit <- i
      next
    }
    i <- i + 1L
  }
  emit_lit(n)
  src$i <- n + 1L
  "eof"
}

# `%* ... ;` comment starting after `%*`; quotes inside are honoured, and an
# unmatched one swallows the rest of the text, as in SAS.
mp_skip_macro_comment <- function(src, k) {
  while (k <= src$n) {
    c <- src$cp[k]
    if (c == 39L || c == 34L) {
      j <- k + 1L
      while (j <= src$n && src$cp[j] != c) j <- j + 1L
      if (j > src$n) {
        cli::cli_warn(
          c("Unmatched quote inside a %* macro comment.", "i" = "As in SAS, the rest of the program is swallowed."),
          class = "sasonr_unmatched_quote_in_comment"
        )
        return(src$n + 1L)
      }
      k <- j + 1L
      next
    }
    if (c == 59L) return(k + 1L)
    k <- k + 1L
  }
  src$n + 1L
}

# Copy DATALINES data raw, from just after the `;` of the DATALINES statement.
mp_copy_datalines <- function(src, i, ses, sink) {
  n <- src$n
  four <- sink$dl4
  e <- i
  while (e <= n && src$cp[e] != 10L) e <- e + 1L
  sink$add(src_text(src, i, e))
  i <- e + 1L
  while (i <= n) {
    e <- i
    while (e <= n && src$cp[e] != 10L) e <- e + 1L
    line_cp <- if (e > i) src$cp[i:(e - 1L)] else integer()
    if (four) {
      if (length(line_cp) >= 4L && all(line_cp[1:4] == 59L)) {
        sink$add(src_text(src, i, i + 3L))
        sink$end_datalines()
        return(i + 4L)
      }
    } else {
      nb <- which(!is_ws_cp(line_cp))
      if (length(nb) > 0 && line_cp[nb[1]] == 59L) {
        sink$add(src_text(src, i, i + nb[1] - 1L))
        sink$end_datalines()
        return(i + nb[1])
      }
    }
    if (!is.null(src$line_of)) ses$line <- src$line_of[i]
    sink$add(src_text(src, i, e))
    i <- e + 1L
  }
  sink$end_datalines()
  n + 1L
}

mp_scan_text <- function(src, ses, stop = NULL, mode = "macro", ...) {
  s <- str_sink()
  reason <- mp_scan(src, ses, s, mode = mode, stop = stop, ...)
  list(text = sink_text(s), reason = reason)
}

mp_resolve <- function(text, ses, mode = "macro") {
  if (!grepl("[&%]", text)) return(text)
  mp_scan_text(mp_src(text), ses, mode = mode)$text
}

# Triggers ----------------------------------------------------------------------

mp_trigger <- function(src, ses, sink, mode, in_dq, put_eq) {
  if (src$cp[src$i] == 38L) return(mp_amp(src, ses, sink, mode, put_eq))
  k <- src$i + 1L
  name <- src_word(src, k)
  src$i <- k + nchar(name)
  mp_percent(tolower(name), name, src, ses, sink, mode, in_dq)
}

# `&...` reference run: (&+ name .?)+ resolved in passes; `&&` -> `&`.
mp_amp <- function(src, ses, sink, mode, put_eq) {
  i <- src$i
  cp <- src$cp
  n <- src$n
  if (put_eq && src_at(src, i + 1L) == 61L) {
    name <- src_word(src, i + 2L)
    src$i <- i + 2L + nchar(name)
    v <- mv_get(ses, name)
    if (is.null(v)) {
      mac_warn(paste0("Apparent symbolic reference ", toupper(name), " not resolved."))
      v <- paste0("&", name)
    }
    sink$add(paste0(toupper(name), "=", v))
    return(invisible())
  }
  j <- i
  repeat {
    a <- j
    while (j <= n && cp[j] == 38L) j <- j + 1L
    if (j > n || !is_alpha_cp(cp[j])) {
      j <- a
      break
    }
    while (j <= n && is_name_cp(cp[j])) j <- j + 1L
    if (j <= n && cp[j] == 46L) j <- j + 1L
    if (j > n || cp[j] != 38L) break
  }
  if (j == i) {
    # `&&` not followed by a name: plain text
    while (j <= n && cp[j] == 38L) j <- j + 1L
    sink$add(src_text(src, i, j - 1L))
    src$i <- j
    return(invisible())
  }
  run <- src_text(src, i, j - 1L)
  src$i <- j
  val <- mp_resolve_amp_run(run, ses)
  if (grepl("%[A-Za-z_]", val)) {
    mp_scan(mp_src(val), ses, sink, mode = mode)
  } else {
    sink$add(val)
  }
  invisible()
}

mp_resolve_amp_run <- function(text, ses) {
  for (pass in 1:100) {
    cp <- utf8ToInt(text)
    n <- length(cp)
    out <- character()
    k <- 1L
    changed <- FALSE
    unresolved <- character()
    while (k <= n) {
      c <- cp[k]
      if (c == 38L && k < n && cp[k + 1L] == 38L) {
        out[length(out) + 1L] <- "&"
        k <- k + 2L
        changed <- TRUE
        next
      }
      if (c == 38L && k < n && is_alpha_cp(cp[k + 1L])) {
        j <- k + 1L
        while (j <= n && is_name_cp(cp[j])) j <- j + 1L
        name <- intToUtf8(cp[(k + 1L):(j - 1L)])
        dot <- j <= n && cp[j] == 46L
        v <- mv_get(ses, name)
        if (is.null(v)) {
          unresolved <- c(unresolved, name)
          out[length(out) + 1L] <- intToUtf8(cp[k:(j - 1L + dot)])
        } else {
          out[length(out) + 1L] <- v
          changed <- TRUE
        }
        k <- j + dot
        next
      }
      out[length(out) + 1L] <- intToUtf8(c)
      k <- k + 1L
    }
    new <- paste(out, collapse = "")
    if (!changed || !grepl("&[A-Za-z_&]", new)) {
      for (u in unique(unresolved)) mac_warn(paste0("Apparent symbolic reference ", toupper(u), " not resolved."))
      return(new)
    }
    text <- new
  }
  abort_runtime("macro variable references nest more than 100 levels deep")
}

# The leading SAS name of a (lower-case, trimmed) statement remainder.
first_word <- function(txt) {
  cp <- utf8ToInt(txt)
  k <- 1L
  while (k <= length(cp) && is_name_cp(cp[k])) k <- k + 1L
  substr(txt, 1L, k - 1L)
}
