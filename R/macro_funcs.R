# Macro functions (%EVAL, quoting functions, character functions, %SYSFUNC).

# Read `( ... )` after a macro function name: returns the resolved argument
# text (macro mode) and leaves the cursor after `)`.
mp_paren_text <- function(src, ses, nm, strip = TRUE, ...) {
  line <- src_line(src)
  j <- src_skip_ws(src)
  if (src_at(src, j) != 40L) abort_syntax(paste0("%", toupper(nm), " requires a parenthesised argument"), line)
  src$i <- j + 1L
  r <- mp_scan_text(src, ses, stop = list(chars = 41L), ...)
  if (r$reason != "char") abort_syntax(paste0("unclosed argument list for %", toupper(nm)), line)
  src$i <- src$i + 1L
  if (strip) mq_strip(r$text) else r$text
}

mp_args <- function(src, ses, nm, min, max) {
  txt <- mp_paren_text(src, ses, nm)
  args <- vapply(mq_split_commas(txt), mq_strip, "", USE.NAMES = FALSE)
  if (length(args) == 1 && !nzchar(args) && min == 0) args <- character()
  if (length(args) < min || length(args) > max) {
    abort_syntax(paste0("wrong number of arguments for %", toupper(nm)), src_line(src))
  }
  args
}

mac_scan_word <- function(text, n, dlm = NULL) {
  dlm_cp <- if (is.null(dlm)) utf8ToInt(" .<(+&!$*);^-/,%|") else utf8ToInt(dlm)
  cp <- utf8ToInt(text)
  is_d <- cp %in% dlm_cp
  runs <- rle(!is_d)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  words <- character()
  for (k in which(runs$values)) words <- c(words, intToUtf8(cp[starts[k]:ends[k]]))
  if (is.na(n) || n == 0) return("")
  idx <- if (n > 0) n else length(words) + n + 1
  if (idx < 1 || idx > length(words)) "" else words[idx]
}

mp_function <- function(nm, src, ses, mode) {
  line <- src_line(src)
  q <- startsWith(nm, "q") && !nm %in% c("quote")
  requote <- function(x) if (q) mq_mask(x) else x
  switch(nm,
    eval = {
      v <- mac_eval(mp_paren_text(src, ses, nm), float = FALSE)
      if (v$kind != "num") {
        abort_runtime(paste0("A character operand was found in the %EVAL function where a numeric operand is required: ", v$value), line)
      }
      mac_num_text(v$value)
    },
    sysevalf = {
      a <- mp_args(src, ses, nm, 1, 2)
      mac_sysevalf(a[1], if (length(a) == 2) trimws(mq_unmask(a[2])) else NULL)
    },
    str = ,
    nrstr = {
      j <- src_skip_ws(src)
      if (src_at(src, j) != 40L) abort_syntax(paste0("%", toupper(nm), " requires a parenthesised argument"), line)
      src$i <- j + 1L
      s <- str_sink()
      r <- mp_scan(src, ses, s, mode = "macro", stop = list(chars = 41L), mask_lit = TRUE, triggers = nm == "str")
      if (r != "char") abort_syntax(paste0("unclosed argument list for %", toupper(nm)), line)
      src$i <- src$i + 1L
      sink_text(s)
    },
    quote = ,
    bquote = mq_mask(mp_paren_text(src, ses, nm, strip = FALSE, mode = if (nm == "bquote") "macro_bq" else "macro"),
                     triggers = FALSE),
    nrquote = ,
    nrbquote = mq_mask(mp_paren_text(src, ses, nm, strip = FALSE, mode = if (nm == "nrbquote") "macro_bq" else "macro"),
                       triggers = TRUE),
    superq = {
      name <- trimws(mq_unmask(mp_paren_text(src, ses, nm)))
      v <- mv_get(ses, name)
      if (is.null(v)) {
        mac_warn(paste0("Apparent symbolic reference ", toupper(name), " not resolved."))
        v <- ""
      }
      mq_mask(v, triggers = TRUE)
    },
    unquote = mq_unmask(mp_paren_text(src, ses, nm, strip = FALSE)),
    upcase = ,
    qupcase = requote(toupper(mq_unmask(mp_paren_text(src, ses, nm)))),
    lowcase = ,
    qlowcase = requote(tolower(mq_unmask(mp_paren_text(src, ses, nm)))),
    left = ,
    qleft = requote(sub("^[ \t\n]+", "", mq_unmask(mp_paren_text(src, ses, nm)))),
    trim = ,
    qtrim = requote(sub("[ \t\n]+$", "", mq_unmask(mp_paren_text(src, ses, nm)))),
    cmpres = trimws(gsub("[ \t\n]+", " ", mq_unmask(mp_paren_text(src, ses, nm)))),
    length = as.character(nchar(mq_unmask(mp_paren_text(src, ses, nm)))),
    index = {
      a <- mq_unmask(mp_args(src, ses, nm, 2, 2))
      as.character(regexpr(a[2], a[1], fixed = TRUE) |> pmax(0L))
    },
    scan = ,
    qscan = {
      a <- mp_args(src, ses, nm, 2, 3)
      n <- mac_eval_int(a[2], paste0("%", toupper(nm)))
      requote(mac_scan_word(mq_unmask(a[1]), n, if (length(a) == 3) mq_unmask(a[3]) else NULL))
    },
    substr = ,
    qsubstr = {
      a <- mp_args(src, ses, nm, 2, 3)
      x <- mq_unmask(a[1])
      pos <- mac_eval_int(a[2], paste0("%", toupper(nm)))
      len <- if (length(a) == 3) mac_eval_int(a[3], paste0("%", toupper(nm))) else nchar(x) - pos + 1
      if (is.na(pos) || pos < 1 || pos > nchar(x)) {
        abort_runtime(paste0("Argument 2 to macro function %", toupper(nm), " is out of range"), line)
      }
      if (is.na(len) || len < 0 || pos + len - 1 > nchar(x)) {
        abort_runtime(paste0("Argument 3 to macro function %", toupper(nm), " is out of range"), line)
      }
      requote(substr(x, pos, pos + len - 1))
    },
    symexist = if (!is.null(mv_find(ses, tolower(trimws(mq_unmask(mp_paren_text(src, ses, nm))))))) "1" else "0",
    symglobl = {
      v <- tolower(trimws(mq_unmask(mp_paren_text(src, ses, nm))))
      if (exists(v, envir = ses$globals, inherits = FALSE)) "1" else "0"
    },
    symlocal = {
      v <- tolower(trimws(mq_unmask(mp_paren_text(src, ses, nm))))
      e <- mv_find(ses, v)
      if (!is.null(e) && !identical(e, ses$globals)) "1" else "0"
    },
    sysmacexist = if (exists(tolower(trimws(mq_unmask(mp_paren_text(src, ses, nm)))), envir = ses$macros, inherits = FALSE)) "1" else "0",
    datatyp = {
      x <- trimws(mq_unmask(mp_paren_text(src, ses, nm)))
      if (nzchar(x) && !is.na(suppressWarnings(as.numeric(x)))) "NUMERIC" else "CHAR"
    },
    sysfunc = ,
    qsysfunc = requote(mac_sysfunc(mp_paren_text(src, ses, nm), line)),
    abort_unsupported(paste0("macro function %", toupper(nm)), line)
  )
}

# %SYSFUNC --------------------------------------------------------------------------

# name = c(arg types: n numeric, c character), result type
sysfunc_table <- list(
  compress = list(args = c("c", "c"), min = 1), upcase = list(args = "c", min = 1),
  lowcase = list(args = "c", min = 1), strip = list(args = "c", min = 1),
  trim = list(args = "c", min = 1), left = list(args = "c", min = 1),
  cats = list(args = "c*", min = 1), catx = list(args = "c*", min = 2),
  today = list(args = character(), min = 0), date = list(args = character(), min = 0),
  putn = list(args = c("n", "f"), min = 2), putc = list(args = c("c", "f"), min = 2),
  sum = list(args = "n*", min = 1), min = list(args = "n*", min = 1), max = list(args = "n*", min = 1),
  int = list(args = "n", min = 1), abs = list(args = "n", min = 1), round = list(args = c("n", "n"), min = 1),
  mod = list(args = c("n", "n"), min = 2), mdy = list(args = c("n", "n", "n"), min = 3),
  year = list(args = "n", min = 1), month = list(args = "n", min = 1), day = list(args = "n", min = 1),
  index = list(args = c("c", "c"), min = 2), length = list(args = "c", min = 1),
  substr = list(args = c("c", "n", "n"), min = 2), scan = list(args = c("c", "n", "c"), min = 2),
  tranwrd = list(args = c("c", "c", "c"), min = 3)
)

sysfunc_ws_significant <- c("cats", "catx", "tranwrd")

mac_parse_format <- function(text, line = NULL) {
  p <- new_parser(sas_lex(text))
  if (!is_format_ahead(p)) abort_syntax(paste0("invalid format ", text), line)
  f <- parse_format(p)
  if (!at_eof(p)) abort_syntax(paste0("invalid format ", text), line)
  check_format(f, line)
  f
}

mac_sysfunc <- function(text, line = NULL) {
  cp <- utf8ToInt(text)
  lp <- match(40L, cp)
  if (is.na(lp)) abort_syntax("%SYSFUNC requires a function call", line)
  fname <- tolower(trimws(intToUtf8(cp[seq_len(lp - 1L)])))
  src <- mp_src(text)
  rp <- raw_match_paren(src, lp)
  if (is.na(rp)) abort_syntax("unbalanced parentheses in %SYSFUNC", line)
  inner <- src_text(src, lp + 1L, rp - 1L)
  rest <- trimws(src_text(src, rp + 1L, src$n))
  fmt <- NULL
  if (nzchar(rest)) {
    if (!startsWith(rest, ",")) abort_syntax("invalid %SYSFUNC argument", line)
    fmt <- mac_parse_format(trimws(substring(rest, 2L)), line)
  }
  spec <- sysfunc_table[[fname]]
  if (is.null(spec)) abort_unsupported(paste0("%SYSFUNC(", toupper(fname), ")"), line)
  args <- if (nzchar(trimws(inner))) mq_split_commas(inner) else character()
  args <- mq_unmask(args)
  if (!fname %in% sysfunc_ws_significant) args <- trimws(args)
  variadic <- length(spec$args) == 1 && endsWith(spec$args, "*")
  if (length(args) < spec$min || (!variadic && length(args) > length(spec$args))) {
    abort_runtime(paste0("The function ", toupper(fname), " referenced by %SYSFUNC has the wrong number of arguments"), line)
  }
  types <- if (variadic) rep(substr(spec$args, 1, 1), length(args)) else spec$args[seq_along(args)]
  vals <- lapply(seq_along(args), function(k) {
    switch(types[k],
      n = {
        v <- if (args[k] == ".") NA_real_ else suppressWarnings(as.numeric(args[k]))
        if (is.na(v) && args[k] != ".") {
          abort_runtime(paste0("Argument ", k, " to function ", toupper(fname), " referenced by %SYSFUNC is not a number"), line)
        }
        v
      },
      f = mac_parse_format(args[k], line),
      args[k]
    )
  })
  res <- switch(fname,
    putn = sas_format(vals[[1]], vals[[2]]),
    putc = sas_format(vals[[1]], vals[[2]]),
    do.call(get(paste0("sas_f_", fname), envir = asNamespace("sasonr")), vals)
  )
  if (is.numeric(res)) {
    if (!is.null(fmt)) return(sas_format(res, fmt))
    return(trimws(sas_fmt_best(res, 12L)))
  }
  if (!is.null(fmt)) return(sas_format(res, fmt))
  res
}
