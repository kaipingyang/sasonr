# Generate the reference-page examples inst/examples/sas_f_<fn>.R.  Every
# example line is an R call followed by the result real SAS 9.4 gave for the
# same call; only calls where sasonr agrees with SAS are written.
#
# Usage (package root; the local conformance assets must be present):
#   Rscript tools/gen_examples.R programs   # write tests/conformance/fn_examples/<FN>/program.sas
#   python -I tools/refresh_gold.py tests/conformance/fn_examples   # SAS gold standard
#   Rscript tools/gen_examples.R [conformance.csv]   # compare, write inst/examples/ and the results
# The optional CSV (tools/run_conformance.R --out) adds the whole-case results of
# the fn_examples cases (all six layers) to the mismatch report.
#
# Two sources:
#   1. the function pilot (tests/conformance/functions/<FN>/<FN>_<id>/): one
#      case per call, listed in `pilot` below (case id -> R call);
#   2. tools/fn_examples.txt: 3-6 calls per function, one case per function
#      (tests/conformance/fn_examples/<FN>/, results in data set WORK.EX,
#      variables r1, r2, ...).  Each call is evaluated in R and compared with
#      the SAS value (numbers bit for bit or within 1e-9 relative, character
#      values without trailing blanks).  The per-call results go to
#      tests/conformance/fn_examples/results.csv (local) and the calls that
#      differ to ../sasonr-conformance/docs/conformance/fn_examples_mismatch.md
#      when that private repository is present.

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) && args[1] == "programs") "programs" else "examples"
invisible(Sys.setlocale("LC_TIME", "C"))

# ---- 1. function pilot --------------------------------------------------------

pilot <- list(
  CATX = c(`001` = 'sas_f_catx("-", "a", "b", "c")', `002` = 'sas_f_catx("-", "  a ", "b  ", " c")',
           `003` = 'sas_f_catx("-", "a", " ", "c")', `006` = 'sas_f_catx("-", "a", NA, "c")'),
  COMPRESS = c(`001` = 'sas_f_compress("a b c")', `002` = 'sas_f_compress("a-b-c", "-")',
               `004` = 'sas_f_compress("a1 b2", "", "d")', `006` = 'sas_f_compress("a1 b2", "", "kd")',
               `007` = 'sas_f_compress("ABC abc", "a", "i")'),
  INDEX = c(`001` = 'sas_f_index("abcabc", "bc")', `002` = 'sas_f_index("abc", "d")',
            `004` = 'sas_f_index("ABC", "b")', `005` = 'sas_f_index("a bc", " ")'),
  INPUT = c(`001` = 'sas_f_inputn("123", "8.")', `004` = 'sas_f_inputn("1234", "4.2")',
            `005` = 'sas_f_inputn("12.34", "5.1")', `006` = 'sas_f_inputn("1,234", "comma8.")',
            `008` = 'sas_f_inputn("(12)", "comma5.")'),
  INTCK = c(`001` = 'sas_f_intck("month", 21945, 21946)', `002` = 'sas_f_intck("month", 21945, 21946, "c")',
            `006` = 'sas_f_intck("year", 21914, 21915)', `009` = 'sas_f_intck("month", 21975, 21915)'),
  INTNX = c(`001` = 'sas_f_intnx("month", 21929, 1)', `002` = 'sas_f_intnx("month", 21929, 1, "e")',
            `004` = 'sas_f_intnx("month", 21929, 1, "s")', `005` = 'sas_f_intnx("month", 21945, 1, "s")',
            `008` = 'sas_f_intnx("month", 21929, -1)'),
  PUT = c(`001` = 'sas_f_putn(123, "8.")', `003` = 'sas_f_putn(12.345, "8.2")', `005` = 'sas_f_putn(2.5, "1.")',
          `008` = 'sas_f_putn(1e10, "8.")', `011` = 'sas_f_putn(-1234.5, "dollar10.2")',
          `012` = 'sas_f_putn(0.125, "percent8.1")'),
  ROUND = c(`001` = 'sas_f_round(2.5)', `002` = 'sas_f_round(-2.5)', `005` = 'sas_f_round(2.45, 0.1)',
            `006` = 'sas_f_round(1.005, 0.01)', `007` = 'sas_f_round(1234.5678, 100)', `011` = 'sas_f_round(2/3, 1/3)'),
  SCAN = c(`001` = 'sas_f_scan("a b c", 1)', `004` = 'sas_f_scan("a b c", -1)', `003` = 'sas_f_scan("a b c", 4)',
           `011` = 'sas_f_scan("a,b,,c", 3)'),
  SUBSTR = c(`001` = 'sas_f_substr("abcdef", 2)', `002` = 'sas_f_substr("abcdef", 2, 3)',
             `006` = 'sas_f_substr("abcdef", 7)', `011` = 'sas_f_substr("abcdef", 2, 10)')
)

# what each pilot example demonstrates (shown above the call)
pilot_desc <- list(
  CATX = c(`001` = "Join the items with the delimiter '-'.",
           `002` = "Leading and trailing blanks of every item are removed before joining.",
           `003` = "A blank item is skipped (no doubled delimiter).",
           `006` = "A numeric missing item is not skipped: it is written as '.'."),
  COMPRESS = c(`001` = "With one argument COMPRESS removes all blanks.",
               `002` = "The second argument lists the characters to remove: here '-'.",
               `004` = "The list '' is one blank and modifier 'd' adds the digits: blanks and digits are removed.",
               `006` = "Modifier 'k' keeps instead of removing: the listed blank and the digits ('d') are kept.",
               `007` = "Modifier 'i' ignores case: removing 'a' also removes 'A'."),
  INDEX = c(`001` = "Position of the first occurrence of 'bc'.",
            `002` = "0 when the excerpt does not occur.",
            `004` = "The search is case-sensitive: 'b' does not match 'B'.",
            `005` = "A blank excerpt finds the first blank."),
  INPUT = c(`001` = "Read the text '123' with the 8. informat.",
            `004` = "Without a decimal point in the text, the informat 4.2 places two implied decimals: 12.34.",
            `005` = "A decimal point in the text overrides the informat's decimals.",
            `006` = "COMMA8. ignores the thousands separator.",
            `008` = "COMMA5. reads a value in parentheses as negative."),
  INTCK = c(`001` = "Discrete method (default): one month boundary between 31 Jan and 1 Feb 2020.",
            `002` = "Continuous method 'c': less than a full month between 31 Jan and 1 Feb 2020, so 0.",
            `006` = "One year boundary between 31 Dec 2019 and 1 Jan 2020.",
            `009` = "A start after the end counts boundaries backwards: -2 months."),
  INTNX = c(`001` = "Advance 1 month: the beginning of the next month (alignment 'b' is the default).",
            `002` = "Alignment 'e': the end of the next month.",
            `004` = "Alignment 's': the same day in the next month.",
            `005` = "Alignment 's' from 31 Jan: the same day does not exist in February, so the last day (29 Feb 2020).",
            `008` = "A negative increment goes back: the beginning of the previous month."),
  PUT = c(`001` = "Write 123 with the 8. format: right-aligned in 8 columns.",
          `003` = "8.2 rounds to two decimals.",
          `005` = "Format 1. rounds 2.5 half away from zero: 3.",
          `008` = "A value too wide for 8. is written in E notation.",
          `011` = "DOLLAR10.2: dollar sign, thousands separator, minus sign.",
          `012` = "PERCENT8.1 multiplies by 100 and adds a percent sign."),
  ROUND = c(`001` = "Round to the nearest integer: halves go away from zero.",
            `002` = "Negative halves also go away from zero: -3.",
            `005` = "Rounding unit 0.1: SAS treats 2.45 as the exact decimal, so 2.5 (base R gives 2.4).",
            `006` = "Rounding unit 0.01: 1.005 gives 1.01.",
            `007` = "Rounding unit 100: the nearest multiple of 100.",
            `011` = "A rounding unit of 1/3: the nearest multiple of 1/3."),
  SCAN = c(`001` = "The first word (default delimiters include the blank).",
           `004` = "A negative count counts words from the end: -1 is the last word.",
           `003` = "A count beyond the number of words gives a blank.",
           `011` = "Consecutive delimiters count as one, so the third word is 'c'."),
  SUBSTR = c(`001` = "From position 2 to the end.",
             `002` = "Three characters from position 2.",
             `006` = "A position beyond the end is invalid: blank result, NOTE and _ERROR_ = 1 in a DATA step.",
             `011` = "A length beyond the end: the rest of the string (with a NOTE and _ERROR_ = 1 in a DATA step).")
)

pilot_result <- function(root, fn, id) {
  case <- file.path(root, fn, paste0(fn, "_", id))
  meta <- readLines(file.path(case, "meta.yaml"))
  expr <- sub('^.*\\[(.*)\\]"?$', "\\1", grep("^title:", meta, value = TRUE))
  f <- file.path(case, "expected", "files", "results.txt")
  if (!file.exists(f)) return(NULL)
  r <- readLines(f, warn = FALSE)
  q <- grep(paste0("^@@Q ", fn, "_", id, " "), r, value = TRUE)
  n <- grep(paste0("^@@N ", fn, "_", id, " "), r, value = TRUE)
  val <- if (length(q)) sub(paste0("^@@Q ", fn, "_", id, " "), "", q[1]) else
    if (length(n)) trimws(sub("^.*best=", "", n[1])) else return(NULL)
  list(expr = gsub("\\\\'", "'", expr), value = val)
}

write_pilot <- function() {
  root <- file.path("tests", "conformance", "functions")
  if (!dir.exists(root)) stop("tests/conformance/functions not found (local conformance assets)")
  for (fn in names(pilot)) {
    out <- c(sprintf("# %s: each call is explained above it and followed by the result of the same call in SAS 9.4", fn),
             "# (dates are SAS day counts from 1960-01-01)")
    for (id in names(pilot[[fn]])) {
      g <- pilot_result(root, fn, id)
      if (is.null(g)) next
      d <- pilot_desc[[fn]][[id]] %||% ""
      out <- c(out, "", if (nzchar(d)) paste("#", strwrap(d, 78)),
               sprintf("%s  # SAS 9.4: %s = %s", pilot[[fn]][[id]], g$expr, g$value))
    }
    nm <- if (fn == "INPUT") "inputn" else if (fn == "PUT") "putn" else tolower(fn)
    writeLines(out, file.path("inst", "examples", paste0("sas_f_", nm, ".R")))
  }
  length(pilot)
}

# ---- 2. tools/fn_examples.txt -------------------------------------------------

fx_root <- file.path("tests", "conformance", "fn_examples")

parse_spec <- function(path = "tools/fn_examples.txt") {
  x <- readLines(path, warn = FALSE)
  out <- list()
  cur <- NULL
  desc <- ""
  for (ln in x) {
    if (grepl("^## ", ln)) {
      cur <- trimws(sub("^## ", "", ln))
      out[[cur]] <- list(name = cur, calls = list(), glob = character(), pre = character(), mode = "data", note = "")
      next
    }
    if (is.null(cur) || !nzchar(trimws(ln)) || grepl("^#", ln)) next
    if (grepl("^desc: ", ln)) {
      desc <- sub("^desc: ", "", ln)
      next
    }
    if (grepl("^(glob|pre|mode|note): ", ln)) {
      key <- sub(":.*$", "", ln)
      val <- sub("^[a-z]+: ", "", ln)
      if (key %in% c("glob", "pre")) out[[cur]][[key]] <- c(out[[cur]][[key]], val) else out[[cur]][[key]] <- val
      next
    }
    parts <- strsplit(ln, " => ", fixed = TRUE)[[1]]
    sas <- trimws(parts[1])
    r <- if (length(parts) > 1) trimws(paste(parts[-1], collapse = " => ")) else sas_to_r(sas)
    out[[cur]]$calls[[length(out[[cur]]$calls) + 1L]] <- list(sas = sas, r = r, desc = desc)
    desc <- ""
  }
  out
}

sas_day <- function(s) as.numeric(as.Date(s, "%d%b%Y") - as.Date("1960-01-01"))
sas_secs <- function(s) {
  p <- as.numeric(strsplit(s, ":", fixed = TRUE)[[1]])
  sum(p * c(3600, 60, 1)[seq_along(p)])
}
num_lit <- function(v) format(v, scientific = FALSE, digits = 15)

# SAS expression with literal arguments -> R call of the sas_f_* functions
# (. is the numeric missing value NA_real_, special missing values use sas_missing())
sas_to_r <- function(e) {
  pat <- paste0("'(?:[^']|'')*'(?:[dD][tT]|[dD]|[tT])?(?![A-Za-z0-9_])|\"(?:[^\"]|\"\")*\"(?:[dD][tT]|[dD]|[tT])?(?![A-Za-z0-9_])",
                "|[A-Za-z_][A-Za-z0-9_]*|(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?|\\.[A-Za-z_](?![A-Za-z0-9_])|\\.",
                "|<=|>=|\\^=|\\s+|.")
  tk <- regmatches(e, gregexpr(pat, e, perl = TRUE))[[1]]
  stopifnot(identical(paste(tk, collapse = ""), e))
  nonblank <- which(!grepl("^\\s+$", tk))
  out <- character(length(tk))
  for (i in seq_along(tk)) {
    t <- tk[i]
    nxt <- nonblank[nonblank > i][1]
    out[i] <- if (grepl("^['\"]", t)) {
      q <- substr(t, 1L, 1L)
      close <- max(gregexpr(q, t, fixed = TRUE)[[1]])
      body <- gsub(paste0(q, q), q, substr(t, 2L, close - 1L), fixed = TRUE)
      suf <- tolower(substring(t, close + 1L))
      switch(suf,
        d = num_lit(sas_day(body)),
        dt = num_lit(sas_day(sub(":.*$", "", body)) * 86400 + sas_secs(sub("^[^:]*:", "", body))),
        t = num_lit(sas_secs(body)),
        # SAS has no zero-length literal: '' is one blank
        encodeString(if (nzchar(body)) body else " ", quote = "\""))
    } else if (grepl("^[A-Za-z_]", t)) {
      if (!is.na(nxt) && tk[nxt] == "(") paste0("sas_f_", tolower(t)) else t
    } else if (t == ".") {
      "NA_real_"
    } else if (grepl("^\\.[A-Za-z_]$", t)) {
      sprintf("sas_missing(\".%s\")", toupper(substr(t, 2L, 2L)))
    } else if (t == "=") {
      "=="
    } else if (t == "^=") {
      "!="
    } else t
  }
  paste(out, collapse = "")
}

fx_source <- function(nm) {
  if (startsWith(nm, "CVT_")) return("SAS 9.4 Language Reference: Concepts, automatic numeric-character conversion")
  if (startsWith(nm, "SQL")) return("SAS 9.4 SQL Procedure User's Guide, sql-expression")
  sprintf("SAS 9.4 Functions and CALL Routines, %s Function", nm)
}

fx_program <- function(s) {
  hdr <- c(sprintf("/* fn_examples/%s: reference-page examples of %s", s$name, s$name),
           sprintf("   source: %s", fx_source(s$name)),
           sprintf("   tags: function, %s, fn_examples */", tolower(s$name)))
  k <- seq_along(s$calls)
  sas <- vapply(s$calls, `[[`, "", "sas")
  put <- paste0("put ", paste0("r", k, "=", collapse = " "), ";")
  body <- if (s$mode == "sql") {
    c("data work.one; x = 1; run;", s$glob, "proc sql;", "  create table work.ex as",
      paste0(ifelse(k == 1L, "  select ", "         "), sas, " as r", k, ifelse(k < length(k), ",", "")),
      "  from work.one;", "quit;", paste("data _null_; set work.ex;", put, "run;"))
  } else {
    c(s$glob, "data work.ex;", if (length(s$pre)) paste0("  ", s$pre), sprintf("  r%d = %s;", k, sas), paste0("  ", put), "run;")
  }
  c(hdr, body)
}

write_programs <- function(spec) {
  for (s in spec) {
    d <- file.path(fx_root, s$name)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(d, "program.sas")
    new <- fx_program(s)
    if (!file.exists(f) || !identical(readLines(f, warn = FALSE), new)) writeLines(new, f)
  }
  length(spec)
}

# SAS value of r<k> in the gold data set: list(type, v, miss) or NULL
sas_value <- function(ds, k) {
  col <- ds[[paste0("r", k)]]
  if (is.null(col)) return(NULL)
  if (is.character(col)) return(list(type = "C", v = sub("\\s+$", "", col[1])))
  v <- as.numeric(unclass(col))[1]
  tag <- haven::na_tag(col)[1]
  list(type = "N", v = v, miss = if (is.na(v)) (if (is.na(tag)) "." else paste0(".", toupper(tag))) else NA_character_)
}

son_value <- function(x) {
  if (is.logical(x)) x <- as.numeric(x)
  if (length(x) != 1L) return(list(type = "?", v = paste(format(x), collapse = " ")))
  if (is.character(x)) return(list(type = "C", v = sasonr::sas_rtrim(x)))
  if (!is.numeric(x)) return(list(type = "?", v = class(x)[1]))
  code <- sasonr:::sas_miss_code(x)
  list(type = "N", v = if (is.na(code)) x else NA_real_,
       miss = if (is.na(code)) NA_character_ else if (code == 1L) "." else paste0(".", sasonr:::miss_label(code)))
}

show_value <- function(g, exact = FALSE, digits = 15) {
  if (is.null(g)) return("(none)")
  if (g$type == "C") return(encodeString(g$v, quote = "\""))
  if (g$type != "N") return(g$v)
  if (!is.na(g$miss)) return(g$miss)
  v <- g$v
  if (exact) return(sprintf("%.17g", v))
  if (v == 0) return("0")
  if (abs(v) < 1e15 && v == round(v)) return(sprintf("%.0f", v))
  s <- formatC(v, digits = digits, format = "g")
  s <- sub("\\.?0+(e|$)", "\\1", s)
  sub("e([-+])0*(\\d)", "e\\1\\2", trimws(s))
}

compare_value <- function(a, b) {
  if (is.null(a)) return("no_sas_value")
  if (a$type != b$type) return("type")
  if (a$type == "C") return(if (identical(a$v, b$v)) "agree" else "mismatch")
  if (!is.na(a$miss) || !is.na(b$miss)) return(if (identical(a$miss, b$miss)) "agree" else "mismatch")
  if (identical(a$v, b$v)) return("agree")
  # no tolerance: a number that differs only in the last bits is "approximate" (not shown as an example)
  if (is.finite(a$v) && is.finite(b$v) && abs(a$v - b$v) <= 1e-9 * max(abs(a$v), abs(b$v), 1e-300)) return("approximate")
  "mismatch"
}

eval_call <- function(rc, env) {
  tryCatch({
    v <- NULL
    utils::capture.output(v <- suppressWarnings(suppressMessages(eval(parse(text = rc)[[1]], envir = env))))
    list(status = "ok", value = son_value(v))
  },
  sasonr_unsupported = function(e) list(status = "unsupported", msg = conditionMessage(e)),
  error = function(e) list(status = "error", msg = conditionMessage(e)))
}

no_example_reason <- c(
  sqlmonotonic = "MONOTONIC() numbers the rows of a PROC SQL query; outside a query it has no value to show"
)

run_examples <- function(spec) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE))
  rows <- list()
  for (s in spec) {
    case <- file.path(fx_root, s$name)
    dsf <- file.path(case, "expected", "datasets", "ex.sas7bdat")
    ds <- if (file.exists(dsf)) haven::read_sas(dsf) else NULL
    rc <- if (file.exists(file.path(case, "meta.yaml"))) {
      as.integer(sub("^sas_rc: *", "", grep("^sas_rc:", readLines(file.path(case, "meta.yaml")), value = TRUE)))
    } else NA_integer_
    rt_env <- get("rt", asNamespace("sasonr"))
    rt_env$prx <- list()
    env <- new.env(parent = globalenv())
    for (k in seq_along(s$calls)) {
      cl <- s$calls[[k]]
      g <- if (is.null(ds)) NULL else sas_value(ds, k)
      r <- eval_call(cl$r, env)
      st <- if (is.null(ds)) "no_gold" else if (r$status != "ok") r$status else compare_value(g, r$value)
      # within the tolerance: show only the significant digits both agree on
      dig <- if (st == "approximate") {
        rel <- abs(g$v - r$value$v) / max(abs(g$v), abs(r$value$v))
        max(6L, min(15L, floor(-log10(rel)) - 1L))
      } else 15L
      rows[[length(rows) + 1L]] <- data.frame(
        fn = s$name, k = k, sas = cl$sas, r = cl$r, desc = cl$desc %||% "", status = st, sas_rc = rc,
        sas_value = show_value(g, TRUE), son_value = if (r$status == "ok") show_value(r$value, TRUE) else r$msg,
        sas_show = show_value(g, digits = dig), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

write_fx_examples <- function(spec, res) {
  n <- 0L
  for (s in spec) {
    f <- file.path("inst", "examples", paste0("sas_f_", tolower(s$name), ".R"))
    ok <- res[res$fn == s$name & res$status == "agree", ]
    if (nrow(ok) == 0L) {
      if (file.exists(f)) unlink(f)
      next
    }
    dates <- any(grepl("'[dD]|'[dD][tT]|'[tT]", paste(ok$sas, collapse = " ")))
    out <- c(sprintf("# %s: each call is explained above it and followed by the result of the same call in SAS 9.4", s$name),
             if (dates) "# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)",
             if (nzchar(s$note)) paste("#", strwrap(s$note, 76)))
    for (j in seq_len(nrow(ok))) {
      out <- c(out, "", if (nzchar(ok$desc[j])) paste("#", strwrap(ok$desc[j], 78)),
               sprintf("%s  # SAS 9.4: %s = %s", ok$r[j], ok$sas[j], ok$sas_show[j]))
    }
    writeLines(out, f)
    n <- n + 1L
  }
  n
}

write_mismatch_md <- function(res, path, conf_csv = "") {
  bad <- res[!res$status %in% c("agree", "approximate"), ]
  tol <- res[res$status == "approximate", ]
  md_esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  fns <- unique(res$fn)
  agree_fns <- unique(res$fn[res$status == "agree"])
  out <- c(
    "# sas_f_* 参考页示例：sasonr 与 SAS 9.4 不一致的调用",
    "",
    sprintf("> %s 由 `sasonr/tools/gen_examples.R` 生成（勿手改）。用例：`sasonr/tests/conformance/fn_examples/<FN>/`（仅本地），SAS 金标准由 `tools/refresh_gold.py` 取得。", format(Sys.Date())),
    "",
    sprintf("- 函数 %d 个、调用 %d 个；逐位一致 %d，仅末位不同（approximate，不进示例）%d，不一致/不支持/出错 %d。",
            length(fns), nrow(res), sum(res$status == "agree"), nrow(tol), nrow(bad)),
    sprintf("- 有示例的函数 %d 个；没有任何一致调用的函数：%s。", length(agree_fns),
            if (length(setdiff(fns, agree_fns))) paste(setdiff(fns, agree_fns), collapse = ", ") else "无"),
    "- 只有逐位一致的调用进入 `inst/examples/`；下面两表的调用都没有进示例。",
    "- 状态：`mismatch` 值不同；`type` 类型不同；`unsupported` sasonr 报 `sasonr_unsupported`；`error` sasonr 报其它错误；`no_sas_value` SAS 未产生该结果。",
    "",
    "## 不一致清单",
    "",
    "| 函数 | SAS 调用 | sasonr 调用 | 状态 | SAS 结果 | sasonr 结果 |",
    "|---|---|---|---|---|---|",
    sprintf("| %s | `%s` | `%s` | %s | %s | %s |", bad$fn, md_esc(bad$sas), md_esc(bad$r), bad$status,
            md_esc(bad$sas_value), md_esc(gsub("\n", " ", bad$son_value))),
    "",
    "## 仅末位不同（approximate，相对误差 ≤ 1e-9，不算一致、不进示例）",
    "",
    "| 函数 | SAS 调用 | SAS 结果 | sasonr 结果 |",
    "|---|---|---|---|",
    sprintf("| %s | `%s` | %s | %s |", tol$fn, md_esc(tol$sas), tol$sas_value, tol$son_value)
  )
  if (nzchar(conf_csv) && file.exists(conf_csv)) {
    cr <- utils::read.csv(conf_csv, colClasses = "character")
    cr <- cr[startsWith(cr$case, "fn_examples/"), ]
    nb <- cr[cr$status != "pass", ]
    lay <- unlist(strsplit(nb$failed_layers[nzchar(nb$failed_layers)], "+", fixed = TRUE))
    out <- c(out, "", "## 整用例运行（`sas_run()` 翻译整个程序，六层比较）", "",
             "逐调用比较之外，每个用例也作为完整 SAS 程序由 `tools/run_conformance.R` 运行（DATA 步翻译路径，比较数据集、属性、log 关键消息、listing、PUT 输出）。",
             "",
             sprintf("- 用例 %d 个：%s。", nrow(cr), paste(sprintf("%s %d", names(table(cr$status)), as.integer(table(cr$status))), collapse = "，")),
             sprintf("- 不一致的层（用例数）：%s。", if (length(lay)) paste(sprintf("L-%s %d", toupper(names(table(lay))), as.integer(table(lay))), collapse = "，") else "无"),
             "- L-LOG 主要是 SAS 对缺失参数/无效参数写的汇总 NOTE（`Missing values were generated ...`、`Mathematical operations could not be performed ...`、`Invalid argument to function F(args)` 带参数值）sasonr 未写或写法不同；L-PUT 主要是 `put r=` 对空字符值 SAS 输出两个空格。",
             "",
             "| 用例 | 状态 | 不一致层 | 首个差异 |", "|---|---|---|---|",
             sprintf("| %s | %s | %s | %s |", sub("^fn_examples/", "", nb$case), nb$status, nb$failed_layers,
                     md_esc(substr(gsub("\n", " ", nb$detail), 1, 220))))
  }
  writeLines(out, path)
}

# ---- main -----------------------------------------------------------------------

spec <- parse_spec()
if (mode == "programs") {
  cat(write_programs(spec), "programs in", fx_root, "\n")
} else {
  dir.create(file.path("inst", "examples"), recursive = TRUE, showWarnings = FALSE)
  np <- write_pilot()
  res <- run_examples(spec)
  utils::write.csv(res, file.path(fx_root, "results.csv"), row.names = FALSE)
  nf <- write_fx_examples(spec, res)
  md <- file.path("..", "sasonr-conformance", "docs", "conformance", "fn_examples_mismatch.md")
  if (dir.exists(dirname(md))) write_mismatch_md(res, md, if (length(args) && args[1] != "examples") args[1] else "")
  print(table(res$status))
  cat("examples written:", np, "pilot +", nf, "fn_examples functions\n")
}
