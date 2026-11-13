#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# tools/report_conformance.R
#
# Render the sasonr conformance corpus (SAS gold standard) into one
# self-contained HTML report: inline CSS, no external resources, no new
# package dependencies beyond base R (jsonlite is used when available,
# otherwise the cache falls back to RDS).
#
# Usage:
#   Rscript tools/report_conformance.R \
#       --cases E:/dsh/sasonr-conformance/cases \
#       --out   E:/dsh/_dsh_tmp/conformance.html \
#       [--cache FILE] [--no-run]
#
#   --cases DIR   case root holding own/ and (optionally) ts1m2/
#   --out FILE    HTML output (default E:/dsh/_dsh_tmp/conformance.html)
#   --cache FILE  case-result cache; default <out>.json when jsonlite is
#                 installed, else <out>.rds
#   --no-run      do not execute cases, render from --cache
#
# The run itself uses pkgload::load_all() plus tests/testthat/helper-conformance.R
# (conf_run_case); each case is compared with the stored SAS gold.  The script
# never writes to the package, the case corpus or the registries.
# ---------------------------------------------------------------------------

options(warn = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = "1")

CACHE_FORMAT <- "sasonr-conformance-cache"
CACHE_VERSION <- 1L
LAYER_ORDER <- c("ds", "attr", "log", "lst", "put", "files")
STATUS_ORDER <- c(
  "pass", "both_error", "unsupported", "fail", "error", "lenient",
  "sas_error_unsupported", "sas_error_rerror", "no_gold", "no_program",
  "runner_error"
)
REGISTRY_STATUS_ORDER <- c(
  "mismatch", "approximate", "not_comparable", "unsupported", "partial",
  "unverified", "verified"
)
DEFAULT_CASES <- "E:/dsh/sasonr-conformance/cases"
DEFAULT_OUT <- "E:/dsh/_dsh_tmp/conformance.html"

# ---- small utilities -------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

jsonlite_ok <- function() requireNamespace("jsonlite", quietly = TRUE)

now_string <- function(t = Sys.time()) {
  tz <- tryCatch(Sys.timezone(), error = function(e) "")
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) tz <- "local"
  sprintf("%s (%s)", format(t, "%Y-%m-%d %H:%M:%S"), tz)
}

fmt_int <- function(n) {
  if (length(n) == 0L || is.na(n)) return("—")
  formatC(as.integer(n), format = "d", big.mark = ",")
}

fmt_pct <- function(k, n) {
  if (is.na(n) || n == 0L) return("—")
  sprintf("%.1f%%", 100 * k / n)
}

fmt_secs <- function(secs) {
  if (length(secs) == 0L || is.na(secs)) return("—")
  if (secs < 90) return(sprintf("%.1f s", secs))
  sprintf("%.1f s（约 %.1f 分钟）", secs, secs / 60)
}

num_or_na <- function(x) suppressWarnings(as.numeric(x))

trunc_chars <- function(x, n) {
  x <- as.character(x)
  if (is.na(x)) return("")
  if (nchar(x) <= n) x else paste0(substr(x, 1L, n), "…")
}

as_utf8 <- function(x) {
  x <- enc2utf8(as.character(x))
  bad <- !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], "latin1", "UTF-8", sub = "?")
  x
}

# HTML escaping: & < > " ' (the report only ever emits escaped user data)
html_escape <- function(x) {
  x <- as_utf8(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

esc <- html_escape

cls_status <- function(x) gsub("[^A-Za-z0-9_]+", "-", as.character(x))

git_sha <- function(pkg_root) {
  out <- tryCatch(
    suppressWarnings(system2(
      "git", c("-C", shQuote(pkg_root), "rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character()
  )
  if (length(out) >= 1L && grepl("^[0-9a-f]{7,}$", out[[1L]])) out[[1L]] else "unknown"
}

pkg_version <- function(pkg_root) {
  d <- file.path(pkg_root, "DESCRIPTION")
  if (!file.exists(d)) return("unknown")
  v <- tryCatch(read.dcf(d)[1L, "Version"], error = function(e) NA_character_)
  if (is.null(v) || is.na(v) || !nzchar(v)) "unknown" else as.character(v)
}

# ---- CLI -------------------------------------------------------------------

usage_text <- function() {
  paste0(
    "用法:\n",
    "  Rscript tools/report_conformance.R [--cases DIR] [--out FILE] [--cache FILE] [--no-run]\n\n",
    "选项:\n",
    "  --cases DIR   用例根目录（含 own/ 与可选的 ts1m2/），默认 ", DEFAULT_CASES, "\n",
    "  --out FILE    输出 HTML，默认 ", DEFAULT_OUT, "\n",
    "  --cache FILE  用例结果缓存；默认 <out>.json（jsonlite 可用）或 <out>.rds\n",
    "  --no-run      不运行用例，直接用 --cache 渲染（缓存不存在则报错）\n",
    "  -h, --help    显示本帮助\n\n",
    "示例:\n",
    "  Rscript tools/report_conformance.R --cases E:/dsh/sasonr-conformance/cases \\\n",
    "      --out E:/dsh/_dsh_tmp/conformance.html\n",
    "  Rscript tools/report_conformance.R --no-run \\\n",
    "      --cache E:/dsh/_dsh_tmp/conformance.json \\\n",
    "      --out E:/dsh/_dsh_tmp/conformance.html\n"
  )
}

parse_cli <- function(args) {
  out <- list(
    cases = DEFAULT_CASES, out = DEFAULT_OUT, cache = NA_character_,
    no_run = FALSE, help = FALSE, cases_given = FALSE
  )
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a %in% c("-h", "--help")) {
      out$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(a, "--no-run")) {
      out$no_run <- TRUE
      i <- i + 1L
      next
    }
    m <- regmatches(a, regexec("^--(cases|out|cache)=(.*)$", a))[[1L]]
    if (length(m) == 3L) {
      out[[m[[2L]]]] <- m[[3L]]
      if (identical(m[[2L]], "cases")) out$cases_given <- TRUE
      i <- i + 1L
      next
    }
    if (a %in% c("--cases", "--out", "--cache")) {
      if (i == length(args)) stop("选项 ", a, " 缺少参数", call. = FALSE)
      key <- substring(a, 3L)
      out[[key]] <- args[[i + 1L]]
      if (identical(key, "cases")) out$cases_given <- TRUE
      i <- i + 2L
      next
    }
    stop("未知参数: ", a, call. = FALSE)
  }
  out
}

# ---- case discovery --------------------------------------------------------

list_cases <- function(root) {
  progs <- list.files(
    root, pattern = "^program\\.sas$", recursive = TRUE,
    full.names = TRUE, all.files = FALSE
  )
  sort(unique(dirname(progs)))
}

# `--cases` may point at the corpus (own/, ts1m2/ below it) or directly at one
# case root.  books/ is deliberately not picked up: its programs live in
# SASONR_BOOKS_ROOT and its gold is not part of the local corpus.
discover_roots <- function(cases_dir) {
  d <- normalizePath(cases_dir, winslash = "/", mustWork = TRUE)
  known <- c("own", "ts1m2")
  present <- known[dir.exists(file.path(d, known))]
  if (length(present)) {
    roots <- file.path(d, present)
    names(roots) <- present
    return(roots)
  }
  r <- d
  names(r) <- basename(d)
  r
}

# Grouping key.  rounding/ is split by its proc/ and values/ sub-area; every
# other area is one group.  Non-own roots keep their root name in the key so
# the own-only baseline stays visible next to the local TS1M2 gold.
case_group <- function(root_name, rel) {
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  area <- parts[[1L]]
  subarea <- NA_character_
  if (identical(area, "rounding") && length(parts) >= 3L && parts[[2L]] %in% c("proc", "values")) {
    subarea <- parts[[2L]]
    label <- paste(parts[-(1:2)], collapse = "/")
    key <- paste0("rounding/", subarea)
  } else {
    rest <- parts[-1L]
    label <- if (length(rest)) paste(rest, collapse = "/") else area
    key <- area
  }
  if (!identical(root_name, "own")) key <- paste0(root_name, "/", key)
  list(area = area, subarea = subarea, label = label, group = key)
}

# ---- cache -----------------------------------------------------------------

default_cache_path <- function(out) {
  base <- sub("\\.html?$", "", out, ignore.case = TRUE)
  paste0(base, if (jsonlite_ok()) ".json" else ".rds")
}

cache_format_of <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  h <- readBin(con, what = "raw", n = 4L)
  if (length(h) >= 1L && rawToChar(h[[1L]]) == "{") return("json")
  "rds"
}

write_cache <- function(payload, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  if (grepl("\\.rds$", path, ignore.case = TRUE) || !jsonlite_ok()) {
    saveRDS(payload, tmp)
  } else {
    jsonlite::write_json(
      payload, tmp, auto_unbox = TRUE, pretty = TRUE,
      null = "null", digits = NA
    )
  }
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

read_cache <- function(path) {
  fmt <- cache_format_of(path)
  if (identical(fmt, "json")) {
    if (!jsonlite_ok()) {
      stop("缓存是 JSON，但 jsonlite 不可用；请用 --cache 指向 .rds 缓存", call. = FALSE)
    }
    p <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else {
    p <- readRDS(path)
  }
  if (!identical(p$format, CACHE_FORMAT)) {
    stop("缓存格式不匹配（期望 format=", CACHE_FORMAT, "）: ", path, call. = FALSE)
  }
  if (!identical(as.integer(p$version), CACHE_VERSION)) {
    stop("缓存版本不匹配（期望 ", CACHE_VERSION, "）: ", path, call. = FALSE)
  }
  p$roots <- as.character(unlist(p$roots %||% character()))
  p$cases <- lapply(p$cases %||% list(), function(cs) {
    cs$root <- as.character(cs$root %||% "")[[1L]]
    cs$rel <- as.character(cs$rel %||% "")[[1L]]
    cs$status <- as.character(cs$status %||% "unknown")[[1L]]
    cs$msg <- if (length(cs$msg)) as.character(cs$msg)[[1L]] else ""
    cs$layers <- as.list(cs$layers %||% list())
    cs$diffs <- as.list(cs$diffs %||% list())
    cs$secs <- suppressWarnings(as.numeric(cs$secs %||% NA_real_))[1L]
    cs$sas_version <- as.character(cs$sas_version %||% "")[[1L]]
    cs$known_fail <- as.character(cs$known_fail %||% "")[[1L]]
    cs$sas_rc <- suppressWarnings(as.integer(cs$sas_rc %||% NA_integer_))[1L]
    cs
  })
  p
}

# ---- running the corpus ----------------------------------------------------

run_cases <- function(roots, pkg_root, cache_path, cases_dir) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("运行用例需要 pkgload（pkgload::load_all）", call. = FALSE)
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("运行用例需要 haven（helper-conformance.R 读取 SAS 数据）", call. = FALSE)
  }
  message("载入 sasonr 包（pkgload::load_all）…")
  suppressMessages(pkgload::load_all(pkg_root, quiet = TRUE, helpers = FALSE))
  # As tools/run_conformance.R: devtools::load_all() leaves closures
  # uncompiled; byte-compiling the namespace once makes the run ~1/3 faster.
  local({
    ns <- asNamespace("sasonr")
    for (nm in ls(ns, all.names = TRUE)) {
      f <- get(nm, envir = ns, inherits = FALSE)
      if (is.function(f) && !is.primitive(f)) {
        unlockBinding(nm, ns)
        assign(nm, compiler::cmpfun(f), envir = ns)
        lockBinding(nm, ns)
      }
    }
  })
  env <- new.env(parent = asNamespace("sasonr"))
  helper <- file.path(pkg_root, "tests", "testthat", "helper-conformance.R")
  if (!file.exists(helper)) stop("找不到 helper: ", helper, call. = FALSE)
  sys.source(helper, envir = env)

  per_root <- lapply(roots, list_cases)
  total <- sum(vapply(per_root, length, integer(1L)))
  if (total == 0L) stop("用例目录里没有找到 program.sas", call. = FALSE)

  cases <- vector("list", total)
  started <- Sys.time()
  i <- 0L
  for (root_name in names(roots)) {
    for (case_dir in per_root[[root_name]]) {
      i <- i + 1L
      rel <- substring(case_dir, nchar(roots[[root_name]]) + 2L)
      t1 <- Sys.time()
      r <- tryCatch(
        env$conf_run_case(case_dir),
        error = function(e) {
          list(
            status = "runner_error",
            msg = paste0("[runner] ", conditionMessage(e)),
            layers = logical(), diffs = character()
          )
        }
      )
      secs <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
      meta <- tryCatch(
        env$conf_read_meta(file.path(case_dir, "meta.yaml")),
        error = function(e) list()
      )
      cases[[i]] <- list(
        root = root_name,
        rel = rel,
        status = as.character(r$status %||% "runner_error")[[1L]],
        msg = as.character(r$msg %||% "")[[1L]],
        layers = as.list(r$layers %||% logical()),
        diffs = as.list(r$diffs %||% character()),
        secs = round(secs, 3),
        sas_version = as.character(meta$sas_version %||% "")[[1L]],
        known_fail = paste(as.character(meta$known_fail %||% character()), collapse = "+"),
        sas_rc = suppressWarnings(as.integer(meta$sas_rc %||% NA_integer_))[1L]
      )
      if (i %% 50L == 0L || i == total) {
        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        eta <- if (i > 0L) elapsed / i * (total - i) else 0
        message(sprintf(
          "[%s] %d/%d cases  %.0fs elapsed  ~%.0fs left  last=%s",
          root_name, i, total, elapsed, eta, cases[[i]]$status
        ))
        if (i %% 100L == 0L && !is.na(cache_path)) {
          partial <- build_payload(
            cases[seq_len(i)], roots, started = started,
            complete = FALSE, cases_dir = cases_dir, pkg_root = pkg_root
          )
          try(write_cache(partial, cache_path), silent = TRUE)
        }
      }
    }
  }
  build_payload(
    cases, roots, started = started, complete = TRUE,
    cases_dir = cases_dir, pkg_root = pkg_root
  )
}

build_payload <- function(cases, roots, started, complete, cases_dir, pkg_root) {
  finished <- Sys.time()
  list(
    format = CACHE_FORMAT,
    version = CACHE_VERSION,
    complete = isTRUE(complete),
    generated = now_string(finished),
    started = now_string(started),
    finished = now_string(finished),
    elapsed_sec = round(as.numeric(difftime(finished, started, units = "secs")), 2),
    cases_dir = cases_dir,
    roots = as.list(names(roots)),
    sasonr_sha = git_sha(pkg_root),
    sasonr_version = pkg_version(pkg_root),
    r_version = R.version.string,
    strict = isTRUE(getOption("sasonr.strict", TRUE)),
    cases = cases
  )
}

# ---- fail-loud reason classification --------------------------------------
#
# Priority order is fixed: the first matching rule wins, so every case gets
# exactly one reason code.  Patterns are literal (fixed = TRUE).
REASON_RULES <- list(
  gold_missing = list(
    label = "gold_missing",
    desc = "缺少 SAS gold / 没有验证用例（本机无法产出 gold）",
    patterns = c("no SAS verification cases", "no gold", "gold", "not verified")
  ),
  algorithm_undocumented = list(
    label = "algorithm_undocumented",
    desc = "算法未文档化、无法确定，或只能近似",
    patterns = c("undocumented", "not determined", "not verified against SAS", "approximate")
  ),
  version_difference = list(
    label = "version_difference",
    desc = "SAS 版本差异（TS1M2 / TS1M7）",
    patterns = c("TS1M", "version")
  ),
  domain_guard = list(
    label = "domain_guard",
    desc = "常规域守卫（Unsupported: / 组合未验证）",
    patterns = c("not verified", "Unsupported:")
  ),
  not_implemented = list(
    label = "not_implemented",
    desc = "未实现（函数 / 过程 / 选项）",
    patterns = c("not implemented", "unknown function", "unknown procedure", "unknown option")
  )
)

classify_reason <- function(msg) {
  m <- gsub("\\s+", " ", as.character(msg %||% ""))
  if (!nzchar(m)) return("other")
  ml <- tolower(m)
  for (code in names(REASON_RULES)) {
    for (p in REASON_RULES[[code]]$patterns) {
      hit <- if (identical(p, "version")) {
        # whole word: "conversion" is not a SAS version difference
        grepl("\\bversion\\b", ml, perl = TRUE)
      } else {
        grepl(tolower(p), ml, fixed = TRUE)
      }
      if (hit) return(code)
    }
  }
  "other"
}

# both_error is treated as agreement (SAS and sasonr reported a SAS ERROR at
# the same step and the compared layers agree), so it does not take part in
# the fail-loud attribution; the report states this explicitly.
is_ok_status <- function(status) status %in% c("pass", "both_error")

# ---- statistics ------------------------------------------------------------

status_counts <- function(cases) {
  st <- vapply(cases, function(cs) as.character(cs$status)[[1L]], "")
  lv <- c(STATUS_ORDER, sort(setdiff(unique(st), STATUS_ORDER)))
  setNames(vapply(lv, function(s) sum(st == s), integer(1L)), lv)
}

summarise_cases <- function(cases) {
  st <- vapply(cases, function(cs) as.character(cs$status)[[1L]], "")
  n <- length(st)
  known <- c("pass", "both_error", "unsupported", "fail", "error")
  list(
    n = n,
    pass = sum(st == "pass"),
    both_error = sum(st == "both_error"),
    unsupported = sum(st == "unsupported"),
    fail = sum(st == "fail"),
    error = sum(st == "error"),
    other = sum(!st %in% known)
  )
}

# ---- cache / HTML writing --------------------------------------------------

write_html <- function(html, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(html), con, useBytes = TRUE, sep = "\n")
  invisible(path)
}

# ===========================================================================
# HTML rendering
# ===========================================================================

css_text <- function() {
  paste(c(
    ":root{--fg:#1f2328;--muted:#59636e;--bg:#f6f8fa;--card:#fff;--border:#d1d9e0;",
    "--pass:#1a7f37;--both:#0969da;--unsup:#9a6700;--fail:#cf222e;--err:#a40e26;--other:#6e7781;",
    "--pass-bg:#dafbe1;--both-bg:#ddf4ff;--unsup-bg:#fff8c5;--fail-bg:#ffebe9;--other-bg:#eaeef2}",
    "*{box-sizing:border-box}",
    "body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.6 -apple-system,BlinkMacSystemFont,'Segoe UI','Noto Sans SC','Microsoft YaHei',sans-serif}",
    "code,pre,.mono,.case-path{font-family:ui-monospace,SFMono-Regular,'Cascadia Code',Consolas,'Liberation Mono',monospace}",
    "a{color:#0969da;text-decoration:none}a:hover{text-decoration:underline}",
    ".wrap{max-width:1200px;margin:0 auto;padding:0 24px}",
    ".hero{background:linear-gradient(135deg,#0b1f33 0%,#123a5c 60%,#0d5c63 100%);color:#fff;padding:38px 0 26px;border-bottom:3px solid #1f6feb}",
    ".hero .kicker{font-size:12px;letter-spacing:.14em;text-transform:uppercase;opacity:.72}",
    ".hero h1{margin:6px 0;font-size:34px;letter-spacing:-.01em}",
    ".hero .sub{margin:0;opacity:.85;font-size:14px}",
    ".hero .hero-chips{margin:14px 0 0}",
    ".hero .hero-chips .chip{box-shadow:0 1px 3px rgba(0,0,0,.28)}",
    ".hero code{background:rgba(255,255,255,.15);padding:1px 6px;border-radius:4px}",
    "main{max-width:1200px;margin:0 auto;padding:24px 24px 56px}",
    "nav.toc{display:flex;flex-wrap:wrap;gap:8px;margin:0 0 18px}",
    "nav.toc a{background:#fff;border:1px solid var(--border);border-radius:999px;padding:4px 12px;font-size:13px}",
    ".card{background:var(--card);border:1px solid var(--border);border-radius:10px;padding:20px 22px;margin:0 0 20px;box-shadow:0 1px 2px rgba(31,35,40,.04)}",
    "h2{font-size:20px;margin:0 0 14px}h3{font-size:15px;margin:0 0 8px}",
    ".muted{color:var(--muted)}.small{font-size:12px}",
    ".meta-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:10px 22px}",
    ".meta-grid>div{display:flex;flex-direction:column;gap:1px;min-width:0}",
    ".meta-grid .k{font-size:11px;color:var(--muted);letter-spacing:.06em;text-transform:uppercase}",
    ".meta-grid .v{font-size:13px;overflow-wrap:anywhere}",
    ".chips{display:flex;flex-wrap:wrap;gap:8px;margin:12px 0 4px}",
    ".chip{display:inline-block;border-radius:999px;padding:2px 10px;font-size:12px;font-weight:600;border:1px solid transparent;white-space:nowrap}",
    ".chip b{font-weight:700}",
    ".chip.status-pass{background:var(--pass-bg);color:var(--pass);border-color:#aceebb}",
    ".chip.status-both_error{background:var(--both-bg);color:var(--both);border-color:#b6e3ff}",
    ".chip.status-unsupported{background:var(--unsup-bg);color:var(--unsup);border-color:#f2d98c}",
    ".chip.status-fail{background:var(--fail-bg);color:var(--fail);border-color:#ffcecb}",
    ".chip.status-error{background:var(--fail-bg);color:var(--err);border-color:#ffcecb}",
    ".chip.status-lenient,.chip.status-sas_error_unsupported,.chip.status-sas_error_rerror,.chip.status-no_gold,.chip.status-no_program,.chip.status-runner_error,.chip.status-unknown{background:var(--other-bg);color:var(--other);border-color:#d1d9e0}",
    ".chip.status-mismatch{background:var(--fail-bg);color:var(--fail);border-color:#ffcecb}",
    ".chip.status-approximate{background:var(--unsup-bg);color:var(--unsup);border-color:#f2d98c}",
    ".chip.status-partial{background:#ddf4ff;color:#0a5ea8;border-color:#b6e3ff}",
    ".chip.status-unverified,.chip.status-not_comparable{background:var(--other-bg);color:var(--other);border-color:#d1d9e0}",
    ".chip.reason{background:#f6f8fa;color:#4b5563;border-color:#d1d9e0;font-weight:500}",
    ".roots{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:14px;margin:16px 0 4px}",
    ".root-card{border:1px solid var(--border);border-radius:8px;padding:12px 14px}",
    ".root-card h3{display:flex;justify-content:space-between;align-items:baseline;gap:10px}",
    ".bar{display:flex;height:10px;border-radius:999px;overflow:hidden;background:#eaeef2;margin:8px 0 6px}",
    ".bar .seg{display:block;height:100%}",
    ".seg.pass{background:#2da44e}.seg.both_error{background:#54aeff}.seg.unsupported{background:#d4a72c}.seg.fail,.seg.error{background:#e16f24}.seg.other{background:#8c959f}",
    ".root-stats{font-size:12px;color:var(--muted)}",
    "table{border-collapse:collapse;width:100%;font-size:13px}",
    "th,td{border-bottom:1px solid #eaeef2;padding:6px 9px;text-align:left;vertical-align:top}",
    "th{font-size:11px;letter-spacing:.05em;text-transform:uppercase;color:var(--muted);background:#f6f8fa}",
    "td.num,th.num{text-align:right;font-variant-numeric:tabular-nums}",
    "tbody tr:hover{background:#f6f8fa}",
    "tr.subtotal td{background:#f6f8fa;font-weight:600}",
    "tr.total td{border-top:2px solid var(--border);font-weight:700}",
    ".areas a{font-weight:600}",
    ".area .area-meta{font-size:12.5px;color:var(--muted);margin:-6px 0 8px}",
    ".area .area-desc{font-size:13px;color:var(--muted);margin:0 0 12px}",
    ".case-list{border:1px solid var(--border);border-radius:8px;padding:4px 8px;max-height:640px;overflow:auto;background:#fff}",
    ".case{padding:4px;border-bottom:1px solid #f0f2f5;font-size:13px}",
    "div.case,details.case>summary{display:grid;grid-template-columns:10px minmax(200px,1.4fr) auto auto minmax(110px,1.8fr);gap:8px;align-items:baseline}",
    ".case:last-child{border-bottom:none}",
    ".case .dot{width:8px;height:8px;border-radius:50%;background:var(--other);align-self:center}",
    ".case.status-pass .dot{background:#2da44e}.case.status-both_error .dot{background:#54aeff}",
    ".case.status-unsupported .dot{background:#d4a72c}.case.status-fail .dot{background:#e16f24}",
    ".case.status-error .dot{background:#cf222e}",
    ".case.status-pass{box-shadow:inset 3px 0 0 #2da44e}",
    ".case.status-both_error{box-shadow:inset 3px 0 0 #54aeff}",
    ".case.status-unsupported{box-shadow:inset 3px 0 0 #d4a72c}",
    ".case.status-fail{box-shadow:inset 3px 0 0 #e16f24}",
    ".case.status-error{box-shadow:inset 3px 0 0 #cf222e}",
    ".case.status-lenient,.case.status-sas_error_unsupported,.case.status-sas_error_rerror,.case.status-no_gold,.case.status-no_program,.case.status-runner_error,.case.status-unknown{box-shadow:inset 3px 0 0 #8c959f}",
    ".case-path{font-size:12.5px;overflow-wrap:anywhere}",
    ".case-note{color:var(--muted);font-size:12px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}",
    "details.case{padding:0}",
    "details.case>summary{display:grid;grid-template-columns:10px minmax(200px,1.4fr) auto auto minmax(110px,1.8fr);gap:8px;align-items:baseline;cursor:pointer;list-style:none;padding:4px;border-radius:4px}",
    "details.case>summary::-webkit-details-marker{display:none}",
    "details.case>summary:hover{background:#fff8c5}",
    "details.case[open]>summary{background:#fff8c5;border-radius:4px 4px 0 0}",
    ".case-body{background:#fffdf5;border:1px solid #f0e6c0;border-radius:0 0 6px 6px;padding:12px 14px;margin:0 0 6px;font-size:13px}",
    ".kv{display:grid;grid-template-columns:110px minmax(0,1fr);gap:4px 12px;margin:0 0 10px}",
    ".kv .k{color:var(--muted);font-size:12px;padding-top:1px}",
    ".kv .v{overflow-wrap:anywhere}",
    ".block{margin:10px 0 0}",
    ".block-title{font-size:11px;letter-spacing:.06em;text-transform:uppercase;color:var(--muted);margin:0 0 5px}",
    ".layers{display:flex;flex-wrap:wrap;gap:6px}",
    ".layer{display:inline-block;border-radius:4px;padding:1px 7px;font-size:11.5px;font-family:ui-monospace,Consolas,monospace;border:1px solid}",
    ".layer.ok{background:#dafbe1;color:#116329;border-color:#aceebb}",
    ".layer.bad{background:#ffebe9;color:#a40e26;border-color:#ffcecb}",
    ".layer.na{background:#eaeef2;color:#6e7781;border-color:#d1d9e0}",
    "pre{background:#0d1117;color:#e6edf3;padding:10px 12px;border-radius:6px;overflow:auto;font-size:12px;line-height:1.5;white-space:pre-wrap;overflow-wrap:anywhere;margin:0;max-height:360px}",
    ".diff{margin:10px 0 0}",
    ".diff-name{font-family:ui-monospace,Consolas,monospace;font-size:12px;font-weight:700;margin:0 0 4px;color:#a40e26}",
    ".callout{border-left:4px solid #1f6feb;background:#ddf4ff;padding:10px 14px;border-radius:0 6px 6px 0;margin:14px 0 0;font-size:13.5px}",
    ".banner{background:#fff8c5;border:1px solid #f2d98c;padding:10px 14px;border-radius:8px;margin:0 0 18px;font-size:13px}",
    ".banner.bad{background:#ffebe9;border-color:#ffcecb}",
    ".notes li{margin:4px 0}",
    "footer{color:var(--muted);font-size:12.5px;border-top:1px solid var(--border);margin-top:8px;padding-top:18px}",
    "footer p{margin:6px 0}",
    ".tag{display:inline-block;background:#eaeef2;border-radius:4px;padding:0 6px;font-size:11px;color:#4b5563}",
    "@media (max-width:760px){.case,details.case>summary{grid-template-columns:10px 1fr auto}.case-note{display:none}}",
    "@media print{body{background:#fff}.card{box-shadow:none}.case-list{max-height:none;overflow:visible}pre{max-height:none}}"
  ), collapse = "\n")
}

status_chip <- function(status, n = NULL) {
  sprintf(
    '<span class="chip status-%s">%s%s</span>',
    cls_status(status), esc(status),
    if (is.null(n)) "" else sprintf(" <b>%s</b>", fmt_int(n))
  )
}

reason_chip <- function(reason) {
  if (is.na(reason) || !nzchar(reason)) return("")
  sprintf('<span class="chip reason" title="fail-loud 原因码">%s</span>', esc(reason))
}

layer_chip <- function(name, value) {
  if (is.null(value)) {
    return(sprintf('<span class="layer na">%s —</span>', esc(name)))
  }
  if (isTRUE(value)) {
    sprintf('<span class="layer ok">%s ✓</span>', esc(name))
  } else {
    sprintf('<span class="layer bad">%s ✗</span>', esc(name))
  }
}

stacked_bar <- function(cases) {
  s <- summarise_cases(cases)
  if (s$n == 0L) return("")
  segs <- c(
    pass = s$pass, both_error = s$both_error, unsupported = s$unsupported,
    fail = s$fail, error = s$error, other = s$other
  )
  parts <- character()
  for (nm in names(segs)) {
    if (segs[[nm]] <= 0L) next
    parts <- c(parts, sprintf(
      '<span class="seg %s" style="width:%.2f%%" title="%s %d"></span>',
      nm, 100 * segs[[nm]] / s$n, esc(nm), as.integer(segs[[nm]])
    ))
  }
  paste(parts, collapse = "")
}

area_row <- function(label, cases, id = NULL, klass = "") {
  s <- summarise_cases(cases)
  name_cell <- if (is.null(id)) esc(label) else sprintf('<a href="#%s">%s</a>', id, esc(label))
  sprintf(
    paste0(
      '<tr class="%s"><td>%s</td><td class="num">%d</td>',
      '<td class="num">%d</td><td class="num">%d</td><td class="num">%d</td>',
      '<td class="num">%d</td><td class="num">%d</td><td class="num">%d</td>',
      '<td class="num">%s</td><td class="num">%s</td></tr>'
    ),
    klass, name_cell, s$n, s$pass, s$both_error, s$unsupported, s$fail,
    s$error, s$other, fmt_pct(s$pass, s$n), fmt_pct(s$pass + s$both_error, s$n)
  )
}

area_id <- function(group) paste0("area-", gsub("[^A-Za-z0-9]+", "-", group))

case_row <- function(cs, idx, cases_dir, area_key) {
  st <- as.character(cs$status)[[1L]]
  cls <- cls_status(st)
  id <- sprintf("case-%s-%04d", area_key, idx)
  full <- paste(cs$root, cs$rel, sep = "/")
  label <- esc(case_group(cs$root, cs$rel)$label)
  layers <- cs$layers %||% list()
  n_ok <- sum(vapply(layers, isTRUE, logical(1L)))
  diff_names <- names(cs$diffs)[vapply(cs$diffs, function(x) nzchar(as.character(x)[[1L]]), logical(1L))]
  msg <- as.character(cs$msg %||% "")
  if (is_ok_status(st)) {
    note <- if (identical(st, "both_error")) {
      "both_error：SAS 与 sasonr 同一步报错，比较层一致"
    } else {
      sprintf("layers %d/%d 一致", n_ok, length(layers))
    }
    return(sprintf(
      '<div class="case status-%s" id="%s"><span class="dot"></span><span class="case-path" title="%s">%s</span>%s<span class="case-note">%s</span></div>',
      cls, id, esc(full), label, status_chip(st), esc(note)
    ))
  }
  reason <- classify_reason(msg)
  note <- if (length(diff_names)) {
    first <- as.character(cs$diffs[[diff_names[[1L]]]])[[1L]]
    sprintf("%s：%s", diff_names[[1L]], trunc_chars(gsub("\\s+", " ", first), 90))
  } else if (nzchar(msg)) {
    trunc_chars(gsub("\\s+", " ", msg), 100)
  } else {
    "无 layer 差异细节"
  }
  kv <- c(
    sprintf('<span class="k">用例路径</span><span class="v"><code>%s</code></span>', esc(full)),
    sprintf('<span class="k">状态</span><span class="v">%s %s</span>', status_chip(st), reason_chip(reason)),
    sprintf(
      '<span class="k">gold 目录</span><span class="v"><code>%s</code></span>',
      esc(file.path(cases_dir, cs$root, cs$rel, "expected"))
    )
  )
  if (nzchar(cs$sas_version %||% "")) {
    kv <- c(kv, sprintf(
      '<span class="k">gold 的 SAS</span><span class="v">%s</span>',
      esc(cs$sas_version)
    ))
  }
  if (nzchar(cs$known_fail %||% "")) {
    kv <- c(kv, sprintf(
      '<span class="k">known_fail</span><span class="v">%s</span>',
      esc(cs$known_fail)
    ))
  }
  if (!is.na(cs$secs %||% NA_real_)) {
    kv <- c(kv, sprintf(
      '<span class="k">本用例耗时</span><span class="v">%s</span>',
      fmt_secs(cs$secs)
    ))
  }

  layer_chips <- vapply(
    union(LAYER_ORDER, names(layers)),
    function(nm) layer_chip(nm, layers[[nm]]),
    ""
  )

  if (nzchar(msg)) {
    shown <- if (nchar(msg) > 400L) substr(msg, 1L, 400L) else msg
    cut_note <- if (nchar(msg) > 400L) {
      sprintf('（已截断：显示前 400 / 共 %d 字符）', nchar(msg))
    } else {
      ""
    }
    msg_block <- c(
      '<div class="block"><div class="block-title">fail-loud 消息</div>',
      sprintf('<pre class="msg">%s</pre>', esc(shown)),
      if (nzchar(cut_note)) sprintf('<div class="muted small">%s</div>', esc(cut_note)),
      '</div>'
    )
  } else {
    msg_block <- c(
      '<div class="block"><div class="block-title">fail-loud 消息</div>',
      '<div class="muted">（该状态没有消息：由 layer 差异直接判定）</div></div>'
    )
  }

  diff_blocks <- character()
  for (nm in names(cs$diffs)) {
    txt <- as.character(cs$diffs[[nm]])[[1L]]
    if (!nzchar(txt)) next
    diff_blocks <- c(
      diff_blocks,
      sprintf(
        '<div class="diff"><div class="diff-name">%s</div><pre>%s</pre></div>',
        esc(nm), esc(trunc_chars(txt, 2000))
      )
    )
  }
  if (!length(diff_blocks)) {
    diff_blocks <- '<div class="block"><div class="block-title">layer 差异</div><div class="muted">没有 layer 差异记录（运行在比较前中止）。</div></div>'
  } else {
    diff_blocks <- c('<div class="block"><div class="block-title">layer 差异（首个差异 / 多行原文）</div>', diff_blocks, '</div>')
  }

  c(
    sprintf('<details class="case status-%s" id="%s"><summary>', cls, id),
    '<span class="dot"></span>',
    sprintf('<span class="case-path" title="%s">%s</span>', esc(full), label),
    status_chip(st),
    reason_chip(reason),
    sprintf('<span class="case-note">%s</span>', esc(note)),
    '</summary>',
    '<div class="case-body">',
    sprintf('<div class="kv">%s</div>', paste(kv, collapse = "")),
    msg_block,
    sprintf(
      '<div class="block"><div class="block-title">layers（%d/%d 一致）</div><div class="layers">%s</div></div>',
      n_ok, length(layers), paste(layer_chips, collapse = "")
    ),
    diff_blocks,
    '</div></details>'
  )
}

area_table <- function(groups, roots) {
  rows <- character()
  for (g in names(groups)) {
    rows <- c(rows, area_row(g, groups[[g]], id = area_id(g)))
  }
  if (length(roots) > 1L) {
    for (r in roots) {
      cases <- Filter(function(cs) identical(cs$root, r), unlist(groups, recursive = FALSE))
      rows <- c(rows, area_row(paste0(r, " 小计"), cases, klass = "subtotal"))
    }
  }
  all_cases <- unlist(groups, recursive = FALSE)
  rows <- c(rows, area_row("合计", all_cases, klass = "total"))
  c(
    '<table class="areas"><thead><tr>',
    '<th>区域</th><th class="num">用例</th><th class="num">pass</th>',
    '<th class="num">both_error</th><th class="num">unsupported</th>',
    '<th class="num">fail</th><th class="num">error</th><th class="num">其他</th>',
    '<th class="num">pass 率</th><th class="num">一致率</th>',
    '</tr></thead><tbody>',
    rows,
    '</tbody></table>'
  )
}

meta_grid <- function(pairs) {
  c(
    '<div class="meta-grid">',
    vapply(pairs, function(p) sprintf(
      '<div><span class="k">%s</span><span class="v">%s</span></div>',
      esc(p[[1L]]), p[[2L]]
    ), ""),
    '</div>'
  )
}

status_chips <- function(counts, only_nonzero_extra = TRUE) {
  core <- c("pass", "both_error", "unsupported", "fail", "error")
  out <- vapply(core, function(s) status_chip(s, counts[[s]] %||% 0L), "")
  extra <- setdiff(names(counts), core)
  if (only_nonzero_extra) extra <- extra[counts[extra] > 0L]
  c(out, vapply(extra, function(s) status_chip(s, counts[[s]]), ""))
}

render_summary <- function(cases, groups, root_names, ctx, payload) {
  counts <- status_counts(cases)
  n <- length(cases)
  pairs <- list(
    list("生成时间", esc(ctx$generated)),
    list("sasonr git sha", sprintf("<code>%s</code>", esc(ctx$git_sha))),
    list("sasonr 版本", esc(ctx$pkg_version)),
    list("R 版本", esc(payload$r_version %||% R.version.string)),
    list(
      "数据模式",
      if (ctx$cache_used) "缓存渲染（--no-run，不重跑用例）" else "本次运行（全部用例已执行）"
    ),
    list(
      "严格模式",
      if (isTRUE(payload$strict)) "TRUE（未验证 / 近似特性 fail loud）" else "FALSE"
    ),
    list("用例目录", sprintf("<code>%s</code>", esc(ctx$cases_dir))),
    list("用例总数", fmt_int(n)),
    list(
      "缓存文件",
      sprintf("<code>%s</code>%s", esc(ctx$cache_path),
              if (ctx$cache_used) "" else "（本次运行写入）")
    ),
    list(
      "运行耗时",
      if (is.na(payload$elapsed_sec %||% NA_real_)) "—" else fmt_secs(payload$elapsed_sec)
    )
  )
  if (!isTRUE(payload$complete)) {
    pairs <- c(pairs, list(list("缓存完整性", '<span class="muted">不完整（运行中断）</span>')))
  }

  root_cards <- vapply(root_names, function(r) {
    rc <- Filter(function(cs) identical(cs$root, r), cases)
    s <- summarise_cases(rc)
    sprintf(
      '<div class="root-card"><h3><span>%s</span><span class="muted small">%d 用例</span></h3><div class="bar">%s</div><div class="root-stats">pass %d · both_error %d · unsupported %d · fail %d · error %d · 其他 %d</div></div>',
      esc(r), s$n, stacked_bar(rc), s$pass, s$both_error, s$unsupported,
      s$fail, s$error, s$other
    )
  }, "")

  c(
    '<section class="card" id="summary">',
    '<h2>概览</h2>',
    meta_grid(pairs),
    '<h3 style="margin-top:18px">状态计数</h3>',
    sprintf('<div class="chips">%s</div>', paste(status_chips(counts), collapse = "")),
    '<h3 style="margin-top:16px">按 gold 根</h3>',
    sprintf('<div class="roots">%s</div>', paste(root_cards, collapse = "")),
    '<h3 style="margin-top:18px">区域通过率</h3>',
    '<p class="muted small" style="margin-top:-4px">pass 率 = pass / 用例；一致率 = (pass + both_error) / 用例。rounding 按 proc / values 子区拆分；ts1m2/ 是本机 TS1M2 gold。</p>',
    area_table(groups, root_names),
    '</section>'
  )
}

AREA_DESC <- c(
  functions = "DATA step 函数用例，每个函数一个目录（CATX/CATX_001 形式）。",
  freq_stats = "PROC FREQ 统计量与精确检验。",
  fn_examples = "SAS 函数参考页示例，每个函数一个用例。",
  "rounding/proc" = "PROC 输出取整与显示引擎（proc 子区）。",
  "rounding/values" = "数值函数取整（values 子区，ROUND 等）。",
  semantics = "DATA step / 引擎语义行为。",
  parse_stress = "解析器压力用例（语法覆盖）。"
)

area_desc <- function(group) {
  if (group %in% names(AREA_DESC)) return(AREA_DESC[[group]])
  if (startsWith(group, "ts1m2/")) {
    return("本机 SAS 9.4 TS1M2 gold（gold 重跑；程序与 own 同 id）——仅作版本差异参考。")
  }
  ""
}

render_case_list <- function(cases, cases_dir, area_key) {
  ok <- vapply(cases, function(cs) is_ok_status(cs$status), logical(1L))
  ord <- order(ok, match(vapply(cases, function(cs) cs$status, ""), STATUS_ORDER, nomatch = 99L),
               vapply(cases, function(cs) cs$rel, ""))
  cases <- cases[ord]
  rows <- character(length(cases))
  for (k in seq_along(cases)) {
    rows[[k]] <- paste(case_row(cases[[k]], k, cases_dir, area_key), collapse = "\n")
  }
  c('<div class="case-list">', rows, '</div>')
}

render_areas <- function(groups, cases_dir) {
  sections <- character()
  for (g in names(groups)) {
    cases <- groups[[g]]
    s <- summarise_cases(cases)
    sections <- c(sections, c(
      sprintf('<section class="card area" id="%s">', area_id(g)),
      sprintf('<h2>%s</h2>', esc(g)),
      sprintf(
        '<div class="area-meta">%d 用例 · pass %d · both_error %d · unsupported %d · fail %d · error %d · other %d · 一致率 %s</div>',
        s$n, s$pass, s$both_error, s$unsupported, s$fail, s$error, s$other,
        fmt_pct(s$pass + s$both_error, s$n)
      ),
      if (nzchar(area_desc(g))) sprintf('<p class="area-desc">%s</p>', esc(area_desc(g))),
      render_case_list(cases, cases_dir, area_id(g)),
      '</section>'
    ))
  }
  c('<div id="areas">', sections, '</div>')
}

render_reasons <- function(cases) {
  attributed <- Filter(function(cs) !is_ok_status(cs$status), cases)
  n_ok <- length(cases) - length(attributed)
  reasons <- vapply(attributed, function(cs) classify_reason(cs$msg), "")
  both_err <- sum(vapply(cases, function(cs) identical(cs$status, "both_error"), logical(1L)))
  codes <- c(names(REASON_RULES), "other")
  counts <- vapply(codes, function(code) sum(reasons == code), integer(1L))

  rows <- character()
  for (code in codes) {
    rule <- REASON_RULES[[code]]
    pats <- if (is.null(rule)) "（兜底：不匹配以上任何规则）" else paste(rule$patterns, collapse = " / ")
    desc <- if (is.null(rule)) "其他未归因消息（含空消息）" else rule$desc
    examples <- unique(vapply(
      attributed[reasons == code],
      function(cs) trunc_chars(gsub("\\s+", " ", cs$msg %||% ""), 80),
      ""
    ))
    examples <- examples[nzchar(examples)]
    ex <- if (length(examples)) paste(utils::head(examples, 2L), collapse = " ¶ ") else "—"
    rows <- c(rows, sprintf(
      '<tr><td><span class="chip reason">%s</span></td><td class="num">%d</td><td class="num">%s</td><td>%s</td><td class="small muted">%s</td></tr>',
      esc(code), counts[[code]], fmt_pct(counts[[code]], length(attributed)),
      esc(desc), esc(ex)
    ))
  }
  c(
    '<section class="card" id="reasons">',
    '<h2>fail-loud 原因分类</h2>',
    sprintf(
      '<p class="muted">归因对象：%d 个“非 pass / 非 both_error”用例（共 %d 个用例，pass+both_error 视为一致：%d 个）。每个用例只落一类，按下列优先级顺序取首个匹配。</p>',
      length(attributed), length(cases), n_ok
    ),
    '<table><thead><tr><th>原因码</th><th class="num">数量</th><th class="num">占比</th><th>定义</th><th>示例消息（截断）</th></tr></thead><tbody>',
    rows,
    '</tbody></table>',
    '<h3 style="margin-top:18px">匹配规则（按优先级）</h3>',
    '<table><thead><tr><th class="num">#</th><th>原因码</th><th>匹配关键词（literal，忽略大小写）</th></tr></thead><tbody>',
    vapply(seq_along(REASON_RULES), function(i) {
      code <- names(REASON_RULES)[[i]]
      pats <- REASON_RULES[[code]]$patterns
      pats_disp <- ifelse(pats == "version", "version（整词）", pats)
      sprintf(
        '<tr><td class="num">%d</td><td><span class="chip reason">%s</span></td><td class="small">%s</td></tr>',
        i, esc(code), esc(paste(pats_disp, collapse = " / "))
      )
    }, ""),
    '</tbody></table>',
    sprintf(
      '<p class="callout">both_error %d 个：SAS 与 sasonr 在同一步报出 SAS ERROR，且比较层一致，按“一致”处理，不参与 fail-loud 归因。</p>',
      both_err
    ),
    '</section>'
  )
}

registry_specs <- function(pkg_root) {
  list(
    list(
      id = "functions", label = "functions.csv",
      file = file.path(pkg_root, "inst", "registry", "functions.csv"),
      name = "name", cases = "cases", pass = "pass"
    ),
    list(
      id = "formats", label = "formats.csv",
      file = file.path(pkg_root, "inst", "registry", "formats.csv"),
      name = "name", cases = "cases", pass = "pass"
    ),
    list(
      id = "proc_stats", label = "proc_stats.csv",
      file = file.path(pkg_root, "inst", "registry", "proc_stats.csv"),
      name = "proc", cases = "compared", pass = "identical"
    )
  )
}

registry_name <- function(df) {
  if ("name" %in% names(df)) return(df$name)
  if (all(c("proc", "statistic") %in% names(df))) {
    nm <- paste(df$proc, df$statistic, sep = " · ")
    if ("condition" %in% names(df)) {
      cond <- df$condition
      sel <- !is.na(cond) & nzchar(cond) & !(cond %in% c("default"))
      nm[sel] <- paste0(nm[sel], " (", cond[sel], ")")
    }
    return(nm)
  }
  if ("statistic" %in% names(df)) return(df$statistic)
  seq_len(nrow(df))
}

registry_note <- function(df) {
  for (nm in c("note", "notes")) {
    if (nm %in% names(df)) return(as.character(df[[nm]]))
  }
  rep("", nrow(df))
}

registry_domain <- function(df) {
  if ("domain" %in% names(df)) return(as.character(df$domain))
  rep("", nrow(df))
}

render_registry <- function(spec, pkg_root) {
  if (!file.exists(spec$file)) {
    return(c(
      sprintf('<section class="card" id="registry-%s">', spec$id),
      sprintf('<h2>注册表：%s</h2>', esc(spec$label)),
      sprintf('<p class="banner bad">文件不存在：<code>%s</code></p>', esc(spec$file)),
      '</section>'
    ))
  }
  df <- utils::read.csv(
    spec$file, colClasses = "character", check.names = FALSE,
    na.strings = character(), encoding = "UTF-8"
  )
  status <- as.character(df$status)
  name <- registry_name(df)
  note <- registry_note(df)
  domain <- registry_domain(df)
  cases <- if (spec$cases %in% names(df)) suppressWarnings(as.numeric(df[[spec$cases]])) else rep(NA_real_, nrow(df))
  pass <- if (spec$pass %in% names(df)) suppressWarnings(as.numeric(df[[spec$pass]])) else rep(NA_real_, nrow(df))
  present <- unique(status)
  lv <- c(REGISTRY_STATUS_ORDER, sort(setdiff(present, REGISTRY_STATUS_ORDER)))
  lv <- lv[lv %in% present]

  dist_rows <- vapply(lv, function(s) sprintf(
    '<tr><td>%s</td><td class="num">%d</td><td class="num">%s</td></tr>',
    esc(s), sum(status == s, na.rm = TRUE),
    fmt_pct(sum(status == s, na.rm = TRUE), nrow(df))
  ), "")
  dist_rows <- c(dist_rows, sprintf(
    '<tr class="total"><td>合计</td><td class="num">%d</td><td class="num">100.0%%</td></tr>',
    nrow(df)
  ))

  nv <- which(status != "verified")
  nv <- nv[order(match(status[nv], lv), name[nv])]
  if (length(nv)) {
    nv_rows <- vapply(nv, function(i) {
      note_txt <- note[[i]] %||% ""
      note_disp <- trunc_chars(gsub("\\s+", " ", note_txt), 140)
      dom <- domain[[i]] %||% ""
      dom_disp <- trunc_chars(gsub("\\s+", " ", dom), 90)
      sprintf(
        '<tr><td>%s</td><td><span class="chip status-%s">%s</span></td><td class="num">%s</td><td class="num">%s</td><td class="small">%s</td><td class="small">%s</td></tr>',
        esc(name[[i]]), cls_status(status[[i]]), esc(status[[i]]),
        if (is.na(cases[[i]])) "—" else fmt_int(cases[[i]]),
        if (is.na(pass[[i]])) "—" else fmt_int(pass[[i]]),
        if (nzchar(dom_disp)) esc(dom_disp) else "—",
        if (nzchar(note_disp)) sprintf('<span title="%s">%s</span>', esc(note_txt), esc(note_disp)) else "—"
      )
    }, "")
  } else {
    nv_rows <- '<tr><td colspan="6" class="muted">全部为 verified。</td></tr>'
  }

  cases_label <- switch(spec$id, proc_stats = "compared", "cases")
  pass_label <- switch(spec$id, proc_stats = "identical", "pass")
  c(
    sprintf('<section class="card" id="registry-%s">', spec$id),
    sprintf('<h2>注册表：%s</h2>', esc(spec$label)),
    sprintf(
      '<p class="muted">路径 <code>%s</code> · %d 行 · verified %d · 非 verified %d（%s）%s</p>',
      esc(spec$file), nrow(df), sum(status == "verified"), length(nv),
      fmt_pct(length(nv), nrow(df)),
      if (all(is.na(cases))) "" else sprintf(
        " · %s 合计 %s，%s 合计 %s",
        cases_label, fmt_int(sum(cases, na.rm = TRUE)),
        pass_label, fmt_int(sum(pass, na.rm = TRUE))
      )
    ),
    '<h3>status 分布</h3>',
    '<table><thead><tr><th>status</th><th class="num">行数</th><th class="num">占比</th></tr></thead><tbody>',
    dist_rows, '</tbody></table>',
    sprintf('<h3 style="margin-top:18px">status 不是 verified 的行（%d）</h3>', length(nv)),
    paste0(
      '<table><thead><tr><th>name</th><th>status</th><th class="num">', cases_label,
      '</th><th class="num">', pass_label, '</th><th>domain</th><th>note（截断 140）</th></tr></thead><tbody>'
    ),
    nv_rows,
    '</tbody></table>',
    '</section>'
  )
}

render_registries <- function(pkg_root) {
  specs <- registry_specs(pkg_root)
  unlist(c(
    '<section class="card" id="registry">',
    '<h2>注册表</h2>',
    '<p class="muted">三个注册表的状态分布，以及所有 status 不是 verified 的行（note 按 140 字符截断，悬停可看全文）。注册表是 sasonr 的权威特性清单；本报告只读取，不修改。</p>',
    '</section>',
    lapply(specs, render_registry, pkg_root = pkg_root)
  ), use.names = FALSE)
}

render_versions <- function() {
  c(
    '<section class="card" id="versions">',
    '<h2>版本注意事项</h2>',
    '<ul class="notes">',
    '<li><strong>PROC FREQ exact CL（TS1M2 vs TS1M7）</strong>：EXACT RELRISK / RISKDIFF 的 exact 置信限值不同。c06（exact RR）TS1M7 gold 为 <code>0.0901 / 0.9111</code>，本机 TS1M2 为 <code>0.0000 / 17.9211</code>；c05（exact RD）上限 TS1M7 为 <code>-0.0587</code>，TS1M2 为 <code>-0.0672</code>。TS1M7 的标签/脚注也不同（<code>Exact (Score)</code>、<code>95% Exact 95%</code>）。</li>',
    '<li><strong>CONSTANT()</strong>：LOGSMALL / LOGBIG 的值随 SAS 版本不同。本机 TS1M2 <code>LOGSMALL = -708.3964185</code>，TS1M7 gold <code>-708.4018337</code>；LOGBIG 相差 1 ulp。这是版本差异，不能靠本机 TS1M2 关闭。</li>',
    '<li><strong>编码</strong>：本机 TS1M2 gold 为 <code>latin1</code>（WLATIN1，英文 config）；TS1M7 gold 的 encoding 待定（spec 04 以 utf-8 为 gold 环境）。两种编码的验证结果不能混用。</li>',
    '<li><strong>gold 根</strong>：<code>cases/own</code> 是 TS1M7 gold（sasonr 的权威对照）；<code>cases/ts1m2</code> 是本机 TS1M2 复跑 gold，只用于观察版本差异。</li>',
    '</ul>',
    '<p class="callout">本机 SAS 版本变化后需重跑本报告：<code>Rscript tools/report_conformance.R --cases E:/dsh/sasonr-conformance/cases --out E:/dsh/_dsh_tmp/conformance.html</code></p>',
    '</section>'
  )
}

render_footer <- function(ctx, payload) {
  reg_paths <- vapply(registry_specs(ctx$pkg_root), function(s) s$file, "")
  c(
    '<footer>',
    '<p><strong>数据来源</strong>：用例与 SAS gold 在 <code>',
    esc(ctx$cases_dir), '</code>（own = TS1M7 gold；ts1m2 = 本机 TS1M2 gold）；用例 runner 是 <code>',
    esc(file.path(ctx$pkg_root, "tests", "testthat", "helper-conformance.R")),
    '</code>；注册表是 ', paste(sprintf('<code>%s</code>', esc(reg_paths)), collapse = "、"), '。</p>',
    '<p><strong>gold 目录</strong>：每个用例的 SAS gold 在该用例目录的 <code>expected/</code> 下（datasets、attrs.csv、log_notes.txt、put.txt、files/）。</p>',
    '<p><strong>报告只是投影，权威数据是 registry/cases。</strong>本报告不修改任何用例、gold 或注册表；所有数字均由本次运行或缓存投影得出。HTML 自包含（内联 CSS，无外部资源、无网络请求）。</p>',
    sprintf(
      '<p>由 <code>tools/report_conformance.R</code> 生成 · 渲染于 %s · 数据模式：%s%s</p>',
      esc(ctx$generated),
      if (ctx$cache_used) "缓存（--no-run）" else "本次运行",
      if (!isTRUE(payload$complete)) " · 缓存不完整" else ""
    ),
    '</footer>'
  )
}

render_report <- function(payload, ctx) {
  cases <- payload$cases
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    g <- case_group(cs$root, cs$rel)
    cs$area <- g$area
    cs$subarea <- g$subarea
    cs$group <- g$group
    cases[[i]] <- cs
  }
  keys <- vapply(cases, function(cs) cs$group, "")
  own_groups <- sort(unique(keys[!startsWith(keys, "ts1m2/")]))
  pref <- c(
    "functions", "freq_stats", "fn_examples", "rounding/proc", "rounding/values",
    "semantics", "parse_stress"
  )
  own_groups <- c(intersect(pref, own_groups), setdiff(own_groups, pref))
  other_groups <- sort(unique(keys[startsWith(keys, "ts1m2/")]))
  group_order <- c(own_groups, other_groups)
  groups <- lapply(group_order, function(g) cases[keys == g])
  names(groups) <- group_order
  # keep every case even if a future root produces a key outside the order
  leftover <- setdiff(unique(keys), group_order)
  if (length(leftover)) {
    groups <- c(groups, lapply(leftover, function(g) cases[keys == g]))
    names(groups) <- c(group_order, leftover)
  }

  ctx$generated <- now_string()
  counts <- status_counts(cases)
  root_names <- unique(vapply(cases, function(cs) cs$root, ""))
  root_names <- c(intersect("own", root_names), setdiff(root_names, "own"))

  nav <- c(
    '<nav class="toc">',
    '<a href="#summary">概览</a>',
    '<a href="#areas">区域详情</a>',
    '<a href="#reasons">fail-loud 归因</a>',
    '<a href="#registry">注册表</a>',
    '<a href="#versions">版本注意事项</a>',
    '</nav>'
  )

  banners <- character()
  if (ctx$cache_used) {
    cls <- if (isTRUE(payload$complete) && !isTRUE(ctx$cases_dir_mismatch)) "banner" else "banner bad"
    msg <- sprintf(
      "本报告由缓存渲染（--no-run），没有重新运行用例。缓存：<code>%s</code>（生成于 %s，%s 个用例）。",
      esc(ctx$cache_path), esc(payload$generated %||% "unknown"), fmt_int(length(cases))
    )
    if (!isTRUE(payload$complete)) msg <- paste0(msg, " <strong>缓存不完整</strong>：上次运行在完成前中断。")
    if (isTRUE(ctx$cases_dir_mismatch)) {
      msg <- paste0(msg, " <strong>缓存记录的 cases 目录与当前 --cases 不同</strong>：", esc(payload$cases_dir %||% "?"))
    }
    if (!identical(payload$sasonr_sha %||% "unknown", ctx$git_sha)) {
      msg <- paste0(
        msg, " 缓存生成时 sasonr sha = <code>", esc(payload$sasonr_sha %||% "unknown"),
        "</code>，当前 = <code>", esc(ctx$git_sha), "</code>。"
      )
    }
    banners <- c(banners, sprintf('<div class="%s">%s</div>', cls, msg))
  }

  hero <- c(
    '<header class="hero"><div class="wrap">',
    '<div class="kicker">sasonr · SAS 9.4 conformance</div>',
    '<h1>sasonr conformance report</h1>',
    sprintf(
      '<p class="sub">%d 用例 · SAS gold 对照 · 生成于 %s · 由 <code>tools/report_conformance.R</code> 生成</p>',
      length(cases), esc(ctx$generated)
    ),
    sprintf(
      '<div class="chips hero-chips">%s</div>',
      paste(status_chips(counts), collapse = "")
    ),
    '</div></header>'
  )

  c(
    '<!DOCTYPE html>',
    '<html lang="zh-CN">',
    '<head>',
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>sasonr conformance report</title>',
    paste0('<style>', css_text(), '</style>'),
    '</head>',
    '<body>',
    hero,
    '<main>',
    banners,
    nav,
    render_summary(cases, groups, root_names, ctx, payload),
    render_areas(groups, ctx$cases_dir),
    render_reasons(cases),
    render_registries(ctx$pkg_root),
    render_versions(),
    render_footer(ctx, payload),
    '</main>',
    '</body>',
    '</html>'
  )
}

# ===========================================================================
# main
# ===========================================================================

group_sort_key <- function(keys) {
  own <- sort(unique(keys[!startsWith(keys, "ts1m2/")]))
  pref <- c(
    "functions", "freq_stats", "fn_examples", "rounding/proc", "rounding/values",
    "semantics", "parse_stress"
  )
  own <- c(intersect(pref, own), setdiff(own, pref))
  c(own, sort(setdiff(unique(keys), own)))
}

run_report <- function(cli) {
  out <- normalizePath(cli$out, winslash = "/", mustWork = FALSE)
  cache_path <- if (is.na(cli$cache) || !nzchar(cli$cache)) default_cache_path(out) else {
    normalizePath(cli$cache, winslash = "/", mustWork = FALSE)
  }

  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else {
    file.path("tools", "report_conformance.R")
  }
  pkg_root <- normalizePath(dirname(dirname(script_path)), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    if (file.exists("DESCRIPTION")) {
      pkg_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
    } else {
      stop("找不到 sasonr 包根（脚本应位于 <pkg>/tools/）", call. = FALSE)
    }
  }

  if (cli$no_run) {
    if (!file.exists(cache_path)) {
      stop("--no-run 需要已有缓存，但文件不存在: ", cache_path, call. = FALSE)
    }
    message("读取缓存（不运行用例）: ", cache_path)
    payload <- read_cache(cache_path)
    cache_used <- TRUE
    # Without an explicit --cases, use the corpus recorded in the cache so
    # `--no-run` is self-consistent (and no false "cases dir moved" banner).
    cases_dir <- if (isTRUE(cli$cases_given)) {
      normalizePath(cli$cases, winslash = "/", mustWork = TRUE)
    } else {
      normalizePath(payload$cases_dir %||% DEFAULT_CASES, winslash = "/", mustWork = FALSE)
    }
    if (!dir.exists(cases_dir)) {
      stop("缓存记录的用例目录不存在: ", cases_dir,
           "（可用 --cases 显式指定）", call. = FALSE)
    }
    payload$cases_dir <- cases_dir
    roots <- discover_roots(cases_dir)
  } else {
    cases_dir <- normalizePath(cli$cases, winslash = "/", mustWork = TRUE)
    roots <- discover_roots(cases_dir)
    payload <- run_cases(roots, pkg_root, cache_path, cases_dir)
    message("写入缓存: ", cache_path)
    write_cache(payload, cache_path)
    cache_used <- FALSE
  }
  payload$cases_dir <- payload$cases_dir %||% cases_dir

  ctx <- list(
    cases_dir = cases_dir,
    roots = roots,
    pkg_root = pkg_root,
    out = out,
    cache_path = cache_path,
    cache_used = cache_used,
    cases_dir_mismatch = !identical(
      normalizePath(payload$cases_dir, winslash = "/", mustWork = FALSE), cases_dir
    ),
    git_sha = git_sha(pkg_root),
    pkg_version = pkg_version(pkg_root),
    generated = now_string()
  )

  html <- render_report(payload, ctx)
  write_html(html, out)

  # console summary (also the machine-checkable numbers)
  counts <- status_counts(payload$cases)
  keys <- vapply(payload$cases, function(cs) case_group(cs$root, cs$rel)$group, "")
  cat("\n== sasonr conformance report ==\n")
  cat(sprintf("cases: %d\n", length(payload$cases)))
  cat("status: ", paste(sprintf("%s=%d", names(counts), counts), collapse = "  "), "\n", sep = "")
  cat("by area:\n")
  for (g in group_sort_key(keys)) {
    if (!any(keys == g)) next
    s <- summarise_cases(payload$cases[keys == g])
    cat(sprintf(
      "  %-24s %4d  pass %4d  both_error %3d  unsupported %3d  fail %3d  error %3d  other %3d  pass-rate %6s  agree %6s\n",
      g, s$n, s$pass, s$both_error, s$unsupported, s$fail, s$error, s$other,
      fmt_pct(s$pass, s$n), fmt_pct(s$pass + s$both_error, s$n)
    ))
  }
  sz <- file.info(out)$size
  cat(sprintf("HTML: %s (%.1f KB)\n", out, sz / 1024))
  cat(sprintf("cache: %s (%s)\n", cache_path,
              if (cache_used) "read" else if (file.exists(cache_path)) "written" else "missing"))
  if (!is.na(payload$elapsed_sec %||% NA_real_)) {
    cat(sprintf("elapsed: %s\n", fmt_secs(payload$elapsed_sec)))
  }
  invisible(0)
}

main <- function() {
  cli <- tryCatch(
    parse_cli(commandArgs(trailingOnly = TRUE)),
    error = function(e) {
      message("参数错误: ", conditionMessage(e))
      cat(usage_text(), file = stderr())
      quit(save = "no", status = 1L)
    }
  )
  if (isTRUE(cli$help)) {
    cat(usage_text())
    return(invisible(NULL))
  }
  tryCatch(
    run_report(cli),
    error = function(e) {
      message("失败: ", conditionMessage(e))
      quit(save = "no", status = 1L)
    }
  )
}

main()
