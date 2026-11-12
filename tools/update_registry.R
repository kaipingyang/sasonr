# Build the feature registries inst/registry/functions.csv and formats.csv from
# the offline conformance results (never edited by hand).
#
# Usage (package root; one R process at a time):
#   OPENBLAS_NUM_THREADS=1 Rscript tools/run_conformance.R tests/conformance --pattern '/(fn_matrix|fmt_matrix|functions)/' --out /tmp/matrix.csv
#   Rscript tools/update_registry.R /tmp/matrix.csv [more.csv ...]
#
# Inputs: the per-case CSV of tools/run_conformance.R (areas fn_matrix/,
# fmt_matrix/ and the function pilot functions/), the case lists
# tests/conformance/{fn_matrix,fmt_matrix}/<NAME>/cases.csv (context of each
# case), tools/fn_matrix.txt / tools/fmt_matrix.txt (which features are
# clock-dependent, format names) and, when present, the SAS 9.4 inventory
# exported from SAS (../sasonr-conformance/data/inventory/: argument counts and
# types, width limits).  Without the local cases the registries are kept.
#
# Per case:   pass         every layer agrees bit for bit (or SAS and sasonr both stop with an error)
#             approximate  only numbers differ (last bits; at most 1e-9 relative): NOT a pass
#             log          only the log NOTEs differ
#             unsupported  sasonr fails loud (sasonr_unsupported)
#             mismatch     anything else (values, lengths, _ERROR_, sasonr error, lenient)
# Per feature: verified (all cases pass), approximate (some numbers differ in the
# last bits because the algorithm differs; fails loud with sasonr.strict = TRUE),
# partial (no mismatch; some cases differ in a log NOTE only or fail loud),
# mismatch (some case is a silent difference),
# not_comparable (depends on the session clock), unverified (no case).

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("usage: Rscript tools/update_registry.R results.csv [...]")
suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
ns <- asNamespace("sasonr")
today <- format(Sys.Date())

res <- do.call(rbind, lapply(args, utils::read.csv, stringsAsFactors = FALSE))
res <- res[!duplicated(res$case, fromLast = TRUE), ]
res$fails <- ifelse(is.na(res$failed_layers), "", res$failed_layers)
res$class <- with(res, ifelse(status %in% c("pass", "both_error"), "pass",
                       ifelse(status == "unsupported", "unsupported",
                       ifelse(status == "fail" & fails == "log", "log",
                       ifelse(status == "fail" & !is.na(tol) & tol <= 1e-9 & !grepl("attr|log", fails), "approximate",
                              "mismatch")))))
# A function case whose value is bit-identical (same hex=) and whose only
# difference is the BEST32. display in results.txt is a BESTw. (w > 16) edge,
# not a difference of the function (BEST widths above 16 are partly undetermined).
hex_of <- function(d, who) sub("^.*?hex=([0-9A-F]+).*$", "\\1", regmatches(d, regexpr(paste0(who, "='[^']*'"), d)))
disp_only <- vapply(seq_len(nrow(res)), function(i) {
  if (!identical(res$status[i], "fail") || !identical(res$fails[i], "files") || !grepl("^fn_matrix/", res$case[i])) return(FALSE)
  d <- res$detail[i]
  a <- hex_of(d, "SAS")
  b <- hex_of(d, "sasonr")
  length(a) == 1 && length(b) == 1 && grepl("^[0-9A-F]{16}$", a) && a == b
}, logical(1))
res$class[disp_only] <- "pass"
res$feature <- sub("^(fn_matrix|fmt_matrix|functions)/([^/]+)/.*$", "\\2", res$case)
res$area <- sub("/.*$", "", res$case)

# Targeted probes (tests/conformance/fn_probe/<PROBE>/: random inputs, one data
# set WORK.P with the inputs and one column per call).  Each value is checked
# by calling the function layer row by row: pass (bit-identical), unsupported
# (sasonr fails loud: outside the verified domain), approximate (differs).
probe_calls <- list(
  MOD = list(r = quote(sas_f_mod(a, b)), rz = quote(sas_f_modz(a, b))),
  LOG = list(l = quote(sas_f_log(x)), l10 = quote(sas_f_log10(x)), l2 = quote(sas_f_log2(x)),
             e = quote(sas_f_exp(x / 1000)), s = quote(sas_f_sqrt(x))),
  SINH = list(s = quote(sas_f_sinh(x)), c = quote(sas_f_cosh(x)), t = quote(sas_f_tanh(x))),
  COMB = list(c = quote(sas_f_comb(n, k)), p = quote(sas_f_perm(min(n, 170), min(k, n, 170)))),
  YRDIF = list(age = quote(sas_f_yrdif(d1, d2, "AGE")), aa = quote(sas_f_yrdif(d1, d2, "ACT/ACT")),
               t3 = quote(sas_f_yrdif(d1, d2, "30/360")), a365 = quote(sas_f_yrdif(d1, d2, "ACT/365")),
               a360 = quote(sas_f_yrdif(d1, d2, "ACT/360"))),
  GEOMEAN = list(g = quote(sas_f_geomean(a, b, c, d)), g2 = quote(sas_f_geomean(a, b)), h = quote(sas_f_harmean(a, b, c, d)))
)
probe_rows <- list()
for (pr in names(probe_calls)) {
  f <- file.path("tests", "conformance", "fn_probe", pr, "expected", "datasets", "p.sas7bdat")
  if (!file.exists(f)) next
  P <- as.data.frame(haven::read_sas(f))
  for (col in names(probe_calls[[pr]])) {
    call <- probe_calls[[pr]][[col]]
    fname <- toupper(sub("^sas_f_", "", as.character(call[[1]])))
    cls <- vapply(seq_len(nrow(P)), function(i) {
      got <- tryCatch(suppressWarnings(eval(call, as.list(P[i, ]), ns)), sasonr_unsupported = function(e) "u",
                      error = function(e) "e")
      if (identical(got, "u")) return("unsupported")
      if (identical(got, "e")) return("mismatch")
      want <- as.numeric(P[[col]][i])
      same <- (is.na(want) && is.na(got)) || (!is.na(want) && !is.na(got) &&
                                                identical(writeBin(as.numeric(want), raw()), writeBin(as.numeric(got), raw())))
      if (same) "pass" else "approximate"
    }, "")
    probe_rows[[length(probe_rows) + 1L]] <- data.frame(
      case = sprintf("fn_probe/%s/%s/%d", pr, col, seq_len(nrow(P))), status = "", fails = "", tol = NA_real_,
      class = cls, feature = fname, area = "fn_probe", stringsAsFactors = FALSE)
  }
}
if (length(probe_rows)) {
  pr <- do.call(rbind, probe_rows)
  for (k in setdiff(names(res), names(pr))) pr[[k]] <- NA
  res <- rbind(res, pr[names(res)])
}

inv_dir <- file.path("..", "sasonr-conformance", "data", "inventory")
read_inv <- function(f) {
  p <- file.path(inv_dir, f)
  if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, colClasses = "character") else NULL
}

case_context <- function(area, feature) {
  f <- file.path("tests", "conformance", area, feature, "cases.csv")
  if (!file.exists(f)) return(NULL)
  x <- utils::read.csv(f, stringsAsFactors = FALSE)
  stats::setNames(x$context, x$case)
}

summarise <- function(rows) {
  cl <- rows$class
  n <- length(cl)
  cnt <- function(k) sum(cl == k)
  status <- if (n == 0) "unverified" else if (cnt("pass") == n) "verified" else if (cnt("mismatch") > 0) "mismatch" else
    if (cnt("approximate") > 0) "approximate" else "partial"
  why <- c(
    if (cnt("approximate")) sprintf("%d case(s) differ in the last bits (largest relative difference %.1e; different algorithm)",
                                    cnt("approximate"), max(rows$tol[rows$class == "approximate"], na.rm = TRUE)),
    if (cnt("log")) sprintf("%d case(s) differ in a log NOTE only", cnt("log")),
    if (cnt("unsupported")) sprintf("%d case(s) fail loud (sasonr_unsupported)", cnt("unsupported")),
    if (cnt("mismatch")) sprintf("%d case(s) differ", cnt("mismatch"))
  )
  list(status = status, cases = n, pass = cnt("pass"), approximate = cnt("approximate"), log = cnt("log"),
       unsupported = cnt("unsupported"), mismatch = cnt("mismatch"), note = paste(why, collapse = "; "))
}

# ---- functions --------------------------------------------------------------------

fn_inv <- read_inv("sas_functions.csv")
inv_row <- function(nm) {
  if (is.null(fn_inv)) return(NULL)
  r <- fn_inv[toupper(fn_inv$fncname) == nm & fn_inv$fnctype %in% c("N", "C"), , drop = FALSE]
  if (!nrow(r)) return(NULL)
  r[order(r$source != "B"), , drop = FALSE][1, ]
}
arg_types <- function(mask) {
  m <- as.numeric(mask)
  g <- integer()
  while (m > 0) {
    g <- c(g, m %% 8)
    m <- m %/% 8
  }
  paste(c("?", "num", "char", "any", "?", "?", "?", "any")[g + 1L], collapse = " ")
}

mx_lines <- readLines("tools/fn_matrix.txt")
hdr <- grep("^## ", mx_lines, value = TRUE)
env_fns <- toupper(sub("^## (\\S+).*$", "\\1", hdr[grepl("\\benv\\b", hdr)]))

docs <- utils::read.delim("tools/fn_docs.tsv", quote = "", colClasses = "character")
# verified input domain of `partial` functions (the runtime guard fails loud outside it)
fn_domains <- utils::read.delim("tools/fn_domains.tsv", quote = "", colClasses = "character", comment.char = "#")
docs <- docs[!startsWith(docs$name, "cvt_") & !startsWith(docs$name, "sql") & docs$name != "monotonic", ]
ft <- get("fn_table", ns)
fn_rows <- lapply(docs$name, function(nm) {
  NM <- toupper(nm)
  inv <- inv_row(NM)
  spec <- ft[[nm]]
  rows <- res[res$area %in% c("fn_matrix", "functions", "fn_probe") & res$feature == NM, , drop = FALSE]
  ctx <- c(case_context("fn_matrix", NM))
  rows$context <- if (is.null(ctx)) rep(NA_character_, nrow(rows)) else unname(ctx[sub("^.*/", "", rows$case)])
  # function pilot cases: the context is in the case name of the meta tags (data step unless %SYSFUNC / SQL)
  pil <- rows$area == "functions"
  if (any(pil)) {
    tags <- vapply(rows$case[pil], function(cs) {
      f <- file.path("tests", "conformance", cs, "meta.yaml")
      if (!file.exists(f)) return("data")
      tg <- grep("^tags:", readLines(f, warn = FALSE), value = TRUE)
      if (grepl("sysfunc", tg)) "sysfunc" else if (grepl("\\bsql\\b", tg)) "sql" else "data"
    }, "")
    rows$context[pil] <- tags
  }
  rows$context[is.na(rows$context)] <- "data"
  rows$context[rows$context %in% c("datastep", "ce")] <- "data"
  s <- summarise(rows)
  if (NM %in% env_fns) {
    s$status <- "not_comparable"
    s$note <- "the value depends on the session clock; only its type and range are compared"
  }
  ok_ctx <- vapply(c("data", "sysfunc", "sql"), function(k) {
    r <- rows[rows$context == k, , drop = FALSE]
    nrow(r) > 0 && all(r$class == "pass")
  }, logical(1))
  len <- if (is.null(spec)) NA else if (spec$type == "num") "8 (numeric)" else if (identical(spec$len, "arg")) "length of argument 1" else
    if (identical(spec$len, "fmt")) "format width" else as.character(spec$len)
  data.frame(
    name = NM, kind = "function",
    sas_minarg = if (is.null(inv)) NA else inv$minarg,
    sas_maxarg = if (is.null(inv)) NA else ifelse(inv$maxarg == "0", "any", inv$maxarg),
    sas_arg_types = if (is.null(inv)) NA else arg_types(inv$fncargs),
    returns = if (is.null(spec)) (if (is.null(inv)) NA else c(N = "num", C = "char")[[inv$fnctype]]) else spec$type,
    result_length = len,
    sasonr_args = if (is.null(spec)) NA else sprintf("%s-%s", spec$min, if (is.infinite(spec$max)) "any" else spec$max),
    contexts_verified = paste(c(data = "data", sysfunc = "sysfunc", sql = "sql")[ok_ctx], collapse = " "),
    impl = paste0("sas_f_", nm), status = s$status, cases = s$cases, pass = s$pass, approximate = s$approximate,
    log_only = s$log, unsupported = s$unsupported, mismatch = s$mismatch, note = s$note,
    domain = if (s$status == "partial" && NM %in% fn_domains$name) fn_domains$domain[fn_domains$name == NM][1] else "",
    updated = today, stringsAsFactors = FALSE
  )
})
fns <- do.call(rbind, fn_rows)

# ---- formats and informats ----------------------------------------------------------

fm_lines <- readLines("tools/fmt_matrix.txt")
fm_hdr <- grep("^## ", fm_lines, value = TRUE)
fm <- do.call(rbind, lapply(strsplit(sub("^## ", "", fm_hdr), "\\s+"), function(p) {
  data.frame(dir = p[1], kind = p[2], type = p[3], base = if (length(p) > 3) p[4] else p[1], stringsAsFactors = FALSE)
}))
fm$sas <- toupper(ifelse(fm$base == "-", "w.d", ifelse(fm$type == "C" & !startsWith(fm$base, "$"), paste0("$", fm$base), fm$base)))
fm$sas[fm$sas == "W.D"] <- "w.d"

fmt_inv <- read_inv("sas_formats.csv")
implemented <- local({
  nd <- names(get("num_fmt_defaults", ns))
  sv <- names(get("sep_variants", ns))
  sb <- get("sep_fmt_bases", ns)
  f <- c(setdiff(nd, c("w", "f")), "f", "w.d", as.vector(outer(sb, sv, paste0)),
         paste0("$", setdiff(get("char_fmt_names", ns), "")), "$")
  ni <- get("num_informats", ns)
  i <- c(setdiff(ni, "w"), "w.d", as.vector(outer(c("yymmdd", "mmddyy", "ddmmyy"), sv, paste0)),
         paste0("$", setdiff(get("char_informats", ns), "")), "$")
  rbind(data.frame(sas = toupper(f), kind = "format", stringsAsFactors = FALSE),
        data.frame(sas = toupper(i), kind = "informat", stringsAsFactors = FALSE))
})
implemented$sas[implemented$sas == "W.D"] <- "w.d"
fmt_rows <- lapply(seq_len(nrow(implemented)), function(i) {
  nm <- implemented$sas[i]
  kind <- implemented$kind[i]
  dirs <- fm$dir[fm$sas == nm & fm$kind == kind]
  rows <- res[res$area == "fmt_matrix" & res$feature %in% dirs, , drop = FALSE]
  s <- summarise(rows)
  inv <- NULL
  if (!is.null(fmt_inv)) {
    key <- if (nm == "w.d") "F" else nm
    inv <- fmt_inv[fmt_inv$fmtname == key & fmt_inv$fmttype == (if (kind == "format") "F" else "I"), , drop = FALSE]
    if (!nrow(inv)) inv <- NULL else inv <- inv[1, ]
  }
  g <- function(k) if (is.null(inv)) NA else inv[[k]]
  data.frame(name = nm, kind = kind, type = if (startsWith(nm, "$")) "char" else "num",
             sas_minw = g("minw"), sas_maxw = g("maxw"), sas_defw = g("defw"), sas_mind = g("mind"),
             sas_maxd = g("maxd"), sas_defd = g("defd"), status = s$status, cases = s$cases, pass = s$pass,
             approximate = s$approximate, log_only = s$log, unsupported = s$unsupported, mismatch = s$mismatch,
             note = s$note, updated = today, stringsAsFactors = FALSE)
})
fmts <- do.call(rbind, fmt_rows)
fmts <- fmts[order(fmts$kind, fmts$name), ]

dir.create(file.path("inst", "registry"), showWarnings = FALSE)
utils::write.csv(fns, file.path("inst", "registry", "functions.csv"), row.names = FALSE, na = "")
utils::write.csv(fmts, file.path("inst", "registry", "formats.csv"), row.names = FALSE, na = "")
tab <- function(x) paste(sprintf("%s %d", names(table(x)), as.integer(table(x))), collapse = ", ")
cat("functions:", nrow(fns), "-", tab(fns$status), "\n")
cat("formats/informats:", nrow(fmts), "-", tab(fmts$status), "\n")
cat("cases:", nrow(res), "-", tab(res$class), "\n")
