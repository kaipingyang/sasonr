# Offline conformance runner: compare sasonr with the SAS gold standard stored
# in a conformance case directory (no SAS needed).  Used by
# tests/testthat/test-conformance-cases.R and tools/run_conformance.R.
#
# Case layout (see the case-format document in the conformance repository):
#   <case>/program.sas            (or meta.yaml `program_ref`, resolved against
#                                  $SASONR_BOOKS_ROOT)
#   <case>/data/                  optional input files (copied to the run dir)
#   <case>/meta.yaml              sas_rc, encoding, cwd, layers, timeout, known_fail ...
#   <case>/expected/datasets/<mem>.sas7bdat, attrs.csv,
#                   log_notes.txt, listing.txt, put.txt, files/<path>
# Layers: ds (data set values from SAS's own sas7bdat files, read with haven),
# attr (type/length/format/informat/label), log (key NOTE/WARNING/ERROR
# messages in order), lst (listing, whitespace-normalised), put (PUT/%PUT
# output and data-error dumps), files (files written by the program).

conf_read_meta <- function(path) {
  meta <- list()
  if (!file.exists(path)) return(meta)
  for (line in readLines(path, warn = FALSE, encoding = "UTF-8")) {
    if (!nzchar(trimws(line)) || startsWith(trimws(line), "#")) next
    k <- sub(":.*$", "", line)
    v <- trimws(sub("^[^:]*:", "", line))
    if (startsWith(v, "[") && endsWith(v, "]")) {
      inner <- trimws(substr(v, 2L, nchar(v) - 1L))
      items <- if (nzchar(inner)) trimws(strsplit(inner, ",", fixed = TRUE)[[1]]) else character()
      meta[[k]] <- gsub("^\"|\"$", "", items)
    } else if (startsWith(v, "\"")) {
      meta[[k]] <- jsonlite_unquote(v)
    } else {
      meta[[k]] <- v
    }
  }
  meta
}

# minimal JSON string unquote (meta.yaml quoted scalars are JSON strings)
jsonlite_unquote <- function(s) {
  s <- substr(s, 2L, nchar(s) - 1L)
  s <- gsub("\\\\\"", "\"", s)
  gsub("\\\\\\\\", "\\\\", s)
}

# The SAS release of a case's gold standard (meta.yaml `sas_version`, e.g.
# "9.4 (TS1M2)"): sasonr runs the case for that release.  A case without the
# field, or with a release sasonr does not know, stops the runner (no silent
# comparison against the wrong release).
conf_meta_version <- function(meta) {
  v <- meta$sas_version
  if (is.null(v) || !nzchar(v)) stop("case without sas_version in meta.yaml")
  m <- regmatches(v, regexpr("TS[0-9]M[0-9]+", v))
  if (!length(m) || !m %in% sas_versions) stop("case gold from an unsupported SAS release: ", v)
  m
}

conf_root <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) "tests/testthat")
  cands <- c(file.path(tp, "..", "conformance"), file.path("tests", "conformance"))
  for (p in cands) if (dir.exists(p)) return(normalizePath(p))
  NULL
}

conf_list_cases <- function(root) {
  metas <- list.files(root, pattern = "^meta\\.yaml$", recursive = TRUE, full.names = TRUE)
  metas <- metas[!grepl("/expected/", metas, fixed = TRUE)]
  sort(dirname(metas))
}

conf_program_path <- function(case, meta) {
  if (!is.null(meta$program_ref)) {
    root <- Sys.getenv("SASONR_BOOKS_ROOT")
    if (!nzchar(root)) return(NA_character_)
    return(file.path(root, meta$program_ref, "program.sas"))
  }
  file.path(case, "program.sas")
}

# ---- reading expected / produced files ------------------------------------

conf_lines <- function(path, encoding = "UTF-8") {
  if (!file.exists(path)) return(character())
  x <- readLines(path, warn = FALSE, encoding = if (encoding == "UTF-8") "UTF-8" else "unknown")
  if (encoding != "UTF-8") x <- iconv(x, encoding, "UTF-8", sub = "?")
  x
}

conf_csv <- function(path, encoding = "UTF-8") {
  txt <- conf_lines(path, encoding)
  if (length(txt) == 0) return(list())
  d <- utils::read.csv(
    text = paste(txt, collapse = "\n"), header = FALSE, colClasses = "character", na.strings = character(),
    blank.lines.skip = FALSE, quote = "\"", comment.char = "", check.names = FALSE, strip.white = FALSE,
    fill = TRUE, encoding = "UTF-8"
  )
  lapply(seq_len(nrow(d)), function(i) unname(unlist(d[i, ], use.names = FALSE)))
}

# ---- L-DS / L-ATTR -----------------------------------------------------------

# Raw SAS numbers of a column read by haven (dates and times are converted
# back to SAS day / second counts); missing values as ".", ".A".., "._".
conf_sas_num <- function(x) {
  if (inherits(x, "Date")) return(list(v = as.numeric(x) + 3653, miss = ifelse(is.na(x), ".", NA)))
  if (inherits(x, "POSIXct")) return(list(v = as.numeric(x) + 315619200, miss = ifelse(is.na(x), ".", NA)))
  v <- as.numeric(unclass(x))
  miss <- rep(NA_character_, length(v))
  tag <- haven::na_tag(x)
  miss[is.na(v)] <- ifelse(is.na(tag[is.na(v)]), ".", paste0(".", toupper(tag[is.na(v)])))
  list(v = v, miss = miss)
}

conf_son_num <- function(x) {
  code <- sas_miss_code(x)
  list(v = ifelse(is.na(code), x, NA_real_), miss = ifelse(is.na(code), NA_character_,
                                                          ifelse(code == 1L, ".", paste0(".", miss_label(code)))))
}

# Numbers must be bit-identical (IEEE 754 pattern; 0 and -0 differ); missing
# values must be the same kind.  There is no tolerance.
conf_num_eq <- function(a, b) {
  bits <- function(v) {
    out <- rep(NA_character_, length(v))
    ok <- !is.na(v)
    if (any(ok)) {
      m <- matrix(writeBin(as.double(v[ok]), raw(), size = 8L), nrow = 8L)
      out[ok] <- apply(m, 2, paste, collapse = "")
    }
    out
  }
  same <- (is.na(a$miss) & is.na(b$miss) & bits(a$v) == bits(b$v)) |
    (!is.na(a$miss) & !is.na(b$miss) & a$miss == b$miss)
  same[is.na(same)] <- FALSE
  same
}

# L-DS: expected/datasets/<mem>.sas7bdat (SAS's own data sets, read with
# haven) against sasonr's WORK: member set, column names and order, row
# count, values (numbers bit for bit, no tolerance;
# character values without trailing blanks).
conf_cmp_ds <- function(exp_dir, res) {
  ef <- list.files(exp_dir, pattern = "\\.sas7bdat$")
  em <- stats::setNames(file.path(exp_dir, ef), toupper(sub("\\.sas7bdat$", "", ef)))
  wn <- names(res$work) %||% character()
  om <- stats::setNames(wn, toupper(wn))
  if (!setequal(names(em), names(om))) {
    return(sprintf("members SAS=%s sasonr=%s", paste(sort(names(em)), collapse = " "), paste(sort(names(om)), collapse = " ")))
  }
  unordered <- toupper(res$unordered %||% character())
  for (m in sort(names(em))) {
    # haven cannot read a data set without variables (SAS writes one when a step stops)
    e <- tryCatch(haven::read_sas(em[[m]]), error = function(err) tibble::tibble())
    nm <- om[[m]]
    o <- res$work[[nm]]
    disp <- vapply(names(o), function(v) res$meta[[nm]]$disp[[v]] %||% v, "")
    if (!identical(names(e), unname(disp))) {
      return(sprintf("%s: columns SAS=%s sasonr=%s", m, paste(utils::head(names(e), 8), collapse = ","),
                     paste(utils::head(disp, 8), collapse = ",")))
    }
    if (nrow(e) != nrow(o)) return(sprintf("%s: rows SAS=%d sasonr=%d", m, nrow(e), nrow(o)))
    if (nrow(e) == 0) next
    up <- toupper(names(e))
    skip_cols <- if (all(c("MEMNAME", "NAME", "VARNUM", "NPOS", "ENGINE") %in% up)) which(up %in% c("CRDATE", "MODATE")) else integer()
    latin <- function(x) {
      bad <- !is.na(x) & !validUTF8(x)
      x[bad] <- iconv(x[bad], "latin1", "UTF-8")
      x
    }
    cols_e <- lapply(seq_along(e), function(j) if (is.character(e[[j]])) sub("\\s+$", "", latin(e[[j]])) else conf_sas_num(e[[j]]))
    cols_o <- lapply(seq_along(o), function(j) if (is.character(o[[j]])) sas_rtrim(o[[j]]) else conf_son_num(o[[j]]))
    if (m %in% unordered) {
      key <- function(cols) {
        parts <- lapply(cols, function(cl) if (is.list(cl)) ifelse(is.na(cl$miss), sprintf("%.17g", cl$v), cl$miss) else cl)
        order(do.call(paste, c(parts, sep = "\x1f")))
      }
      ke <- key(cols_e)
      ko <- key(cols_o)
      cols_e <- lapply(cols_e, function(cl) if (is.list(cl)) lapply(cl, `[`, ke) else cl[ke])
      cols_o <- lapply(cols_o, function(cl) if (is.list(cl)) lapply(cl, `[`, ko) else cl[ko])
    }
    for (j in setdiff(seq_along(e), skip_cols)) {
      a <- cols_e[[j]]
      b <- cols_o[[j]]
      if (is.list(a) != is.list(b)) return(sprintf("%s %s: type differs", m, names(e)[j]))
      ok <- if (is.list(a)) conf_num_eq(a, b) else a == b
      if (!all(ok)) {
        i <- which(!ok)[1]
        sv <- if (is.list(a)) (if (is.na(a$miss[i])) sprintf("%.17g", a$v[i]) else a$miss[i]) else encodeString(a[i], quote = "'")
        ov <- if (is.list(b)) (if (is.na(b$miss[i])) sprintf("%.17g", b$v[i]) else b$miss[i]) else encodeString(b[i], quote = "'")
        return(sprintf("%s row %d %s: SAS=%s sasonr=%s", m, i, names(e)[j], sv, ov))
      }
    }
  }
  ""
}

conf_attrs <- function(path, encoding) {
  rows <- conf_csv(path, encoding)
  out <- list()
  for (r in rows[-1]) {
    if (length(r) < 8) next
    out[[paste(toupper(r[1]), tolower(r[3]), sep = ".")]] <- c(r[4], r[5], toupper(r[6]), toupper(r[7]), r[8])
  }
  out
}

conf_cmp_attr <- function(exp_file, son_file) {
  s <- conf_attrs(exp_file, "UTF-8")
  o <- conf_attrs(son_file, "latin1")
  nm <- c("type", "length", "format", "informat", "label")
  for (k in sort(intersect(names(s), names(o)))) {
    for (i in seq_along(nm)) {
      if (!identical(s[[k]][i], o[[k]][i])) {
        return(sprintf("%s %s: SAS=%s sasonr=%s", k, nm[i], encodeString(s[[k]][i], quote = "'"),
                       encodeString(o[[k]][i], quote = "'")))
      }
    }
  }
  ""
}

# ---- L-LOG ---------------------------------------------------------------------

# gold_version 2 (meta.yaml): the key set also keeps the messages that describe
# SAS's handling of an ERROR (tools/sanitize.py KEY_V2)
conf_key_v2 <- c(
  "NOTE: Due to ERROR\\(s\\) above, SAS set option OBS=0, enabling syntax check mode\\.",
  "NOTE: Statement not executed due to NOEXEC option\\.",
  "NOTE: The SAS System stopped processing this step because of errors\\.",
  "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements\\.",
  "WARNING: The data set WORK\\.\\S+ may be incomplete\\.",
  "WARNING: Data set WORK\\.\\S+ was not replaced because this step was stopped\\.",
  "WARNING: Limit set by ERRORS= option reached\\."
)

conf_key_re <- paste0("^(?:", paste(c(
  "NOTE: The data set WORK\\.\\S+ has \\d+ observations and \\d+ variables\\.",
  "NOTE: There were \\d+ observations read from the data set WORK\\.",
  "NOTE: Missing values were generated",
  "NOTE: Mathematical operations could not be performed",
  "NOTE: Variable \\S+ is uninitialized\\.",
  "NOTE: (Character|Numeric) values have been converted",
  "NOTE: Division by zero detected",
  "NOTE: Invalid .*(argument|data)",
  "NOTE: LOST CARD\\.",
  "NOTE: SAS went to a new line",
  "NOTE: \\d+ (observations with duplicate key values|duplicate observations) were deleted\\.",
  "NOTE: Table WORK\\.\\S+ created, with",
  "NOTE: The query requires remerging",
  "NOTE: \\d+ rows? (was|were) (inserted into|updated in|deleted from)",
  "NOTE: No rows were selected\\.",
  "NOTE: No observations in data set WORK\\.",
  "WARNING: Apparent (symbolic reference|invocation of macro)",
  "ERROR( \\d+-\\d+)?:"
), collapse = "|"), ")")
conf_key_re2 <- paste0(substr(conf_key_re, 1L, nchar(conf_key_re) - 1L), "|", paste(conf_key_v2, collapse = "|"), ")")

conf_norm_msg <- function(parts) {
  ws <- function(x) gsub("\\s+", " ", x, perl = TRUE)
  head <- trimws(ws(parts[1]))
  rest <- character()
  for (p in parts[-1]) {
    p <- trimws(p)
    if (startsWith(p, "WHERE ")) p <- paste0("WHERE ", tolower(gsub("[[:space:]()]", "", substring(p, 7L))))
    rest <- c(rest, ws(p))
  }
  if (startsWith(head, "NOTE: Missing values") || startsWith(head, "NOTE: Mathematical") || grepl("converted", head, fixed = TRUE)) {
    rest <- rest[!startsWith(rest, "missing values.")]
    rest <- paste(strsplit(trimws(paste(rest, collapse = " ")), "\\s+", perl = TRUE)[[1]], collapse = " ")
  }
  paste(c(head, rest[nzchar(rest)]), collapse = " | ")
}

conf_key_messages <- function(lines, version = 1L) {
  out <- character()
  i <- 1L
  n <- length(lines)
  key <- if (version >= 2L) conf_key_re2 else conf_key_re
  while (i <= n) {
    l <- lines[i]
    if (grepl(key, l, perl = TRUE) && !grepl("SASUSER|parameter catalog|registry", l, ignore.case = TRUE)) {
      parts <- l
      j <- i + 1L
      while (j <= n && grepl("^ {6}\\S", lines[j], perl = TRUE) && !startsWith(lines[j], "      real time")) {
        parts <- c(parts, lines[j])
        j <- j + 1L
      }
      if (startsWith(l, "ERROR")) parts <- parts[1]
      out <- c(out, conf_norm_msg(parts))
      i <- j
      next
    }
    i <- i + 1L
  }
  out
}

# ---- PUT output ------------------------------------------------------------------

conf_put_lines <- function(lines) {
  out <- character()
  cont <- FALSE
  prev_src <- FALSE
  for (l in gsub("\f", "", lines, fixed = TRUE)) {
    if (!nzchar(trimws(l))) next # blank lines may sit inside a NOTE's continuation
    if (grepl("^(NOTE|WARNING|ERROR|INFO)( \\d+-\\d+)?(\\(.*?\\))?:", l, perl = TRUE)) {
      cont <- TRUE
      prev_src <- FALSE
      next
    }
    if (cont && startsWith(l, "      ")) next
    cont <- FALSE
    if (grepl("^\\d+(\\s|$)", l, perl = TRUE) || grepl("^(MPRINT|MLOGIC|SYMBOLGEN|MACROGEN|MAUTOCOMPLOC)\\b", l, perl = TRUE)) {
      prev_src <- TRUE
      next
    }
    if (prev_src && grepl("^\\s+[_\\d\\s]+$", l, perl = TRUE)) next
    prev_src <- FALSE
    out <- c(out, sub("\\s+$", "", l))
  }
  out
}

# ---- L-LST ------------------------------------------------------------------------

conf_norm_lst <- function(lines) {
  host <- paste0(
    "^\\s*(Engine/Host Dependent Information|Data Set Page Size|Number of Data Set Pages|First Data Page|",
    "Max Obs per Page|Obs in First Data Page|Number of Data Set Repairs|Filename|Release Created|",
    "Host Created|Inode Number|Access Permission|Owner Name|File Size|File Size \\(bytes\\)|",
    "Index File Page Size|Number of Index File Pages)(\\s{2,}|$)"
  )
  lines <- lines[!grepl(host, lines, perl = TRUE)]
  lines <- sub("^(\\s*(?:Created|Last Modified)\\s+)\\d\\d/\\d\\d/\\d{4} \\d\\d:\\d\\d:\\d\\d", "\\1<datetime>", lines, perl = TRUE)
  lines <- trimws(gsub("\\s+", " ", gsub("\f", "", lines, fixed = TRUE), perl = TRUE))
  lines[nzchar(lines)]
}

conf_first_diff <- function(s, o, what = "#") {
  if (identical(s, o)) return("")
  for (k in seq_len(max(length(s), length(o)))) {
    a <- if (k <= length(s)) s[k] else "<none>"
    b <- if (k <= length(o)) o[k] else "<none>"
    if (!identical(a, b)) {
      return(sprintf("%s%d: SAS=%s sasonr=%s", what, k, encodeString(substr(a, 1, 150), quote = "'"),
                     encodeString(substr(b, 1, 150), quote = "'")))
    }
  }
  ""
}

# ---- files written ------------------------------------------------------------------

conf_written <- function(dir) {
  f <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  top <- sub("/.*$", "", f)
  keep <- !(top %in% c("program.sas", "data", "lib", ".sasuser", "ds", "html")) &
    !grepl("\\.(html?|rtf|pdf|xlsx?|sas7b\\w+|png|svg|gif|jpe?g|css)$", f, ignore.case = TRUE)
  sort(f[keep])
}

# ---- numeric tolerance -------------------------------------------------------------------

# Lines that differ only in numbers (decimal, or IEEE hex after "hex=") within a
# relative tolerance; returns the largest relative difference, or NA when the
# lines differ otherwise.
conf_hex_num <- function(h) {
  if (!grepl("^[0-9A-Fa-f]{16}$", h)) return(NA_real_)
  b <- as.raw(strtoi(substring(h, seq(1, 15, 2), seq(2, 16, 2)), 16L))
  readBin(b, "double", endian = "big")
}
conf_tol_diff <- function(s, o) {
  if (length(s) != length(o)) return(NA_real_)
  worst <- 0
  for (k in seq_along(s)) {
    if (identical(s[k], o[k])) next
    ts <- strsplit(trimws(s[k]), "\\s+")[[1]]
    to <- strsplit(trimws(o[k]), "\\s+")[[1]]
    if (length(ts) != length(to)) return(NA_real_)
    # the same value (hex=) written differently: a display difference, not a
    # difference in the last bits of the value
    hx <- startsWith(ts, "hex=")
    if (any(hx) && identical(ts[hx], to[hx])) return(NA_real_)
    for (j in seq_along(ts)) {
      if (identical(ts[j], to[j])) next
      ps <- sub("^([A-Za-z_][A-Za-z0-9_]*=)?(.*)$", "\\1", ts[j])
      po <- sub("^([A-Za-z_][A-Za-z0-9_]*=)?(.*)$", "\\1", to[j])
      if (!identical(ps, po)) return(NA_real_)
      vs <- substring(ts[j], nchar(ps) + 1L)
      vo <- substring(to[j], nchar(po) + 1L)
      if (ps == "hex=") {
        a <- conf_hex_num(vs)
        b <- conf_hex_num(vo)
      } else {
        a <- suppressWarnings(as.numeric(vs))
        b <- suppressWarnings(as.numeric(vo))
      }
      if (is.na(a) || is.na(b) || !is.finite(a) || !is.finite(b)) return(NA_real_)
      if (a == b) next
      if (sign(a) != sign(b)) return(NA_real_)
      worst <- max(worst, abs(a - b) / max(abs(a), abs(b)))
    }
  }
  worst
}

# ---- verdicts -------------------------------------------------------------------------------
#
# Used by the tests (test-conformance-cases.R) and by the feature registry
# (tools/update_registry.R), so that both apply the same rules.

# Registry class of one case result: "pass" only when every layer agrees
# (values, attributes, log NOTEs, PUT text, files, listing; both_error: the
# layers compared after a SAS ERROR at the same step agree); "approximate"
# when the only differences are numbers in their last bits (at most 1e-9
# relative, `tol`; a display difference of the same value has no `tol`);
# "unsupported" when sasonr failed loud where SAS ran; "mismatch" otherwise,
# also for a log-only or display-only difference and for a SAS ERROR answered
# by fail loud or an R error.
conf_case_class <- function(status, failed_layers = "", tol = NA_real_) {
  failed_layers[is.na(failed_layers)] <- ""
  ifelse(status %in% c("pass", "both_error"), "pass",
  ifelse(status == "unsupported", "unsupported",
  ifelse(status == "fail" & !is.na(tol) & tol <= 1e-9 & !grepl("attr|log|lst", failed_layers), "approximate",
         "mismatch")))
}

# Registry status of a feature from the classes of its cases.  `partial`:
# some cases pass and every other one fails loud (the feature runs only inside
# its verified domain); `unsupported`: every case fails loud.
conf_feature_status <- function(cl) {
  stopifnot(all(cl %in% c("pass", "approximate", "unsupported", "mismatch")))
  n <- length(cl)
  if (n == 0) return("unverified")
  if (all(cl == "pass")) return("verified")
  if (any(cl == "mismatch")) return("mismatch")
  if (any(cl == "approximate")) return("approximate")
  if (any(cl == "pass")) "partial" else "unsupported"
}

# Test verdict of one case result: "skip" (no gold, or fail loud: not a silent
# error), "pass" or "fail" (any difference, also in the last bits of a number:
# strict mode makes the approximate features fail loud, so a numeric
# difference is a feature that runs although it differs from SAS).
conf_case_verdict <- function(r) {
  if (r$status %in% c("no_program", "no_gold", "unsupported", "sas_error_unsupported")) return("skip")
  if (r$status %in% c("lenient", "error", "sas_error_rerror")) return("fail")
  if (r$status == "both_error") return("pass")
  if (length(r$layers) && all(r$layers)) "pass" else "fail"
}

# ---- one case ----------------------------------------------------------------------------

# Status of a case in which sasonr stopped (fail loud or an error condition).
# `both_error` only when SAS stopped with an ERROR too and sasonr raised a SAS
# ERROR (syntax or run-time error of the SAS error model) at the same step;
# when SAS errored and sasonr failed loud or stopped with an R error the case
# is `sas_error_unsupported` / `sas_error_rerror`: not a pass (the feature
# registry counts both as mismatches).
conf_fail_status <- function(sas_err, unsupported, sas_style) {
  if (!sas_err) return(if (unsupported) "unsupported" else "error")
  if (sas_style && !unsupported) return("both_error")
  if (unsupported) "sas_error_unsupported" else "sas_error_rerror"
}

conf_layers_all <- c("ds", "attr", "log", "lst", "put", "files")

#' @noRd
conf_run_case <- function(case, root_copy = NULL, timeout = NULL, keep = NULL, keep_work = FALSE) {
  case <- normalizePath(case)
  meta <- conf_read_meta(file.path(case, "meta.yaml"))
  exp <- file.path(case, "expected")
  out <- list(case = case, meta = meta, status = NA_character_, msg = "", layers = logical(), diffs = character())
  prog <- conf_program_path(case, meta)
  if (is.na(prog) || !file.exists(prog)) {
    out$status <- "no_program"
    return(out)
  }
  if (!dir.exists(exp)) {
    out$status <- "no_gold"
    return(out)
  }
  enc <- meta$encoding %||% "latin1"
  code <- readLines(prog, warn = FALSE, encoding = if (enc %in% c("utf-8", "UTF-8")) "UTF-8" else "latin1")
  run_dir <- if (identical(meta$cwd, "_root") && !is.null(root_copy)) root_copy else {
    d <- tempfile("sasonr_case_")
    dir.create(d)
    for (s in c("out", "lib")) dir.create(file.path(d, s))
    src_data <- file.path(dirname(prog), "data")
    if (dir.exists(src_data)) file.copy(src_data, d, recursive = TRUE)
    d
  }
  own_dir <- !identical(run_dir, root_copy)
  if (own_dir) on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  old <- setwd(run_dir)
  on.exit(setwd(old), add = TRUE)
  old_opt <- options(sasonr.step_notes = TRUE, sasonr.sas_version = conf_meta_version(meta))
  on.exit(options(old_opt), add = TRUE)
  limit <- as.numeric(timeout %||% meta$timeout %||% 30)
  setTimeLimit(elapsed = limit, transient = TRUE)
  res <- tryCatch(
    withCallingHandlers(sas_run(code, on_error = "sas"), warning = function(w) invokeRestart("muffleWarning")),
    sasonr_unsupported = function(e) structure(list(rc = 2L, msg = conditionMessage(e)), class = "conf_fail"),
    error = function(e) {
      m <- conditionMessage(e)
      structure(list(rc = if (grepl("time limit", m, fixed = TRUE)) 3L else 1L,
                     msg = paste0("[", class(e)[1], "] ", m)), class = "conf_fail")
    }
  )
  setTimeLimit(elapsed = Inf)
  sas_rc <- as.integer(meta$sas_rc %||% 0L)
  if (inherits(res, "conf_fail")) {
    # SAS stopped with an ERROR but sasonr did not report one the SAS way (an
    # ERROR in its result): fail loud or an R error is not a both_error
    out$msg <- res$msg
    out$status <- conf_fail_status(sas_rc >= 2L, res$rc == 2L, FALSE)
    return(out)
  }
  # the data sets sasonr left in WORK (tools/run_conformance.R: PROC statistics evidence)
  if (keep_work) out$work <- res$work
  # sas_run(on_error = "sas"): a SAS ERROR gives a result with status "error"
  gold_v <- as.integer(meta$gold_version %||% 1L)
  both_err <- identical(res$status, "error")
  if (both_err && sas_rc < 2L) {
    out$msg <- paste("[SAS ERROR in sasonr]", res$errors[1])
    out$status <- "error"
    return(out)
  }
  if (both_err && gold_v < 2L) {
    # gold from before version 2 has no WORK captured after an ERROR
    out$msg <- res$errors[1]
    out$status <- "both_error"
    return(out)
  }
  # SAS ended with an error: sasonr must report one too (then the layers are compared)
  if (sas_rc >= 2L && !any(startsWith(res$log, "ERROR"))) {
    out$status <- "lenient"
    return(out)
  }
  layers <- meta$layers %||% conf_layers_all
  # both stopped with an ERROR: compare WORK as left behind, the log (ERROR
  # texts reduced to "ERROR": SAS's syntax messages list its own parser's
  # expected tokens) and the PUT output
  if (both_err) layers <- intersect(layers, c("ds", "log", "put"))
  son_ds <- file.path(tempfile("sasonr_ds_"))
  on.exit(unlink(son_ds, recursive = TRUE), add = TRUE)
  sas_export_work(res, son_ds, values = FALSE)
  d <- list()
  sas_log <- conf_lines(file.path(exp, "log_notes.txt"))
  son_log <- conf_key_messages(res$log, gold_v)
  if (both_err) {
    # one statement can give SAS several ERROR lines (22-322 and 76-322)
    norm_err <- function(l) {
      l <- sub("^ERROR.*$", "ERROR", l)
      l[!(l == "ERROR" & c("", utils::head(l, -1L)) == "ERROR")]
    }
    sas_log <- norm_err(sas_log)
    son_log <- norm_err(son_log)
  }
  if ("ds" %in% layers) d$ds <- conf_cmp_ds(file.path(exp, "datasets"), res)
  if ("attr" %in% layers) d$attr <- conf_cmp_attr(file.path(exp, "attrs.csv"), file.path(son_ds, "__attr.csv"))
  if ("log" %in% layers) d$log <- conf_first_diff(sas_log, son_log)
  if ("lst" %in% layers) {
    d$lst <- conf_first_diff(conf_norm_lst(conf_lines(file.path(exp, "listing.txt"))), conf_norm_lst(res$listing), "line ")
  }
  if ("put" %in% layers) {
    pe <- conf_lines(file.path(exp, "put.txt"))
    po <- conf_put_lines(res$log)
    d$put <- conf_first_diff(pe, po, "line ")
    if (nzchar(d$put)) out$tol_put <- conf_tol_diff(pe, po)
  }
  if ("files" %in% layers && own_dir) {
    ef <- if (dir.exists(file.path(exp, "files"))) sort(list.files(file.path(exp, "files"), recursive = TRUE, all.files = TRUE)) else character()
    of <- conf_written(run_dir)
    if (!identical(ef, of)) {
      d$files <- sprintf("files SAS=%s sasonr=%s", paste(ef, collapse = " "), paste(of, collapse = " "))
    } else {
      d$files <- ""
      tol <- 0
      for (f in ef) {
        a <- sub("\\s+$", "", conf_lines(file.path(exp, "files", f)))
        b <- sub("\\s+$", "", conf_lines(file.path(run_dir, f), "latin1"))
        x <- conf_first_diff(a, b, paste0(f, " line "))
        if (nzchar(x)) {
          tol <- max(tol, conf_tol_diff(a, b))
          if (!nzchar(d$files)) d$files <- x
        }
      }
      out$tol_files <- tol
    }
  }
  if (!is.null(keep)) {
    dir.create(keep, recursive = TRUE, showWarnings = FALSE)
    writeLines(res$log, file.path(keep, "log"), useBytes = TRUE)
    writeLines(res$listing %||% character(), file.path(keep, "lst"), useBytes = TRUE)
    file.copy(son_ds, keep, recursive = TRUE)
    if (own_dir) for (f in conf_written(run_dir)) {
      dir.create(dirname(file.path(keep, "files", f)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(run_dir, f), file.path(keep, "files", f))
    }
  }
  out$diffs <- unlist(d)
  out$layers <- stats::setNames(!nzchar(out$diffs), names(out$diffs))
  sas_err <- any(startsWith(sas_log, "ERROR"))
  son_err <- any(startsWith(son_log, "ERROR"))
  out$status <- if (sas_err && !son_err) "lenient" else if (all(out$layers)) (if (both_err) "both_error" else "pass") else "fail"
  if (both_err) out$msg <- res$errors[1]
  out
}

# One column of a SAS data set of a case against the same column of the data
# set sasonr left in WORK (`work`, NULL when sasonr stopped): c(compared, same)
# (tools/update_proc_registry.R, PROC statistics evidence `case|dataset|column`).
conf_col_check <- function(case, work, ds, col) {
  if (is.null(work)) return(c(compared = 0, same = 0))
  e <- haven::read_sas(file.path(case, "expected", "datasets", paste0(ds, ".sas7bdat")))
  nm <- names(work)[toupper(names(work)) == toupper(ds)]
  if (!length(nm)) return(c(compared = 0, same = 0))
  o <- work[[nm]]
  j <- match(toupper(col), toupper(names(o)))
  k <- match(toupper(col), toupper(names(e)))
  if (is.na(j) || is.na(k) || nrow(o) != nrow(e)) return(c(compared = nrow(e), same = 0))
  same <- conf_num_eq(conf_sas_num(e[[k]]), conf_son_num(o[[j]]))
  c(compared = length(same), same = sum(same))
}

# ---- matrix layout (fn_matrix/, fmt_matrix/) --------------------------------------------
#
# One program per function or format (meta.yaml `layout: matrix`); every case is
# one step between `%put @@BEGIN <case> <context>;` and `%put @@END <case>;`
# (inputs in cases.csv).  The SAS gold standard holds one data set WORK.MATRIX
# with one column per case (named like the case, `$` replaced by `_`), the log
# NOTEs with the case markers, the PUT output and results.txt.  sasonr runs the
# case blocks with their original line numbers (the header is kept, other lines
# are blank; the @@COLLECT trailer that builds WORK.MATRIX is SAS-side storage
# and is not run) and each case is compared with its own part of every layer:
# ds (its column against sasonr's one-row data set), attr, log and put (the
# lines between its markers), files (the results.txt records of the case), lst.
# When sasonr stops (fail loud, syntax or run-time error) in case k, the cases
# before k are re-run without it, case k gets the status, and the run resumes
# at case k + 1.

conf_matrix_blocks <- function(lines) {
  b <- grep("^%put @@BEGIN \\S+ \\S+;\\s*$", lines)
  e <- grep("^%put @@END \\S+;\\s*$", lines)
  ids <- sub("^%put @@BEGIN (\\S+) .*$", "\\1", lines[b])
  if (length(b) != length(e) || any(e < b) || !identical(ids, sub("^%put @@END (\\S+);.*$", "\\1", lines[e]))) {
    stop("matrix program: unbalanced @@BEGIN / @@END markers")
  }
  list(ids = ids, begin = b, end = e, header = seq_len(if (length(b)) b[1] - 1L else length(lines)))
}

conf_matrix_ds_name <- function(id) gsub("$", "_", id, fixed = TRUE)

# Program text of blocks `idx` with the original line numbers.
conf_matrix_segment <- function(lines, bl, idx) {
  out <- character(max(bl$end[idx]))
  out[bl$header] <- lines[bl$header]
  for (k in idx) out[bl$begin[k]:bl$end[k]] <- lines[bl$begin[k]:bl$end[k]]
  out
}

# Split marked lines into the parts between `@@BEGIN <id>` and `@@END <id>`
# (inclusive = keep the marker lines, as in put.txt).
conf_matrix_split <- function(lines, inclusive = FALSE) {
  out <- list()
  cur <- NULL
  for (l in lines) {
    if (startsWith(l, "@@BEGIN ")) {
      cur <- strsplit(l, " ", fixed = TRUE)[[1]][2]
      out[[cur]] <- if (inclusive) l else character()
      next
    }
    if (startsWith(l, "@@END ") || startsWith(l, "@@COLLECT")) {
      if (!is.null(cur) && inclusive && startsWith(l, "@@END ")) out[[cur]] <- c(out[[cur]], l)
      cur <- NULL
      next
    }
    if (!is.null(cur)) out[[cur]] <- c(out[[cur]], l)
  }
  out
}

# results.txt records by case (the second word of every record)
conf_matrix_records <- function(lines) {
  id <- vapply(strsplit(lines, " ", fixed = TRUE), function(p) if (length(p) >= 2) p[2] else "", "")
  split(lines, factor(id, levels = unique(id)))
}

conf_key_messages_marked <- function(lines, version = 1L) {
  mark <- grepl("^@@(BEGIN|END|COLLECT)\\b", lines, perl = TRUE)
  if (!any(mark)) return(conf_key_messages(lines, version))
  grp <- cumsum(mark)
  out <- character()
  for (g in unique(grp)) {
    seg <- lines[grp == g]
    if (mark[grp == g][1]) {
      out <- c(out, sub("\\s+$", "", seg[1]))
      seg <- seg[-1]
    }
    out <- c(out, conf_key_messages(seg, version))
  }
  out
}

# One sas_run() of a program segment in a fresh directory.  On an error the
# result is a "conf_fail" whose `partial` holds the result of the steps run
# before the error (NULL after a time limit or when it cannot be built) and
# keeps its run directory.
conf_matrix_run_segment <- function(code, data_dir, limit) {
  d <- tempfile("sasonr_mx_")
  dir.create(d)
  for (s in c("out", "lib")) dir.create(file.path(d, s))
  if (dir.exists(data_dir)) file.copy(data_dir, d, recursive = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  setTimeLimit(elapsed = limit, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  rp <- tryCatch(
    withCallingHandlers(engine_run_partial(code), warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) list(result = NULL, error = e)
  )
  setTimeLimit(elapsed = Inf)
  if (!is.null(rp$error)) {
    e <- rp$error
    m <- conditionMessage(e)
    timeout <- grepl("time limit", m, fixed = TRUE)
    partial <- if (timeout) NULL else rp$result
    if (is.null(partial)) unlink(d, recursive = TRUE) else partial$run_dir <- d
    return(structure(list(unsupported = inherits(e, "sasonr_unsupported"),
                          sas_style = inherits(e, c("sasonr_syntax_error", "sasonr_runtime_error")),
                          line = suppressWarnings(as.integer(e$sas_line %||% NA)),
                          msg = paste0("[", class(e)[1], "] ", m), timeout = timeout, partial = partial),
                     class = "conf_fail"))
  }
  res <- rp$result
  res$run_dir <- d
  res
}

# Did SAS stop with an ERROR in matrix case `id` (`s_log`: the case's log NOTEs)?
conf_matrix_sas_err <- function(s_log, id) {
  ds <- conf_matrix_ds_name(id)
  done_note <- c(paste0("NOTE: The data set WORK.", toupper(ds), " has "), paste0("NOTE: Table WORK.", toupper(ds), " created"))
  any(startsWith(s_log, "ERROR")) && !any(startsWith(s_log, done_note[1]) | startsWith(s_log, done_note[2]))
}

#' @noRd
conf_run_matrix <- function(case, keep = NULL, only = NULL) {
  case <- normalizePath(case)
  meta <- conf_read_meta(file.path(case, "meta.yaml"))
  exp <- file.path(case, "expected")
  lines <- readLines(file.path(case, "program.sas"), warn = FALSE, encoding = "latin1")
  bl <- conf_matrix_blocks(lines)
  n <- length(bl$ids)
  blank <- function(id) list(case = id, meta = meta, status = NA_character_, msg = "", layers = logical(), diffs = character())
  if (!dir.exists(exp)) return(lapply(stats::setNames(bl$ids, bl$ids), function(id) modifyList(blank(id), list(status = "no_gold"))))
  data_dir <- file.path(case, "data")
  per <- as.numeric(meta$timeout %||% 30)
  # the gold logs hold the step NOTEs (testthat turns them off, helper-options.R)
  old_opt <- options(sasonr.step_notes = TRUE, sasonr.sas_version = conf_meta_version(meta))
  on.exit(options(old_opt), add = TRUE)
  segs <- list()
  owner <- rep(NA_integer_, n)
  fails <- vector("list", n)
  partial_dirs <- character()
  on.exit(unlink(partial_dirs, recursive = TRUE), add = TRUE)
  run <- function(idx) {
    r <- conf_matrix_run_segment(conf_matrix_segment(lines, bl, idx), data_dir, per + 5 * length(idx))
    if (!inherits(r, "conf_fail")) {
      segs[[length(segs) + 1L]] <<- r
      owner[idx] <<- length(segs)
    } else if (!is.null(r$partial)) {
      partial_dirs <<- c(partial_dirs, r$partial$run_dir)
    }
    r
  }
  one_by_one <- function(idx) for (j in idx) {
    r <- run(j)
    if (inherits(r, "conf_fail")) fails[[j]] <<- r
  }
  # the blocks to run: all, or only those of the selected cases (`only`; the
  # blocks are independent steps)
  sel <- if (is.null(only)) seq_len(n) else which(bl$ids %in% only)
  start <- 1L
  while (start <= length(sel)) {
    idx <- sel[start:length(sel)]
    r <- run(idx)
    if (!inherits(r, "conf_fail")) break
    k <- if (is.na(r$line)) NA_integer_ else which(bl$begin <= r$line & bl$end >= r$line)[1]
    pos <- match(k, sel)
    if (is.na(pos) || pos < start) {
      one_by_one(idx)
      break
    }
    if (pos > start) {
      before <- sel[start:(pos - 1L)]
      if (!is.null(r$partial)) {
        # the cases before the failing one ran to completion in this segment:
        # keep their results (without the failing case's data set)
        p <- r$partial
        ds_k <- tolower(conf_matrix_ds_name(bl$ids[k]))
        p$work[[ds_k]] <- NULL
        p$meta[[ds_k]] <- NULL
        partial_dirs <- setdiff(partial_dirs, p$run_dir)
        segs[[length(segs) + 1L]] <- p
        owner[before] <- length(segs)
      } else {
        r2 <- run(before)
        if (inherits(r2, "conf_fail")) one_by_one(before)
      }
    }
    fails[[k]] <- r
    start <- pos + 1L
  }
  on.exit(for (s in segs) unlink(s$run_dir, recursive = TRUE), add = TRUE)

  # SAS side
  sas_log <- conf_matrix_split(conf_lines(file.path(exp, "log_notes.txt")))
  sas_put <- conf_matrix_split(conf_lines(file.path(exp, "put.txt")), inclusive = TRUE)
  rf <- file.path(exp, "files", "results.txt")
  sas_rec <- conf_matrix_records(if (file.exists(rf)) sub("\\s+$", "", conf_lines(rf)) else character())
  mx <- list.files(file.path(exp, "datasets"), pattern = "^matrix\\.sas7bdat$", full.names = TRUE)
  sas_ds <- if (length(mx)) haven::read_sas(mx) else NULL
  sas_attr <- conf_attrs(file.path(exp, "attrs.csv"), "UTF-8")
  sas_lst <- conf_norm_lst(conf_lines(file.path(exp, "listing.txt")))

  # sasonr side, per successful segment
  seg_info <- lapply(seq_along(segs), function(s) {
    r <- segs[[s]]
    son_ds <- tempfile("sasonr_ds_")
    sas_export_work(r, son_ds, values = FALSE)
    a <- conf_attrs(file.path(son_ds, "__attr.csv"), "latin1")
    unlink(son_ds, recursive = TRUE)
    rf <- file.path(r$run_dir, "results.txt")
    if (!is.null(keep)) {
      kd <- file.path(keep, sprintf("segment_%02d", s))
      dir.create(kd, recursive = TRUE, showWarnings = FALSE)
      writeLines(r$log, file.path(kd, "log"), useBytes = TRUE)
      if (file.exists(rf)) file.copy(rf, kd)
    }
    list(log = conf_matrix_split(conf_key_messages_marked(r$log, as.integer(meta$gold_version %||% 1L))),
         put = conf_matrix_split(conf_put_lines(r$log), inclusive = TRUE),
         rec = conf_matrix_records(if (file.exists(rf)) sub("\\s+$", "", conf_lines(rf, "latin1")) else character()),
         attr = a, lst = conf_norm_lst(r$listing), log_raw = r$log)
  })

  out <- list()
  for (k in seq_len(n)) {
    id <- bl$ids[k]
    if (!is.null(only) && !id %in% only) next
    o <- blank(id)
    ds <- conf_matrix_ds_name(id)
    s_log <- sas_log[[id]] %||% character()
    # SAS "stopped with an ERROR" in this case (the one-case layout used SAS's return
    # code): an ERROR and the case's step did not complete (no data set / table NOTE).
    # A function that writes an ERROR line but returns a value (PRXMATCH with an
    # invalid pattern: SAS return code 0) does not count.
    sas_err <- conf_matrix_sas_err(s_log, id)
    if (!is.null(fails[[k]])) {
      o$msg <- fails[[k]]$msg
      o$status <- conf_fail_status(sas_err, isTRUE(fails[[k]]$unsupported), isTRUE(fails[[k]]$sas_style))
      out[[id]] <- o
      next
    }
    si <- seg_info[[owner[k]]]
    res <- segs[[owner[k]]]
    o_log <- si$log[[id]] %||% character()
    son_err <- any(startsWith(o_log, "ERROR"))
    if (sas_err && !son_err) {
      o$status <- "lenient"
      out[[id]] <- o
      next
    }
    d <- list()
    # ds: the case's column of WORK.MATRIX against sasonr's one-row data set
    has_e <- !is.null(sas_ds) && ds %in% names(sas_ds)
    od <- res$work[[tolower(ds)]]
    has_o <- !is.null(od) && nrow(od) > 0
    d$ds <- if (!has_e && !has_o) "" else if (has_e != has_o) {
      sprintf("members SAS=%s sasonr=%s", if (has_e) toupper(ds) else "", if (has_o) toupper(ds) else "")
    } else {
      disp <- vapply(names(od), function(v) res$meta[[tolower(ds)]]$disp[[v]] %||% v, "")
      if (!identical(unname(disp), "r")) {
        sprintf("%s: columns SAS=r sasonr=%s", toupper(ds), paste(utils::head(disp, 8), collapse = ","))
      } else if (nrow(od) != 1L) {
        sprintf("%s: rows SAS=1 sasonr=%d", toupper(ds), nrow(od))
      } else {
        a <- sas_ds[[ds]]
        b <- od[[1]]
        a <- if (is.character(a)) {
          bad <- !is.na(a) & !validUTF8(a)
          a[bad] <- iconv(a[bad], "latin1", "UTF-8")
          sub("\\s+$", "", a)
        } else conf_sas_num(a)
        b <- if (is.character(b)) sas_rtrim(b) else conf_son_num(b)
        if (is.list(a) != is.list(b)) {
          sprintf("%s r: type differs", toupper(ds))
        } else if (!isTRUE(if (is.list(a)) conf_num_eq(a, b) else a == b)) {
          sv <- if (is.list(a)) (if (is.na(a$miss)) sprintf("%.17g", a$v) else a$miss) else encodeString(a, quote = "'")
          ov <- if (is.list(b)) (if (is.na(b$miss)) sprintf("%.17g", b$v) else b$miss) else encodeString(b, quote = "'")
          sprintf("%s row 1 r: SAS=%s sasonr=%s", toupper(ds), sv, ov)
        } else ""
      }
    }
    # attr
    ka <- paste("MATRIX", tolower(ds), sep = ".")
    kb <- paste(toupper(ds), "r", sep = ".")
    d$attr <- ""
    if (!is.null(sas_attr[[ka]]) && !is.null(si$attr[[kb]])) {
      nm <- c("type", "length", "format", "informat", "label")
      for (i in seq_along(nm)) if (!identical(sas_attr[[ka]][i], si$attr[[kb]][i])) {
        d$attr <- sprintf("%s %s: SAS=%s sasonr=%s", kb, nm[i], encodeString(sas_attr[[ka]][i], quote = "'"),
                          encodeString(si$attr[[kb]][i], quote = "'"))
        break
      }
    }
    d$log <- conf_first_diff(s_log, o_log)
    d$lst <- conf_first_diff(sas_lst, si$lst, "line ")
    pe <- sas_put[[id]] %||% character()
    po <- si$put[[id]] %||% character()
    d$put <- conf_first_diff(pe, po, "line ")
    if (nzchar(d$put)) o$tol_put <- conf_tol_diff(pe, po)
    fe <- sas_rec[[id]] %||% character()
    fo <- si$rec[[id]] %||% character()
    if (length(fe) == 0 && length(fo) == 0) {
      d$files <- ""
    } else if ((length(fe) == 0) != (length(fo) == 0)) {
      d$files <- sprintf("files SAS=%s sasonr=%s", if (length(fe)) "results.txt" else "", if (length(fo)) "results.txt" else "")
    } else {
      d$files <- conf_first_diff(fe, fo, "results.txt line ")
      o$tol_files <- if (nzchar(d$files)) conf_tol_diff(fe, fo) else 0
    }
    o$diffs <- unlist(d)
    o$layers <- stats::setNames(!nzchar(o$diffs), names(o$diffs))
    o$status <- if (all(o$layers)) "pass" else "fail"
    out[[id]] <- o
  }
  out
}

# ---- test tiers ----------------------------------------------------------------------------
#
# none   R CMD check (NOT_CRAN unset) or SASONR_CONFORMANCE=none: unit tests only
# smoke  local devtools::test() (NOT_CRAN=true), the default: unit tests and a
#        deterministic sample of the conformance cases (conf_smoke_units())
# full   SASONR_CONFORMANCE=full: every case.  The release check is
#        tools/run_conformance.R; this tier mirrors it inside testthat.
conf_tier <- function() {
  v <- tolower(Sys.getenv("SASONR_CONFORMANCE"))
  if (v %in% c("none", "smoke", "full")) return(v)
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return("none")
  "smoke"
}

# Test units of a conformance root: list(case, ids, subset).  `ids` is NULL for a
# one-program case, the case ids of a matrix program otherwise (`subset`: run only
# these blocks).  fn_examples/ is compared call by call in test-fn-examples.R.
# Smoke rule (stable, no randomness): the first case (sorted path) of every
# directory that holds cases - one per function directory (functions/<FN>/) and
# per flat area (rounding/, semantics/, parse_stress/, freq_stats/, ...) - and
# the first case (cases.csv order) of every function / format matrix.
conf_units_for_tier <- function(root, tier = conf_tier()) {
  cases <- conf_list_cases(root)
  rel <- substring(cases, nchar(root) + 2L)
  keep <- !startsWith(rel, "fn_examples/")
  cases <- cases[keep]
  rel <- rel[keep]
  matrix <- vapply(cases, function(d) identical(conf_read_meta(file.path(d, "meta.yaml"))$layout, "matrix"), TRUE)
  if (tier == "smoke") {
    grp <- dirname(rel)
    keep <- matrix | !duplicated(grp)
    cases <- cases[keep]
    matrix <- matrix[keep]
  }
  lapply(seq_along(cases), function(k) {
    if (!matrix[k]) return(list(case = cases[k], ids = NULL, subset = FALSE))
    ids <- utils::read.csv(file.path(cases[k], "cases.csv"), colClasses = "character")$case
    if (tier == "smoke") list(case = cases[k], ids = ids[1], subset = TRUE) else list(case = cases[k], ids = ids, subset = FALSE)
  })
}
