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
    e <- haven::read_sas(em[[m]])
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

conf_key_messages <- function(lines) {
  out <- character()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    l <- lines[i]
    if (grepl(conf_key_re, l, perl = TRUE) && !grepl("SASUSER|parameter catalog|registry", l, ignore.case = TRUE)) {
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

# Largest relative difference in an L-DS message "SAS=<num> sasonr=<num>" (NA otherwise).
conf_ds_rel_diff <- function(r) {
  m <- regmatches(r$diffs[["ds"]], regexec("SAS=([-0-9.eE+]+) sasonr=([-0-9.eE+]+)$", r$diffs[["ds"]]))[[1]]
  if (length(m) < 3) return(NA_real_)
  a <- as.numeric(m[2])
  b <- as.numeric(m[3])
  if (is.na(a) || is.na(b) || a == b || sign(a) != sign(b)) return(NA_real_)
  abs(a - b) / max(abs(a), abs(b))
}

# ---- one case ----------------------------------------------------------------------------

conf_layers_all <- c("ds", "attr", "log", "lst", "put", "files")

#' @noRd
conf_run_case <- function(case, root_copy = NULL, timeout = NULL, keep = NULL) {
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
  old_opt <- options(sasonr.step_notes = TRUE)
  on.exit(options(old_opt), add = TRUE)
  limit <- as.numeric(timeout %||% meta$timeout %||% 30)
  setTimeLimit(elapsed = limit, transient = TRUE)
  res <- tryCatch(
    withCallingHandlers(sas_run(code), warning = function(w) invokeRestart("muffleWarning")),
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
    out$msg <- res$msg
    out$status <- if (sas_rc >= 2L) "both_error" else if (res$rc == 2L) "unsupported" else "error"
    return(out)
  }
  # SAS ended with an error: sasonr must report one too (then the layers are compared)
  if (sas_rc >= 2L && !any(startsWith(res$log, "ERROR"))) {
    out$status <- "lenient"
    return(out)
  }
  layers <- meta$layers %||% conf_layers_all
  son_ds <- file.path(tempfile("sasonr_ds_"))
  on.exit(unlink(son_ds, recursive = TRUE), add = TRUE)
  sas_export_work(res, son_ds)
  d <- list()
  sas_log <- conf_lines(file.path(exp, "log_notes.txt"))
  son_log <- conf_key_messages(res$log)
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
  out$status <- if (sas_err && !son_err) "lenient" else if (all(out$layers)) "pass" else "fail"
  out
}
