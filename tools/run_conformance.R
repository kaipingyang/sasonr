# Offline conformance report: run every case under one or more conformance
# roots through sasonr and compare with the stored SAS gold standard.
#
# Usage (from the package root; one R process at a time, or
# .claude/conf_parallel.sh, which calls this script):
#   OPENBLAS_NUM_THREADS=1 Rscript tools/run_conformance.R [options] [ROOT ...]
# Options:
#   --pattern REGEX   only cases whose path matches (matrix cases: the case id may match)
#   --out FILE.csv    per-case CSV (status, failing layers, first difference) of
#                     the strict verdicts; FILE-strict-false.csv: the
#                     sasonr.strict = FALSE verdicts of the same run
#   --keep DIR        save sasonr's outputs per case (log, lst, ds/, files/)
#   --strict true|false
#                     a run in one mode only (sasonr.strict = TRUE / FALSE).
#                     Without it (the default) every case runs ONCE with
#                     sasonr.strict = FALSE and both verdicts are made from
#                     that run: strict mode only differs where a strict gate
#                     (approximate / unverified feature) fires, and the runner
#                     records each gate instead of aborting.  A case without a
#                     gate has the same strict verdict; a case whose first
#                     gate is not caught inside sasonr fails loud in strict
#                     mode (unsupported / sas_error_unsupported, with the
#                     gate's message); a case whose gate is inside a handler
#                     of sasonr (error recovery), and every matrix program
#                     with a gate, is run a second time in strict mode.
#   --status S[,S]    only cases of functions / formats whose registry status is
#                     S (e.g. `--status approximate --strict false` measures how
#                     close the approximate functions are)
#   --since BASE      incremental: run only the case directories whose recorded
#                     functions or registry rows changed since commit BASE
#                     (default origin/main), or whose files changed; the other
#                     cases keep their verdicts from the archived run of BASE
#                     (tools/conf_impact.R; without an archived run of BASE
#                     every case runs)
#   --baseline C      compare the verdicts case by case with the archived run
#                     of commit C (both modes) and print the changed cases
#   --no-archive      do not write the results archive
#   --list-units F    (conf_parallel.sh) write the selected case directories
#                     to F and stop;  --units F  run only the directories in F;
#                     --root-names A,B  archive names of the ROOTs (local copies);
#                     --shard-out F.rds  write the raw results and stop;
#                     --merge F.rds ... --selection S.rds  combine shards
# ROOT defaults to tests/conformance.  Each case runs for the SAS release of its
# gold (meta.yaml `sas_version`: TS1M7, or TS1M2 for cases/ts1m2/ of the
# private repository), see conf_meta_version().
#
# Results archive (tools/conf_archive.R): every run over all cases of a root
# (also an incremental --since run) writes the per-case CSVs of both modes, the
# summaries and the impact map to .claude/results/<commit>/ (local only).
# Per-case status:
#   pass         every compared layer agrees
#   fail         sasonr ran, some layer differs (columns ds/attr/log/lst/put/files)
#   unsupported  sasonr_unsupported (SAS ran normally)
#   error        sasonr stopped with an error (SAS ran normally)
#   both_error   SAS stopped with an ERROR and sasonr reported a SAS ERROR at the
#                same step (the layers compared after it agree)
#   sas_error_unsupported  SAS stopped with an ERROR, sasonr failed loud instead
#   sas_error_rerror       SAS stopped with an ERROR, sasonr stopped with an R error
#                (both are failures for the feature registry)
#   lenient      SAS reported an error but sasonr ran without one  (must be 0)
# Matrix programs (fn_matrix/, fmt_matrix/: meta.yaml `layout: matrix`) are
# reported per case (`<area>/<NAME>/<case>`).  The program text of cases with
# `program_ref` (book programs) is read from $SASONR_BOOKS_ROOT.

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL, flag_value = NULL) {
  k <- match(name, args)
  if (is.na(k)) return(default)
  if (!is.null(flag_value) && (k == length(args) || startsWith(args[k + 1L], "--") || dir.exists(args[k + 1L]))) {
    args <<- args[-k]
    return(flag_value)
  }
  v <- args[k + 1L]
  args <<- args[-c(k, k + 1L)]
  v
}
flag <- function(name) {
  k <- match(name, args)
  if (is.na(k)) return(FALSE)
  args <<- args[-k]
  TRUE
}
pattern <- opt("--pattern", ".")
out_csv <- opt("--out", "")
keep_dir <- opt("--keep", "")
strict <- opt("--strict", "")
status_sel <- opt("--status", "")
since <- opt("--since", "", flag_value = "origin/main")
baseline <- opt("--baseline", "")
no_archive <- flag("--no-archive")
list_units <- opt("--list-units", "")
units_file <- opt("--units", "")
shard_out <- opt("--shard-out", "")
selection_file <- opt("--selection", "")
root_names <- opt("--root-names", "")
merge <- flag("--merge")
merge_files <- if (merge) args[grepl("\\.rds$", args)] else character()
if (merge) args <- args[!grepl("\\.rds$", args)]
roots <- if (length(args)) args else "tests/conformance"
mode <- if (nzchar(strict)) (if (as.logical(toupper(strict))) "strict" else "nonstrict") else "both"
pkg <- normalizePath(".")
t_start <- Sys.time()

tools_env <- new.env()
sys.source("tools/conf_archive.R", envir = tools_env)
sys.source("tools/conf_impact.R", envir = tools_env)
for (nm in ls(tools_env)) assign(nm, get(nm, envir = tools_env))

# ---- case selection (no package needed) -------------------------------------------------

sel_env <- new.env()
sys.source("tests/testthat/helper-conformance.R", envir = sel_env)

# rows of a units file: root (path), root_name, rel
read_units <- function(f) {
  if (!file.exists(f) || !length(readLines(f, warn = FALSE))) return(data.frame(root = character(), root_name = character(), rel = character()))
  utils::read.delim(f, header = FALSE, col.names = c("root", "root_name", "rel", "secs"), colClasses = "character",
                    quote = "")[, 1:3]
}

# Selection for every root: list(root, root_name, rels (to run), incremental info)
select_roots <- function() {
  out <- list()
  ch <- NULL
  if (nzchar(since)) {
    base_key <- arc_resolve(pkg, since)
    ch <- imp_changes(base_key)
  }
  fp <- arc_fingerprint(pkg)
  rnames <- if (nzchar(root_names)) strsplit(root_names, ",", fixed = TRUE)[[1]] else character()
  for (i in seq_along(roots)) {
    root <- normalizePath(roots[i])
    rn <- if (length(rnames) >= i) rnames[i] else arc_root_name(root, pkg)
    cases <- imp_list_cases(root)
    rels <- substring(cases, nchar(root) + 2L)
    s <- list(root = root, root_name = rn, rels = rels, all = rels, inc = NULL, full = TRUE)
    if (nzchar(status_sel)) {
      st <- strsplit(status_sel, ",", fixed = TRUE)[[1]]
      reg_f <- utils::read.csv("inst/registry/functions.csv", colClasses = "character")
      reg_m <- utils::read.csv("inst/registry/formats.csv", colClasses = "character")
      if (!is.null(reg_f$sas_version)) reg_f <- reg_f[reg_f$sas_version == "TS1M7", ]
      if (!is.null(reg_m$sas_version)) reg_m <- reg_m[reg_m$sas_version == "TS1M7", ]
      fm <- readLines("tools/fmt_matrix.txt")
      fm <- strsplit(sub("^## ", "", grep("^## ", fm, value = TRUE)), "\\s+")
      fm_dir <- vapply(fm, `[`, "", 1L)
      fm_sas <- toupper(vapply(fm, function(p) {
        b <- if (length(p) > 3) p[4] else p[1]
        if (b == "-") "w.d" else if (p[3] == "C" && !startsWith(b, "$")) paste0("$", b) else b
      }, ""))
      fm_sas[fm_sas == "W.D"] <- "w.d"
      fm_kind <- vapply(fm, `[`, "", 2L)
      hit_f <- reg_f$name[reg_f$status %in% st]
      hit_m <- fm_dir[paste(fm_sas, fm_kind) %in% paste(reg_m$name, reg_m$kind)[reg_m$status %in% st]]
      sel_dirs <- c(file.path("fn_matrix", hit_f), file.path("functions", hit_f), file.path("fn_where", hit_f),
                    file.path("fmt_matrix", hit_m))
      s$rels <- s$rels[s$rels %in% sel_dirs | sub("/[^/]+$", "", s$rels) %in% sel_dirs]
      s$full <- FALSE
    }
    if (!identical(pattern, ".")) {
      # a matrix directory stays when one of its case ids matches
      hit <- grepl(pattern, cases, perl = TRUE)
      has_csv <- paste0(rels, "/cases.csv") %in% imp_root_index(root)$path
      for (i in which(!hit & has_csv)) {
        if (any(grepl("^layout:\\s*matrix", readLines(file.path(cases[i], "meta.yaml"), warn = FALSE)))) {
          ids <- utils::read.csv(file.path(cases[i], "cases.csv"), colClasses = "character")$case
          hit[i] <- any(grepl(pattern, file.path(rels[i], ids), perl = TRUE))
        }
      }
      s$rels <- intersect(s$rels, rels[hit])
      s$full <- FALSE
    }
    if (!is.null(ch)) {
      map <- arc_read_map(base_key, rn, pkg)
      if (is.null(map)) {
        cat(sprintf("%s: no archived run of %s -> every case\n", rn, substr(base_key, 1, 10)))
      } else {
        hashes <- stats::setNames(lapply(cases, imp_unit_hash, root = root), rels)
        sel <- imp_select(ch, map, rels, hashes, fp)
        cat(sprintf("%s: since %s: %s\n", rn, substr(base_key, 1, 10), if (sel$all) paste("every case:", sel$reason) else sel$reason))
        if (!sel$all) {
          s$rels <- intersect(s$rels, sel$run)
          s$inc <- list(base = base_key, keep = sel$keep, keymap = sel$keymap, map = map)
        }
      }
    }
    out[[rn]] <- s
  }
  out
}

if (merge) {
  selection <- readRDS(selection_file)
} else if (nzchar(units_file)) {
  u <- read_units(units_file)
  selection <- lapply(split(u, factor(u$root_name, levels = unique(u$root_name))), function(x) {
    list(root = normalizePath(x$root[1]), root_name = x$root_name[1], rels = x$rel, full = FALSE)
  })
} else {
  selection <- select_roots()
}

if (nzchar(list_units)) {
  lines <- character()
  for (s in selection) {
    lm <- arc_latest_map(s$root_name, pkg)
    secs <- vapply(s$rels, function(r) {
      x <- lm$units[[r]]$secs
      if (is.null(x)) NA_real_ else x
    }, 0)
    if (length(s$rels)) lines <- c(lines, paste(s$root, s$root_name, s$rels, ifelse(is.na(secs), "", sprintf("%.2f", secs)), sep = "\t"))
  }
  writeLines(lines, list_units)
  saveRDS(selection, sub("\\.[^.]*$", ".selection.rds", list_units))
  cat(sprintf("selected %d case dir(s)\n", length(lines)))
  quit(save = "no", status = 0)
}

# ---- run ---------------------------------------------------------------------------------

row_of <- function(root_name, area, rel, r, meta, env) {
  fails <- names(r$layers)[!r$layers]
  first <- if (length(fails)) r$diffs[[fails[1]]] else r$msg
  data.frame(
    root = root_name, area = area, case = rel,
    sas_version = tryCatch(env$conf_meta_version(meta), error = function(e) NA_character_), status = r$status,
    failed_layers = paste(fails, collapse = "+"),
    ds = if ("ds" %in% names(r$layers)) r$layers[["ds"]] else NA,
    attr = if ("attr" %in% names(r$layers)) r$layers[["attr"]] else NA,
    log = if ("log" %in% names(r$layers)) r$layers[["log"]] else NA,
    lst = if ("lst" %in% names(r$layers)) r$layers[["lst"]] else NA,
    put = if ("put" %in% names(r$layers)) r$layers[["put"]] else NA,
    files = if ("files" %in% names(r$layers)) r$layers[["files"]] else NA,
    known_fail = paste(meta$known_fail %||% character(), collapse = "+"),
    # diagnostic only (a difference is a failure): the largest relative difference when
    # the failing layers differ only in numbers (ds, files: hex/best values, put); NA otherwise
    tol = {
      tl <- c(if ("files" %in% fails) r$tol_files %||% NA_real_, if ("put" %in% fails) r$tol_put %||% NA_real_)
      if (length(fails) && all(fails %in% c("ds", "files", "put")) && any(c("files", "put") %in% fails)) max(tl) else NA_real_
    },
    detail = gsub("[\r\n]+", " ", substr(first, 1, 300)),
    gate = r$gate %||% "",
    source = "run"
  )
}

run_selection <- function(selection) {
  # the package with instrumented code (impact map) and recorded registry reads
  load_keys <- imp_load(".")
  # devtools::load_all() leaves the package functions uncompiled: R's JIT then
  # compiles every closure created at run time (the lexer's local helpers, the
  # macro sink, lapply() bodies ...) again in every sas_run().  Byte-compile the
  # namespace once, as an installed package is (about 25 s, cached; the cases
  # then run about a third faster); the wrappers below are compiled at the end.
  imp_compile()
  imp_wrap_registry()
  ns <- asNamespace("sasonr")
  # strict gates: record every gate of a case (as strict mode would abort there)
  gate <- new.env(parent = emptyenv())
  gate_catch <- c("error", "condition", "sasonr_unsupported", "sasonr_not_verified", "sasonr_error", "rlang_error",
                  "simpleError")
  gate_caught <- function() {
    calls <- sys.calls()
    parents <- sys.parents()
    nm <- vapply(calls, function(cl) if (is.symbol(cl[[1]])) as.character(cl[[1]]) else "", "")
    b <- which(nm %in% c("sas_run", "engine_run_partial"))
    if (!length(b)) return(TRUE)
    b <- b[1]
    for (i in seq_along(calls)) {
      if (i <= b) next
      if (nm[i] %in% c("try", "try_fetch", "safely", "possibly", "quietly")) return(TRUE)
      if (nm[i] %in% c("tryCatch", "withCallingHandlers")) {
        h <- setdiff(names(calls[[i]]), c("", "expr", "finally"))
        if (nm[b] == "engine_run_partial" && parents[i] == b) next
        if (any(h %in% gate_catch)) return(TRUE)
      }
    }
    FALSE
  }
  for (gn in c("feature_check", "proc_stat_check")) local({
    orig <- get(gn, envir = ns)
    w <- function(...) {
      if (isTRUE(gate$probe)) {
        old <- options(sasonr.strict = TRUE)
        e <- tryCatch({
          orig(...)
          NULL
        }, sasonr_not_verified = function(e) e, finally = options(old))
        if (!is.null(e)) {
          gate$events[[length(gate$events) + 1L]] <- list(msg = conditionMessage(e), class = class(e)[1],
                                                         line = suppressWarnings(as.integer(e$sas_line %||% NA)),
                                                         caught = gate_caught())
        }
      }
      orig(...)
    }
    unlockBinding(gn, ns)
    assign(gn, w, envir = ns)
    lockBinding(gn, ns)
  })
  for (gn in c(names(imp_accessors), "sas_feature_registry", "sas_proc_stat_registry", "feature_check", "proc_stat_check")) {
    f <- get(gn, envir = ns)
    unlockBinding(gn, ns)
    assign(gn, compiler::cmpfun(f), envir = ns)
    lockBinding(gn, ns)
  }
  if (mode != "both") options(sasonr.strict = mode == "strict")
  env <- new.env(parent = ns)
  sys.source("tests/testthat/helper-conformance.R", envir = env)
  # PROC statistics evidence columns (case|dataset|column) of tools/update_proc_registry.R
  evidence <- list()
  if (mode != "strict" && file.exists("tools/proc_stats.tsv")) {
    spec <- utils::read.delim("tools/proc_stats.tsv", comment.char = "#", colClasses = "character", quote = "")
    ev <- unlist(strsplit(spec$evidence[!is.na(spec$evidence)], ";", fixed = TRUE))
    ev <- unique(ev[grepl("|", ev, fixed = TRUE)])
    for (e in ev) {
      p <- strsplit(e, "|", fixed = TRUE)[[1]]
      evidence[[p[1]]] <- c(evidence[[p[1]]], list(p[2:3]))
    }
  }

  # one case directory in the run mode; returns list(nonstrict = <results>, strict = <results>)
  # (<results>: one result, or the named list of a matrix program)
  run_unit <- function(case, meta, rc, keep, only, work = FALSE) {
    one <- function(strict_mode, keep) {
      if (identical(meta$layout, "matrix")) {
        tryCatch(env$conf_run_matrix(case, keep = keep, only = only),
                 error = function(e) list(`?` = list(status = "error", layers = logical(), diffs = character(),
                                                    msg = paste("[runner]", conditionMessage(e)))))
      } else {
        tryCatch(env$conf_run_case(case, root_copy = rc, keep = keep, keep_work = work && !strict_mode),
                 error = function(e) list(status = "error", layers = logical(), diffs = character(),
                                          msg = paste("[runner]", conditionMessage(e)), meta = meta))
      }
    }
    if (mode != "both") {
      r <- one(mode == "strict", keep)
      return(stats::setNames(list(r), if (mode == "strict") "strict" else "nonstrict"))
    }
    gate$events <- list()
    gate$probe <- TRUE
    options(sasonr.strict = FALSE)
    rn <- one(FALSE, keep)
    gate$probe <- FALSE
    ev <- gate$events
    rs <- rn
    rerun <- FALSE
    if (length(ev) && !identical(meta$layout, "matrix")) {
      # strict mode stops at the first gate
      if (ev[[1]]$caught) {
        rerun <- TRUE
      } else {
        sas_rc <- as.integer(meta$sas_rc %||% 0L)
        rs <- list(status = env$conf_fail_status(sas_rc >= 2L, TRUE, FALSE), msg = ev[[1]]$msg, layers = logical(),
                   diffs = character(), meta = rn$meta %||% meta, gate = "derived")
      }
    } else if (length(ev)) {
      # matrix: strict mode stops in each case with a gate (that case fails loud)
      # and resumes with the next case; a gate outside a case block, without a
      # line or inside a handler of sasonr -> run the program in strict mode
      lines <- readLines(file.path(case, "program.sas"), warn = FALSE, encoding = "latin1")
      bl <- env$conf_matrix_blocks(lines)
      blk <- vapply(ev, function(g) {
        if (g$caught || is.na(g$line)) return(NA_integer_)
        k <- which(bl$begin <= g$line & bl$end >= g$line)
        if (length(k) == 1L) k else NA_integer_
      }, 1L)
      if (anyNA(blk) || !all(names(rn) %in% bl$ids)) {
        rerun <- TRUE
      } else {
        s_log <- env$conf_matrix_split(env$conf_lines(file.path(case, "expected", "log_notes.txt")))
        for (k in unique(blk)) {
          id <- bl$ids[k]
          if (is.null(rs[[id]])) next
          g <- ev[[match(k, blk)]]
          rs[[id]] <- list(case = id, meta = meta, status = env$conf_fail_status(env$conf_matrix_sas_err(s_log[[id]] %||% character(), id), TRUE, FALSE),
                           msg = paste0("[", g$class, "] ", g$msg), layers = logical(), diffs = character(), gate = "derived")
        }
      }
    }
    if (rerun) {
      options(sasonr.strict = TRUE)
      rs <- one(TRUE, if (nzchar(keep %||% "")) file.path(keep, "_strict"))
      options(sasonr.strict = FALSE)
      if (identical(meta$layout, "matrix")) rs <- lapply(rs, function(x) modifyList(x, list(gate = "rerun"))) else rs$gate <- "rerun"
    }
    list(nonstrict = rn, strict = rs)
  }

  t0 <- Sys.time()
  done <- 0L
  out <- list()
  for (s in selection) {
    root <- s$root
    rows <- list(strict = list(), nonstrict = list())
    units <- list()
    copies <- list()
    cases <- file.path(root, s$rels)
    for (k in seq_along(cases)) {
      case <- cases[k]
      rel <- s$rels[k]
      area <- sub("/.*$", "", rel)
      meta <- env$conf_read_meta(file.path(case, "meta.yaml"))
      only <- NULL
      if (identical(meta$layout, "matrix")) {
        if (!grepl(pattern, case, perl = TRUE)) {
          ids <- utils::read.csv(file.path(case, "cases.csv"), colClasses = "character")$case
          only <- ids[grepl(pattern, file.path(rel, ids), perl = TRUE)]
          if (!length(only)) next
        }
      } else if (!grepl(pattern, case, perl = TRUE)) next
      rc <- NULL
      if (!identical(meta$layout, "matrix") && identical(meta$cwd, "_root")) {
        if (is.null(copies[[area]])) {
          d <- tempfile("sasonr_root_")
          dir.create(d)
          file.copy(list.files(file.path(root, area, "_root"), full.names = TRUE, all.files = TRUE, no.. = TRUE), d,
                    recursive = TRUE)
          copies[[area]] <- d
        }
        rc <- copies[[area]]
      }
      imp_reset()
      tu <- Sys.time()
      ev_cols <- evidence[[rel]]
      res <- run_unit(case, meta, rc, if (nzchar(keep_dir)) file.path(keep_dir, rel), only, work = !is.null(ev_cols))
      cols <- NULL
      if (!is.null(ev_cols) && !is.null(res$nonstrict)) {
        # PROC statistics evidence (tools/proc_stats.tsv): sasonr's column against SAS's, strict = FALSE
        w <- res$nonstrict$work
        cols <- do.call(rbind, lapply(ev_cols, function(dc) {
          r <- env$conf_col_check(case, w, dc[1], dc[2])
          data.frame(case = rel, dataset = dc[1], column = dc[2], compared = r[["compared"]], same = r[["same"]])
        }))
        res$nonstrict$work <- NULL
      }
      secs <- as.numeric(difftime(Sys.time(), tu, units = "secs"))
      imp <- imp_collect()
      units[[rel]] <- list(keys = imp$keys, reg = imp$reg, hash = imp_unit_hash(case, root), secs = secs, proc_cols = cols)
      for (m in names(res)) {
        r <- res[[m]]
        if (identical(meta$layout, "matrix")) {
          for (id in names(r)) rows[[m]][[length(rows[[m]]) + 1L]] <- row_of(s$root_name, area, file.path(rel, id), r[[id]], meta, env)
        } else {
          rows[[m]][[length(rows[[m]]) + 1L]] <- row_of(s$root_name, area, rel, r, meta, env)
        }
      }
      done <- done + if (identical(meta$layout, "matrix")) length(res[[1]]) else 1L
      if (k %% 50 == 0) {
        cat(sprintf("%s: %d/%d  (%d cases)  %.0fs\n", s$root_name, k, length(cases), done,
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
        invisible(gc())
      }
    }
    for (d in copies) unlink(d, recursive = TRUE)
    out[[s$root_name]] <- list(
      strict = if (length(rows$strict)) do.call(rbind, rows$strict),
      nonstrict = if (length(rows$nonstrict)) do.call(rbind, rows$nonstrict),
      units = units)
  }
  list(roots = out, load_keys = load_keys, fingerprint = arc_fingerprint(pkg), mode = mode)
}

# ---- finalize: inherit, print, archive, compare ------------------------------------------

empty_res <- function() {
  data.frame(root = character(), area = character(), case = character(), sas_version = character(), status = character(),
             failed_layers = character(), ds = logical(), attr = logical(), log = logical(), lst = logical(),
             put = logical(), files = logical(), known_fail = character(), tol = numeric(), detail = character(),
             gate = character(), source = character())
}

finalize <- function(selection, shards) {
  modes <- if (shards[[1]]$mode == "both") c("nonstrict", "strict") else shards[[1]]$mode
  all <- list(strict = list(), nonstrict = list())
  key <- arc_key(pkg)
  for (s in selection) {
    rn <- s$root_name
    res <- list()
    units <- list()
    for (sh in shards) {
      x <- sh$roots[[rn]]
      if (is.null(x)) next
      for (m in modes) res[[m]] <- rbind(res[[m]], x[[m]])
      units <- c(units, x$units)
    }
    for (m in modes) if (is.null(res[[m]])) res[[m]] <- empty_res()
    inherited <- 0L
    if (!is.null(s$inc)) {
      # cases not affected by the changes: verdicts and impact entries of the base run
      bk <- s$inc$base
      for (m in modes) {
        old <- arc_read(bk, rn, strict = m == "strict", pkg)
        if (is.null(old)) stop("archived results of ", rn, " at ", bk, " are missing (", m, ")")
        unit_of <- function(case) ifelse(case %in% s$inc$keep, case, sub("/[^/]*$", "", case))
        old <- old[unit_of(old$case) %in% s$inc$keep, , drop = FALSE]
        if (!"gate" %in% names(old)) old$gate <- ""
        if (is.null(old$source)) old$source <- "run"
        old$source <- ifelse(is.na(old$source) | old$source == "run", paste0("inherited:", substr(bk, 1, 12)), old$source)
        old$root <- rn
        res[[m]] <- rbind(res[[m]], old[, names(res[[m]])])
        inherited <- nrow(old)
      }
      km <- s$inc$keymap
      for (u in s$inc$keep) {
        e <- s$inc$map$units[[u]]
        e$keys <- sort(unique(ifelse(e$keys %in% names(km), km[e$keys], e$keys)))
        units[[u]] <- e
      }
    }
    for (m in modes) {
      res[[m]] <- res[[m]][order(res[[m]]$case, method = "radix"), , drop = FALSE]
      all[[m]][[rn]] <- res[[m]]
    }
    if (isTRUE(s$full) && !no_archive && !nzchar(units_file)) {
      map <- list(units = units, load_keys = shards[[1]]$load_keys, fingerprint = shards[[1]]$fingerprint, commit = key)
      info <- c(paste("commit:", key), paste("date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                paste("mode:", shards[[1]]$mode), paste("shards:", length(shards)),
                paste("incremental:", if (is.null(s$inc)) "no" else paste("since", s$inc$base, "-", length(units) - length(s$inc$keep), "case dirs run,", length(s$inc$keep), "inherited")),
                paste("elapsed:", sprintf("%.0fs", as.numeric(difftime(Sys.time(), t_start, units = "secs")))))
      summ <- lapply(stats::setNames(modes, modes), function(m) arc_summary(res[[m]], m == "strict"))
      pc <- do.call(rbind, lapply(units, `[[`, "proc_cols"))
      arc_write(key, rn, res, map, info, summ, pkg, extra = if (!is.null(pc)) list(`proc-columns` = pc))
      cat(sprintf("archived %s -> .claude/results/%s/\n", rn, key))
    }
    if (nzchar(baseline)) {
      bk <- arc_resolve(pkg, baseline)
      for (m in modes) cat(arc_compare(arc_read(bk, rn, strict = m == "strict", pkg), res[[m]],
                                       sprintf("%s %s %s", substr(bk, 1, 10), rn, if (m == "strict") "strict" else "strict=false")),
                           sep = "\n")
    }
  }
  for (m in modes) {
    r <- do.call(rbind, all[[m]])
    if (is.null(r)) r <- empty_res()
    if (nzchar(out_csv)) {
      f <- if (m == "strict" || length(modes) == 1L) out_csv else sub("(\\.csv)?$", "-strict-false.csv", out_csv)
      utils::write.csv(r, f, row.names = FALSE)
    }
    cat("\n", arc_summary(r, m == "strict"), sep = "\n")
  }
}

if (merge) {
  shards <- lapply(merge_files, readRDS)
  finalize(selection, shards)
} else {
  sh <- run_selection(selection)
  if (nzchar(shard_out)) {
    saveRDS(sh, shard_out)
  } else {
    finalize(selection, list(sh))
  }
}
