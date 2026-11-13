# Test impact analysis for the conformance runner (tools/run_conformance.R).
#
# Recording (every run): imp_load() loads a copy of the package whose sources
# have a marker at the start of every function body (nested functions and
# function literals included, key "R/<file>:<line>:<column of `function`>");
# the runner clears the hits before each case directory and
# stores the keys that ran (imp_collect()) in the impact map of the results
# archive (tools/conf_archive.R).  The registry accessors are wrapped as well
# (imp_wrap_registry()): each records the registry row it reads ("functions|NAME",
# "formats|NAME", "proc_stats|PROC"); any other read of a registry table
# records the whole table ("functions|*").  The package caches (registry,
# SASHELP) are emptied before each case directory, so that what a case needs
# is computed - and recorded - in that case.
#
# Selection (`--since BASE`): imp_select() maps `git diff BASE` (committed,
# uncommitted and untracked changes) onto the map recorded at BASE:
#   R/<file>.R (modified)  structural diff per top-level definition (comments and
#                          layout ignored): a changed function body or header ->
#                          its key; a changed, added or removed definition name
#                          -> the functions that refer to the name (symbol or
#                          string); a changed top-level value (list, constant,
#                          factory call) -> the functions that refer to it or to
#                          what is computed from it at load time
#   inst/registry/*.csv    the changed rows -> the cases that read them
#   case directories       a changed or new case (file sizes / times, program_ref
#                          program, data, _root) -> that case
#   every case             a new or deleted R file, any other top-level statement,
#                          a function that runs at load time, a name that code
#                          looks up dynamically (prefix literal such as "sas_f_",
#                          S3-like name), the runner (tools/run_conformance.R,
#                          tools/conf_impact.R, tools/conf_archive.R,
#                          tests/testthat/helper-conformance.R), DESCRIPTION,
#                          NAMESPACE, other files under inst/, a registry column
#                          change, a registry row change together with a change
#                          of R/registry.R or R/proc_stats_registry.R, a
#                          different R / package / SASHELP environment
#   nothing                anything else (docs, vignettes, tests, other tools)

imp_reg <- new.env(parent = emptyenv())
imp_state <- new.env(parent = emptyenv())
imp_state$depth <- 0L

imp_runner_files <- c("tools/run_conformance.R", "tools/conf_impact.R", "tools/conf_archive.R",
                      "tests/testthat/helper-conformance.R", "DESCRIPTION", "NAMESPACE")

# Calls whose arguments are code as data (not run as written): not instrumented.
imp_data_calls <- c("quote", "bquote", "expression", "substitute", "~", "alist")

imp_is_fun <- function(e) is.call(e) && identical(e[[1]], as.name("function"))
imp_is_data <- function(e) is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% imp_data_calls

# Function nodes of an expression in pre-order (the traversal shared by the
# instrumentation and the diff, so that the keys agree).
imp_fun_nodes <- function(e) {
  out <- list()
  walk <- function(x) {
    if (!is.call(x) || imp_is_data(x)) return()
    if (imp_is_fun(x)) {
      out[[length(out) + 1L]] <<- x
      fm <- x[[2]]
      for (i in seq_along(fm)) if (is.call(fm[[i]])) walk(fm[[i]])
      walk(x[[3]])
      return()
    }
    for (i in seq_along(x)) if (is.call(x[[i]])) walk(x[[i]])
  }
  walk(e)
  out
}

imp_key <- function(file, f) paste0(file, ":", f[[4]][1], ":", f[[4]][5])

imp_r_files <- function(pkg = ".") {
  sort(list.files(file.path(pkg, "R"), pattern = "\\.[Rr]$"), method = "radix")
}

# Source text of an R file with a marker at the start of every function body
# (inserted on the same lines: line numbers are unchanged).  The marker sets
# .imp_env[["<key>"]] the first time the function runs.
imp_instrument_text <- function(lines, file) {
  sf <- srcfilecopy(file, lines)
  parse(text = lines, keep.source = TRUE, srcfile = sf, encoding = "UTF-8")
  pd <- utils::getParseData(sf)
  if (is.null(pd) || !nrow(pd)) return(lines)
  # expressions whose arguments are code as data
  callee <- pd$parent[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% imp_data_calls]
  data_ids <- c(pd$parent[pd$id %in% callee], pd$parent[pd$token == "'~'"])
  par <- stats::setNames(pd$parent, pd$id)
  in_data <- function(id) {
    while (!is.na(id) && id > 0) {
      if (id %in% data_ids) return(TRUE)
      id <- par[[as.character(id)]]
    }
    FALSE
  }
  fn <- pd[pd$token %in% c("FUNCTION", "'\\\\'"), , drop = FALSE]
  ins <- data.frame(line = integer(), pos = numeric(), text = character())
  for (i in seq_len(nrow(fn))) {
    p <- fn$parent[i]
    if (in_data(p)) next
    ch <- pd[pd$parent == p, , drop = FALSE]
    ch <- ch[order(ch$line1, ch$col1), , drop = FALSE]
    body <- ch[nrow(ch), ]
    key <- paste0(file, ":", fn$line1[i], ":", fn$col1[i])
    mk <- sprintf("{if (is.null(.imp_env[[\"%s\"]])) assign(\"%s\", TRUE, envir = .imp_env); ", key, key)
    ins <- rbind(ins, data.frame(line = c(body$line1, body$line2), pos = c(body$col1 - 0.5, body$col2 + 0.5),
                                 text = c(mk, "}")))
  }
  for (ln in unique(ins$line)) {
    x <- ins[ins$line == ln, , drop = FALSE]
    x <- x[order(-x$pos), , drop = FALSE]
    l <- lines[ln]
    for (j in seq_len(nrow(x))) {
      at <- floor(x$pos[j])
      l <- paste0(substr(l, 1, at), x$text[j], substr(l, at + 1, nchar(l)))
    }
    lines[ln] <- l
  }
  lines
}

# Persistent cache directory for the instrumented sources, the meta.yaml facts
# and the compiled closures.  On POSIX machines it lives under /tmp (fast,
# ephemeral); Windows has no /tmp and a session tempdir would not survive between
# runs, so the per-user R cache directory is used there.  SASONR_IMP_CACHE
# overrides both (e.g. a faster local disk).
imp_cache_dir <- function() {
  d <- Sys.getenv("SASONR_IMP_CACHE", unset = "")
  if (nzchar(d)) return(d)
  if (.Platform$OS.type == "windows") {
    file.path(tools::R_user_dir("sasonr", "cache"), "imp")
  } else {
    file.path("/tmp", paste0("sasonr_imp_cache_", Sys.getenv("USER")))
  }
}

# Load the package instrumented: a copy of DESCRIPTION, NAMESPACE and the
# instrumented R/ (inst/ linked) loaded with devtools::load_all().  Returns the
# keys of the functions that ran at load time.
imp_load <- function(pkg = ".") {
  d <- file.path(tempfile("sasonr_imp_"), "sasonr")
  dir.create(file.path(d, "R"), recursive = TRUE)
  file.copy(file.path(pkg, c("DESCRIPTION", "NAMESPACE")), d)
  # a symlink saves the copy of inst/; creating one needs a privilege Windows
  # users usually do not have, so Windows gets a plain copy (inst/ is small)
  inst_src <- normalizePath(file.path(pkg, "inst"))
  inst_dst <- file.path(d, "inst")
  if (.Platform$OS.type == "windows") {
    file.copy(inst_src, d, recursive = TRUE)
  } else {
    file.symlink(inst_src, inst_dst)
  }
  writeLines(".imp_env <- new.env(parent = emptyenv())", file.path(d, "R", "0000_imp_env.R"))
  # instrumented files are cached by the md5 of their source (local disk)
  cache <- imp_cache_dir()
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  for (f in imp_r_files(pkg)) {
    src <- file.path(pkg, "R", f)
    cf <- file.path(cache, paste0(tools::md5sum(src), "_", f))
    if (!file.exists(cf)) {
      lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
      tmp <- tempfile(tmpdir = cache)
      writeLines(enc2utf8(imp_instrument_text(lines, paste0("R/", f))), tmp, useBytes = TRUE)
      file.rename(tmp, cf)
    }
    file.copy(cf, file.path(d, "R", f))
  }
  suppressMessages(devtools::load_all(d, quiet = TRUE, helpers = FALSE))
  imp_state$hits <- asNamespace("sasonr")[[".imp_env"]]
  keys <- sort(ls(imp_state$hits, all.names = TRUE))
  rm(list = keys, envir = imp_state$hits)
  keys
}

# Registry accessors: name -> function of the accessor's arguments giving the
# registry key(s) read.  Any other caller of a registry table records "<table>|*".
imp_norm_fmt <- function(x) {
  x <- toupper(sub("^\\$", "", x))
  x[x %in% c("W.D", "F")] <- ""
  x
}
imp_accessors <- list(
  feature_status = function(kind, name) paste0(if (kind == "function") "functions|" else "formats|",
                                               if (kind == "function") toupper(name) else imp_norm_fmt(name)),
  feature_contexts = function(name) paste0("functions|", toupper(name)),
  registry_arg_types = function(name, n) paste0("functions|", toupper(name)),
  registry_arg_counts = function(name) paste0("functions|", toupper(name)),
  registry_defw = function(kind, fmt) paste0("formats|", imp_norm_fmt(fmt$name)),
  proc_stat_status = function(proc, stat, condition = "default") {
    p <- toupper(proc)
    paste0("proc_stats|", if (p == "SUMMARY") "MEANS" else p)
  },
  sysfunc_spec_from_registry = function(fname) paste0("functions|", toupper(fname)),
  where_arg_types = function(e, st) paste0("functions|", toupper(e$name))
)

imp_wrap_registry <- function() {
  ns <- asNamespace("sasonr")
  wrap <- function(nm, w) {
    unlockBinding(nm, ns)
    assign(nm, w, envir = ns)
    lockBinding(nm, ns)
  }
  for (nm in names(imp_accessors)) {
    if (!exists(nm, envir = ns, inherits = FALSE)) stop("conf_impact: registry accessor ", nm, " not found")
    local({
      orig <- get(nm, envir = ns)
      kf <- imp_accessors[[nm]]
      if (!identical(names(formals(kf)), names(formals(orig)))) {
        stop("conf_impact: arguments of registry accessor ", nm, " changed; update imp_accessors")
      }
      w <- function(...) {
        k <- tryCatch(kf(...), error = function(e) NULL)
        if (length(k) == 1L && !is.na(k)) assign(k, TRUE, envir = imp_reg) else imp_state$wild <- TRUE
        imp_state$depth <- imp_state$depth + 1L
        on.exit(imp_state$depth <- imp_state$depth - 1L)
        orig(...)
      }
      wrap(nm, w)
    })
  }
  # whole-table reads outside the accessors
  fr <- get("sas_feature_registry", envir = ns)
  w <- fr
  body(w) <- substitute({
    out <- ORIG(kind = kind, sas_version = sas_version)
    if (IMP$depth == 0L) assign(paste0(match.arg(kind), "|*"), TRUE, envir = REG)
    out
  }, list(ORIG = fr, IMP = imp_state, REG = imp_reg))
  wrap("sas_feature_registry", w)
  pr <- get("sas_proc_stat_registry", envir = ns)
  if (length(formals(pr))) stop("conf_impact: sas_proc_stat_registry() has arguments; update imp_wrap_registry()")
  w <- pr
  body(w) <- substitute({
    out <- ORIG()
    if (IMP$depth == 0L) assign("proc_stats|*", TRUE, envir = REG)
    out
  }, list(ORIG = pr, IMP = imp_state, REG = imp_reg))
  wrap("sas_proc_stat_registry", w)
  invisible()
}

# Before each case directory: no hits, empty package caches.
imp_reset <- function() {
  rm(list = ls(imp_state$hits, all.names = TRUE), envir = imp_state$hits)
  rm(list = ls(imp_reg, all.names = TRUE), envir = imp_reg)
  imp_state$wild <- FALSE
  ns <- asNamespace("sasonr")
  for (cn in c("registry_cache", "sashelp_cache")) {
    ce <- ns[[cn]]
    if (is.environment(ce)) rm(list = ls(ce, all.names = TRUE), envir = ce)
  }
}

imp_collect <- function() {
  reg <- sort(ls(imp_reg, all.names = TRUE))
  # an accessor call whose key could not be computed: the whole registry
  if (isTRUE(imp_state$wild)) reg <- c(reg, "*")
  list(keys = sort(ls(imp_state$hits, all.names = TRUE)), reg = reg)
}

# ---- case directory fingerprint ---------------------------------------------------------

# Every file under a root: relative path, size and the md5 of its content
# (symbolic links followed, dot files included).  The file list and times are
# slow to read on the shared file system (latency-bound), so the root is read
# by parallel `find` processes, one per second-level directory; the md5 sums
# are cached on local disk by (path, size, mtime, ctime) - a content change
# always changes the ctime, which no copy or `touch` can set back - and only
# new or changed files are read.
#
# Windows (local port): there is no POSIX `find`, `xargs` or `md5sum`, and R's
# system() runs through cmd.exe, so the same index is built with list.files() /
# file.info() / tools::md5sum(); the cache directory comes from imp_cache_dir().
imp_index_cache <- new.env(parent = emptyenv())
imp_md5_cache_file <- function() file.path(imp_cache_dir(), "md5_index.rds")
imp_root_index <- function(root) {
  root <- normalizePath(root)
  if (!is.null(imp_index_cache[[root]])) return(imp_index_cache[[root]])
  if (.Platform$OS.type == "windows") {
    # no POSIX find/xargs: same index from base R; ctime is used only as a
    # cache signature, so whole-second resolution is enough
    rel <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                      include.dirs = FALSE, full.names = FALSE)
    info <- file.info(file.path(root, rel))
    ix <- data.frame(path = rel, size = as.character(info$size),
                     mtime = as.character(as.integer(info$mtime)),
                     ctime = as.character(as.integer(info$ctime)),
                     stringsAsFactors = FALSE)
    ix <- ix[!is.na(ix$mtime), , drop = FALSE]
  } else {
    w <- tempfile("sasonr_idx_")
    dir.create(w)
    on.exit(unlink(w, recursive = TRUE))
    fmt <- shQuote("%p\\t%s\\t%T@\\t%C@\\n")
    sh <- paste0(
      "cd ", shQuote(root), " && find -L . -maxdepth 2 -type f -printf ", fmt, " > ", shQuote(file.path(w, "top")),
      " && find -L . -mindepth 2 -maxdepth 2 -type d -print0 | xargs -0 -r -P 16 -n 8 sh -c ",
      shQuote(paste0("for d; do find -L \"$d\" -mindepth 1 -type f -printf ", fmt, "; done > ", w, "/part_$$"), type = "sh"), " _")
    if (system(sh) != 0) stop("conf_impact: indexing ", root, " failed")
    x <- unlist(lapply(list.files(w, full.names = TRUE), readLines, warn = FALSE, encoding = "UTF-8"))
    p <- strsplit(x, "\t", fixed = TRUE)
    ix <- data.frame(path = sub("^\\./", "", vapply(p, `[`, "", 1L)), size = vapply(p, `[`, "", 2L),
                     mtime = vapply(p, `[`, "", 3L), ctime = vapply(p, `[`, "", 4L), stringsAsFactors = FALSE)
    ix <- ix[!duplicated(ix$path), , drop = FALSE]
  }
  ix <- ix[order(ix$path, method = "radix"), , drop = FALSE]
  # content hashes, cached by signature
  cf <- imp_md5_cache_file()
  dir.create(dirname(cf), recursive = TRUE, showWarnings = FALSE)
  cache <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) character()) else character()
  sig <- paste(root, ix$path, ix$size, ix$mtime, ix$ctime, sep = "\t")
  md5 <- unname(cache[sig])
  miss <- which(is.na(md5))
  if (length(miss)) {
    if (.Platform$OS.type == "windows") {
      md5[miss] <- unname(tools::md5sum(file.path(root, ix$path[miss])))
    } else {
      lst <- file.path(w, "md5_list")
      writeBin(unlist(lapply(ix$path[miss], function(p) c(charToRaw(p), as.raw(0L)))), lst)
      out <- system(paste0("cd ", shQuote(root), " && xargs -0 -r -P 8 -n 200 md5sum -- < ", shQuote(lst)), intern = TRUE)
      h <- stats::setNames(substr(out, 1L, 32L), sub("^[0-9a-f]{32}  ", "", out))
      md5[miss] <- unname(h[ix$path[miss]])
    }
    if (anyNA(md5)) stop("conf_impact: md5 of ", sum(is.na(md5)), " file(s) under ", root, " failed")
    cache[sig[miss]] <- md5[miss]
    tmp <- tempfile(tmpdir = dirname(cf))
    saveRDS(cache, tmp)
    file.rename(tmp, cf)
  }
  ix$md5 <- md5
  assign(root, ix, envir = imp_index_cache)
  ix
}

# Case directories of a root (as conf_list_cases(): every meta.yaml outside expected/).
imp_list_cases <- function(root) {
  ix <- imp_root_index(root)
  m <- ix$path[basename(ix$path) == "meta.yaml" & !grepl("/expected/", paste0("/", ix$path), fixed = TRUE)]
  sort(file.path(normalizePath(root), dirname(m)))
}

# program_ref and cwd of a case (meta.yaml), cached by the meta.yaml's size and
# time (reading thousands of small files on the shared file system is slow).
imp_meta_cache <- new.env(parent = emptyenv())
imp_meta_info <- function(case, sig) {
  ck <- file.path(imp_cache_dir(), "meta_info.rds")
  if (is.null(imp_meta_cache$tab)) {
    imp_meta_cache$tab <- if (file.exists(ck)) tryCatch(readRDS(ck), error = function(e) list()) else list()
    imp_meta_cache$dirty <- FALSE
  }
  k <- paste(case, sig)
  hit <- imp_meta_cache$tab[[k]]
  if (!is.null(hit)) return(hit)
  meta <- readLines(file.path(case, "meta.yaml"), warn = FALSE)
  ref <- sub("^program_ref:\\s*", "", grep("^program_ref:", meta, value = TRUE))
  v <- list(ref = if (length(ref)) gsub("^\"|\"$", "", ref[1]) else "", root = any(grepl("^cwd:\\s*_root\\s*$", meta)))
  imp_meta_cache$tab[[k]] <- v
  imp_meta_cache$dirty <- TRUE
  v
}
imp_meta_save <- function() {
  if (!isTRUE(imp_meta_cache$dirty)) return(invisible())
  d <- imp_cache_dir()
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = d)
  saveRDS(imp_meta_cache$tab, tmp)
  file.rename(tmp, file.path(d, "meta_info.rds"))
  imp_meta_cache$dirty <- FALSE
}

# Files a case directory's verdict depends on besides the code: the directory
# itself (program, meta.yaml, expected/, data/), a program_ref program and its
# data/ (with the books root), the area's _root/.  Signature: path and content md5 of each.
imp_unit_hash <- function(case, root) {
  root <- normalizePath(root)
  ix <- imp_root_index(root)
  rel <- substring(case, nchar(root) + 2L)
  sel <- startsWith(ix$path, paste0(rel, "/"))
  lines <- paste(ix$path[sel], ix$md5[sel], sep = "\t")
  mi <- match(paste0(rel, "/meta.yaml"), ix$path)
  info <- if (is.na(mi)) list(ref = "", root = FALSE) else imp_meta_info(case, ix$md5[mi])
  if (nzchar(info$ref)) {
    br <- Sys.getenv("SASONR_BOOKS_ROOT")
    if (!nzchar(br)) {
      lines <- c(lines, "<no books root>")
    } else {
      pd <- file.path(br, info$ref)
      fs <- c(file.path(pd, "program.sas"), list.files(file.path(pd, "data"), recursive = TRUE, full.names = TRUE,
                                                       all.files = TRUE))
      md <- ifelse(file.exists(fs), unname(tools::md5sum(fs)), "<missing>")
      lines <- c(lines, paste(normalizePath(br), substring(fs, nchar(br) + 2L), md, sep = "\t"))
    }
  }
  if (isTRUE(info$root)) {
    rd <- paste0(sub("/.*$", "", rel), "/_root/")
    s2 <- startsWith(ix$path, rd)
    lines <- c(lines, paste(ix$path[s2], ix$md5[s2], sep = "\t"))
  }
  paste(lines, collapse = "\n")
}

# ---- structural diff ----------------------------------------------------------------------

imp_git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) stop("git ", paste(c(...), collapse = " "), " failed")
  out
}

imp_dep <- function(e) paste(deparse(e, width.cutoff = 500L, control = c("keepInteger", "keepNA", "niceNames")), collapse = "\n")

# Skeleton of a function node (or a top-level value): nested functions replaced
# by a placeholder, so that a change inside a nested function is attributed to it.
imp_skel <- function(e, root = TRUE) {
  if (!is.call(e) || imp_is_data(e)) return(e)
  if (imp_is_fun(e) && !root) return(as.name(".FN."))
  if (imp_is_fun(e)) {
    e[4] <- list(NULL)
    fm <- e[[2]]
    for (i in seq_along(fm)) if (is.call(fm[[i]])) fm[[i]] <- imp_skel(fm[[i]], FALSE)
    if (!is.null(fm)) e[[2]] <- fm
    e[[3]] <- imp_skel(e[[3]], FALSE)
    return(e)
  }
  for (i in seq_along(e)) if (is.call(e[[i]])) e[[i]] <- imp_skel(e[[i]], FALSE)
  e
}

# Names (symbols) and string constants used anywhere in an expression.
imp_refs <- function(e) {
  strs <- character()
  walk <- function(x) {
    if (is.character(x)) strs <<- c(strs, x)
    else if (is.call(x) || is.pairlist(x)) for (i in seq_along(x)) if (!is.null(x[[i]]) && !identical(x[[i]], quote(expr = ))) walk(x[[i]])
  }
  walk(e)
  list(names = unique(all.names(e)), strings = unique(strs))
}

# Top-level nodes of an R file (text): kind def (name <- value) or stmt, the
# deparsed skeleton, the function nodes with keys (pre-order).
imp_nodes <- function(text, file) {
  ex <- parse(text = text, keep.source = TRUE, encoding = "UTF-8")
  sr <- attr(ex, "srcref")
  lapply(seq_along(ex), function(i) {
    e <- ex[[i]]
    is_def <- is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]])
    fn <- imp_fun_nodes(e)
    keys <- vapply(fn, function(f) imp_key(file, f), "")
    list(kind = if (is_def) "def" else if (is.null(e) || is.character(e)) "doc" else "stmt",
         name = if (is_def) as.character(e[[2]]) else "",
         is_fun = is_def && imp_is_fun(e[[3]]),
         skel = imp_dep(imp_skel(if (is_def) e[[3]] else e, root = is_def && imp_is_fun(e[[3]]))),
         header = if (is_def && imp_is_fun(e[[3]])) imp_dep(e[[3]][[2]]) else "",
         fun_skel = vapply(fn, function(f) imp_dep(imp_skel(f)), ""),
         keys = keys, expr = e, line = sr[[i]][1])
  })
}

imp_file_text <- function(base, file) {
  if (is.null(base)) return(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  paste(imp_git("show", paste0(base, ":", file)), collapse = "\n")
}

# Diff of one R file between BASE and the working tree.
#   changed  old keys whose own code changed
#   names    names for the reference rule (added / removed / changed values,
#            changed function headers)
#   full     reason for a full run ("" when none)
#   keymap   old key -> new key for the unchanged functions (renumbering)
imp_diff_file <- function(base, file) {
  old <- imp_nodes(imp_file_text(base, file), file)
  new <- imp_nodes(imp_file_text(NULL, file), file)
  changed <- character()
  names_ <- character()
  keymap <- character()
  full <- ""
  # top-level statements other than definitions and documentation
  so <- vapply(Filter(function(n) n$kind == "stmt", old), `[[`, "", "skel")
  sn <- vapply(Filter(function(n) n$kind == "stmt", new), `[[`, "", "skel")
  if (!identical(so, sn)) full <- paste0(file, ": a top-level statement changed")
  od <- Filter(function(n) n$kind == "def", old)
  nd <- Filter(function(n) n$kind == "def", new)
  on <- vapply(od, `[[`, "", "name")
  nn <- vapply(nd, `[[`, "", "name")
  if (anyDuplicated(on) || anyDuplicated(nn)) full <- paste0(file, ": a name is defined twice")
  for (i in seq_along(od)) {
    o <- od[[i]]
    j <- match(o$name, nn)
    if (is.na(j)) {
      changed <- c(changed, o$keys)
      names_ <- c(names_, o$name)
      next
    }
    n <- nd[[j]]
    if (!identical(o$skel, n$skel) || o$is_fun != n$is_fun) {
      if (o$is_fun) changed <- c(changed, o$keys[1]) else names_ <- c(names_, o$name)
      if (o$is_fun && !identical(o$header, n$header)) names_ <- c(names_, o$name)
    }
    if (length(o$keys) != length(n$keys)) {
      changed <- c(changed, o$keys)
      next
    }
    for (k in seq_along(o$keys)) {
      if (o$is_fun && k == 1L) {
        if (identical(o$skel, n$skel)) keymap[o$keys[k]] <- n$keys[k]
        next
      }
      if (identical(o$fun_skel[k], n$fun_skel[k])) keymap[o$keys[k]] <- n$keys[k] else changed <- c(changed, o$keys[k])
    }
  }
  added <- setdiff(nn, on)
  list(changed = unique(changed), names = unique(names_), added = added, full = full, keymap = keymap)
}

# Function keys of a whole package version -> names and strings they use;
# top-level value definitions -> names they use (load-time dependencies).
imp_package_index <- function(base, files) {
  fk <- list()
  top <- list()
  defined <- character()
  strs <- character()
  for (f in files) {
    nodes <- imp_nodes(imp_file_text(base, f), f)
    for (n in nodes) {
      if (n$kind == "def") defined <- c(defined, n$name)
      r <- imp_refs(n$expr)
      strs <- c(strs, r$strings)
      if (n$kind != "doc" && !n$is_fun) top[[length(top) + 1L]] <- list(name = n$name, names = r$names)
      fns <- imp_fun_nodes(n$expr)
      for (k in seq_along(fns)) fk[[n$keys[k]]] <- imp_refs(fns[[k]])
    }
  }
  list(funs = fk, top = top, defined = unique(defined), strings = unique(strs))
}

# Prefix / suffix literals with which code builds names at run time.
imp_dynamic <- function(idx) {
  s <- idx$strings
  pre <- unique(c(s[grepl("^[A-Za-z.][A-Za-z0-9._]*_$", s) & nchar(s) >= 3],
                  sub("%s.*$", "", s[grepl("^[A-Za-z.][A-Za-z0-9._]*_%s", s)])))
  suf <- unique(s[grepl("^_[A-Za-z0-9_]+$", s)])
  list(prefix = pre, suffix = suf)
}

imp_name_is_dynamic <- function(nm, dyn) {
  any(startsWith(nm, dyn$prefix) & nchar(nm) > nchar(dyn$prefix)) ||
    any(endsWith(nm, dyn$suffix) & nchar(nm) > nchar(dyn$suffix)) ||
    grepl("^[^.].*\\.", nm)
}

# Keys of the functions that use any of `nms` (symbol, or a word in a string).
imp_referring <- function(nms, idx) {
  if (!length(nms)) return(character())
  # names computed from them at load time
  repeat {
    more <- unique(unlist(lapply(idx$top, function(t) if (nzchar(t$name) && any(nms %in% t$names)) t$name)))
    new <- setdiff(more, nms)
    if (!length(new)) break
    nms <- c(nms, new)
  }
  pat <- paste0("(^|[^A-Za-z0-9._])(", paste(gsub(".", "\\.", nms, fixed = TRUE), collapse = "|"), ")($|[^A-Za-z0-9._])")
  hit <- vapply(idx$funs, function(r) any(nms %in% r$names) || any(grepl(pat, r$strings, perl = TRUE)), TRUE)
  names(idx$funs)[hit]
}

# Registry rows changed between BASE and the working tree -> keys.
imp_registry_keys <- function(base, file) {
  rd <- function(txt) utils::read.csv(text = txt, colClasses = "character", check.names = FALSE)
  old <- tryCatch(rd(paste(imp_git("show", paste0(base, ":", file)), collapse = "\n")), error = function(e) NULL)
  new <- if (file.exists(file)) rd(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")) else NULL
  if (is.null(old) || is.null(new)) return(list(full = paste0(file, " added or removed")))
  if (!identical(names(old), names(new))) return(list(full = paste0(file, ": columns changed")))
  row <- function(d) do.call(paste, c(d, sep = "\r"))
  ch <- rbind(old[!row(old) %in% row(new), , drop = FALSE], new[!row(new) %in% row(old), , drop = FALSE])
  tb <- sub("\\.csv$", "", basename(file))
  keys <- switch(tb,
    functions = paste0("functions|", toupper(ch$name)),
    formats = paste0("formats|", imp_norm_fmt(ch$name)),
    proc_stats = paste0("proc_stats|", ifelse(toupper(ch$proc) == "SUMMARY", "MEANS", toupper(ch$proc))),
    if (nrow(ch)) paste0(tb, "|*") else character())
  list(full = "", keys = unique(keys), table = tb)
}

# Changes since BASE -> list(full = reason or "", keys, reg, keymap, summary).
imp_changes <- function(base) {
  files <- unique(c(imp_git("diff", "--name-only", base), imp_git("ls-files", "--others", "--exclude-standard")))
  base_files <- imp_git("ls-tree", "-r", "--name-only", base)
  res <- list(full = "", keys = character(), reg = character(), reg_tables = character(), keymap = character(),
              files = files, names = character(), added = character())
  r_changed <- character()
  for (f in files) {
    in_base <- f %in% base_files
    exists_now <- file.exists(f)
    if (f %in% imp_runner_files) {
      res$full <- paste0("runner changed (", f, ")")
    } else if (grepl("^R/[^/]+\\.[Rr]$", f)) {
      if (!in_base || !exists_now) res$full <- paste0("R file added or removed (", f, ")") else r_changed <- c(r_changed, f)
    } else if (grepl("^inst/registry/[^/]+\\.csv$", f)) {
      rk <- imp_registry_keys(base, f)
      if (nzchar(rk$full)) res$full <- rk$full
      res$reg <- c(res$reg, rk$keys)
      if (length(rk$keys)) res$reg_tables <- c(res$reg_tables, rk$table)
    } else if (grepl("^inst/", f) || grepl("^R/", f)) {
      res$full <- paste0("package file changed (", f, ")")
    }
    if (nzchar(res$full)) return(res)
  }
  if (length(res$reg) && any(c("R/registry.R", "R/proc_stats_registry.R") %in% r_changed)) {
    res$full <- "registry rows and registry code changed together"
    return(res)
  }
  if (!length(r_changed)) return(res)
  for (f in r_changed) {
    d <- imp_diff_file(base, f)
    if (nzchar(d$full)) {
      res$full <- d$full
      return(res)
    }
    res$keys <- c(res$keys, d$changed)
    res$names <- c(res$names, d$names)
    res$added <- c(res$added, d$added)
    res$keymap <- c(res$keymap, d$keymap)
  }
  nms <- unique(c(res$names, res$added))
  if (length(nms)) {
    idx <- imp_package_index(base, sort(grep("^R/[^/]+\\.[Rr]$", base_files, value = TRUE), method = "radix"))
    dyn <- imp_dynamic(idx)
    dn <- nms[vapply(nms, imp_name_is_dynamic, TRUE, dyn = dyn)]
    if (length(dn)) {
      res$full <- paste0("name looked up at run time changed (", paste(utils::head(dn, 3), collapse = ", "), ")")
      return(res)
    }
    redef <- intersect(res$added, idx$defined)
    if (length(redef)) {
      res$full <- paste0("definition moved between files (", paste(utils::head(redef, 3), collapse = ", "), ")")
      return(res)
    }
    res$keys <- c(res$keys, imp_referring(nms, idx))
  }
  res$keys <- unique(res$keys)
  res
}

# Units to run for an incremental run: list(all, reason, run = <rel>, keep = <rel>,
# keymap, summary).  `map` is the impact map of the archived run at BASE for
# this root; `units` the current case directories (rel paths) with their hashes.
imp_select <- function(ch, map, units, hashes, fingerprint) {
  if (nzchar(ch$full)) return(list(all = TRUE, reason = ch$full))
  if (!identical(map$fingerprint, fingerprint)) {
    d <- names(fingerprint)[!mapply(identical, fingerprint[names(fingerprint)], map$fingerprint[names(fingerprint)])]
    return(list(all = TRUE, reason = paste0("environment differs from the archived run (", paste(d, collapse = ", "), ")")))
  }
  hit_load <- intersect(ch$keys, map$load_keys)
  if (length(hit_load)) return(list(all = TRUE, reason = paste0("code run at load time changed (", hit_load[1], ")")))
  run <- character()
  why <- c(code = 0L, registry = 0L, case = 0L, new = 0L)
  for (u in units) {
    m <- map$units[[u]]
    if (is.null(m)) {
      run <- c(run, u)
      why["new"] <- why["new"] + 1L
      next
    }
    if (!identical(m$hash, hashes[[u]])) {
      run <- c(run, u)
      why["case"] <- why["case"] + 1L
      next
    }
    if (length(ch$keys) && any(m$keys %in% ch$keys)) {
      run <- c(run, u)
      why["code"] <- why["code"] + 1L
      next
    }
    if (length(ch$reg) && ("*" %in% m$reg || any(m$reg %in% ch$reg) || any(paste0(ch$reg_tables, "|*") %in% m$reg) ||
                           any(sub("\\|.*$", "|*", ch$reg) %in% m$reg))) {
      run <- c(run, u)
      why["registry"] <- why["registry"] + 1L
    }
  }
  list(all = FALSE, run = run, keep = setdiff(intersect(units, names(map$units)), run), keymap = ch$keymap,
       reason = sprintf("%d of %d case dirs (code %d, registry %d, changed case %d, new case %d)", length(run),
                        length(units), why[["code"]], why[["registry"]], why[["case"]], why[["new"]]))
}

# Byte-compile the namespace functions (as an installed package is).  The
# compiled closures are cached (local disk) together with their source
# closures: a function whose body and formals are unchanged reuses its
# compiled version (a full compile takes about 25 s per R process; sources are
# compared without srcrefs).
# Cache file of the compiled closures: per R version, NAMESPACE (imports
# change what a name resolves to, and the compiler inlines base functions) and
# the set of names in the namespace (a new definition named like a base
# function changes the inlining of every caller).
imp_compile_cache_file <- function(ns, cache, namespace_file = file.path(getNamespaceInfo(ns, "path"), "NAMESPACE")) {
  nsf <- if (file.exists(namespace_file)) unname(tools::md5sum(namespace_file)) else "-"
  tf <- tempfile()
  on.exit(unlink(tf))
  writeLines(sort(ls(ns, all.names = TRUE), method = "radix"), tf)
  file.path(cache, paste0("compiled_", gsub("[^A-Za-z0-9.]", "_", R.version.string), "_", substr(nsf, 1L, 12L), "_",
                          substr(unname(tools::md5sum(tf)), 1L, 12L), ".rds"))
}

imp_compile <- function() {
  ns <- asNamespace("sasonr")
  cache <- imp_cache_dir()
  cf <- imp_compile_cache_file(ns, cache)
  old <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) list()) else list()
  new <- list()
  changed <- 0L
  for (nm in ls(ns, all.names = TRUE)) {
    f <- get(nm, envir = ns, inherits = FALSE)
    if (!is.function(f) || is.primitive(f)) next
    own <- identical(environment(f), ns)
    o <- old[[nm]]
    src <- if (own) utils::removeSource(f)
    hit <- own && !is.null(o) && identical(o$src, src, ignore.environment = TRUE)
    g <- if (hit) o$cmp else compiler::cmpfun(f)
    if (own) new[[nm]] <- list(src = src, cmp = g)
    if (own && !hit) changed <- changed + 1L
    unlockBinding(nm, ns)
    assign(nm, g, envir = ns)
    lockBinding(nm, ns)
  }
  if (changed > 0L) {
    tmp <- tempfile(tmpdir = cache)
    saveRDS(new, tmp)
    file.rename(tmp, cf)
  }
  invisible(changed)
}

# ---- strict-gate derivation helpers (tools/run_conformance.R) ----------------------------

# Is a strict gate caught by sasonr itself (error recovery), given the call
# stack at the gate?  `calls`, `parents`: sys.calls() / sys.parents() at the
# gate.  Frames below sas_run / engine_run_partial whose call is a known
# catcher (also namespace-qualified: base::tryCatch, rlang::try_fetch) with an
# error handler, a call through do.call / exec / Recall, or a call whose head
# is not a name (an anonymous function, f()(), x[["f"]]()) count as caught:
# the runner then reruns the case in strict mode instead of deriving its
# verdict.
imp_gate_catch <- c("error", "condition", "sasonr_unsupported", "sasonr_not_verified", "sasonr_error", "rlang_error",
                    "simpleError")
imp_gate_caught <- function(calls, parents) {
  # the function name of a call head: f, pkg::f, pkg:::f, obj$f; NA for any
  # other head (an anonymous function, f()(), x[["f"]](), ...)
  head_name <- function(h) {
    if (is.symbol(h)) return(as.character(h))
    if (is.call(h) && length(h) == 3L && is.symbol(h[[1]]) && as.character(h[[1]]) %in% c("::", ":::", "$", "@") &&
        is.symbol(h[[3]])) return(as.character(h[[3]]))
    NA_character_
  }
  nm <- vapply(calls, function(cl) head_name(cl[[1]]), "")
  b <- which(nm %in% c("sas_run", "engine_run_partial"))
  if (!length(b)) return(TRUE)
  b <- b[1]
  for (i in seq_along(calls)) {
    if (i <= b) next
    if (is.na(nm[i])) return(TRUE)
    if (nm[i] %in% c("try", "try_fetch", "safely", "possibly", "quietly", "do.call", "Recall", "exec", "inject")) return(TRUE)
    if (nm[i] %in% c("tryCatch", "withCallingHandlers")) {
      h <- setdiff(names(calls[[i]]), c("", "expr", "finally"))
      if (nm[b] == "engine_run_partial" && parents[i] == b) next
      if (any(h %in% imp_gate_catch)) return(TRUE)
    }
  }
  FALSE
}

# Is a matrix block free of effects on the later blocks?  A block is one DATA
# step whose only output is its own data set; anything else (macro
# statements, CALL SYMPUT/EXECUTE, OPTIONS, PROC steps, LIBNAME/FILENAME,
# TITLE/FOOTNOTE, ODS, a SET/MERGE/UPDATE/MODIFY of another data set, ...) can
# change what the later blocks see.  In strict mode a gated block ends its
# segment and the next block starts a new SAS session, so a verdict derived
# from the strict = FALSE run of the whole program is only valid when every
# block is free of such effects.
imp_block_pure <- function(lines) {
  x <- tolower(trimws(lines))
  x <- x[nzchar(x) & !grepl("^%put @@(begin|end) ", x)]
  if (!length(x) || !grepl("^data\\s", x[1]) || !any(grepl("^run;", x))) return(FALSE)
  bad <- c("^%", "%let\\b", "%global\\b", "%sysexec\\b", "call\\s+(symput|symputx|symputn|execute)\\b",
           "^options\\b", "^proc\\b", "^libname\\b", "^filename\\b", "^title", "^footnote", "^ods\\b", "^x\\s",
           "^dm\\s", "^(set|merge|update|modify)\\b", ";\\s*(set|merge|update|modify)\\b", "^%include\\b",
           "^data\\s.*;.*\\bdata\\s")
  !any(vapply(bad, function(p) any(grepl(p, x, perl = TRUE)), TRUE))
}
