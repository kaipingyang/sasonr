# Incremental regression: map the changes since a git commit to the
# conformance cases they can affect (used by tools/run_conformance.R --since).
#
#   affected_cases(base = "origin/main", roots = "tests/conformance")
#
# returns list(all = TRUE/FALSE, cases = <case directories>, summary = <lines>).
# Changed files = `git diff --name-only <base>` (committed and uncommitted
# changes) plus untracked files.  Mapping:
#   R/functions.R, R/rounding.R  changed top-level definitions -> SAS functions
#                                (sas_f_<name>; a helper maps to every sas_f_*
#                                that calls it, directly or through other
#                                helpers); a helper used by no sas_f_* -> all
#   R/formats.R                  all format/informat matrices, the rounding
#                                cases and the PUT/INPUT functions
#   R/proc_<name>.R              cases whose program has PROC <name>
#   R/sql_*.R                    cases with PROC SQL (and the PROC SQL cases of
#                                the function matrices)
#   R/doc_*.R, docs, tests/testthat, vignettes, NEWS ...  nothing
#   inst/registry/*.csv          features whose row changed (status) -> their
#                                matrices and every case that uses them
#   tools/gen_matrix.py, tools/fn_matrix.txt, tools/fmt_matrix.txt,
#   tools/fn_examples.txt        the function and format matrices
#   any other file under R/, the runner (tools/run_conformance.R,
#   tools/affected_cases.R, tests/testthat/helper-conformance.R), DESCRIPTION,
#   NAMESPACE                    every case (shared code: lexer, parser,
#                                macro processor, runtime core)
# A function selects its matrix (fn_matrix/<NAME>), its pilot cases
# (functions/<NAME>/), the reference examples (fn_examples/<NAME>/) and every
# other case whose program calls it (`name(`, case-insensitive).  Book programs
# are read from $SASONR_BOOKS_ROOT; without it every book case is selected.

aff_git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) stop("git ", paste(c(...), collapse = " "), " failed")
  out
}

# Top-level definitions (name <- function) of an R file with their line ranges.
aff_defs <- function(path) {
  if (!file.exists(path)) return(data.frame(name = character(), from = integer(), to = integer()))
  x <- parse(path, keep.source = TRUE)
  sr <- attr(x, "srcref")
  nm <- vapply(x, function(e) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) as.character(e[[2]]) else ""
  }, "")
  data.frame(name = nm, from = vapply(sr, `[`, 1L, 1L), to = vapply(sr, `[`, 1L, 3L), stringsAsFactors = FALSE)
}

# Changed line numbers (new file) of a file since base.
aff_changed_lines <- function(base, path) {
  d <- aff_git("diff", "-U0", base, "--", path)
  h <- regmatches(d, regexec("^@@ -[0-9,]+ \\+([0-9]+)(?:,([0-9]+))? @@", d, perl = TRUE))
  h <- Filter(length, h)
  unlist(lapply(h, function(m) {
    s <- as.integer(m[2])
    n <- if (nzchar(m[3])) as.integer(m[3]) else 1L
    if (n == 0L) s else seq.int(s, length.out = n)
  }))
}

# Every top-level function of the package -> the names it references.
aff_call_graph <- function() {
  files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  g <- list()
  for (f in files) {
    x <- parse(f, keep.source = FALSE)
    for (e in x) {
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) {
        g[[as.character(e[[2]])]] <- unique(all.names(e[[3]]))
      }
    }
  }
  g
}

# sas_f_* functions that reach any of `helpers`.
aff_callers <- function(helpers, g) {
  reach <- helpers
  repeat {
    more <- names(g)[vapply(g, function(n) any(n %in% reach), logical(1))]
    new <- setdiff(more, reach)
    if (!length(new)) break
    reach <- c(reach, new)
  }
  reach[startsWith(reach, "sas_f_")]
}

aff_program_text <- function(case) {
  meta_f <- file.path(case, "meta.yaml")
  ref <- if (file.exists(meta_f)) sub("^program_ref:\\s*", "", grep("^program_ref:", readLines(meta_f, warn = FALSE), value = TRUE)) else character()
  p <- if (length(ref)) {
    root <- Sys.getenv("SASONR_BOOKS_ROOT")
    if (!nzchar(root)) return(NA_character_)
    file.path(root, gsub("^\"|\"$", "", ref), "program.sas")
  } else file.path(case, "program.sas")
  if (!file.exists(p)) return(NA_character_)
  tolower(paste(readLines(p, warn = FALSE, encoding = "latin1"), collapse = "\n"))
}

affected_cases <- function(base = "origin/main", roots = "tests/conformance") {
  changed <- unique(c(aff_git("diff", "--name-only", base), aff_git("ls-files", "--others", "--exclude-standard")))
  changed <- changed[file.exists(changed) | changed %in% aff_git("diff", "--name-only", base)]
  summary <- sprintf("since %s: %d changed file(s)", base, length(changed))
  shared <- character()
  fns <- character()
  procs <- character()
  sql <- FALSE
  formats <- FALSE
  matrices <- FALSE
  g <- NULL
  for (f in changed) {
    if (grepl("^R/doc_.*\\.R$", f) || !grepl("^(R/|inst/registry/|tools/|tests/testthat/helper-conformance\\.R$|DESCRIPTION$|NAMESPACE$)", f)) next
    if (f %in% c("R/functions.R", "R/rounding.R")) {
      lines <- aff_changed_lines(base, f)
      defs <- aff_defs(f)
      hit <- unique(defs$name[vapply(seq_len(nrow(defs)), function(i) any(lines >= defs$from[i] & lines <= defs$to[i]), logical(1))])
      # changed code outside a named definition (not blank, not a comment) is shared
      src <- readLines(f, warn = FALSE)
      outside <- lines[!vapply(lines, function(l) any(l >= defs$from & l <= defs$to), logical(1))]
      outside <- outside[outside <= length(src) & !grepl("^\\s*(#.*)?$", src[pmin(outside, length(src))])]
      if (any(!nzchar(hit)) || length(outside)) shared <- c(shared, f)
      if (is.null(g)) g <- aff_call_graph()
      for (h in hit[nzchar(hit)]) {
        cf <- if (startsWith(h, "sas_f_")) h else aff_callers(h, g)
        if (!length(cf)) shared <- c(shared, paste0(f, " (", h, ")"))
        fns <- c(fns, sub("^sas_f_", "", cf))
      }
    } else if (f == "R/formats.R") {
      formats <- TRUE
    } else if (grepl("^R/proc_[a-z]+\\.R$", f) && f != "R/proc_util.R") {
      p <- sub("^R/proc_([a-z]+)\\.R$", "\\1", f)
      procs <- c(procs, if (p == "means") c("means", "summary") else p)
    } else if (grepl("^R/sql_", f)) {
      sql <- TRUE
    } else if (grepl("^inst/registry/(functions|formats)\\.csv$", f)) {
      old <- tryCatch(utils::read.csv(text = paste(aff_git("show", paste0(base, ":", f)), collapse = "\n"),
                                      colClasses = "character"), error = function(e) NULL)
      new <- utils::read.csv(f, colClasses = "character")
      key <- function(d) paste(d$name, d$kind, d$status)
      ch <- if (is.null(old)) new$name else unique(c(new$name[!key(new) %in% key(old)], old$name[!key(old) %in% key(new)]))
      if (grepl("functions", f)) fns <- c(fns, tolower(ch)) else if (length(ch)) formats <- TRUE
    } else if (f %in% c("tools/gen_matrix.py", "tools/fn_matrix.txt", "tools/fmt_matrix.txt", "tools/fn_examples.txt")) {
      matrices <- TRUE
    } else if (f %in% c("tools/run_conformance.R", "tools/affected_cases.R", "tests/testthat/helper-conformance.R") ||
               grepl("^R/", f) || f %in% c("DESCRIPTION", "NAMESPACE")) {
      shared <- c(shared, f)
    }
  }
  fns <- sort(unique(fns))
  if (length(shared)) {
    return(list(all = TRUE, cases = character(), summary = c(summary, paste0(
      "shared code changed (", paste(utils::head(shared, 5), collapse = ", "), if (length(shared) > 5) ", ...", ") -> every case"))))
  }
  out <- character()
  for (root in roots) {
    root <- normalizePath(root)
    cases <- conf_list_cases_fast(root)
    rel <- substring(cases, nchar(root) + 2L)
    area <- sub("/.*$", "", rel)
    feat <- sub("^[^/]+/([^/]+).*$", "\\1", rel)
    sel <- rep(FALSE, length(cases))
    if (matrices) sel <- sel | area %in% c("fn_matrix", "fmt_matrix")
    if (formats) sel <- sel | area %in% c("fmt_matrix", "rounding") |
      (area %in% c("fn_matrix", "functions", "fn_examples") & toupper(feat) %in% c("PUT", "PUTN", "PUTC", "INPUT", "INPUTN", "INPUTC"))
    sel <- sel | (area %in% c("fn_matrix", "functions", "fn_examples") & toupper(feat) %in% toupper(fns))
    if (sql) sel <- sel | vapply(seq_along(cases), function(i) {
      area[i] == "fn_matrix" && any(utils::read.csv(file.path(cases[i], "cases.csv"), colClasses = "character")$context == "sql")
    }, logical(1))
    # other cases: by program text
    pats <- c(if (length(fns)) paste0("\\b(", paste(fns, collapse = "|"), ")\\s*\\("),
              if (length(procs)) paste0("\\bproc\\s+(", paste(procs, collapse = "|"), ")\\b"),
              if (sql) "\\bproc\\s+sql\\b",
              if (formats) "\\b(put|input|format|informat|attrib)\\b")
    if (length(pats)) {
      pat <- paste(pats, collapse = "|")
      other <- which(!area %in% c("fn_matrix", "fmt_matrix") & !sel)
      for (i in other) {
        txt <- aff_program_text(cases[i])
        if (is.na(txt) || grepl(pat, txt, perl = TRUE)) sel[i] <- TRUE
      }
    }
    out <- c(out, cases[sel])
  }
  what <- c(if (length(fns)) paste0("functions ", paste(toupper(utils::head(fns, 12)), collapse = " "), if (length(fns) > 12) " ..."),
            if (formats) "formats", if (length(procs)) paste0("PROC ", paste(toupper(procs), collapse = " ")),
            if (sql) "PROC SQL", if (matrices) "matrix generator")
  list(all = FALSE, cases = out, summary = c(summary, sprintf("affected: %s -> %d case dir(s)",
                                                              if (length(what)) paste(what, collapse = "; ") else "nothing",
                                                              length(out))))
}

# Case directories (meta.yaml) without descending into a case: matrix
# directories are not searched for nested cases.
conf_list_cases_fast <- function(root) {
  out <- character()
  walk <- function(d) {
    if (file.exists(file.path(d, "meta.yaml"))) {
      out <<- c(out, d)
      return(invisible())
    }
    for (s in list.dirs(d, recursive = FALSE)) if (!basename(s) %in% c("expected", "data", "_root")) walk(s)
  }
  walk(root)
  sort(out)
}
