# ODS OUTPUT: ODS tables written as data sets.
#
# `ODS OUTPUT <table>=<data set> ...;` adds requests to the session ODS state
# (`SasSession$ods_output`).  The requests apply to the next PROC step: a DATA
# step leaves them pending; PROC PRINT produces no requested table, so it
# writes the WARNING "Output ... was not created" and the requests end; a
# PROC MEANS or PROC TABULATE step writes each requested table to WORK (sas_rt_proc()); any
# other step with requests pending fails loud.  The log writes the data set
# NOTEs, then the WARNING, before the observations read (log_close_order,
# parts "ods" and "ods.warn").  Without PERSIST= the requests end with the
# PROC step.  A MEANS or TABULATE step that writes only ODS OUTPUT data sets makes the
# data set `_LAST_` (with OUT= data sets the last OUT= data set stays
# `_LAST_`).
#
# Supported: the table name `Summary` or the path `Means.Summary` (without
# BY) of PROC MEANS, the table name `Table` or the path
# `Tabulate.Report.Table` of PROC TABULATE (proc_tabulate_output.R), several
# requests in one statement, a one-level or WORK. data set name.
# MATCH_ALL, PERSIST=, quoted labels, `name#n`, data set options, the default
# DATAn name and the actions (CLOSE, CLEAR, SHOW, SELECT, EXCLUDE, ALL, NONE)
# fail loud.

parse_ods_output <- function(p, line) {
  advance(p, 2L)
  reqs <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after ODS OUTPUT", line)
    if (tk_type(p) != "name") abort_unsupported(paste("ODS OUTPUT with", tok_desc(p), "(supported: <table>=<data set>)"), line)
    parts <- tk_val(p)
    advance(p)
    while (tk_type(p) == "dot" && tk_type(p, 1L) == "name") {
      parts <- c(parts, tk_val(p, 1L))
      advance(p, 2L)
    }
    if (tk_type(p) == "lparen") abort_unsupported("ODS OUTPUT options MATCH_ALL, PERSIST= and NOWARN", line)
    if (!is_op(p, "=")) {
      abort_unsupported(paste0("ODS OUTPUT ", toupper(paste(parts, collapse = ".")),
                               " (supported: <table>=<data set>; actions and the default data set name are not)"), line)
    }
    advance(p)
    if (tk_type(p) != "name") abort_unsupported("ODS OUTPUT with a data set name that is not a SAS name", line)
    ds <- tk_low(p)
    advance(p)
    if (tk_type(p) == "dot" && tk_type(p, 1L) == "name") {
      if (ds != "work") abort_unsupported(paste0("ODS OUTPUT data set in library ", toupper(ds), " (supported: WORK)"), line)
      ds <- tk_low(p, 1L)
      advance(p, 2L)
    }
    if (tk_type(p) == "lparen") abort_unsupported("data set options on an ODS OUTPUT data set", line)
    reqs[[length(reqs) + 1L]] <- list(object = parts, ds = ds, line = line)
  }
  advance(p)
  list(type = "global", kind = "ods_output", requests = reqs, line = line)
}

# The requests that match the table `tb` (name or full path, case-insensitive).
ods_output_match <- function(req, tb) {
  obj <- tolower(req$object)
  if (length(obj) == 1L) return(identical(obj, tolower(attr(tb, "name", exact = TRUE))))
  identical(paste(obj, collapse = "."), tolower(attr(tb, "path", exact = TRUE)))
}

# Write the requested tables of a step.  `writer(tables, spec, line)` builds the
# data set (list(data, meta)) of one request from the tables it matches.
# Returns the data sets (named list for sas_proc_store()) and queues the NOTEs.
ods_output_write <- function(tables, spec, reqs, writer, proc) {
  rt <- rt_diag()
  out <- list()
  for (rq in reqs) {
    if (length(rq$object) > 3L) {
      abort_unsupported(paste0("ODS OUTPUT path ", paste(rq$object, collapse = "."), " (supported: <table> or the full path)"), rq$line)
    }
    if (length(rq$object) > 1L && !identical(tolower(rq$object[1]), tolower(proc))) {
      abort_unsupported(paste0("ODS OUTPUT ", paste(rq$object, collapse = "."), " in a PROC ", toupper(proc),
                               " step (a table this step does not produce)"), rq$line)
    }
    hit <- Filter(function(tb) ods_output_match(rq, tb), tables)
    if (length(hit) == 0L) {
      # a table name the step does not produce: the WARNING after the data set
      # NOTEs (a path, or Summary of a step without printed tables, fails loud)
      if (length(rq$object) > 1L || length(tables) == 0L) {
        abort_unsupported(paste0("ODS OUTPUT ", paste(rq$object, collapse = "."),
                                 ": the step does not produce the table"), rq$line)
      }
      if (length(rt$ods_missing) > 0L) abort_unsupported("two ODS OUTPUT requests for tables the step does not produce", rq$line)
      rt$ods_missing <- c(rt$ods_missing, rq$object)
      next
    }
    if (rq$ds %in% names(out)) abort_unsupported(paste0("two ODS OUTPUT requests for the data set WORK.", toupper(rq$ds)), rq$line)
    res <- writer(hit, spec, rq$line)
    out[[rq$ds]] <- res
    rt$ods_notes <- c(rt$ods_notes, list(list(name = toupper(rq$ds), nobs = nrow(res$data), nvar = ncol(res$data))))
  }
  out
}

# MEANS Summary as a data set.  The columns: the BY variables, the CLASS
# variables (with their format and label), NObs (with CLASS), then per
# analysis variable VName_<var> and Label_<var> (more than one variable;
# Label_ only when one of them has a label) and <var>_<Statistic>.  The tables
# of the step (BY groups, TYPES) are concatenated in order; a CLASS variable
# that is not in a table is blank.  N and NObs have the format BEST<w>. (w:
# the digits of the largest count), the other statistics BEST12.  Layouts
# outside this fail loud.
means_ods_stats <- c("n", "mean", "std", "min", "max", "sum", "median", "q1", "q3", "lclm", "uclm")

means_ods_output <- function(tables, sp, line) {
  no <- function(what) abort_unsupported(paste("ODS OUTPUT of the MEANS Summary table with", what), line)
  if (length(sp$ways) > 0L) no("WAYS")
  if (isTRUE(sp$descendtypes) || any(sp$by_desc %||% FALSE)) no("DESCENDING")
  if (isTRUE(sp$nonobs)) no("NONOBS")
  if (!is.null(sp$freq) || !is.null(sp$weight) || length(sp$ids) > 0L) no("FREQ, WEIGHT or ID")
  if (!is.null(sp$maxdec) || !identical(as.integer(sp$fw), 12L)) no("MAXDEC= or FW=")
  if (!identical(sp$vardef, "df")) no("VARDEF=")
  bad_stats <- setdiff(sp$stats, means_ods_stats)
  if (length(bad_stats) > 0L) no(paste0("the statistic(s) ", toupper(paste(bad_stats, collapse = ", "))))
  if (any(c("lclm", "uclm") %in% sp$stats) && (!isTRUE(sp$two_sided) || !identical(sp$alpha, 0.05))) {
    no("one-sided confidence limits or ALPHA=")
  }
  info <- sp$info
  cn <- vapply(sp$class, `[[`, "", "name")
  for (cl in sp$class) if (cl$missing || cl$descending || !identical(cl$order, "internal")) no("CLASS options MISSING, ORDER= or DESCENDING")
  cols <- list()
  for (v in sp$by) {
    i <- info[[v]]
    if (!i$char || !is.null(i$fmt) || !is.null(i$label)) no("a numeric, formatted or labelled BY variable")
    cols[[length(cols) + 1L]] <- list(name = v, disp = i$disp, char = TRUE, len = i$len, src = "by")
  }
  for (v in cn) {
    i <- info[[v]]
    if (!i$char) no("a numeric CLASS variable")
    cols[[length(cols) + 1L]] <- list(name = v, disp = i$disp, char = TRUE, len = i$len, fmt = i$fmt, label = i$label, src = "class")
  }
  # counts: BEST<w>. with w the digits of the largest count of the step
  counts <- unlist(lapply(tables, function(tb) {
    nm <- c(if (length(cn) > 0L) "NObs", if ("n" %in% sp$stats) paste0(vapply(sp$vars, function(v) info[[v]]$disp, ""), "_N"))
    unlist(lapply(nm, function(x) tb[[x]]))
  }))
  cw <- if (length(counts) > 0L) max(1L, nchar(sprintf("%.0f", max(counts, 0)))) else 1L
  best_n <- fmt_spec("best", cw)
  best <- fmt_spec("best", 12L)
  if (length(cn) > 0L) cols[[length(cols) + 1L]] <- list(name = "nobs", disp = "NObs", char = FALSE, len = 8L, fmt = best_n, label = "N Obs", src = "NObs")
  multi <- length(sp$vars) > 1L
  has_label <- any(vapply(sp$vars, function(v) !is.null(info[[v]]$label), TRUE))
  for (v in sp$vars) {
    ai <- info[[v]]
    if (ai$char) no("a character analysis variable")
    if (multi) {
      cols[[length(cols) + 1L]] <- list(name = tolower(paste0("VName_", ai$disp)), disp = paste0("VName_", ai$disp), char = TRUE,
                                        len = nchar(ai$disp), label = "Variable", value = ai$disp)
      if (has_label) {
        lab <- ai$label %||% ""
        cols[[length(cols) + 1L]] <- list(name = tolower(paste0("Label_", ai$disp)), disp = paste0("Label_", ai$disp), char = TRUE,
                                          len = max(1L, nchar(lab)), label = "Label", value = lab)
      }
    }
    for (s in sp$stats) {
      disp <- paste0(ai$disp, "_", means_autoname[[s]])
      cols[[length(cols) + 1L]] <- list(name = tolower(disp), disp = disp, char = FALSE, len = 8L,
                                        fmt = if (s == "n") best_n else best,
                                        label = paste(means_stat_heading(s, sp$alpha), collapse = " "), src = disp)
    }
  }
  nms <- vapply(cols, `[[`, "", "name")
  if (anyDuplicated(nms)) no(paste0("two columns named ", toupper(nms[anyDuplicated(nms)])))
  meta <- proc_out_meta(lapply(cols, function(cc) cc[setdiff(names(cc), c("src", "value"))]))
  if (length(sp$by) == 0L && length(sp$types) == 0L && length(tables) != 1L) no("more than one table")
  parts <- lapply(tables, function(tb) {
    by <- attr(tb, "by", exact = TRUE)
    n <- nrow(tb)
    if (length(cn) > 0L && !any(vapply(cn, function(v) info[[v]]$disp %in% names(tb), TRUE))) {
      no("a TYPES request that includes the type ()")
    }
    x <- lapply(cols, function(cc) {
      val <- if (identical(cc$src, "by")) {
        by$values[[cc$name]]
      } else if (identical(cc$src, "class")) {
        if (cc$disp %in% names(tb)) tb[[cc$disp]] else rep("", n)
      } else if (!is.null(cc$value)) {
        cc$value
      } else {
        tb[[cc$src]]
      }
      vctrs::vec_recycle(val, n)
    })
    names(x) <- nms
    tibble::new_tibble(x, nrow = n)
  })
  data <- vctrs::vec_rbind(!!!parts)
  names(data) <- meta$vars
  list(data = data, meta = meta)
}

# The WARNING for a requested table the step did not produce, wrapped at
# LINESIZE with continuation lines under the message text.  Only the line
# breaks of the documented form (after "Also," and "verify that") are
# supported.
ods_not_created_lines <- function(name, ls = rt_linesize()) {
  text <- paste0("WARNING: Output '", name, "' was not created.  Make sure that the output object name, label, or path is ",
                 "spelled correctly.  Also, verify that the appropriate procedure options are used to produce the requested ",
                 "output object.  For example, verify that the NOPRINT option is not used.")
  lines <- log_wrap(text, indent = 9L, ls = ls)
  if (length(lines) != 3L || !endsWith(lines[1], "correctly.  Also,") || !endsWith(lines[2], "For example, verify that")) {
    abort_unsupported(paste0("the WARNING for the ODS OUTPUT table ", name, " at LINESIZE=", ls), NULL)
  }
  lines
}

