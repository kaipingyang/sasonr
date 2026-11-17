# ODS OUTPUT: ODS tables written as data sets.
#
# `ODS OUTPUT <table>=<data set> ...;` adds requests to the session ODS state
# (`SasSession$ods_output`).  The requests apply to the next step; it must be a
# PROC MEANS step (any other step with requests pending fails loud).  The
# driver (sas_rt_proc()) writes each requested table of the step to WORK and
# queues the data set NOTE, which the log writes before the observations read
# (log_close_order, part "ods").  Without PERSIST= the requests end with the
# step.
#
# Supported: the table name `Summary` or the path `Means.Summary` (without
# BY), several requests in one statement, a one-level or WORK. data set name.
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
    if (length(rq$object) > 2L) {
      abort_unsupported(paste0("ODS OUTPUT path ", paste(rq$object, collapse = "."), " (supported: <table> or <proc>.<table>)"), rq$line)
    }
    if (length(rq$object) > 1L && !identical(tolower(rq$object[1]), tolower(proc))) {
      abort_unsupported(paste0("ODS OUTPUT ", paste(rq$object, collapse = "."), " in a PROC ", toupper(proc),
                               " step (a table this step does not produce)"), rq$line)
    }
    hit <- Filter(function(tb) ods_output_match(rq, tb), tables)
    if (length(hit) == 0L) {
      abort_unsupported(paste0("ODS OUTPUT ", paste(rq$object, collapse = "."),
                               ": the step does not produce the table (the WARNING is not supported)"), rq$line)
    }
    if (rq$ds %in% names(out)) abort_unsupported(paste0("two ODS OUTPUT requests for the data set WORK.", toupper(rq$ds)), rq$line)
    res <- writer(hit, spec, rq$line)
    out[[rq$ds]] <- res
    rt$ods_notes <- c(rt$ods_notes, list(list(name = toupper(rq$ds), nobs = nrow(res$data), nvar = ncol(res$data))))
  }
  out
}

# MEANS Summary as a data set: the BY variables, then the CLASS variable, NObs
# (with CLASS) and one column <var>_<Statistic> per statistic, BY groups
# concatenated.  The column attributes are those of the documented layout
# (num 8, format BEST., labels "N Obs", "Mean", "Sum"); every other layout
# fails loud.
means_ods_output <- function(tables, sp, line) {
  no <- function(what) abort_unsupported(paste("ODS OUTPUT of the MEANS Summary table with", what), line)
  if (length(sp$class) > 1L) no("more than one CLASS variable")
  if (length(sp$class) > 0L && length(sp$by) > 0L) no("both BY and CLASS")
  if (length(sp$by) > 1L) no("more than one BY variable")
  if (!is.null(sp$types) || length(sp$ways) > 0L || isTRUE(sp$nway)) no("TYPES, WAYS or NWAY")
  if (isTRUE(sp$nonobs)) no("NONOBS")
  if (!is.null(sp$freq) || !is.null(sp$weight) || length(sp$ids) > 0L) no("FREQ, WEIGHT or ID")
  if (length(sp$vars) != 1L) no("more than one analysis variable")
  bad_stats <- setdiff(sp$stats, c("mean", "sum"))
  if (length(bad_stats) > 0L) no(paste0("the statistic(s) ", toupper(paste(bad_stats, collapse = ", ")), " (supported: MEAN, SUM)"))
  plain <- function(v, what) {
    i <- sp$info[[v]]
    if (!is.null(i$fmt) || !is.null(i$label)) no(paste("a", what, "variable that has a format or a label"))
    i
  }
  ai <- plain(sp$vars[[1]], "analysis")
  if (ai$char) no("a character analysis variable")
  cols <- list()
  for (v in c(sp$by, vapply(sp$class, `[[`, "", "name"))) {
    i <- plain(v, "BY or CLASS")
    if (!i$char) no("a numeric BY or CLASS variable")
    cols[[length(cols) + 1L]] <- list(name = v, disp = i$disp, char = TRUE, len = i$len)
  }
  best <- fmt_spec("best")
  if (length(sp$class) > 0L) cols[[length(cols) + 1L]] <- list(name = "nobs", disp = "NObs", char = FALSE, len = 8L, fmt = best, label = "N Obs")
  for (s in sp$stats) {
    disp <- paste0(ai$disp, "_", means_autoname[[s]])
    cols[[length(cols) + 1L]] <- list(name = tolower(disp), disp = disp, char = FALSE, len = 8L, fmt = best,
                                      label = means_stat_heading(s))
  }
  meta <- proc_out_meta(cols)
  parts <- lapply(tables, function(tb) {
    by <- attr(tb, "by", exact = TRUE)
    x <- list()
    for (v in sp$by) x[[v]] <- by$values[[v]]
    for (cc in cols[seq_along(cols) > length(sp$by)]) x[[cc$name]] <- tb[[cc$disp]]
    tibble::new_tibble(lapply(x, function(col) vctrs::vec_recycle(col, nrow(tb))), nrow = nrow(tb))
  })
  if (length(sp$by) > 0L && length(parts) != length(unique(lapply(tables, function(tb) attr(tb, "by", exact = TRUE)$group)))) {
    no("more than one table per BY group")
  }
  if (length(sp$by) == 0L && length(parts) != 1L) no("more than one table")
  data <- vctrs::vec_rbind(!!!parts)
  names(data) <- meta$vars
  list(data = data, meta = meta)
}
