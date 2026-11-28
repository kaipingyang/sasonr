# Data-level PROC functions (layer 2): a data frame in, the ODS tables (or the
# output data set) out, without writing a program.  The two paths share the
# core: a program runs sas_rt_proc() -> compute(); these functions call the
# same compute() with a spec from the same builder (no sas_run()).

data_api_names <- function(x, what) {
  if (is.null(x)) return(character())
  if (!is.character(x) || anyNA(x) || !all(grepl("^[A-Za-z_][A-Za-z0-9_]{0,31}$", x))) {
    rlang::abort(paste0("`", what, "` must be variable names."), class = "sasonr_error")
  }
  x
}

data_api_stmt <- function(kw, x) if (length(x)) paste0("  ", kw, " ", paste(x, collapse = " "), ";\n") else ""

# The PROC step text of a data-level call (parsed by the same front end as a
# program, so options and statements mean the same).
data_api_text <- function(proc, options, body, out = NULL) {
  if (!rlang::is_string(options)) rlang::abort("`options` must be one string of PROC statement options.", class = "sasonr_error")
  paste0("proc ", proc, " data=data", if (nzchar(options)) " ", options,
         if (!is.null(out)) paste0(" out=", out), ";\n", body, "run;")
}

# Run the core of a registry procedure on a data frame: the data go through
# the same import as sas_session(work = ), the spec comes from the same
# builder as the translator's (means_step_spec(), tab_step_spec()), and the
# computation is the registry entry's compute() -- the function sas_rt_proc()
# calls in a program.  Returns the result of compute() with `messages` (the
# WARNINGs and NOTEs of the computation).
data_api_core <- function(data, proc, code, builder) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.", class = "sasonr_error")
  s <- SasSession$new()
  on.exit(s$close(), add = TRUE)
  session_add_work(s, list(data = data))
  old <- sas_rt_active(s)
  on.exit(sas_rt_active(old), add = TRUE)
  step <- sas_parse(code)$steps[[1]]
  b <- builder(step, s$meta, s$last)
  d <- tibble::as_tibble(get("data", envir = s$work))
  if (identical(proc, "transpose")) {
    # PROC TRANSPOSE writes an output data set (sas_rt_store(), as in a program)
    r <- sas_rt_transpose(d, b$spec)
    sas_rt_store(s$work, r$out)
    return(list(out = tibble::as_tibble(get(b$spec$out, envir = s$work)), messages = c(b$warnings, s$rt$proc_notes)))
  }
  res <- proc_registry_entry(proc)$compute(d, b$spec)
  res$messages <- c(b$warnings, res$events)
  res
}

data_api_tables <- function(res) {
  tb <- res$tables
  out <- if (length(tb) == 1L) tb[[1]] else structure(tb, class = "sasonr_ods_list")
  attr(out, "sasonr_log") <- res$messages
  out
}

#' Data-level PROC TABULATE, PROC FREQ and PROC TRANSPOSE
#'
#' Run one procedure on a data frame without writing a SAS program.
#' `sas_tabulate_df()` and `sas_freq_df()` return the ODS
#' table of the step (a `sasonr_ods_table`, a tibble with the SAS table
#' attributes), or a list of them when there are several (one per BY group
#' or table request); `sas_freq_df()` supports one-way (OneWayFreqs) and
#' two-way (CrossTabFreqs) tables without statistics options so far; `sas_transpose_df()`
#' returns the output data set.  The arguments are read by the same parser
#' and specification builder as the PROC step of a program, and the
#' computation is the one [sas_run()] calls for that step, so the values and
#' the column attributes are the same.  `sas_log()` gives the WARNINGs and
#' NOTEs of the computation and `sas_html()` the HTML document of the tables
#' (both are S3 generics with methods for these results).
#'
#' @param data A data frame.
#' @param var,class,by,id Variable names (character vectors).
#' @param table The TABLE statement of PROC TABULATE, as SAS text
#'   (`"grp all, x*(n mean)"`).
#' @param tables The TABLES statement of PROC FREQ, as SAS text (`"grp sex"`,
#'   `"grp / nocum"`).
#' @param weight The WEIGHT variable (one name).
#' @param options Further options of the PROC statement, as SAS text.
#' @return See Description.
#' @examples
#' d <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))

#' as.data.frame(sas_tabulate_df(d, "grp all, x*(n sum)", class = "grp", var = "x"))
#' as.data.frame(sas_freq_df(d, "grp"))
#' sas_transpose_df(d, var = "x")
#' @name sas_data_procs
NULL

#' PROC MEANS and PROC SUMMARY on a data frame
#'
#' Runs one PROC MEANS (or SUMMARY) step on a data frame, without a session
#' or a program. The arguments are those of [sas_proc_means()] (the options
#' of the PROC statement, `stats`, and the statements as arguments named
#' after them); the data frame takes the place of `data`. The step is built
#' and computed as the step of a program, so the values are the same.
#' @param df A data frame.
#' @param ... Options, `stats` and statements, as in [sas_proc_means()].
#' @return The ODS table of the step (a `sasonr_ods_table`), or a list of
#'   them (several tables, BY groups). `sas_log()` gives the WARNINGs and
#'   NOTEs of the computation and `sas_html()` the HTML document.
#' @examples
#' df <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))
#' sas_means_df(df, stats = c(n, mean), class = grp, var = x)
#' sas_summary_df(df, print = TRUE, var = x)
#' @export
sas_means_df <- function(df, ...) {
  data_api_means_like("means", df, rlang::enexprs(...), parent.frame())
}

#' @rdname sas_means_df
#' @export
sas_summary_df <- function(df, ...) {
  data_api_means_like("summary", df, rlang::enexprs(...), parent.frame())
}

#' @rdname sas_data_procs
#' @export
sas_tabulate_df <- function(data, table, class = NULL, var = NULL, by = NULL, options = "") {
  if (!rlang::is_string(table)) rlang::abort("`table` must be the text of one TABLE statement.", class = "sasonr_error")
  body <- paste0(data_api_stmt("class", data_api_names(class, "class")), data_api_stmt("by", data_api_names(by, "by")),
                 data_api_stmt("var", data_api_names(var, "var")), "  table ", table, ";\n")
  data_api_tables(data_api_core(data, "tabulate", data_api_text("tabulate", options, body), tab_step_spec))
}

#' @rdname sas_data_procs
#' @export
sas_freq_df <- function(data, tables, by = NULL, weight = NULL, options = "") {
  if (!rlang::is_string(tables)) rlang::abort("`tables` must be the text of one TABLES statement.", class = "sasonr_error")
  weight <- data_api_names(weight, "weight")
  if (length(weight) > 1L) rlang::abort("`weight` must be one variable name.", class = "sasonr_error")
  body <- paste0(data_api_stmt("by", data_api_names(by, "by")), data_api_stmt("weight", weight), "  tables ", tables, ";\n")
  res <- data_api_core(data, "freq", data_api_text("freq", options, body), freq_step_spec)
  # FREQ output that has no ODS
  # table yet: it would be left out of the result
  lines <- Filter(function(tb) identical(attr(tb, "template", exact = TRUE), "base.freq.lines"), res$tables)
  content <- Filter(function(tb) !isTRUE(attr(tb, "decor", exact = TRUE)), lines)
  if (any(lengths(lapply(content, attr, "lines")) > 0L)) {
    abort_unsupported("sas_freq_df() for PROC FREQ output without an ODS table")
  }
  res$tables <- Filter(function(tb) !identical(attr(tb, "template", exact = TRUE), "base.freq.lines"), res$tables)
  if (!length(res$tables)) abort_unsupported("sas_freq_df() without a printed table (NOPRINT)")
  data_api_tables(res)
}

#' @rdname sas_data_procs
#' @export
sas_transpose_df <- function(data, var = NULL, by = NULL, id = NULL, options = "") {
  body <- paste0(data_api_stmt("by", data_api_names(by, "by")), data_api_stmt("id", data_api_names(id, "id")),
                 data_api_stmt("var", data_api_names(var, "var")))
  code <- data_api_text("transpose", options, body, out = "transposed")
  res <- data_api_core(data, "transpose", code, transpose_step_spec)
  # character values without their trailing blanks, as sas_work() gives them
  out <- dplyr::mutate(res$out, dplyr::across(dplyr::where(is.character), sas_rt_rtrim))
  attr(out, "sasonr_log") <- res$messages
  out
}

#' @export
as.data.frame.sasonr_ods_table <- function(x, row.names = NULL, optional = FALSE, ...) {
  keep <- setdiff(names(attributes(x)), c("names", "row.names", "class"))
  attributes(x)[keep] <- NULL
  as.data.frame(tibble::as_tibble(unclass(x)), row.names = row.names, optional = optional, ...)
}

#' @export
print.sasonr_ods_list <- function(x, ...) {
  cat("<sasonr_ods_list> ", length(x), " tables\n", sep = "")
  for (tb in x) print(tb)
  invisible(x)
}

# The data-level form of a procedure on the spec 11 converter: the arguments
# as in its verb (`sas_proc_means(s, data = , ...)`), the data frame as
# `data`; the step IR from proc_ir_of_args(), no SAS text.
data_api_means_like <- function(name, df, dots, env) {
  if (!is.data.frame(df)) rlang::abort("`df` must be a data frame.", class = "sasonr_error")
  sp <- proc_api_spec[[name]]
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    rlang::abort(paste0("Every argument of `sas_", name, "_df()` must be named."), class = "sasonr_error")
  }
  if ("data" %in% names(dots)) rlang::abort(paste0("`sas_", name, "_df()` takes its data frame as `df`."), class = "sasonr_error")
  is_stmt <- names(dots) %in% sp$stmts
  ev <- dots[!is_stmt]
  ev[names(ev) != "stats"] <- lapply(ev[names(ev) != "stats"], eval, envir = env)
  step <- proc_ir_of_args(name, c(list(data = "data"), ev), dots[is_stmt])
  data_api_tables(data_api_core_ir(df, name, step, means_step_spec))
}

# data_api_core() for a step IR.
data_api_core_ir <- function(df, proc, step, builder) {
  s <- SasSession$new()
  on.exit(s$close(), add = TRUE)
  session_add_work(s, list(data = df))
  old <- sas_rt_active(s)
  on.exit(sas_rt_active(old), add = TRUE)
  b <- builder(step, s$meta, s$last)
  d <- tibble::as_tibble(get("data", envir = s$work))
  res <- proc_registry_entry(proc)$compute(d, b$spec)
  res$messages <- c(b$warnings, res$events)
  res
}
