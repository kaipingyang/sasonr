# Data-level PROC functions (layer 2): a data frame in, the ODS tables (or the
# output data set) out, without writing a SAS program.  Each function builds
# the PROC step from its arguments and runs it on a session whose WORK holds
# the data, so the results are those of the same step in sas_run(); the log
# and the HTML document of that run stay attached to the result.

data_api_names <- function(x, what) {
  if (is.null(x)) return(character())
  if (!is.character(x) || anyNA(x) || !all(grepl("^[A-Za-z_][A-Za-z0-9_]{0,31}$", x))) {
    rlang::abort(paste0("`", what, "` must be variable names."), class = "sasonr_error")
  }
  x
}

data_api_stmt <- function(kw, x) if (length(x)) paste0("  ", kw, " ", paste(x, collapse = " "), ";\n") else ""

data_api_run <- function(data, proc, options, body, out = NULL) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.", class = "sasonr_error")
  if (!rlang::is_string(options)) rlang::abort("`options` must be one string of PROC statement options.", class = "sasonr_error")
  s <- sas_session(work = list(data = data))
  code <- paste0("proc ", proc, " data=data", if (nzchar(options)) " ", options,
                 if (!is.null(out)) paste0(" out=", out), ";\n", body, "run;")
  r <- sas_run(code, session = s)
  if (!identical(r$status, "ok")) {
    rlang::abort(c(paste0("PROC ", toupper(proc), " stopped with an ERROR."), i = paste(r$errors, collapse = "\n")),
                 class = "sasonr_error")
  }
  r
}

data_api_tables <- function(r) {
  tb <- r$ods
  if (length(tb) == 1L) {
    out <- tb[[1]]
  } else {
    out <- structure(tb, class = "sasonr_ods_list")
  }
  attr(out, "sasonr_result") <- r
  out
}

#' Data-level PROC MEANS, PROC TABULATE and PROC TRANSPOSE
#'
#' Run one procedure on a data frame without writing a SAS program.
#' `sas_means_df()` and `sas_tabulate_df()` return the ODS table of the step
#' (a `sasonr_ods_table`, a tibble with the SAS table attributes), or a list
#' of them when there are several (one per BY group); `sas_transpose_df()`
#' returns the output data set.  The step runs in a new session, so the
#' values, the formats of the columns and the log are those of the same step
#' in [sas_run()]; `sas_log()` and `sas_html()` take the result.
#'
#' @param data A data frame.
#' @param var,class,by,id Variable names (character vectors).
#' @param stats Statistic keywords of the PROC MEANS statement (`"n"`,
#'   `"mean"`, `"std"`, ...); `NULL` gives the default statistics.
#' @param table The TABLE statement of PROC TABULATE, as SAS text
#'   (`"grp all, x*(n mean)"`).
#' @param options Further options of the PROC statement, as SAS text.
#' @return See Description.
#' @examples
#' d <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))
#' sas_means_df(d, var = "x", class = "grp", stats = c("n", "mean"))
#' as.data.frame(sas_tabulate_df(d, "grp all, x*(n sum)", class = "grp", var = "x"))
#' sas_transpose_df(d, var = "x")
#' @name sas_data_procs
NULL

#' @rdname sas_data_procs
#' @export
sas_means_df <- function(data, var = NULL, class = NULL, by = NULL, stats = NULL, options = "") {
  stats <- data_api_names(stats, "stats")
  body <- paste0(data_api_stmt("class", data_api_names(class, "class")), data_api_stmt("by", data_api_names(by, "by")),
                 data_api_stmt("var", data_api_names(var, "var")))
  r <- data_api_run(data, "means", paste(c(stats, if (nzchar(options)) options), collapse = " "), body)
  data_api_tables(r)
}

#' @rdname sas_data_procs
#' @export
sas_tabulate_df <- function(data, table, class = NULL, var = NULL, by = NULL, options = "") {
  if (!rlang::is_string(table)) rlang::abort("`table` must be the text of one TABLE statement.", class = "sasonr_error")
  body <- paste0(data_api_stmt("class", data_api_names(class, "class")), data_api_stmt("by", data_api_names(by, "by")),
                 data_api_stmt("var", data_api_names(var, "var")), "  table ", table, ";\n")
  data_api_tables(data_api_run(data, "tabulate", options, body))
}

#' @rdname sas_data_procs
#' @export
sas_transpose_df <- function(data, var = NULL, by = NULL, id = NULL, options = "") {
  body <- paste0(data_api_stmt("by", data_api_names(by, "by")), data_api_stmt("id", data_api_names(id, "id")),
                 data_api_stmt("var", data_api_names(var, "var")))
  r <- data_api_run(data, "transpose", options, body, out = "transposed")
  out <- r$work$transposed
  attr(out, "sasonr_result") <- r
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

# the run behind a data-level result (sas_log(), sas_html())
data_api_result <- function(x) {
  if (inherits(x, c("sasonr_ods_table", "sasonr_ods_list", "data.frame"))) attr(x, "sasonr_result", exact = TRUE) else NULL
}
