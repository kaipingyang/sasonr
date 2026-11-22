# PROC registry and driver.
#
# One entry per procedure that runs on the ODS pipeline:
#   compute(d, spec)  returns list(tables = <sasonr_ods_table>..., data = <named
#                     list of OUT= data sets>, events = <log lines>)
#   templates         listing template of each table (render_listing())
# Aliases map to their entry (SUMMARY runs the MEANS entry).  Procedures not
# in the registry keep their own run-time helper.

proc_registry <- list(
  means = list(compute = function(d, spec) means_compute(d, spec), templates = c(Summary = "base.summary"),
               ods_output = function(tables, spec, line) means_ods_output(tables, spec, line)),
  # no procedure title line; the tables of a step are written page by page
  tabulate = list(compute = function(d, spec) tab_compute_out(d, spec), templates = c(Table = "base.tabulate"),
                  proctitle = FALSE,
                  listing = function(tables, ls) tab_listing_tables(tables, ls),
                  ods_output = function(tables, spec, line) tab_output_data(tables, spec, line)),
  # one table per page (BY group); no procedure title
  print = list(compute = function(d, spec) print_compute(d, spec), templates = c(Print = "base.print"),
               proctitle = FALSE, listing = function(tables, ls) print_listing_tables(tables))
)
proc_registry_alias <- c(summary = "means")

proc_registry_entry <- function(name) {
  name <- tolower(name)
  key <- if (name %in% names(proc_registry_alias)) proc_registry_alias[[name]] else name
  e <- proc_registry[[key]]
  if (is.null(e)) rlang::abort(paste0("PROC ", toupper(name), " is not in the PROC registry"), class = "sasonr_internal_error")
  e
}

#' Run a procedure of the PROC registry
#'
#' Computes the tables and output data sets of one PROC step, then routes
#' them: the data sets go to WORK (through `sas_proc_store()` in the
#' translated code), the tables are rendered to the listing.
#' @param name The procedure name as written (`"means"`, `"summary"`).
#' @param d Input data (after WHERE).
#' @param spec Specification built by the translator.
#' @return list(listing, out, tables).
#' @export
#' @keywords internal
sas_rt_proc <- function(name, d, spec) {
  e <- proc_registry_entry(name)
  res <- e$compute(d, spec)
  if (length(res$events) > 0) {
    rt <- rt_diag()
    rt$proc_notes <- c(rt$proc_notes, res$events)
  }
  reqs <- rt_diag()$ods_output
  # PROC PRINT: the engine writes the WARNING of a pending request (run.R)
  if (length(reqs) > 0 && !identical(tolower(name), "print")) {
    # ODS OUTPUT data sets (ods_output.R): PROC MEANS and PROC TABULATE
    if (is.null(e$ods_output) || !tolower(name) %in% c("means", "tabulate")) {
      abort_unsupported(paste0("ODS OUTPUT in a PROC ", toupper(name), " step"), spec$line)
    }
    ods <- ods_output_write(res$tables, spec, reqs, e$ods_output, name)
    clash <- intersect(names(ods), names(res$data))
    if (length(clash) > 0) abort_unsupported(paste0("an ODS OUTPUT data set that is also an OUTPUT data set (WORK.", toupper(clash[1]), ")"), spec$line)
    res$data <- c(ods, res$data)
  }
  listing <- if (is.null(e$listing)) {
    render_listing_tables(res$tables, paste0("The ", toupper(name), " Procedure"), rt_linesize())
  } else {
    e$listing(res$tables, rt_linesize())
  }
  list(listing = as.character(listing), out = res$data, tables = res$tables, pages = attr(listing, "pages", exact = TRUE),
       groups = attr(listing, "groups", exact = TRUE))
}

# Listing renderers ---------------------------------------------------------------

listing_templates <- list(
  base.summary = function(tb, ls, ps) listing_base_summary(tb, ls),
  base.tabulate = function(tb, ls, ps) as.character(tab_listing_tables(list(tb), ls)),
  base.print = function(tb, ls, ps) listing_base_print(tb)
)

#' @noRd
render_listing <- function(table, ls, ps = NULL) {
  f <- listing_templates[[attr(table, "template", exact = TRUE)]]
  if (is.null(f)) {
    rlang::abort(paste0("No listing template ", attr(table, "template", exact = TRUE)), class = "sasonr_internal_error")
  }
  f(table, ls, ps)
}

# The listing of one step: the byline before the first table of each BY
# group, the procedure title before the first table, then each table.
render_listing_tables <- function(tables, title, ls, ps = NULL) {
  out <- character()
  titled <- FALSE
  group <- NULL
  for (tb in tables) {
    by <- attr(tb, "by", exact = TRUE)
    if (!is.null(by) && !identical(by$group, group)) {
      out <- c(out, "", by$text, "")
      group <- by$group
    }
    if (!titled) {
      out <- c(out, title, "")
      titled <- TRUE
    }
    out <- c(out, render_listing(tb, ls, ps))
  }
  out
}
