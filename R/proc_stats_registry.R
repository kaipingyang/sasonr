# PROC statistics registry (inst/registry/proc_stats.csv): one row per
# procedure x statistic (x condition) with its status (a generated file, never
# edited by hand).  Statuses:
#   supported    the statistic runs
#   partial      supported inside a domain; other inputs fail loud at run time
#                in every mode
#   approximate  some values may differ in the last bits
#   unsupported  not supported yet
# approximate / unsupported statistics are gated by `sasonr.strict` exactly like
# functions (see ?sasonr_strict): TRUE (default) fails loud with a condition
# of class `sasonr_strict_error` naming the procedure, the statistic and the
# opt-out; FALSE runs with a `sasonr_strict_warning` warning.

#' PROC statistics registry
#'
#' One row per procedure and statistic (and condition, such as a WEIGHT
#' statement) with its status (`supported`, `partial`, `approximate`,
#' `unsupported`; see [sasonr_strict]).
#' @return A data frame.
#' @keywords internal
sas_proc_stat_registry <- function() {
  if (is.null(registry_cache$proc_stats)) {
    f <- system.file("registry", "proc_stats.csv", package = "sasonr")
    registry_cache$proc_stats <- if (nzchar(f)) {
      utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    } else {
      data.frame(proc = character(), statistic = character(), condition = character(), status = character(),
                 stringsAsFactors = FALSE)
    }
  }
  registry_cache$proc_stats
}

# PROC SUMMARY shares the MEANS rows; a row with statistic "*" covers every
# statistic under its condition (e.g. any statistic with a WEIGHT statement).
proc_stat_status <- function(proc, stat, condition = "default") {
  reg <- sas_proc_stat_registry()
  proc <- toupper(proc)
  if (proc == "SUMMARY") proc <- "MEANS"
  sel <- reg$proc == proc & reg$condition == condition
  hit <- reg$status[sel & reg$statistic == toupper(stat)]
  if (!length(hit)) hit <- reg$status[sel & reg$statistic == "*"]
  if (length(hit)) hit[1] else NA_character_
}

# Gate one statistic of a procedure (`condition`: "default" or a registry
# condition such as "weight").  Returns invisibly when it may run.
proc_stat_check <- function(proc, stat, line = NULL, condition = "default") {
  st <- proc_stat_status(proc, stat, condition)
  if (!feature_gated(st)) return(invisible())
  cond_txt <- if (identical(condition, "default")) "" else paste0(" (", condition, ")")
  what <- sprintf("PROC %s statistic %s%s %s", toupper(proc), toupper(stat), cond_txt, feature_status_text(st))
  strict_gate(what, line, feature = sprintf("PROC %s statistic %s%s", toupper(proc), toupper(stat), cond_txt),
              fields = list(kind = "proc_statistic", proc = toupper(proc), name = toupper(stat), status = st))
}
