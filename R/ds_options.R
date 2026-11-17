# Data set options: one record and one reader for an input data set.
#
# An input data set reference (`set a(keep = x rename = (x = y) where = (y > 1))`)
# is turned into a normalised option record by ds_opts_record(); the code that
# reads the data set with those options applied is built by ds_opts_apply().
# The DATA step reader (ds_code() in codegen_tidy.R) and PROC DATA=
# (proc_source_code() in proc.R, through ds_code()) use it today. The other
# places that apply data set options still have their own code; they move onto
# this record in later steps.
#
# CURRENT BEHAVIOUR (before alignment) -- what each place does today; this is
# not the target, see "Target behaviour" below:
#
# - DATA step input (an_reader() in analyze.R, read by ds_code()): aligned
#   with the target for KEEP=/DROP= naming an unknown variable, RENAME= of an
#   unknown old name and RENAME= onto an existing name (the two ERRORs; the
#   error model keeps the variables the rest of the step references); RENAME=
#   of a variable that KEEP=/DROP= removes fails loud. WHERE= uses the names
#   after KEEP=/DROP= and RENAME= (an unknown or dropped variable is an
#   ERROR, "is not on file"); WHERE= replaces the WHERE statement for its data
#   set. IN= is a 0/1 variable set by sas_set_input() / the merge plan.
#   FIRSTOBS=/OBS= window the rows that satisfy WHERE= / WHERE.
# - PROC DATA= (proc_input() in proc.R, then ds_code()): as DATA step input;
#   in addition a WHERE statement variable not on the data set is an ERROR
#   ("Variable x is not on file WORK.A"). IN= on PROC PRINT is ERROR 22-7,
#   on the other PROCs it fails loud.
# - DATA step output (output_specs() in analyze.R): the output role,
#   ds_out_plan() with the RENAME= rules; WHERE= fails loud (unsupported).
#   IN=, FIRSTOBS=, OBS= are rejected by the parser (output context), as on
#   every output data set.
# - PROC SORT OUT= and PROC OUT= (MEANS, FREQ, TRANSPOSE, CONTENTS): the
#   output role, ds_out_plan() and proc_out_options() below. KEEP= naming one
#   unknown variable is aligned with the target (SORT and MEANS write the
#   WARNING with the compile notes, FREQ, TRANSPOSE and CONTENTS fail loud on
#   it); every other wrong name, and WHERE=, fails loud.
# - PROC SQL table (sql_table_rel() in sql_exec.R): KEEP=/DROP=/RENAME= by
#   ds_in_plan() (the ERRORs as for DATA step input, read as statement ERRORs:
#   NOEXEC, no syntax-check mode at QUIT; a RENAME= clash fails loud); WHERE=
#   is evaluated as an SQL expression on the names after RENAME=; IN= fails
#   loud; FIRSTOBS=/OBS= (ds_obs_rows()) window the rows after WHERE=, then
#   INOBS=.
#
# TARGET BEHAVIOUR (as documented) -- one wrong name in an option; the level depends on
# the role of the data set. "output" rows hold for the DATA step output data
# set; the KEEP= row also for PROC SORT OUT= and PROC MEANS OUTPUT OUT=; the
# "input" rows for the DATA step SET, PROC PRINT DATA= (KEEP=) and PROC SQL
# FROM (KEEP=, RENAME= of an unknown old name). Any
# form not in this table fails loud until its behaviour is established:
#
#   role / option                        message                                      result
#   input  KEEP=/DROP= unknown variable  ERROR: The variable x in the DROP, KEEP, or   the step stops; an output
#                                        RENAME list has never been referenced.       data set of the DATA step
#                                        ERROR: Invalid DROP, KEEP, or RENAME option   keeps the variables the
#                                        on file WORK.A.                              step references (0 obs.)
#   input  RENAME= unknown old name      ERROR: Variable x is not on file WORK.A.     as above; the valid pairs
#                                        ERROR: Invalid DROP, KEEP, ... (as above)    of the option are applied
#   input  RENAME= onto an existing name ERROR: Variable y already exists on file     as above; the rename is
#                                        WORK.A. + the "Invalid" ERROR                applied
#   output KEEP= unknown variable        WARNING: The variable x in the DROP, KEEP,   the step completes; the
#                                        or RENAME list has never been referenced.    variable is not kept
#                                                                                     (KEEP=x alone: 0 variables)
#   output RENAME= unknown old name      the same WARNING                             the pair is ignored
#   output RENAME= onto an existing name WARNING: Variable y already exists on file   the renamed variable stays,
#                                        WORK.OUT.                                    the other one is dropped
#   output WHERE=                        none                                         the rows are filtered
#   PROC PRINT DATA= with IN=            ERROR 22-7: Invalid option name IN.
#
# Order inside the DATA step reader: KEEP=/DROP= and RENAME= (sas_ds), WHERE=
# (or else the WHERE statement), FIRSTOBS=/OBS= on the rows that satisfy it,
# truncation of character values longer than the PDV variable, then the
# "observations were read" note.

#' Normalised data set option record
#'
#' @param d An analysed input data set entry (an element of `datasets` of a
#'   SET/MERGE reader after `an_reader()`): `name`, `options` as returned by
#'   `parse_ds_options()` with the WHERE= condition analysed, `vars` (input
#'   variables after KEEP=/DROP=) and `out_names` (after RENAME=).
#' @return A `sas_ds_opts` list: `name`; `keep`, `drop` (variable-list items
#'   or NULL); `rename` (named character vector old = new, or NULL); `where`
#'   (analysed WHERE= condition, or NULL); `in_var` (IN= variable, or NULL);
#'   `firstobs`, `obs` (numbers, or NULL); `vars`, `out_names`.
#' @noRd
ds_opts_record <- function(d) {
  o <- d$options
  structure(list(
    name = d$name,
    keep = o$keep, drop = o$drop, rename = o$rename, where = o$where,
    in_var = o$in_var, firstobs = o$firstobs, obs = o$obs,
    vars = d$vars, out_names = d$out_names
  ), class = "sas_ds_opts")
}

#' Code that reads an input data set with its options applied
#'
#' @param rec A record from [ds_opts_record()].
#' @param an The analysed step (`meta` and `info` enable the truncation of
#'   longer character values; PROC passes a list without them).
#' @param obs_col Add the `.obs` column (row number before WHERE).
#' @param where The analysed condition of the WHERE statement that applies,
#'   or NULL. WHERE= on the data set takes precedence over it.
#' @return A [cgi()] item: the call (one expression) and its display text.
#' @noRd
ds_opts_apply <- function(rec, an, obs_col = FALSE, where = NULL) {
  args <- list(rlang::sym("work"), rec$name)
  if (!is.null(rec$keep) || !is.null(rec$drop)) args$keep <- cg_val(rec$vars)
  if (!is.null(rec$rename)) args$rename <- rlang::call2("c", !!!as.list(rec$rename))
  x <- cgi(cg_call("sas_ds", args))
  if (obs_col) x <- cgi_pipe(x, quote(dplyr::mutate(.obs = dplyr::row_number())))
  where_stage <- function(e) {
    wcx <- new_cx("vec", an)
    wcx$where <- TRUE
    cg_call("dplyr::filter", list(rlang::expr(sas_where_cond(sas_true(!!gen_lang(e, wcx))))))
  }
  if (!is.null(rec$where)) x <- cgi_pipe(x, where_stage(rec$where))
  # the WHERE statement condition applies to every input data set without a
  # WHERE= option (the data set option takes precedence), before FIRSTOBS=/OBS=
  if (!is.null(where) && is.null(rec$where)) x <- cgi_pipe(x, where_stage(where))
  # FIRSTOBS= / OBS= count observations that satisfy WHERE= and WHERE
  if (!is.null(rec$firstobs) || !is.null(rec$obs)) {
    x <- cgi_pipe(x, cg_call("sas_obs_window", list(if (!is.null(rec$firstobs)) cg_num(rec$firstobs),
                                                    if (!is.null(rec$obs)) cg_num(rec$obs))))
  }
  # values from a data set whose character variable is longer than the PDV
  # variable (defined by an earlier data set) are truncated
  m <- an$meta[[rec$name]]
  if (!is.null(an$info) && !is.null(m)) {
    tr <- list()
    for (j in seq_along(rec$vars)) {
      inf <- an$info[[rec$out_names[j]]]
      ml <- m$len[[rec$vars[j]]]
      if (identical(m$type[[rec$vars[j]]], "char") && identical(inf$type, "char") && !is.null(ml) && ml > inf$len) {
        tr[[rec$out_names[j]]] <- as.integer(inf$len)
      }
    }
    if (length(tr) > 0) x <- cgi_pipe(x, rlang::call2("sas_trunc_chars", cg_val(tr)))
  }
  wexpr <- rec$where %||% where
  cgi_pipe(x, cg_call("sas_note_read", c(list(rec$name), if (!is.null(wexpr)) list(where = where_text(wexpr)))))
}

#' KEEP=, DROP= and RENAME= on an output data set
#'
#' The output role of the data set option component, used by PROC SORT OUT=
#' and the PROC OUT= data sets (MEANS, FREQ, TRANSPOSE, CONTENTS). On an
#' output data set a wrong name is a WARNING and the step completes:
#' - KEEP= naming one variable that is not on the data set: "The variable x
#'   in the DROP, KEEP, or RENAME list has never been referenced." and no
#'   variable is kept;
#' - with `rename_rules` (the DATA step output data set): RENAME= with one
#'   pair whose old name is not on the data set: the same WARNING, the pair
#'   is ignored; RENAME= with one pair whose new name is another variable of
#'   the data set: "Variable y already exists on file WORK.OUT."; the renamed
#'   variable stays at its place and the other one is dropped.
#' The forms these rules do not cover fail loud: KEEP= naming an unknown
#' variable among others, DROP= naming an unknown variable, an unknown or
#' clashing RENAME= pair without `rename_rules` or among several pairs,
#' WHERE=.
#'
#' @param vars The variables of the data set before the options.
#' @param opts The options from `parse_ds_options()` (`keep`, `drop`,
#'   `rename`, `where`).
#' @param file The data set name (for the WARNING text).
#' @param line SAS line for the conditions.
#' @param rename_rules Apply the two RENAME= rules above (else they fail loud).
#' @return `list(vars, names, warnings)`: the variables kept (old names), their
#'   names after RENAME=, and the WARNING lines.
#' @noRd
ds_out_plan <- function(vars, opts, file, line = NULL, rename_rules = FALSE) {
  warn <- character()
  never <- function(v) paste0("WARNING: The variable ", v, " in the DROP, KEEP, or RENAME list has never been referenced.")
  if (!is.null(opts$where)) abort_unsupported("WHERE= on an output data set", line)
  keep <- vars
  if (!is.null(opts$keep)) {
    items <- resolve_ds_items(opts$keep, vars, line)
    miss <- setdiff(items, vars)
    if (length(miss) > 0) {
      if (length(items) > 1) abort_unsupported("KEEP= on an output data set naming a variable that is not on it, among other variables", line)
      warn <- c(warn, never(ds_item_text(opts$keep, miss)))
    }
    keep <- intersect(vars, items)
  }
  if (!is.null(opts$drop)) {
    items <- resolve_ds_items(opts$drop, vars, line)
    if (length(setdiff(items, vars)) > 0) abort_unsupported("DROP= on an output data set naming a variable that is not on it", line)
    keep <- setdiff(keep, items)
  }
  out <- keep
  rn <- opts$rename
  if (length(rn) > 0) {
    hit <- match(names(rn), keep)
    if (anyNA(hit)) {
      if (length(rn) > 1 || !rename_rules) abort_unsupported("RENAME= on an output data set naming a variable that is not on it", line)
      warn <- c(warn, never(names(rn)))
    } else {
      out[hit] <- unname(rn)
      clash <- intersect(unname(rn), keep[-hit])
      if (length(clash) > 0) {
        if (length(rn) > 1 || !rename_rules || match(clash, keep) < hit) {
          abort_unsupported("RENAME= on an output data set to the name of another variable", line)
        }
        warn <- c(warn, paste0("WARNING: Variable ", clash, " already exists on file WORK.", toupper(file), "."))
        k <- match(clash, keep)
        keep <- keep[-k]
        out <- out[-k]
      }
      if (anyDuplicated(out)) abort_unsupported("RENAME= on an output data set giving two variables one name", line)
    }
  }
  list(vars = keep, names = out, warnings = warn)
}

#' Apply KEEP=, DROP= and RENAME= to a PROC output data set (data + metadata)
#'
#' @param d The data frame; `m` its metadata (`vars`, `type`, `len`,
#'   `formats`, `informats`, `labels`, `disp`).
#' @param opts,line As for [ds_out_plan()]; `file` the data set name.
#' @param warned TRUE when the translation already wrote the WARNINGs of
#'   [ds_out_plan()] (compile notes); otherwise a WARNING fails loud.
#' @return `list(data, meta)`.
#' @noRd
proc_out_options <- function(d, m, opts, line = NULL, file = "", warned = FALSE) {
  if (length(opts) == 0) return(list(data = d, meta = m))
  plan <- ds_out_plan(m$vars, opts, file, line)
  if (length(plan$warnings) > 0 && !warned) abort_unsupported(sub("^WARNING: ", "", plan$warnings[1]), line)
  keep <- plan$vars
  new <- plan$names
  d <- d[keep]
  ren <- function(l) {
    l <- l[intersect(keep, names(l))]
    if (length(l) == 0) return(l)
    names(l) <- new[match(names(l), keep)]
    l
  }
  disp <- m$disp[keep]
  ch <- new != keep
  disp[ch] <- as.list(new[ch])
  names(d) <- new
  list(data = d, meta = list(vars = new, type = ren(m$type), len = ren(m$len), formats = ren(m$formats),
                             informats = ren(m$informats), labels = ren(m$labels), disp = stats::setNames(disp, new)))
}

#' KEEP=, DROP= and RENAME= on an input data set
#'
#' The input role of the data set option component (DATA step SET/MERGE,
#' PROC DATA=, PROC SQL FROM). A wrong name is an ERROR followed by
#' "ERROR: Invalid DROP, KEEP, or RENAME option on file WORK.X." and the step
#' (the PROC SQL statement) stops: KEEP=/DROP= naming a variable that is not
#' on the data set ("The variable x in the DROP, KEEP, or RENAME list has
#' never been referenced."), RENAME= of an old name that is not on it
#' ("Variable x is not on file WORK.X."), RENAME= giving two variables one
#' name ("Variable y already exists on file WORK.X."). RENAME= of a variable
#' that KEEP=/DROP= removes fails loud.
#'
#' @param vars The variables of the data set.
#' @param opts The options from `parse_ds_options()`.
#' @param file The data set name.
#' @param line SAS line for the conditions.
#' @param sql In PROC SQL: every ERROR is raised as found while the statement
#'   is read (no syntax-check mode at QUIT); a RENAME= clash fails loud.
#' @return `list(vars, names)`: the variables read (old names) and their names
#'   after RENAME=.
#' @noRd
ds_in_plan <- function(vars, opts, file, line = NULL, sql = FALSE) {
  invalid <- paste0("\nERROR: Invalid DROP, KEEP, or RENAME option on file WORK.", toupper(file), ".")
  err <- function(msg, syntax = FALSE) {
    if (sql || syntax) abort_syntax(paste0(msg, invalid), line) else abort_runtime(paste0(msg, invalid), line)
  }
  for (opt in c("keep", "drop")) {
    if (is.null(opts[[opt]])) next
    miss <- setdiff(resolve_ds_items(opts[[opt]], vars, line), vars)
    if (length(miss) > 0) {
      err(paste0("The variable ", ds_item_text(opts[[opt]], miss[1]), " in the DROP, KEEP, or RENAME list has never been referenced."))
    }
  }
  keep <- vars
  if (!is.null(opts$keep)) keep <- intersect(keep, resolve_ds_items(opts$keep, vars, line))
  if (!is.null(opts$drop)) keep <- setdiff(keep, resolve_ds_items(opts$drop, vars, line))
  out <- keep
  if (!is.null(opts$rename)) {
    old <- names(opts$rename)
    if (any(!old %in% keep & old %in% vars)) abort_unsupported("RENAME= of a variable that KEEP=/DROP= removes", line)
    if (any(!old %in% vars)) err(paste0("Variable ", old[!old %in% vars][1], " is not on file WORK.", toupper(file), "."))
    out[match(old, keep)] <- unname(opts$rename)
  }
  if (anyDuplicated(out)) {
    if (sql) abort_unsupported("RENAME= in PROC SQL giving two columns one name", line)
    err(sprintf("Variable %s already exists on file WORK.%s.", out[anyDuplicated(out)], toupper(file)), syntax = TRUE)
  }
  list(vars = keep, names = out)
}

#' The rows FIRSTOBS= and OBS= keep of `n` rows (those after WHERE=)
#' @noRd
ds_obs_rows <- function(n, firstobs = NULL, obs = NULL) {
  from <- firstobs %||% 1
  to <- min(obs %||% n, n)
  seq_len(max(0, to - from + 1)) + from - 1
}
