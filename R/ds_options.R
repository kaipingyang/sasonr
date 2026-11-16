# Data set options: one record and one reader for an input data set.
#
# An input data set reference (`set a(keep = x rename = (x = y) where = (y > 1))`)
# is turned into a normalised option record by ds_opts_record(); the code that
# reads the data set with those options applied is built by ds_opts_apply().
# The DATA step reader (ds_code() in codegen_tidy.R) and PROC DATA=
# (proc_source_code() in proc.R, through ds_code()) use it today. The other
# places that apply data set options still have their own code; they move onto
# this record in later steps. What each of them does today:
#
# - DATA step input (an_reader() in analyze.R, read by ds_code()): KEEP=/DROP=
#   naming a variable not on the data set is an ERROR ("... has never been
#   referenced", no final period). RENAME= is not checked against the data
#   set: a single unknown old name is ignored, an unknown old name among
#   several pairs stops with an R error. Two variables renamed to one name is
#   an ERROR ("Variable x already exists on file"). WHERE= uses the names
#   after KEEP=/DROP= and RENAME= (an unknown or dropped variable is an
#   ERROR, "is not on file"); WHERE= replaces the WHERE statement for its data
#   set. IN= is a 0/1 variable set by sas_set_input() / the merge plan.
#   FIRSTOBS=/OBS= window the rows that satisfy WHERE= / WHERE.
# - PROC DATA= (proc_input() in proc.R, then ds_code()): as DATA step input;
#   in addition a WHERE statement variable not on the data set is an ERROR
#   ("Variable x is not on file WORK.A"). IN= is accepted by the parser and
#   has no effect.
# - DATA step output (output_specs() in analyze.R): KEEP=/DROP= naming an
#   unknown variable is ignored; RENAME= of an unknown old name is ignored;
#   a RENAME= clash keeps the first variable of that name; WHERE= fails loud
#   (unsupported). IN=, FIRSTOBS=, OBS= are rejected by the parser (output
#   context), as on every output data set.
# - PROC OUT= (proc_out_options() in proc_util.R; MEANS, FREQ, TRANSPOSE,
#   CONTENTS): KEEP=/DROP= naming an unknown variable and RENAME= of an
#   unknown old name are ERRORs ("... has never been referenced.", with a
#   final period); no RENAME= clash check; WHERE= fails loud.
# - PROC SORT OUT= (inline in translate_proc()): KEEP=/DROP= naming an unknown
#   variable is ignored; RENAME= of an unknown old name is ignored; no clash
#   check; WHERE= fails loud.
# - PROC SQL table (sql_table_rel() in sql_exec.R): KEEP=/DROP= naming an
#   unknown variable is ignored; RENAME= is not checked (as DATA step input,
#   including the R error) and has no clash check; WHERE= is evaluated as an
#   SQL expression on the names after RENAME=; IN= fails loud; FIRSTOBS=/OBS=
#   window the rows after WHERE= with their own inline code, then INOBS=.
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
#' @return One R expression as a string.
#' @noRd
ds_opts_apply <- function(rec, an, obs_col = FALSE, where = NULL) {
  args <- c("work", r_str(rec$name))
  if (!is.null(rec$keep) || !is.null(rec$drop)) args <- c(args, paste0("keep = ", r_chr_vec(rec$vars)))
  if (!is.null(rec$rename)) {
    args <- c(args, paste0("rename = c(", paste0(r_str(names(rec$rename)), " = ", r_str(rec$rename), collapse = ", "), ")"))
  }
  code <- sprintf("sas_ds(%s)", paste(args, collapse = ", "))
  if (obs_col) code <- paste0(code, " |> dplyr::mutate(.obs = dplyr::row_number())")
  if (!is.null(rec$where)) {
    wcx <- new_cx("vec", an)
    wcx$where <- TRUE
    code <- sprintf("%s |> dplyr::filter(sas_where_cond(sas_true(%s)))", code, gen_expr(rec$where, wcx))
  }
  # the WHERE statement condition applies to every input data set without a
  # WHERE= option (the data set option takes precedence), before FIRSTOBS=/OBS=
  if (!is.null(where) && is.null(rec$where)) {
    cx <- new_cx("vec", an)
    cx$where <- TRUE
    code <- sprintf("%s |> dplyr::filter(sas_where_cond(sas_true(%s)))", code, gen_expr(where, cx))
  }
  # FIRSTOBS= / OBS= count observations that satisfy WHERE= and WHERE
  if (!is.null(rec$firstobs) || !is.null(rec$obs)) {
    code <- sprintf("%s |> sas_obs_window(%s, %s)", code,
                    if (is.null(rec$firstobs)) "NULL" else r_num(rec$firstobs), if (is.null(rec$obs)) "NULL" else r_num(rec$obs))
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
    if (length(tr) > 0) code <- sprintf("%s |> sas_trunc_chars(%s)", code, r_lit(tr))
  }
  wexpr <- rec$where %||% where
  sprintf("%s |> sas_note_read(%s%s)", code, r_str(rec$name),
          if (is.null(wexpr)) "" else paste0(", where = ", r_str(where_text(wexpr))))
}
