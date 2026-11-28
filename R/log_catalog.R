# The catalogue of the log messages sasonr writes (log.R).
#
# One entry per message id:
#   kind      note, warning or error
#   template  the lines of the message; `<name>` is replaced by the argument
#             `name` of the event (a number is written as an integer)
#   wrap      "none": the lines as written; "fixed": `wrapped` replaces the
#             first line when that line is longer than LINESIZE; "linesize":
#             the message is one sentence wrapped at LINESIZE (log_wrap(),
#             continuation lines indented by 6); "join": the lines joined into
#             one line when that line fits LINESIZE, else the lines as written
#   places    "none"; "list": a list of (Line):(Column) places; "counted":
#             "(Number of times) at (Line):(Column)" places.  Places go on
#             lines of at most LINESIZE columns, indented by 6 (place_lines())
#   order     the order of the places: "line_col" by line, then column;
#             "line_coltext" by line, then the column compared as text
#   pad       width a place is padded to ("%-6s"), 0 for none
#   store     where the places of the current step are kept: "compile" (the
#             compile-time state, ct) or "run" (the run-time state, rt)
#   key       how a place is formed: "at" from c(line, column) (missing parts:
#             "0:0"), "line_col" from line and column (no column: 0)
#   neutral   TRUE for a message in which sasonr names itself as the one that
#             acted, or names no one, where SAS names itself
# Messages with places are merged: one message per step with all its places.

log_msg <- function(kind, template, wrap = "none", wrapped = NULL, places = "none", order = "line_col", pad = 0L,
                    store = NA_character_, key = "at", neutral = FALSE) {
  list(kind = kind, template = template, wrap = wrap, wrapped = wrapped, places = places, order = order,
       pad = pad, store = store, key = key, neutral = neutral)
}

log_mv_head <- c("NOTE: Missing values were generated as a result of performing an operation on missing values.",
                 "      Each place is given by: (Number of times) at (Line):(Column).")

# the exception block, written twice around the traceback heading
log_exception_block <- c("ERROR:  An exception has been encountered.",
                         "Please contact technical support and provide them with the following traceback information:",
                         " ", "The task name is [<task>]", "Segmentation Violation", " ")

log_catalog <- list(
  # steps and the error model (errors.R, step_exception.R, run.R)
  # the source echo of a submission left out (log_items()); written by sas_log() only
  sasonr.echo_omitted = log_msg("note", "NOTE: sasonr wrote no source echo for this submission: <reason>.",
                                wrap = "linesize"),
  call_execute.generated = log_msg("note", "NOTE: CALL EXECUTE generated line."),
  call_execute.none = log_msg("note", "NOTE: CALL EXECUTE routine executed successfully, but no SAS statements were generated."),
  step.stopped = log_msg("note", "NOTE: sasonr stopped processing this step because of errors.", neutral = TRUE),
  step.syntax_check = log_msg("note", c("NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax check mode.",
                                        "      This prevents execution of subsequent data modification statements."),
                              wrap = "join", neutral = TRUE),
  step.exception = log_msg("error", c(log_exception_block, "Traceback of the Exception:", " ", " ", log_exception_block),
                           neutral = TRUE),
  # data sets
  ds.created = log_msg("note", "NOTE: The data set WORK.<name> has <nobs> observations and <nvar> variables."),
  ds.incomplete = log_msg("warning", paste("WARNING: The data set WORK.<name> may be incomplete.  When this step was stopped",
                                           "there were <nobs> observations and <nvar> variables.")),
  ds.not_replaced_stopped = log_msg("warning", "WARNING: Data set WORK.<name> was not replaced because this step was stopped."),
  ds.not_replaced_incomplete = log_msg("warning", "WARNING: Data set WORK.<name> was not replaced because new file is incomplete."),
  ds.looping = log_msg("note", "NOTE: DATA STEP stopped due to looping."),
  ods.rtf_body = log_msg("note", "NOTE: Writing RTF Body file: <file>"),
  ods.html_body = log_msg("note", "NOTE: Writing HTML Body file: <file>"),
  lib.assigned = log_msg("note", "NOTE: Libref <ref> was successfully assigned as follows:"),
  lib.engine = log_msg("note", "      Engine:        <engine>"),
  lib.path = log_msg("note", "      Physical Name: <path>"),
  lib.deassigned = log_msg("note", "NOTE: Libref <ref> has been deassigned."),
  ds.read_lib = log_msg("note", "NOTE: There were <nobs> observations read from the data set <name>."),
  ds.read_where_lib = log_msg("note", c("NOTE: There were <nobs> observations read from the data set <name>.",
                                        "      WHERE <where>;")),
  ds.merge_repeats = log_msg("note", "NOTE: MERGE statement has more than one data set with repeats of BY values."),
  ds.read = log_msg("note", "NOTE: There were <nobs> observations read from the data set WORK.<name>."),
  ds.read_where = log_msg("note", c("NOTE: There were <nobs> observations read from the data set WORK.<name>.",
                                    "      WHERE <where>;")),
  ds.no_obs = log_msg("note", "NOTE: No observations in data set WORK.<name>."),
  ds.no_vars = log_msg("note", "NOTE: No variables in data set WORK.<name>."),
  # global statements
  filename.no_assign = log_msg("warning", "WARNING: No logical assign for filename <quote><name><quote>."),
  # DATA step run time and compile time
  input.new_line = log_msg("note", "NOTE: sasonr went to a new line when INPUT statement reached past the end of a line.",
                           neutral = TRUE),
  mv.generated = log_msg("note", log_mv_head, places = "counted", store = "run"),
  mv.compile = log_msg("note", log_mv_head, places = "list", store = "compile"),
  math.failed = log_msg("note",
    c("NOTE: Mathematical operations could not be performed at the following places. The results of the operations have been set to missing values.",
      "      Each place is given by: (Number of times) at (Line):(Column)."),
    wrap = "fixed",
    wrapped = c("NOTE: Mathematical operations could not be performed at the following places. The results of the operations have been set to ",
                "      missing values."),
    places = "counted", store = "run"),
  conv.n2c = log_msg("note", "NOTE: Numeric values have been converted to character values at the places given by: (Line):(Column).",
                     places = "list", order = "line_coltext", pad = 6L, store = "compile", key = "line_col"),
  conv.c2n = log_msg("note", "NOTE: Character values have been converted to numeric values at the places given by: (Line):(Column).",
                     places = "list", order = "line_coltext", pad = 6L, store = "compile", key = "line_col"),
  # sasonr.strict = FALSE (strict_gate(), registry.R)
  sasonr.approximate = log_msg("note", "NOTE: sasonr used approximate implementations: <features>.", wrap = "linesize"),
  # macro
  sysfunc.math = log_msg("note", paste("NOTE: Mathematical operations could not be performed during %SYSFUNC function execution.",
                                       "The result of the operations have been set to a missing value."), wrap = "linesize"),
  # PROC SQL
  sql.noexec = log_msg("note", "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements."),
  sql.not_executed = log_msg("note", "NOTE: Statement not executed due to NOEXEC option."),
  sql.view_defined = log_msg("note", "NOTE: SQL view WORK.<name> has been defined."),
  sql.table_created = log_msg("note", "NOTE: Table WORK.<name> created, with <nrow> rows and <ncol> columns."),
  sql.view_dropped = log_msg("note", "NOTE: View WORK.<name> has been dropped."),
  sql.table_dropped = log_msg("note", "NOTE: Table WORK.<name> has been dropped."),
  sql.no_file = log_msg("warning", "WARNING: File WORK.<name>.DATA does not exist."),
  sql.outobs = log_msg("warning", "WARNING: Statement terminated early due to OUTOBS=<n> option."),
  sql.no_rows = log_msg("note", "NOTE: No rows were selected."),
  sql.var_exists = log_msg("warning", "WARNING: Variable <name> already exists on file WORK."),
  sql.row_inserted = log_msg("note", "NOTE: <n> row was inserted into WORK.<name>."),
  sql.rows_inserted = log_msg("note", "NOTE: <n> rows were inserted into WORK.<name>."),
  sql.row_updated = log_msg("note", "NOTE: <n> row was updated in WORK.<name>."),
  sql.rows_updated = log_msg("note", "NOTE: <n> rows were updated in WORK.<name>."),
  sql.row_deleted = log_msg("note", "NOTE: <n> row was deleted from WORK.<name>."),
  sql.rows_deleted = log_msg("note", "NOTE: <n> rows were deleted from WORK.<name>."),
  sql.table_modified = log_msg("note", "NOTE: Table WORK.<name> has been modified, with <ncol> columns."),
  sql.remerge = log_msg("note", "NOTE: The query requires remerging summary statistics back with the original data."),
  sql.null_columns = log_msg("warning", "WARNING: A table has been extended with null columns to perform the <op> set operation."),
  sql.invalid_arg = log_msg("note", "NOTE: Invalid argument to function <fn>. Missing values may be generated.")
)

# The parts of the end of a step, in the order they are written, per kind of
# step: "compile" the compile-time NOTEs of the translation, "run" what the
# step wrote while it ran (PUT lines, data-error NOTEs, the PDV dump), a
# message id (input.new_line, mv.generated, math.failed: from the counters
# of the step), "reads" the observations read, "proc" the NOTEs and WARNINGs
# of the procedure, "datasets" the output data sets, "ods" the ODS OUTPUT
# data sets of the step and "ods.warn" the WARNING for a requested ODS table
# the step did not produce (ods_output.R).  `notes`: written only
# under NOTES (and sasonr.step_notes).  PROC SQL writes no "Missing values
# were generated" NOTE.
log_close_order <- list(
  data = data.frame(part = c("compile", "run", "input.new_line", "mv.generated", "math.failed", "looping", "reads", "proc", "datasets"),
                    notes = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
  proc = data.frame(part = c("compile", "run", "input.new_line", "mv.generated", "math.failed", "trace", "ods", "ods.warn", "reads", "proc", "datasets"),
                    notes = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)),
  sql = data.frame(part = c("input.new_line", "math.failed"), notes = c(TRUE, TRUE))
)
