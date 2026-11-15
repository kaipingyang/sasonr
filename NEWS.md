# sasonr 0.0.0.9000

sasonr is in early development and not on CRAN yet. This section lists what
the development version provides.

## Running SAS programs

* `sas_run()` runs a SAS program given as text and returns the log, the
  listing and the WORK data sets; `sas_translate()` shows the R code that is
  generated for each step. `sas_lex()`, `sas_parse()`, `sas_parse_expr()` and
  `sas_analyze()` expose the lexer, the parser and the static analysis.
* `sas_session()` creates a persistent SAS session. Successive
  `sas_run(code, session = s)` calls are submissions to that session: WORK
  data sets, macro variables and macro definitions, user formats, `OPTIONS`,
  titles and filerefs carry over. `sas_work()`, `sas_meta()`,
  `sas_macro_vars()`, `sas_log()` and `sas_listing()` read a session;
  `sas_session_close()` closes its PROC SQL connection. A submission to a
  session that is already running one is refused. The Architecture article
  lists the state that does not carry over between submissions (syntax-check
  mode, PRXPARSE numbering, log line numbers, PROC SQL left open).
* There is no default session: function-layer calls outside `sas_run()` keep
  no hidden state. PRXPARSE ids need `session =`
  (`sas_f_prxparse(re, session = s)`); pattern texts work without one.
* `sas_export_work()` writes the WORK data sets of a result as CSV files
  together with a table of variable attributes.
* The exports are explicit: the user API plus the run-time helpers that
  translated code calls (documented as internal). `sas_format()` and
  `sas_fmt_best()` are internal; use `sas_f_putn()` and `sas_f_putc()`, which
  take the format as text and check it.
* SASHELP members are read from the directory given by
  `options(sasonr.sashelp_dir)` or the environment variable
  `SASONR_SASHELP_DIR` (`<member>.sas7bdat` with its column attributes in
  `<member>_meta.csv`, or a built `.rds`). sasonr ships no SASHELP members;
  without a directory every SASHELP member is missing and fails loud.

## Errors and fail-loud behaviour

* Features that sasonr does not support raise a condition of class
  `sasonr_unsupported` with the SAS line instead of being approximated.
* SAS error model: `sas_run(on_error = c("sas", "abort"))`, default `"sas"`
  (option `sasonr.on_error`). After a SAS ERROR the step is not run and
  syntax-check mode (OBS=0), the documented behaviour of batch jobs, applies
  to the rest of the program: DATA steps create empty data sets, global
  statements and macro code still run, PROC SQL errors set NOEXEC for that
  PROC SQL only, and `SYSERR` and `SYSCC` are set. The result has
  `status = "error"`, `errors` and `error_line`. `sasonr_unsupported` always
  aborts. Calls of names that are not SAS functions give ERROR 68-185;
  unknown SET options are syntax errors.
* In syntax-check mode the TITLE statements of a PROC step and the OPTIONS
  statements of PROC DATASETS are executed; PROC CONTENTS runs (listing, OUT=
  with 0 observations); PROC SORT OUT=, PROC FREQ TABLES OUT= and PROC APPEND
  create a new output data set with 0 observations and 0 variables and keep
  an existing one; PROC MEANS stops with SYSERR 1012; PROC SQL writes the
  NOEXEC NOTEs, sets SQLOBS and SQLRC to 0 and ends with SYSERR 1012; the
  other steps set SYSERR to 3; a DATA step does not replace an existing data
  set. PROC TRANSPOSE, PROC FORMAT, PROC SUMMARY and other combinations fail
  loud.
* An ERROR in a global statement is not handled like a step ERROR: for
  `LINESIZE=` set to a name other than MAX outside a step, sasonr writes
  `ERROR 18-12`, sets SYSCC to 1012 and enters syntax-check mode at the next
  DATA step unless `OPTIONS NOSYNTAXCHECK` comes first. Other ERRORs in
  global statements fail loud.
* The empty data sets left by a failing step are derived from the parsed
  step (types, lengths, LENGTH, RETAIN, ARRAY); when they cannot be derived,
  sasonr fails loud.
* **Strict mode** (`options(sasonr.strict = TRUE)`, the default): a program
  that uses a function, format, informat or procedure statistic whose status
  is `approximate` (the last bits of numeric results may not be exact) or
  `unsupported`, or a function in a context in which it is not supported
  (DATA step, WHERE, PROC SQL, `%SYSFUNC`), stops with a condition of class
  `sasonr_strict_error` (a subclass of `sasonr_unsupported`). With
  `options(sasonr.strict = FALSE)` such features run with a warning of class
  `sasonr_strict_warning`. `PUTN`, `PUTC`, `INPUTN` and `INPUTC` check the
  format they name at run time. See `?sasonr_strict`.
* Feature statuses (`supported`, `partial`, `approximate`, `unsupported`,
  `not_comparable`) are available through `sas_feature_registry()` and
  `sas_proc_stat_registry()`. A `partial` feature runs only inside its
  supported input domain; other inputs fail loud.
* Where behaviour depends on the SAS 9.4 release, sasonr uses the
  release selected with `options(sasonr.sas_version = "SAS 9.4")` (the
  default) or `"SAS 9.4"`; nothing is carried over between releases (see
  `?sasonr_sas_version`).

## DATA step

* Hand-written lexer (strings, comments, name, date and hex literals,
  special missing values, DATALINES blocks, macro tokens), recursive-descent
  statement parser and Pratt expression parser with the SAS operator
  precedence. The AST keeps the program as written.
* DATA steps are compiled to R code at three levels: vectorised `dplyr`
  code (L1), grouped vectorised code for recognised patterns such as BY
  groups with `FIRST.`/`LAST.`, `RETAIN` accumulators, baseline
  carry-forward and unconditional `LAG` (L2), and an explicit row loop that
  follows the DATA step execution rules otherwise (L3). A vectorised step that
  needs row-by-row diagnostics is re-run row by row.
* `_ERROR_` and the data error diagnostics (PDV dump in the log), SAS-style
  log NOTEs for missing values and mathematical operations, and the variable
  attribute model (length, format, informat, label, name case).
* INPUT statement pointers and modifiers, INFILE options, the FILE
  statement and `LINESIZE`.
* An assignment to `SUBSTR()` on the left of `=` is checked as its own
  context of `SUBSTR`; `CONSTANT('EXACTINT', 1E300)` stops the step with the
  SAS exception in a step that runs once and fails loud elsewhere.
* Dates have no 29 February in years divisible by 4000.
* Expressions of the DATA step, WHERE and PROC SQL are parsed with one
  operator table. sasonr treats the following as described here:
  `IN`, `NOT IN` and `IN:` bind tighter than the other comparison operators
  in the DATA step (`a <= b in (0, 5)` is `a <= (b in (0, 5))`); in WHERE,
  `-a ** b` is `(-a) ** b`, `**` binds tighter than `MIN` and `MAX`, and
  `NOT` takes a `MIN`/`MAX` operand; in PROC SQL, `^` (NOT) takes a
  comparison as its operand; colon comparisons (`=:`, `<:` ...) take part in
  comparison chains; `BETWEEN` accepts constant numeric bounds in either
  order; `||`, `LIKE` and `CONTAINS` in WHERE and PROC SQL take character
  operands only (a numeric operand is an error); `**` with invalid arguments
  (`0 ** 0`, a negative base with a fractional exponent) gives a missing value
  with the invalid-argument NOTE, and with constant operands it is folded at
  compile time; WHERE conditions are evaluated without data-error NOTEs.
  These shapes now fail loud: `NOT` as an operand of `+ - * /` in WHERE, a
  PROC SQL comparison chain that starts with a colon comparison, a chain of
  three or more links with a colon comparison, `BETWEEN` with mixed or
  descending character bounds, a function ERROR or a division by zero in a
  WHERE condition, `%'` and `%"` outside `%STR`, `%NRSTR`, `%QUOTE` and
  `%NRQUOTE`, and the remainders of a statement with a missing semicolon
  other than the supported ones.

## Macro language

* The macro processor interleaves with step execution: a step runs as soon
  as it is complete, so values created with `CALL SYMPUT(X)` and PROC SQL
  `INTO:` are available to the macro code that follows.
* Global statements outside a step take effect as soon as they are
  complete, before the macro code after them runs. `OPTIONS LINESIZE=MAX`
  and `NONOTES` inside PROC DATASETS or PROC SQL apply to the macro code after
  them; other in-step settings followed by a `%SYSFUNC` NOTE fail loud. The
  positive and negative forms of an option in one OPTIONS statement apply in
  the order written.
* Text generated by a macro call has the line of the call (NOTE places,
  ERROR lines and fail-loud messages).
* Automatic macro variables: `SYSLAST` and `SYSDSN` follow `_LAST_`,
  `SYSFILRC` is set by FILENAME, and `SYSCC`, `SYSRC` and `SYSFILRC` can be
  assigned with `%let`. Other automatic macro variables fail loud.
* `_LAST_`: PROC SQL CREATE VIEW sets it, INSERT, UPDATE and
  DELETE do not, APPEND sets it to BASE=, and a DATA step stopped by an
  ERROR sets it to its new output data set.
* `%SYSFUNC` NOTEs are wrapped at LINESIZE; single quotes inside `%LET`
  values hide macro triggers.

## PROC SQL

* Own recursive-descent SQL parser; DuckDB runs joins and grouping, and SAS
  semantics are applied in R: missing values, remerging of summary
  statistics with the SAS NOTE, `CALCULATED`, `INTO :macro-variable` and
  `SQLOBS`.
* Computed character columns get fixed lengths (CAT functions
  of column arguments 200, of constant arguments the length of the value).
* The NOTE for an out-of-range function argument (`Invalid argument to
  function MDY. Missing values may be generated.`) is written; other function
  data-error NOTEs in PROC SQL fail loud.
* The summary functions MEAN, CSS, USS, VAR and STD are computed from
  sequential sums in row order; order-dependent statistics on rows whose
  order is not fixed fail loud unless the result does not depend on the
  order.

## Procedures

* PROC SORT, PRINT, MEANS / SUMMARY, FREQ, TRANSPOSE, CONTENTS, DATASETS,
  APPEND, DELETE and FORMAT (VALUE / INVALUE).
* PROC MEANS / SUMMARY: sums in observation order and the moment statistics
  (CSS, USS, VAR, STD, STDERR, CV, T, KURTOSIS), computed with and without
  FREQ and WEIGHT and for the VARDEF= values DF, N, WDF and WEIGHT; T is
  capped at +-99999.99; STD and CV are reported as 0 when the variance is
  negligible against the squared mean; a negative CSS is missing. SKEWNESS,
  PRT and the confidence limits are `approximate`. MAXDEC= uses the `w.d`
  display engine; the printed table chooses its default decimals and splits
  wide tables into panels.
* PROC FREQ: tables and OUT=, the chi-square family, FISHER and EXACT
  FISHER, the 2x2 EXACT CHISQ, BINOMIAL, Cochran-Mantel-Haenszel statistics
  (SCORES=TABLE / RANK), the common odds ratio and relative risks, the
  Breslow-Day test, RISKDIFF and RELRISK (Wald, Agresti-Caffo,
  Hauck-Anderson, Newcombe and exact unconditional limits), TESTP= / TESTF=,
  MISSPRINT and BY groups. Requests outside the supported layouts fail loud.

## Functions, formats and rounding

* About 190 SAS functions as `sas_f_<name>()`, about 100 formats and
  informats, `PUT()` / `INPUT()` and user formats from PROC FORMAT. Missing
  and invalid arguments write the log NOTEs, set `_ERROR_` and give missing
  results; arguments are converted between numeric and character, and the
  number of arguments is checked.
* Every `sas_f_*` function has a reference page with the SAS syntax,
  parameter descriptions, the differences from base R and examples.
* `ROUND` rounds half away from zero with a small relative fuzz and returns
  the double nearest to the decimal multiple; `ROUNDE` and `ROUNDZ` follow
  their documented definitions. Values at the edge of the fuzz window fail
  loud.
* One display engine writes every number that becomes text: `w.d`, `Z`,
  `COMMA`, `DOLLAR`, `NEGPAREN`, `PVALUE`, `PERCENT`, the `BEST` formats,
  E notation, PROC listings and macro values. Values whose formatted text
  sasonr cannot determine exactly (some very wide decimals, long integers
  with decimals, narrow `BEST` widths, some E forms) fail loud.
* `MOD` and `MODZ` compute exact remainders; `YRDIF` supports the 30/360,
  ACT/365 and ACT/360 bases and the AGE and ACT/ACT bases where the result
  is exact; `INTCK` and `INTNX` support the `WEEKDAY`, `TENDAY` and
  `SEMIMONTH` intervals in their plain form; `EXIST()` reports WORK and
  SASHELP members, also in PROC SQL; the `$HEX` informat writes the INPUT
  NOTE and sets `_ERROR_` for invalid data.
