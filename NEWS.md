# sasonr 0.0.0.9000

sasonr is in early development and not on CRAN yet. This section lists what
the development version provides.

## Running SAS programs

* DATA step hash objects: `DECLARE HASH` (also `DCL`) with `dataset:`,
  `ordered:` and `hashexp:`, the methods DEFINEKEY, DEFINEDATA, DEFINEDONE,
  ADD, FIND, CHECK, REPLACE and REMOVE (also with `key:` and `data:`
  arguments) and the
  NUM_ITEMS attribute, in the value of an assignment, in an IF condition or
  as a statement. FIND copies the data variables into the PDV; a key that is
  not there returns 160038, ADD of a key that is there 160036. OUTPUT,
  iterators (DECLARE HITER), SUMINC:, REPLACE or REMOVE of a key that is not
  there, a failing method call statement and a method call in an expression
  that also reads variables fail loud.
* The row-by-row (L3) code of a DATA step shows the step's statements: the
  per-step state (the PDV prototype, reset values, OUTPUT buffers, error
  dump) is one object made by `sas_step_state()`, each SET or MERGE
  statement reads with `pdv <- sas_set_next(.r, pdv)` or `sas_merge_next()`,
  an iteration runs in `sas_iter_run({ ... })` and ends with
  `if (sas_iter_done(.st, .status)) break`. DO loops with constant bounds are
  `while` loops, DO UNTIL without CONTINUE tests after the body, an empty
  OTHERWISE writes no empty `else`, and `_ERROR_` is synced once per
  iteration when the step neither reads it nor writes lines of its own.
* Each SET and MERGE statement of a DATA step reads with its own reader:
  several SET statements (the step stops at the first end of file), SET or
  MERGE in an IF branch, `if 0 then set ds;` (the variables without a read)
  and DOW loops (`do until (last.id); set ds; by id; ... end;`, also DO
  WHILE). A BY statement belongs to the SET or MERGE statement before it,
  also inside a DO loop; FIRST. and LAST. variables start at 1. A SET with BY
  reads one observation ahead, which the observations-read NOTE counts; a
  SET that never ran writes no such NOTE. BY statements in IF or SELECT
  blocks, BY statements with different variables in one step, several MERGE
  statements and a WHERE statement with several readers fail loud.
* The line a macro generates is echoed under its NOTE without the leading
  blanks of the macro text, also when the macro generates a whole step.
* INPUT: invalid data on a line of a record that the error lines do not show
  adds the two NOTEs about errors outside the printed range.

* The R code of `sas_translate()` and `sas_script()` (also the tidy form) is
  laid out like the air formatter writes it: an expression that fits in 80
  columns stays on one line; a longer call puts one argument per line, a
  pipeline one step per line; a `mutate()` of several columns writes one
  column per line. The code that runs is the code shown.
* PROC UNIVARIATE writes its printed output (without NOPRINT): the Moments,
  Basic Statistical Measures, Tests for Location, Quantiles and Extreme
  Observations tables of each analysis variable, with the mode Note and the
  Missing Values table (special missing values included), the label of a
  variable after its name, also under OPTIONS CENTER. BY, CLASS, WEIGHT and
  OUTPUT with printed output, VARDEF= and PCTLDEF=, a quantile column that
  needs the E form, skewness or kurtosis whose last digits depend on the
  order of the sums fail loud.

* A session now starts at OPTIONS CENTER, the SAS default (it started at
  NOCENTER): each title line, procedure title and table heading is centred on
  its own, each table as a whole, and BY lines are written between dashes.
  This covers PROC PRINT (with its N line), PROC MEANS, PROC FREQ with its
  statistics tables, PROC TABULATE, PROC CONTENTS (also the CONTENTS
  statement of PROC DATASETS) and PROC SQL. The option
  `sasonr.center = FALSE` (or the environment variable `SASONR_CENTER=FALSE`)
  starts sessions at NOCENTER. `sas_html()` centres the titles and the tables
  under CENTER and writes them on the left under NOCENTER (it used to centre
  the titles only).
* PROC PRINT column positions: a formatted number's field is the format
  width and its heading is right-aligned; an unformatted number (and Obs) is
  centred rounding up, a character column rounding down; an unformatted
  column whose values are rounded to two decimals because of their magnitude
  is laid out like a formatted one; the heading of a character column whose
  values are of uneven widths starts in the first position (columns up to 16
  wide; wider ones fail loud); a number under a user-defined format is laid
  out as text; character values are written without their leading blanks; a
  formatted number keeps the blanks its format writes after it (PERCENT.);
  a missing value of an unformatted number sits in the decimal
  point column; headings of several lines follow the same rules. Before,
  headings and fields could sit a position or more to the left or right of
  their place.
* PROC FREQ: in a crosstabulation the column variable heading starts at
  least five blanks after the row variable heading; the values of a number
  under a user-defined format are left-aligned in a one-way table (the
  heading stays right-aligned); the "Frequency Missing" line is centred under
  the table rounding up.
* PROC CONTENTS: the Len column widens for lengths of more than three
  digits; a variable table wider than LINESIZE (long labels) fails loud.
* PROC SQL: a column label with blanks wraps word by word at the width of the
  column or of its longest word (it was cut in two lines only); a report
  wider than LINESIZE fails loud (it was written wider than the line, where
  the columns are narrowed).
* PROC MEANS: the "Analysis Variable" heading is centred over its table (it
  started in the first position).
* PROC FREQ statistics tables are laid out column for column: the CMH
  summary, the common odds ratio and relative risks, the Breslow-Day test,
  the risk estimates, the odds ratio and relative risks, the confidence
  limits of RELRISK / RISKDIFF CL= and the exact chi-square tests had their
  columns separated by single blanks; their headings, the "Chi-Square Test"
  heading of a one-way table and the binomial headings are centred over their
  tables, and the "Test" heading of TESTP= / TESTF= sits over its column.
* PROC CONTENTS: the headings "Variables in Creation Order", "Alphabetic List
  of Variables and Attributes" and "Sort Information" are centred over their
  table (they started in the first position).
* PROC CONTENTS SHORT no longer writes the data set attributes (only the list
  of variables).
* PROC CONTENTS: the second column of the data set attributes is 57
  characters wide (it was 56, so the third column started one position early).

* PROC PRINT headings from variable names: a name without a
  lowercase-uppercase change whose piece is still longer than the column
  splits again at an underscore (`abcd_efgh_ijkl` on three lines; it was
  written on two). A name with one lowercase-uppercase change splits there
  only when it is longer than its values and than 4 characters (`AbCd` was
  split); a name with several such changes that is wider than its values
  now fails loud (it was split at one place).
* ODS RTF: a PROC PRINT table wider than the page is split into column
  groups, each a table of its own; the groups are balanced (each takes
  columns up to the average of the page width and the table width per
  group), and a column is at most half the page wide. The Obs column or a
  single ID variable starts every group, each group ends with its own N line,
  and each BY group is split on its own. A split table with several ID
  variables or sum rows still fails loud.
* An ARRAY of existing character variables now keeps the length a LENGTH
  statement gave them: storing through an element reference (`c{j} = ...`)
  no longer cuts the value to 8 characters. An array of character variables
  of different lengths fails loud.
* The Dw.p format: the value is rounded to a number of decimals that depends
  on the width, p (3 when omitted), the sign and the magnitude of the value,
  so that values of one magnitude line up on the decimal point. Widths 6 to
  14 and p = 1, 2, 3, 4 or 6 are supported (a value that needs the E form is
  written as BESTw. writes it); other widths and p values fail loud. Until the format registry lists
  it, the format runs with `options(sasonr.strict = FALSE)` only.

* PROC PRINT LABEL: a label of three to five words that is wider than the
  column is written on at most three lines, at the narrowest width that
  allows it (a longer label still fails loud). A variable name with an
  underscore and a lowercase-uppercase change that is wider than its values
  (such as an AUTONAME name) fails loud: SAS splits it, at places not yet
  measured.
* PROC SQL reads `DICTIONARY.COLUMNS` and `DICTIONARY.TABLES` (and the
  views `SASHELP.VCOLUMN` / `SASHELP.VTABLE`) for the WORK members, data
  sets and views: a query must restrict `libname = 'WORK'`, and the TABLES
  columns that depend on the host (dates, sizes, pages, encoding ...) fail
  loud. Other DICTIONARY tables fail loud.
* The `LABEL=` data set option on a DATA step output data set sets its
  label (PROC CONTENTS, DICTIONARY.TABLES).
* PROC PRINT of a table wider than LINESIZE fails loud (sasonr wrote the
  lines longer than LINESIZE; SAS stacks the headings or splits the columns).
* A word or a number that starts no statement in open code (`foo;`, `1;`),
  and an unresolved macro call at the start of an open-code statement that
  ends with `;` (`%nosuch;`, `%nosuch(1, 2);`, with its WARNING), write
  ERROR 180-322 and start syntax-check mode at the next DATA step (none
  under NOSYNTAXCHECK); the program may also end there. They failed loud.
* PROC UNIVARIATE: `NOPRINT` with `OUTPUT OUT=` for one or several analysis
  variables, with BY and one CLASS variable, `VARDEF=`, `PCTLDEF=`,
  `PCTLPTS=` with `PCTLPRE=` and `PCTLNAME=`, and
  34 statistics (N NMISS NOBS MEAN SUM STD VAR SKEWNESS KURTOSIS SUMWGT MAX
  MIN RANGE the quartiles and P1-P99 QRANGE MODE CSS USS CV STDMEAN T PROBT
  MSIGN PROBM SIGNRANK PROBS). The OUT= columns follow the measured order
  (by statistic, then variable), carry SAS's labels, and the data set has
  type OUT and the label "Selected Summary Statistics" (with the percentile
  definition when a percentile is requested); the step writes no
  observations-read NOTE. WEIGHT gives the weighted N, SUM, MEAN, VAR, STD,
  CSS and USS (other statistics with WEIGHT fail loud). Printed output,
  FREQ, ID, PCTLPTS= with TO / BY or WEIGHT, the
  robust and normality statistics, and a signed rank test of more than 20
  nonzero differences fail loud.
* PROC FREQ: a TABLES OUT= data set has the label "Frequency Counts and
  Percentages".
* PROC TABULATE: numbered variable lists (`v1-v30`) in the VAR and CLASS
  statements, and as the whole content of parentheses in a TABLE dimension
  (`(v1-v30)*sum`); other variable list forms still fail loud. A table without a row dimension
  that is wider than the line is split into panels in the listing, as one
  with a row dimension.
* PROC FREQ crosstable headings: a row variable heading longer than the
  row label column stays on one line with the column heading (five blanks
  between them) when both fit the table width, else the column heading goes
  to the next line; a row value longer than 16 characters is written in
  pieces of 16 on the statistic lines (sasonr put the column heading on the
  next line and widened the column).
* PROC SQL `CREATE TABLE` records sort information that PROC CONTENTS
  shows: `ORDER BY` of result columns (by name, alias or position, with
  `DESC`), `GROUP BY` of one result column and `SELECT DISTINCT` of one
  column (with the sort option NODUPKEY when the rows are unique by it), and
  `UNION` of one column ordered by it. Query shapes whose sort information
  is not measured make PROC CONTENTS of the table fail loud. PROC CONTENTS
  also writes `Character Set ASCII` (it wrote ANSI), the `Sort Option
  NODUPKEY` line, and the names after `DESCENDING` as stored.
* PROC FREQ: `RELRISK(CL=)` together with `RISKDIFF(CL=)` on a 2x2 table
  (CL= alone in both) writes both confidence-limit tables, in the listing
  and in ODS HTML.
* The WHERE line of the "observations read" NOTE longer than LINESIZE
  breaks before the word that does not fit, every line but the
  last at most LINESIZE - 1 columns, continuation lines indented by 6, a
  word too long for a line cut at LINESIZE - 6 (sasonr wrote it on one
  line). A PUT of one value wider than LINESIZE writes it cut every LINESIZE
  characters; a wider value after other items fails loud.
* `sas_script()`: a step on whose lines the macro processor wrote a WARNING
  (an unresolved `&name`) is kept as SAS text in the trace, so running the
  script writes the WARNING again.
* `CALL EXECUTE` in a DATA step: the text is queued and runs after the
  step, with "NOTE: CALL EXECUTE generated line." and the `N + text` echo
  of each generated step. Text made of DATA and PROC steps that end with
  `RUN;` or `QUIT;` is supported. Macro code in the argument runs at the
  call (`%LET`, `%PUT` between the step's PUT lines, macro calls); the text
  it generates is queued, and macro code that generates none writes "NOTE:
  CALL EXECUTE routine executed successfully, but no SAS statements were
  generated.". An entry without text keeps its number. A macro call kept
  by `%NRSTR` runs when its entry runs (one echo line for the entry). An echo longer than LINESIZE breaks at the blank before the
  word that does not fit, the lines after it starting in column 1; a word
  longer than LINESIZE starts a new line and is cut. Other
  macro quoting, a numeric argument, an ERROR in an
  entry with more than one step fail loud; an ERROR in a one-step entry
  stops that step and the queue goes on. `sas_translate()` keeps such a program as SAS text from the
  step with `CALL EXECUTE` on.
* `options(sasonr.start_time = <POSIXct>)` sets the session start time that
  the automatic macro variables SYSDATE, SYSDATE9, SYSDAY and SYSTIME
  report, so a program that branches on `&SYSDAY` gives the same result on
  any day.
* A DATA step whose statement is ignored after a syntax ERROR (ERROR
  76-322, e.g. `PUT()` without a format: ERROR 388-185) compiles on: the
  ignored assignment leaves its target numeric, the type conversion NOTE of
  the later statements is written, and a later character format on a
  numeric variable writes its WARNING and ERROR 48-59 at its own line. In
  PUT, a character format on a numeric variable writes the WARNING; `$w.`
  writes the value with `w.`, `$QUOTE.` and `$CHAR.` stop the step with
  ERROR 48-59.
* `$QUOTEw.` with a width too small for the text cuts it at the width: a
  doubled quote that the cut would split is dropped, and `$QUOTE1.` writes a
  blank (an Unsupported error before).
* `ODS RTF FILE="path";` and `ODS RTF CLOSE;` write the RTF document of
  the PROC PRINT and PROC TABULATE tables (default style) when the
  destination closes. PROC TABULATE: merged row titles and headings, class heading rows
  anywhere in the row dimension, the
  page text of a page dimension (one section per page), the same width rule
  as PROC PRINT, a column heading over several columns setting a minimum
  width for each of them. A table wider than 9322 twips (it may be split
  into column groups) and characters whose advance is not measured are an
  Unsupported error. PROC PRINT: the
  table rows, borders, heading and row-header styles, BY lines, SPLIT= line
  breaks and the column widths (from the measured character advances of the
  default fonts); the N line sets a minimum width for each column it spans
  (also with BY). Other procedures, options of the ODS RTF statement, titles
  in the document, and a destination left open at the end of the program are an Unsupported
  error.
* PROC PRINT supports `OBS=`, `N`, `LABEL`, `SPLIT=` and the ID, BY, SUM and
  SUMBY statements: one page per BY group with its byline, observation
  numbers that continue across pages, subtotals per BY group and the grand
  total. Each value field is as wide as
  its widest entry and centred under the heading. Several BY variables with
  SUM, SUMBY other than the BY variable, N together with SUM, a label of more
  than two words without `SPLIT=`, an ID variable that is also a BY variable,
  BY on unsorted data, VAR options and `DOUBLE` are an Unsupported error
  (`DOUBLE` was accepted and ignored before).
* The ODS table class is now `sasonr_ods_table`, like the other classes of
  the package.
* PROC TABULATE supports the BY statement (each BY group on its own page
  under the titles and its BY line, percentages per group, `#BYVAL(var)` in a
  title) and the percentages COLPCTN, ROWPCTN, REPPCTN, PAGEPCTN, COLPCTSUM,
  ROWPCTSUM and `PCTN<var>` / `PCTSUM<var>` with class variables as the
  denominator. A table of several panels can now have a page dimension.
* PROC TABULATE `CLASSDATA=` (with or without `EXCLUSIVE`), the CLASS option
  `PRELOADFMT` with the TABLE option `PRINTMISS`, and `PRINTMISS` when every
  combination of levels occurs: the levels of the CLASSDATA= data set and of
  the format ranges join the levels of the data in internal order; a level
  without observations has N 0. These options with more than one CLASS
  variable, with BY or OUT=, with ORDER=, DESCENDING or MISSING, or with a
  format range that is not one value are an Unsupported error.
* PROC TABULATE `OUT=` writes the same data set as `ODS OUTPUT Table=` (one
  column builder for both); OUT= with BY, with data set options or with an
  ODS OUTPUT request in the same step is an Unsupported error.
* PROC TABULATE `ORDER=FREQ` puts tied levels in data order (the order of
  their first observation); ties under `ORDER=FREQ` with `DESCENDING` are an
  Unsupported error.
* `sas_means_df()`, `sas_tabulate_df()` and `sas_transpose_df()` run one
  procedure on a data frame without a program and return its ODS table
  (or the output data set of PROC TRANSPOSE). They call the computation that
  `sas_run()` calls for the same step, with the specification from the same
  parser and builder, so the values are the same. `sas_log()` and
  `sas_html()` are S3 generics with methods for these results;
  `as.data.frame()` drops the table attributes.
* `%SYSFUNC` accepts BAND, BYTE, CONSTANT, COUNT, COUNTW, PUTC, RANK,
  REPEAT, REVERSE, and the clock functions DATETIME, TIME and TODAY, now
  supported in the macro context (function registry); DATETIME and TIME are
  zero-argument calls there like TODAY.
* PROC FREQ BINOMIAL(EXACT) gives the exact (Clopper-Pearson) confidence
  limits, as CL=EXACT; before, it ran the exact binomial test (which is the
  EXACT BINOMIAL statement) and kept the Wald limits. BINOMIAL(CORRECT)
  applies the continuity correction to the Wald limits and to the test
  also without CL= (before, it was ignored there), with the footnote of the
  listing.
* PROC FREQ CHISQ together with BINOMIAL for a one-way table writes the
  chi-square test, then the binomial block, then the sample size once,
  whatever the order of the options; before, the chi-square test was left
  out.
* PROC FREQ TABLES / NOPRINT suppresses only the frequency and
  crosstabulation tables: the statistics requested (CHISQ, FISHER, the
  multiway CMH summary, BINOMIAL) are written; before, the request wrote
  nothing. A step whose requests are all NOPRINT without statistics or
  OUT= writes the NOTE that processing terminates and reads no data.
* PROC FREQ runs on the same PROC pipeline: its one-way and two-way tables
  are ODS tables (OneWayFreqs, CrossTabFreqs with one row per cell and per
  total, in `$ods`), so are the CHISQ and FISHER results of a two-way table
  (ChiSq, FishersExact), RELRISK (RelativeRisks, RelRiskCL), RISKDIFF
  (RiskDiffCol1, RiskDiffCol2, PdiffCLs), BINOMIAL (Binomial, BinomialCLs,
  BinomialTest), the one-way chi-square test (OneWayChiSq), NLEVELS (NLevels)
  and the multiway CMH summary (CMH, CommonRelRisks, BreslowDayTest), and `sas_freq_df()` returns them for a data frame.
  The stratum crosstabulations of a multiway request and the LIST table
  are ODS tables too (CrossTabFreqs, List).
* `sas_html()` draws the PROC FREQ one-way tables, crosstabulations (with
  the cell-statistics legend), the chi-square table, Fisher's exact test
  (2x2), the column risk estimates of RISKDIFF, the odds ratio and relative
  risks of RELRISK, the confidence limits of RELRISK(CL=) and RISKDIFF(CL=), the BINOMIAL tables, the one-way chi-square test,
  NLEVELS, the stratum crosstabulations of a multiway request and the
  statistics of each stratum, the LIST table and the multiway CMH summary,
  as in the HTML destination. It is an Unsupported error, naming the procedure and its SAS
  line, when the HTML destination received output that has no HTML view
  yet (PROC PRINT, the other PROC FREQ tables, n-way and LIST tables);
  before, that output was left out of the document.
* The trace of a run (`sas_script()` of a `sas_run()` result) keeps what a
  macro invocation writes with %PUT outside its steps, with the new verb
  `sas_macro_put()`; before, those lines were missing from the script's log.
* The PROC verbs take `line` as one line per statement (the PROC statement
  first), and `sas_script()` passes it when statements share a line, so the
  messages that name a line inside the step name the program's line.
* A translation without a run (`sas_translate()`, `sas_script()`) applies
  the KEEP=, DROP= and RENAME= options of a PROC MEANS OUTPUT data set to
  the variables it gives the following steps; before, a later step could
  select `_TYPE_` and `_FREQ_` that KEEP= had removed.
* The `lang` of a `sas_script()` DATA step node runs a step whose
  vectorised code falls back to row-by-row execution; before, it failed
  with "bad value" (the generated `function` call lacked its srcref element).
* `sas_proc_print()` and `sas_proc_tabulate()` are step verbs too, and
  `sas_script()` writes PROC PRINT and PROC TABULATE steps with them.
* `sas_proc_freq()` and `sas_proc_transpose()` are step verbs like
  `sas_proc_sort()` and `sas_proc_means()`: they take a session, the options
  of the PROC statement and one argument per statement, and `sas_script()`
  writes PROC FREQ and PROC TRANSPOSE steps with them. The run-time helpers
  that had these names until now are internal.
* `ODS OUTPUT Table=` (or the path `Tabulate.Report.Table=`) before a PROC
  TABULATE step writes the table as a data set: the CLASS variables, then
  `_TYPE_`, `_PAGE_` and `_TABLE_`, then one column per analysis variable
  and statistic (`v_N`, `v_Mean`, `v_Sum`, `v_PctN_00_v`; `N` without an
  analysis variable), one observation per class-level combination. N, MEAN,
  SUM and PCTN with one TABLE statement are supported; other statistics,
  `F=`, labels in the TABLE statement, KEYLABEL, a LABEL statement, CLASS
  options and an ODS OUTPUT statement inside the step are an Unsupported
  error.
* PROC TABULATE tables reach the HTML document of `sas_html()` while the
  HTML destination is open: heading rows and the corner cell in `<thead>`,
  row titles spanning their sub-rows, N and NMISS without decimals, one table
  per page of the page dimension under its page text. A heading or MISSTEXT
  longer than its column, a `BOX=` text in the corner and `F=` on an N or
  NMISS cell are an Unsupported error of `sas_html()`; the listing is still
  written.
* PROC TABULATE prints its tables to the listing: one, two and three
  dimensions with crossing, concatenation, `ALL`, labels, `F=`, `BOX=` and
  `MISSTEXT=`, the row-title area of LINESIZE/4, wrapped headings, column
  panels with `(Continued)` and one table per page of the page dimension.
  The table objects are kept in the result's `ods` list. `CLASS / MISSING`
  applies to the variables of its own CLASS statement. Layouts outside these
  rules (and, in strict mode, the statistics not yet in the PROC statistics
  registry) are an Unsupported error.
* In-stream data start on the line after the `DATALINES` (`CARDS`, `LINES`,
  `DATALINES4`, ...) statement. Blanks and `/* */` comments may follow the
  statement on its line; any other text there is now an Unsupported error.
  Before, that text and the rest of the program were taken as data and the
  steps after it were silently skipped.
* Readability passes for the code of vectorised DATA steps (option
  `sasonr.passes`, see `?sas_translate`): a retained numeric variable set only
  at `FIRST.` of a BY group from a column is written as one grouped value per
  BY group (`group_by()`, `dplyr::first()`, `ungroup()`) instead of the
  carry-forward helper, and `FIRST.`/`LAST.` columns that nothing reads are no
  longer created. `sas_run()` and `sas_script()` use the same code; with
  `options(sasonr.passes = character())` the code is unchanged.
* The `level`, `on_error` and `scope` arguments of `sas_run()`,
  `sas_translate()` and `sas_macro_vars()` take their exact values only
  (`rlang::arg_match()`); an abbreviation such as `level = "L"` is an error.
* Internals: the code generator builds its R calls with rlang
  (`call2()`, `expr()`, `sym()`, `exec()`), package-held numbers and lists go
  through vctrs (`vec_cast()`, `list_unchop()`, `vec_rbind()`), and row-wise
  function calls use purrr, a new dependency.
* `sas_run()` runs a SAS program given as text and returns the log, the
  listing and the WORK data sets; `sas_translate()` shows the R code that is
  generated for each step. `sas_lex()`, `sas_parse()`, `sas_parse_expr()` and
  `sas_analyze()` expose the lexer, the parser and the static analysis.
* `sas_script()` writes the translation of a program (`sas_translate()`) or
  the trace of a run (`sas_run()`) as an R script: one step verb per SAS step
  (`sas_data_step()` with the generated code of the step, `sas_proc_sort()`
  and `sas_proc_means()` with the options the program gave, `sas_proc_sql()`,
  `sas_proc()`, `sas_options()`, `sas_title()`, `sas_global()`), each running
  its step in a session; macro code whose result is known only at run time is
  kept as SAS text in `sas_deferred()`. `tidy = TRUE` writes vectorised DATA
  steps as dplyr pipes on R variables (with `library(tidylog)`;
  see `sasonr_tidy_log()`) and keeps a session only for the other steps.
  `sas_work(trim = FALSE)` and `sas_session_add()` move data sets between R
  and a session. The internal PROC MEANS helper is now `sas_rt_proc_means()`.
* `sas_check()` lists every unsupported feature of a program before anything
  runs (step, line, feature), where `sas_run()` stops at the first one. A step
  that uses a macro variable set only at run time (CALL SYMPUT, `INTO :`) is
  reported as checked when the program runs.
* `sas_session()` creates a persistent session. Successive
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
  take the format as text and check it. The PROC SQL helpers `sas_f_cvt_*()`
  and `sas_f_sql*()` are internal exports as well, and `sas_titles_once()` and
  `sas_untranslated()` are new internal exports.
* Translated code calls only base R, package-prefixed functions and the
  exported helpers. The code of PROC SQL and PROC DATASETS steps, which
  sasonr runs itself, is a call of `sas_untranslated()` that stops with
  `sasonr_unsupported`; the `sas_translate()` page describes the environment
  in which the code of a step is evaluated.
* SASHELP members are read from the directory given by
  `options(sasonr.sashelp_dir)` or the environment variable
  `SASONR_SASHELP_DIR` (`<member>.sas7bdat` with its column attributes in
  `<member>_meta.csv`, or a built `.rds`). sasonr ships no SASHELP members;
  without a directory every SASHELP member is missing and fails loud.

## The log

* `$log` of a result shows the program lines with their
  numbers (long lines continued on `!` lines at `LINESIZE`), each step's
  messages after its lines, compile-time ERRORs after their line with an
  underscore marker and the error number. `OPTIONS SOURCE` and `NOSOURCE`
  turn the echo on and off.
* `$log` of a result and `sas_log()` now carry the source echo (before:
  the messages and PUT output only, which `sas_log(x, echo = FALSE)` gives).
  The first argument of `sas_log()` is now `x` (a result or a session);
  `sas_log(session = s)` still works.
* `sas_log(x, format = c("plain", "ansi", "html"), echo = TRUE)` gives the
  log of a result or a session as plain text, as text coloured for the
  terminal (ERROR red, WARNING green, NOTE blue, with a summary line), or as
  HTML with CSS classes (`sas-note`, `sas-warning`, `sas-error`, `sas-put`,
  `sas-src`; style sheet in `system.file("css", "sasonr.css", package =
  "sasonr")`). `echo = FALSE` gives the messages and PUT output only.
* `print()` of a result shows the coloured view in a terminal with colours;
  `options(sasonr.log_style = "plain")` turns it off.
* `$log_events` of a result is a tibble of the log events (kind, message id,
  step, place, text, program line) behind every view.
* A numeric variable as an operand of `||` in a DATA step now gives the NOTE
  "Numeric values have been converted to character values" with the place of
  the variable. A numeric constant or expression as an operand of `||` fails
  loud (unsupported).
* A DATA step with a SET, MERGE or INPUT statement now stops at the first
  iteration that ends without executing one, whatever the condition that
  skipped the read: that iteration completes (its OUTPUT statements run, and
  DELETE or a subsetting IF drops its observation), then the log shows
  "NOTE: DATA STEP stopped due to looping." (before the observations-read
  NOTEs). A STOP statement in that iteration ends the step without the NOTE.
  A reading statement in a DO loop or a SELECT group, or a RETURN
  statement in the step, fails loud when such an iteration happens.

## Errors and fail-loud behaviour

* Features that sasonr does not support raise a condition of class
  `sasonr_unsupported` with the SAS line instead of being approximated.
* Error model: `sas_run(on_error = c("sas", "abort"))`, default `"sas"`
  (option `sasonr.on_error`). After an ERROR in the program the step is not run and
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
  set. PROC TRANSPOSE with DATA=, a new OUT= data set and a VAR statement runs
  on its input with 0 observations. Other forms of PROC TRANSPOSE, PROC
  FORMAT, PROC SUMMARY and other combinations fail loud.
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
  `options(sasonr.strict = FALSE)` such features run: the log of the
  submission ends with one NOTE that lists the approximate features used,
  and each feature signals one warning of class `sasonr_strict_warning` per
  submission. Arguments
  outside the domain in which a function's results are exact go through the
  same gate. `PUTN`, `PUTC`, `INPUTN` and `INPUTC` check the format they name
  at run time. See `?sasonr_strict`.
* Feature statuses (`supported`, `partial`, `approximate`, `unsupported`,
  `not_comparable`) are available through `sas_feature_registry()` and
  `sas_proc_stat_registry()`.
* sasonr targets SAS 9.4 and has one behaviour.

## DATA step

* A numeric arithmetic expression as an operand of `||` is converted to
  character text, the conversion NOTE giving the place of its operator (a
  prefix `-`, `+` or NOT at that operator). The places of one line are listed
  in column order. A constant, function call or comparison operand still
  fails loud.

* Lengths and other integer tokens: ATTRIB LENGTH= no longer reads `$8.5`
  as 85 (a fractional length is truncated, `$8.5` is 8); a numeric length
  is range-checked on the value as written (`8.5` is an error, `3.5` is 3),
  also in LENGTH. A fractional ARRAY dimension or initial-value repetition,
  INPUT end column, INPUT `#n` pointer and FILE LRECL= is a syntax error. A
  PUT item wider than the FILE LRECL fails loud.
* `VLENGTH()` of a numeric variable returns its declared length (3 to 8).
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
* The declarative statements of a DATA step (WHERE, KEEP, DROP, RENAME, BY,
  the attribute statements and the output list) are collected into slots of
  an intermediate representation, and the executable statements are lowered
  to a control-flow graph. Data-flow facts on that graph decide the step
  level. The generated R code is built as R calls and evaluated directly;
  `sas_translate()` shows the deparsed text.
* Data set options (KEEP=, DROP=, RENAME=, WHERE=, IN=, FIRSTOBS=, OBS=)
  of DATA step input and output data sets, PROC DATA=, PROC OUT= and PROC
  SQL tables go through one component. An invalid option on an input data
  set is an ERROR and stops the step; an unknown variable in KEEP=, DROP= or
  RENAME= of an output data set gives a WARNING and the step completes (an
  unknown KEEP= variable leaves a data set without variables, an unknown
  RENAME= pair is ignored). IN= on PROC PRINT DATA= is ERROR 22-7. WHERE= on
  an output data set is not implemented yet and fails loud.
* A BY statement inside a DO, IF or SELECT block was ignored; it now fails
  loud.
* LEAVE inside a SELECT group leaves the SELECT, not the enclosing DO loop.
  An iterative DO evaluates its start, TO and BY values before it assigns the
  start to the index, so `do i = 1 to i;` uses the value of `i` before the
  loop.
* An assignment to `SUBSTR()` on the left of `=` is checked as its own
  context of `SUBSTR`; `CONSTANT('EXACTINT', 1E300)` stops the step with the
  step exception in a step that runs once and fails loud elsewhere.
* Dates have no 29 February in years divisible by 4000.
* Log messages that describe sasonr's own handling name sasonr as the
  subject: "NOTE: sasonr stopped processing this step because of errors.",
  "NOTE: Due to ERROR(s) above, sasonr set option OBS=0, enabling syntax
  check mode." and "NOTE: sasonr went to a new line when INPUT statement
  reached past the end of a line."
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

* A numbered ERROR in text a macro generated writes `NOTE: Line generated by
  the invoked macro "NAME".` (the innermost macro) and the generated line with
  its marker; one on a line of an included file writes that line as
  `n         +text` with its marker. An invalid statement (ERROR 180-322) in a
  DATA step adds no variables to the data set the stopped step leaves.

* `%INCLUDE 'path';` alone on its line reads the file as if its lines
  followed the statement, also in a step and nested: they take the next line
  numbers and are not echoed (NOSOURCE2). A %INCLUDE in a program with macro
  definitions or open-code %IF/%DO, with a fileref or options, of a missing
  file, or under OPTIONS SOURCE2 fails loud.

* OPTIONS MPRINT writes the text each macro call generates, up to each
  semicolon and after the last one, as `MPRINT(NAME):   text` (NAME: the macro
  whose body wrote it), with its blanks and line breaks collapsed to one blank
  and wrapped at LINESIZE. Several blanks or a line break inside a quoted
  string fail loud.

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

* OUTOBS=, INOBS= and LOOPS= (also in RESET) take an integer constant: `2.5`,
  `-1` or a name give ERROR 22-322 and the rest of the step is not executed.
  A GROUP BY or ORDER BY column number that is not a positive integer gives
  "ERROR: GROUP BY clause requires a positive column number." (ORDER BY
  alike); `ORDER BY -1` no longer leaves the rows unordered.
* INSERT ... SET: a column missing from a SET clause is missing in that row
  (`.` or blank), and a value of the wrong type in any SET clause gives
  "ERROR: It is invalid to assign a numeric expression to a character value
  using the SET clause." (and the character-to-numeric message) with no row
  inserted. A PROC SQL step stopped by an ERROR sets SYSERR to 1012.
* PROC SQL expressions are evaluated as R calls, without a text round trip.
* Own recursive-descent SQL parser; DuckDB runs joins and grouping, and SAS
  semantics are applied in R: missing values, remerging of summary
  statistics with the remerge NOTE, `CALCULATED`, `INTO :macro-variable` and
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

* PROC MEANS builds its output as ODS tables (the `Summary` table, with the
  BY groups, the titles and the column headers); the listing and the HTML
  destination render the same table. `sas_html(x, file, log = FALSE)` writes
  the procedure output of a result as an HTML document (style sheet
  `system.file("css", "sasonr.css", package = "sasonr")`). The HTML
  destination is open when a program starts; `ODS HTML CLOSE;` and
  `ODS HTML;` close and reopen it. `ODS HTML FILE=` and the other ODS HTML
  options fail loud.
* `ODS OUTPUT Summary=<data set>;` (or `Means.Summary=`) writes the MEANS
  Summary table to WORK, with its data set NOTEs. MATCH_ALL, PERSIST=,
  BY groups and the ODS actions (CLOSE, SELECT, EXCLUDE, ...) fail loud.
* Options that take an integer constant fail with ERROR 22-322 for a
  fractional value instead of truncating it: PROC MEANS MAXDEC=, FW=,
  QNTLDEF= / PCTLDEF=; PROC FREQ BINOMIAL LEVEL=, RELRISK COLUMN=. PROC FREQ
  BINOMIAL(ALPHA=) is an ERROR (ALPHA= is not a BINOMIAL suboption), TABLES
  ALPHA= and BINOMIAL P= need a number, and PROC FORMAT DEFAULT= needs a
  number. BINOMIAL P= above 1 is a percentage (`P=1.5` tests 0.015). PROC
  MEANS MAXDEC=-1, an ALPHA= outside (0, 1) and a negative WAYS value give
  their WARNING and the procedure runs.
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

* `UPCASE`, `LOWCASE` and `PROPCASE` map letters outside ASCII with the case
  mapping of a UTF-8 session (before: they failed loud). They still
  fail loud in an R session that is not UTF-8, for invalid UTF-8 bytes and for
  a letter whose other case has a different number of bytes. The `PROPCASE`
  page lists which characters start a word.
* In a UTF-8 session the length of a character variable counts bytes: a
  value is cut to its first `n` bytes and padded with blanks to `n` bytes,
  and `LENGTH`, `LENGTHN` and `LENGTHC` count bytes. A cut inside a
  multi-byte character fails loud. New `KLENGTH` counts characters (0 for a
  blank argument). Lengths taken from values (literals, R data frames,
  PROC SQL, PROC TRANSPOSE) count bytes too. Every other function, `PUT` and
  `INPUT` with a character argument that has a byte outside ASCII fail loud
  (`sasonr_strict_error`), as do CALL SYMPUT, SYMPUTX and SORTC; an INPUT
  statement reading such records is not supported.
* In a UTF-8 session `SUBSTR` counts positions and lengths in bytes; a start
  or end inside a multi-byte character fails loud, as do SUBSTR on the left
  of `=`, `%SYSFUNC(SUBSTR())` and PROC SQL calls with a value that has bytes
  outside ASCII. New `KSUBSTR` counts characters; for ASCII values it is
  `SUBSTR`, a value with bytes outside ASCII fails loud.
* `ANY*` and `NOT*` take the class of a character outside ASCII from a
  table of measured classes: every code point from U+00A0 to U+00FF, plus
  U+0100, U+0152, U+0391, U+4E2D and U+20AC. Among them, the multiplication
  and division signs are neither letters nor punctuation, the micro sign is a
  lower case letter, the ordinal indicators and U+4E2D are letters that are
  neither upper nor lower case, the no-break space is a space and the soft
  hyphen is not printable. U+1D11E is known to be neither a letter nor
  punctuation; its other classes and all other characters outside ASCII fail
  loud. New `ANYPRINT` and `NOTPRINT` find the first printable character
  (blank to tilde in ASCII) or the first character that is not printable.
* In a UTF-8 session more character functions accept values with bytes
  outside ASCII and count in bytes: `INDEX`, `INDEXW`, `INDEXC`, `FIND`,
  `FINDW`, `FINDC`, `VERIFY`, `COUNT`, `COUNTC`, `COUNTW`, the `ANY*` and
  `NOT*` functions, `TRANWRD`, `TRANSTRN`, `LEFT`, `RIGHT`, `TRIM`, `STRIP`,
  `COMPBL`, the `CAT` functions, `SCAN`, `SUBSTRN`, `COMPRESS`, `TRANSLATE`,
  `REVERSE` and `REPEAT`. Character lists and delimiters are sets of bytes;
  the `I` modifier and the class modifiers recognise ASCII characters only;
  `ANY*` and `NOT*` classify the characters of a table of measured classes
  (see above) and return the position of the first byte. A result that would leave part of a
  multi-byte character fails loud. A `CAT` result cut inside a character by a
  shorter target variable is blank, with the buffer WARNING and `_ERROR_`.
  The colon comparison (`=:`) truncates in bytes, and `$w.`, `$CHARw.` and
  `$QUOTEw.` count bytes (a cut inside a character fails loud). `%SYSFUNC`
  and PROC SQL calls of these functions, `$UPCASEw.` and the modifiers of
  `SCAN`, `COUNTW` and `FINDW` (other than `E`) with such values still fail
  loud.
* About 190 SAS functions as `sas_f_<name>()`, about 100 formats and
  informats, `PUT()` / `INPUT()` and user formats from PROC FORMAT. Missing
  and invalid arguments write the log NOTEs, set `_ERROR_` and give missing
  results; arguments are converted between numeric and character, and the
  number of arguments is checked.
* A direct call of a `sas_f_*` function from R code is checked as in a DATA
  step: approximate and unsupported functions follow `sasonr.strict`
  (`sasonr_strict_error`, or with `sasonr.strict = FALSE` a
  `sasonr_strict_warning` once per function and R session), arguments
  outside the exact input domain follow the same gate, and a data error (an invalid argument, invalid data) gives the documented
  result with a warning of class `sasonr_data_error` that carries the NOTE
  text. `sas_f_put()` and `sas_f_input()` take the format or informat as text
  (`"8.2"`) and check it against the registry.
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
