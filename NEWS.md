# sasonr (development version)

## Round 10 (2026-11-12): exact procedure statistics, verified domains, SAS error model

* **PROC statistics registry** (`inst/registry/proc_stats.csv`,
  `sas_proc_stat_registry()`): one row per procedure x statistic (MEANS /
  SUMMARY, PROC SQL summary functions, FREQ statistics) with its status,
  built from SAS gold probes (about 1,700 random groups, sorted, reversed and
  shuffled). In strict mode an `approximate` or `unverified` statistic stops
  the program (`sasonr_not_verified`, naming the procedure and the statistic).
* Algorithms aligned with SAS 9.4, bit for bit: sums are sequential double
  additions in observation order (R's `sum()` is extended precision); PROC SQL
  MEAN, CSS, USS, VAR and STD use the first value of the group as a shift and
  fused multiply-adds (emulated exactly); PROC MEANS quantiles interpolate
  `x[j] + g * (x[j+1] - x[j])`; MODE keeps the first value of a run.
* PROC SQL reorders the rows of a GROUP BY group unless the data set is
  flagged as sorted by the group columns: order-dependent statistics on such
  rows (and after joins, views, DISTINCT) fail loud unless the values are
  integers with exact sums.
* Not reproduced exactly and therefore `approximate` (fail loud in strict
  mode): PROC MEANS CSS and everything derived from it (STD, VAR, STDERR, CV,
  USS, SKEWNESS, KURTOSIS, confidence limits), PROC SQL STDERR and CV.
* **Verified domains** for functions whose differences are confined to
  identifiable inputs (status `partial`, domain in the registry and on the
  reference page; other inputs fail loud): MOD, MODZ (now an exact
  fused-multiply-add remainder), YRDIF (30/360, ACT/365, ACT/360; AGE and
  ACT/ACT only where exact). HARMEAN and GEOMEAN sum in argument order. LOG
  values are all bit-identical (617 cases); LOG stays `approximate` only
  because the BEST32. display of one value differs (BEST widths above 16 are
  partly undetermined).
* Targeted function probes (several hundred random arguments each) found
  last-bit differences in EXP, LOG10, COSH, TANH and PERM: they are now
  `approximate`.
* **SAS error model**: `sas_run(on_error = c("sas", "abort"))`, default
  `"sas"` (option `sasonr.on_error`). After a SAS ERROR the step is not run,
  syntax-check mode (OBS=0) applies to the rest of the program (DATA steps
  create empty data sets, global statements and macro code still run),
  PROC SQL errors set NOEXEC for that PROC SQL only, `SYSERR`/`SYSCC` follow
  SAS, and the result has `status = "error"`, `errors` and `error_line`.
  `sasonr_unsupported` always aborts. Calls of names that are not SAS
  functions are SAS's ERROR 68-185; unknown SET options are syntax errors.
* Conformance gold version 2: the log layer keeps SAS's error-handling
  messages and WORK is captured after an ERROR; cases where SAS and sasonr
  both stop now also compare WORK, the log and the PUT output.

## Round 9 (2026-11-12): strict by default, compact verification matrices

* **Strict by default**: `sasonr.strict` now defaults to `TRUE`. A program
  that uses a function, format or informat whose status in the feature
  registry is `approximate` (some results differ from SAS 9.4 in the last
  bits), `unverified` (no SAS case) or that is missing from the registry stops
  with a condition of class `sasonr_not_verified` (a subclass of
  `sasonr_unsupported`) naming the feature, its status and the SAS line. Set
  `options(sasonr.strict = FALSE)` to run such features with a warning of
  class `sasonr_unverified`. The check now also covers `%SYSFUNC` and the
  informats of `INPUT`, `INFORMAT` and `ATTRIB` statements (see
  `?sasonr_strict`).
* Verification matrices: one SAS program per function, format or informat
  (inputs listed per case), each case still compared on its own (value bit
  for bit, type, length, `_ERROR_`, log NOTEs with line numbers, PUT output).
* Fixes found by the new layout: character formats (`$CHAR`, `$HEX`,
  `$QUOTE`, `$UPCASE`, `$`) are now actually verified (their earlier cases
  never ran); `$HEXw.` wider than the value no longer encodes padding blanks;
  `$HEX.` without a width and `$QUOTEw.` cutting through an embedded quote
  fail loud; a DATA step stopped by an execution-time ERROR (`SYMGET(' ')`)
  keeps its incomplete data set like SAS.
* `OPTIONS NOSYNTAXCHECK` is accepted.

## Round 8 (2026-11-12): verification matrix and feature registry

* Every implemented function, format and informat now has a verification
  matrix of SAS 9.4 cases (7,068 programs: documented usages, missing and
  special missing values, extreme numbers, blank and long strings, type
  conversion, argument counts, `%SYSFUNC` and PROC SQL), compared exactly:
  numbers bit for bit, no tolerance.
* Functions: 120 verified, 28 approximate (last bits differ, mostly
  probability functions), 36 partial (some inputs fail loud or differ only in
  a log NOTE), 4 clock-dependent. Formats and informats: 96 verified, 11
  partial. No silent difference.
* Missing and invalid arguments now give SAS's log NOTEs, `_ERROR_` and
  missing results for about 130 functions; argument conversion and argument
  counts follow SAS for all functions; the SAS calendar has no 29 February in
  years divisible by 4000.
* Fixes found by the matrix include CAT/CATT numeric items, COUNTC `V`,
  INDEXC with several excerpts, JULDATE, YRDIF `AGE` (also the default
  basis), MDY two-digit years, noncentral F, DATETIME/TIME rounding,
  default informat widths, NEGPAREN, PVALUE and COMMAX E notation.
* Feature registries `inst/registry/functions.csv` and `formats.csv`
  (Developers > Feature registry); option `sasonr.strict` (`TRUE`: features
  that are not verified fail loud; `FALSE`: warning `sasonr_unverified`;
  unset: no check).
* Reference pages: every example call is explained and checked against SAS
  9.4; parameter descriptions give the SAS semantics.

## Round 7a (2026-11-12)

* Offline conformance cases: program + invented data + SAS 9.4 output (data
  sets kept as `.sas7bdat` for exact comparison); sanitiser, gold refresher
  and offline runner in `tools/`.
* Rounding and formats aligned with SAS: ROUND 98.5%, other rounding
  functions 99.6%, formats 96.2% of about 21,000 SAS points; remaining edge
  cases stop with `sasonr_unsupported`.
* `HEX16.` informat (previously returned missing silently), `FILE`
  statement, `LINESIZE`, log NOTE fixes.
* Semantics registry (`inst/registry/semantics.csv`).
* Reference pages for every `sas_f_*` function: SAS syntax, differences
  from R, verification status, examples from SAS-verified cases.
* New site structure: SAS language guides, Developers, Progress.
* License changed from Apache-2.0 to MIT.
* Every `sas_f_*` reference page has examples (194 of 195 functions), each
  call checked against SAS 9.4; 15 calls that differ from SAS were found and
  are listed under Known gaps.

## Round 6 (2026-11-12): verification

* Function pilot (SUBSTR, SCAN, COMPRESS, INDEX, CATX, ROUND, INPUT, PUT,
  INTNX, INTCK): 340 of 370 SAS-generated cases identical, none different.
* End-to-end textbook programs: 60 of 154 identical to SAS on all four layers, none wrong.
* PROC FREQ statistics checked against 57 SAS-generated cases; CHISQ, FISHER
  and EXACT FISHER enabled, other statistics stay gated.
* Fixed silent differences: SCAN `B` modifier and result length, ROUND for
  large values, CATX, INTNX/INTCK, numeric `LENGTH < 8` storage, chained
  comparisons in PROC SQL, variable order set by LABEL, name case from INPUT
  groups and arrays, ANYDTDTE Julian dates, `$HEX` with PUTC.
* Comparison masks environment fields (time stamps, paths) of PROC CONTENTS.
* Corpus: 1,110 programs completed, 1,066 identical on all four layers, no
  wrong data sets.

## Round 5 (2026-11-11)

* New procedures: PROC MEANS / SUMMARY, PROC FREQ (tables, OUT=, chi-square
  family, Fisher, binomial, CMH), PROC TRANSPOSE, PROC CONTENTS,
  PROC DATASETS, PROC APPEND, PROC DELETE.
* Fixed: single quotes inside `%LET` values hide macro triggers.
* Fixed variable attributes: length from `$w.` formats, name case in
  ATTRIB/LENGTH ranges, `DOLLARw.d` overflow, `6.2` stored as `F6.2`.
* Corpus: 1,104 programs completed, 1,031 identical to SAS on all four layers.

## Round 4

* Four-layer comparison against SAS (data sets, variable attributes, log
  diagnostics, listing).
* Variable attribute model (length, format, informat, label, name case) and
  SAS-style log notes.
* PROC SQL: own recursive-descent parser, DuckDB for joins and grouping, SAS
  semantics in R (missing values, remerge, CALCULATED, INTO:, SQLOBS).

## Round 3

* `_ERROR_` and SAS data error diagnostics (PDV dump in the log).
* About 200 SAS functions, about 80 formats and informats, PROC FORMAT
  VALUE/INVALUE, PUT()/INPUT().
* INPUT statement pointers and modifiers; INFILE options.

## Round 2

* Macro processor that interleaves with step execution.
* L2 compilation of baseline carry-forward patterns.
* PROC SORT and PROC PRINT.

## Round 1

* Lexer, Pratt expression parser, faithful DATA step AST.
* DATA step compilation to tidyverse code (L1/L2/L3).
