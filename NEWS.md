# sasonr (development version)

## Round 12.1 (2026-11-12): review of round 12

An independent review of round 12 found places where the new moment kernel
could still return a wrong or missing value silently. Two new real-SAS
probes (nearly constant data with a zero-weight first observation or values a
few ulps apart; DATA step SKEWNESS of symmetric argument lists) settled the
open rules.

* **Nothing non-finite becomes a missing value silently.** Every product,
  fused multiply-add and sum of the moment kernel is checked: an overflow,
  an underflow below the normal range or a NaN fails loud (`sasonr_unsupported`)
  in PROC MEANS, PROC SQL and the DATA step functions. The emulated fma no
  longer falls back to `a * b + c`.
* **Nearly constant data** (PROC MEANS): SAS treats the mean shift
  `MEAN - c0` as 0 when it is at most about 1E-14 of the first value; sasonr
  now does the same (unweighted data), which fixes CSS, VAR and STDERR of such
  data. Weighted nearly constant data and the narrow undetermined band fail
  loud.
* **STD cut-off measured**: SAS reports STD and CV 0 up to VAR / MEAN\*\*2 =
  1.1966E-26 and keeps them from 1.4312E-26 on (the undetermined band shrank
  from 5.6E-31..4E-18); in the cut-off region the statistics derived from
  STD are run only under the conditions a probe covers, others fail loud;
  ratios below the smallest one probed (2.27E-33) fail loud instead of being
  extrapolated to STD 0.
* **A negative CSS is missing**, now confirmed by SAS (86 of 86 groups,
  together with USS, VAR, STD, STDERR and CV).
* **One moment kernel**: `stats_moments()` holds the DATA step, PROC SQL and
  PROC MEANS derivations, including SKEWNESS / KURTOSIS and the WEIGHT
  formulas; the fma accumulation reuses the fma core.
* DATA step CSS, VAR, STD, STDERR, CV and SKEWNESS are verified for at most
  **9** non-missing arguments (with 10, SAS's CSS differs in the last bit
  for a third of the new probe rows); the SKEWNESS guard for results below
  1E-12 stays, now documented with its evidence (SAS returns 0 for many
  symmetric lists but keeps residues as small as 1E-15 for others).
* PROC MEANS SKEWNESS with VARDEF=N is `approximate` (one probe value
  differs); EXCLNPWGT stays unverified and fails loud.

## Round 12 (2026-11-12): PROC MEANS moment statistics

* **PROC MEANS / SUMMARY CSS family bit-identical to SAS 9.4.** Four new
  real-SAS probes (integer data with exact sums; the same patterns at 14
  offsets; decimal data with missing values, FREQ, WEIGHT, VARDEF=, CLASS x
  BY and groups of up to 100,000 observations) fixed the algorithm: sums
  shifted by the first observation of the cell, the mean taken from the
  plain SUM, and the central sums written as a polynomial in the shift
  `q = MEAN - c0` evaluated by Horner's rule with fused multiply-adds
  (`CSS = fma(q, W q - 2 S1, S2)`). SUM with FREQ/WEIGHT accumulates
  `fma(w, x, SUM)`. CSS, USS, VAR, STD, STDERR, CV, T and KURTOSIS are now
  verified (`partial`: values beyond 1E140 or below 1E-140 fail loud) with
  and without FREQ and WEIGHT and for VARDEF=N/WDF/WEIGHT where SAS computes
  them; the default `proc means` listing no longer stops in strict mode.
  SKEWNESS stays `approximate` (about 97 % of probe values agree); PRT and
  the confidence limits stay `approximate` (they need SAS's t distribution).
* Rules found on the way: T is capped at +-99999.99 (PRT is computed from
  the capped value); a zero CV of a negative mean is +0; a negative CSS is
  missing; SAS reports STD 0 when the variance is negligible against the
  squared mean (a variance between 5.6E-31 and 4E-18 times the squared mean
  fails loud, the cut-off is not known); STDERR is `sqrt(VAR / SUMWGT)`.
* The PROC statistics gate checks every condition that applies (FREQ,
  WEIGHT, VARDEF=, QNTLDEF=, EXCLNPWGT); quantiles and the other order
  statistics with FREQ or WEIGHT are still unverified.
* DATA step `SKEWNESS` is bit-identical for up to 10 non-missing arguments
  (`partial`); `KURTOSIS` moved closer (93 % of the probe rows) but stays
  `approximate`.

## Round 11 (2026-11-12): verification integrity

An independent review of round 10 found ways in which results that do not
match SAS 9.4 could still be counted as verified or run silently. All of
them are closed; the bit-identical counts went down because cases that had
been counted as a pass now fail loud or count as differences.

* **A case is a pass only when every layer agrees**: data set values,
  attributes, log NOTEs, PUT text, written files and listing. The same value
  written differently (a BEST32. display difference) and a difference in a
  log NOTE alone are differences, not passes. When SAS stops with an ERROR,
  sasonr must report a SAS ERROR at the same step; failing loud or stopping
  with an R error there is counted separately and is not a pass.
* **Feature statuses**: `partial` now means that some cases pass and every
  other case fails loud (the feature runs only inside its verified domain;
  a run-time guard refuses everything else); new statuses `mismatch` and
  `unsupported` are gated like `approximate` and `unverified`.
* **Context-aware strict mode**: functions are checked against the contexts
  in which their cases agree with SAS (DATA step, PROC SQL, `%SYSFUNC`);
  a function used in another context -- including any WHERE expression, which
  has no cases yet -- stops with `sasonr_not_verified` in strict mode. PUTN,
  PUTC, INPUTN and INPUTC check the format they name at run time. Language
  constructs are recognised by their AST node, not by the name of a function.
* **BESTw. above 16**: the rounding of the 15th digit is derived from the SAS
  gold; values in the window where SAS both rounds up and truncates fail loud.
* **SAS error model**: the empty data sets left by a failing step are now
  derived from the parsed step and the analyzer (types, lengths, LENGTH,
  RETAIN, ARRAY); when they cannot be derived, sasonr fails loud.
* **One moment kernel** for the DATA step functions, PROC MEANS and the PROC
  SQL summary functions. A new random SAS probe (900 rows, 1 to 12
  arguments) fixed the DATA step algorithm: STD, VAR, CSS, STDERR and CV are
  bit-identical for up to 10 non-missing arguments (they fail loud beyond
  that and are now `partial`), USS is verified, SKEWNESS and KURTOSIS stay
  approximate.
* PROC FREQ statistics go through the common PROC statistics registry and
  `sasonr.strict` (the separate development switch is gone).
* Fixes found on the way: `SYMGET()` of a name longer than 32 characters,
  unknown format names (SAS's ERROR 48-59), `HEXw.` wider than 16, `MISSING()`
  in `%SYSFUNC`, a zero CV of negative values; invalid PRX patterns now fail
  loud (SAS writes a Perl diagnostic that is not reproduced).
* Conformance (strict mode): 7,452 of 9,969 cases bit-identical (round 10:
  7,891 of 9,968), 2,455 fail loud (2,015); no data set value differs
  silently and no program on which SAS stops runs without an ERROR in
  sasonr. Functions: 110 verified, 43 partial, 29 approximate, 2 mismatch
  (CONSTANT, MDY), 4 clock-dependent.

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
