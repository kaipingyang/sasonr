# sasonr (development version)

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
