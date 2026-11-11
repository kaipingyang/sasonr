# sasonr (development version)

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
