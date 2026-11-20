# Base R and the tidyverse inside sasonr

This note records which parts of sasonr are written in base R and which
parts use the tidyverse, why the split falls where it does, and which
questions are still open. It is a design note, not a user guide.

## Layering

| Layer                                       | Language                                                                                                 | Evidence (calls in `R/`)                                                     |
| ------------------------------------------- | -------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| Value runtime: functions, formats, rounding | base R + `stats::`                                                                                       | `functions.R`: 0 tidyverse, 66 base; `stats::` 97 across the package         |
| Exact statistics (moments, CSS)             | base R, bit-level                                                                                        | `stats_exact.R`: 0 tidyverse                                                 |
| PROC engines (MEANS, FREQ, …)               | base R for the arithmetic, tibble only at the result boundary                                            | `proc_means.R`: 84 base / 2 tidyverse; `proc_freq.R`: 172 base / 8 tidyverse |
| DATA step execution                         | two code generators: a vectorised path emits `dplyr` pipelines, the row-wise path emits base R           | `codegen_tidy.R`: 16 `dplyr`/`tibble` calls                                  |
| PROC SQL                                    | own parser and R evaluation; DuckDB is a candidate executor for joins/aggregations, never a passthrough  | `sql_exec.R`: DBI/duckdb 4 each                                              |
| Translation output (`sas_translate()`)      | planned: an `idiomatic` mode that emits tidyverse code and a `faithful` mode that emits `sas_rt_*` calls | not implemented yet                                                          |

## What the tidyverse is used for, and why those uses are safe

These are structural, not numeric, operations, where dplyr’s semantics
are either exact or do not affect the results:

  - `dplyr::if_else()` for conditional assignment in vectorised code —
    stricter than `ifelse()`, which drops attributes;
  - `dplyr::lag()` for LAG, whose semantics is exactly “previous row”;
  - `dplyr::coalesce()` for COALESCEC — first non-missing argument;
  - `dplyr::bind_rows()` and `tibble::as_tibble()` for SET, `OUT=` data
    sets and the result container.

## Why the value layer cannot use the tidyverse

### lubridate: a different calendar

sasonr’s calendar does **not** treat years divisible by 4000 as leap
years. The runtime implements that calendar explicitly (`runtime.R`,
`skip4000_g`/`sas_day_to_greg`), while lubridate follows the proleptic
Gregorian calendar, so dates beyond 4000 differ. Beyond that: SAS dates
are days and datetimes are seconds since 1960-01-01, `Date`/`POSIXct`
add a timezone and a second conversion layer, the valid range is
1582-01-01..20000-12-31, and the SAS informats parse with their own
rules (MDY-before-DMY preference, `YEARCUTOFF`, invalid input sets
`_ERROR_` and returns missing). None of that maps onto lubridate’s
parsers.

### stringr: fixed width, bytes and missing values

SAS character values are fixed-length and blank padded, a missing
character is a blank string (not `NA`), and `LENGTH`/`SUBSTR`/$w. widths
count **bytes** in the session encoding. stringr returns variable-length
strings, propagates `NA`, counts Unicode code points and delegates case
mapping to ICU/locale. Base `substr`/`toupper` is not perfect either,
but its behaviour is deterministic and can be pinned to the SAS rules.

### dplyr aggregations and ordering

The moments must be accumulated in observation order with plain double
arithmetic: R’s `sum()` accumulates in long double and differs in the
last bits, which is why the CSS formula is written out by hand
(`stats_exact.R`). `dplyr::summarise()` would reintroduce `sum()`.
`arrange()` places `NA` last, while missing values sort first in SAS,
and `group_by()` does not promise SAS’s group order. The PROC listings
also need control over the column layout to the character, which a
`summarise()` result does not give.

### tidyr

PROC output is a fixed-width text listing; reshaping only changes the
data frame shape, while the order, names and labels still have to be
produced exactly. Adding tidyr would buy little and cost a dependency.

## Where tidyverse is fine

  - The planned `idiomatic` output of `sas_translate()`: readable dplyr
    code for people who keep the translation. It does not decide the
    results; the numerical results still come from
    `sas_f_*`/`sas_put()`/`sas_input()`.
  - User-facing examples and vignettes.

## Open questions

1.  Is `duckdb` worth keeping as a hard `Imports` for PROC SQL, or
    should it be a `Suggests` with a pure-R fallback? It is kept for
    now.
2.  The vectorised DATA step generator depends on dplyr. Would emitting
    base R for the common cases (mutate-only steps) be worth the
    maintenance cost? It would shrink the dependency surface of
    generated code, not of the engine.
3.  Nothing in the value layer prevents using base R alternatives; the
    rule is: a tidyverse function may be used where its semantics are
    exactly the SAS semantics being implemented or do not affect the
    results, and nowhere else.
