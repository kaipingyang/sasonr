# sasonr (development version)

## Round 16: the SAS session object (2026-11-13)

* **`sas_session()`**: a persistent SAS session. Successive
  `sas_run(code, session = s)` calls behave like submissions to one SAS
  session: WORK data sets, macro variables and macro definitions, user
  formats, `OPTIONS`, titles and filerefs carry over. Accessors
  `sas_work()`, `sas_meta()`, `sas_macro_vars()`, `sas_log()`,
  `sas_listing()`; `sas_session_close()` closes the PROC SQL connection.
* All run state moved from package-level environments into the session
  (R6 class `SasSession`). The engine binds the session while a submission
  runs; two sessions used alternately, or a run nested inside a running step,
  no longer share anything. `sas_run()` without a session behaves exactly as
  before (every conformance verdict unchanged).
* A run no longer leaves state behind for later function-layer calls (for
  example `OPTIONS LINESIZE=` or PRXPARSE ids of the last run); calls outside
  a run use their own default session.

## Round 15.1: review of round 15 (2026-11-13)

* Integers too long for their decimals (16 or more digits with 4 or more
  decimals) keep their exact digits only at the scanned SAS 9.4 points; other
  values fail loud when a fixed-point text could fit. Integers from 1E18 to
  1E21 without decimals likewise (the field width still chooses E notation).
* `ROUNDE` and `ROUNDZ` fail loud from `|x/unit| = 2^49` with a fractional
  unit, as `ROUND` does; only exact multiples are exempt (all three).
* `ROUNDE` fails loud up to 2E-6 below a half point once `1.1E-12 * |q|`
  exceeds 1E-6 (its fuzz window is not rescanned yet).
* Wide `BEST` E forms take their mantissa decimals from the width and fail
  loud when the text does not fit (`BEST16.` wrote 18 columns). The E form of
  `BEST16.`-`BEST19.` and any E form with 13 mantissa decimals fail loud
  until SAS 9.4 output exists, except mantissas of at most 4 significant
  digits (verified: every candidate rule gives the same text).
* A vectorised DATA step whose untaken IF/SELECT branch fails loud or raises a
  SAS exception is re-run row by row; the failed attempt leaves no PRXPARSE
  ids behind, a SAS exception in a branch that does not run is no longer
  reported, and a character value as an IF condition fails loud at
  translation (it was skipped when the branch did not run).
* E notation: an exact tie at the 15th significant digit fails loud when its
  direction decides the digits shown.

## Round 15: SAS 9.4's decimal conversion (2026-11-13)

* **One closed form for fixed-point display rounding.** `w.d`, `Z`, `COMMA`,
  `DOLLAR`, `NEGPAREN`, `PVALUE`, `PERCENT` and the fixed-point forms of `BEST`
  compute `t = fl(fl(|x| + h) * (1 + 2^-52))` (h the double nearest
  0.5 * 10^-d) and write the exact binary value of `t` truncated to `d`
  decimals and at most 15 significant digits (values below 1: at most 14 - E
  decimals). Derived from ulp-by-ulp SAS 9.4 (TS1M7) scans at every binary
  exponent of the domain (185,228 points, round 13 and 15); it replaces the
  fail-loud windows 1 to 3 ulps below a half point, at the 16th significant
  digit and the conservative PERCENT band. `PERCENT` is the same rule on
  `fl(100 * |x|)`.
* **E notation** with up to 12 mantissa decimals: the value rounded to 14
  significant digits, then half up to the mantissa (a double rounding:
  6.6464999999999970E-19 in `E10.` is `6.647E-19`). With 13 or more mantissa
  decimals (`E20.` and wider, wide `BEST` E forms) the 14th digit is
  truncated or rounded only where the scans show no ambiguity; the band in
  between fails loud (it used to truncate up to a rest of 0.888, which was
  wrong for `E20.`).
* **ROUND**: rounds up exactly when `floor(fl(fl(|q| + 0.5) + W)) > floor(|q|)`
  with `q = x / unit` and `W = min(1E-12 * |q|, 1E-6)`; the former window
  (1.1E-12, 2E-6) and its fail-loud edges are gone. From `|x/unit| = 2^49` on,
  a fractional unit fails loud (SAS's results are not the nearest multiple).
* `BEST8.` to `BEST12.` write values below 1E-5 in E notation when it shows
  as many digits, and use an integer mantissa with a negative exponent when
  the decimal mantissa would show fewer than three digits (`-102E-14`).
* Integers too long for their decimals keep their exact digits with fewer
  zeros (`32.15` of 939586291501115008 is `939586291501115008.0000000000000`).
* Still fail loud, each region bounded by the scans: values below 1 with 15
  or more decimals (16 below 0.1, 17 below 0.01), non-integers of 1E15 or more
  with 4 or more decimals, the 14-digit E band, `BEST` at widths up to 7 for
  values below 1, `ROUNDE`/`ROUNDZ` window edges, PROC MEANS default decimals
  for field widths other than 12.
* Verification tooling (review of 37e3ba6): the results archive is keyed by
  the commit and code taken when the cases are selected (a change during the
  run stops it); case hashes use file contents (symbolic links and dot files
  included) and the fingerprint covers BLAS, threads, locale and time zone; a
  release re-checks an archived run incrementally and pushes exactly the
  verified commit; the parallel runner schedules the longest cases first and
  has a development-only `--fail-fast`.

## Review of the merge (2026-11-13)

* PROC SQL function NOTEs have one path again: several NOTEs, or a NOTE in a
  form not verified for PROC SQL, fail loud instead of being rewritten.
* PROC FREQ `EXPECTED`, `DEVIATION` and `CELLCHI2` go through the statistics
  registry of the selected SAS release (TS1M2: not verified, fail loud).
* New TS1M7 gold for rules first measured on TS1M2: PROC MEANS default
  decimals, the sign of zero in SUM/MEAN/MIN/MAX and the panel layout at
  LINESIZE= 64 to 200. The N field of a PROC MEANS table with BY groups is as
  wide as the number of observations of the data set (both releases); with
  CLASS variables or missing values that case fails loud.
* `INTCK`/`INTNX` with `WEEKDAY`, `TENDAY` and `SEMIMONTH` run only in their
  verified form (no multiplier, shift index, DT prefix, method or alignment
  argument); other forms fail loud.
* The `SUBSTR` statement (left of `=`) is gated by the feature registry as its
  own context of `SUBSTR`.
* `EXIST()` and the `$HEX` informat have TS1M7 verification matrices; `$HEX`
  writes the INPUT NOTE and sets `_ERROR_` for invalid hexadecimal data, and
  `EXIST()` works in PROC SQL.
* SASHELP members are read from the local copy (`<member>.sas7bdat` with its
  column attributes in `<member>_meta.csv`, or a built `.rds`) in a
  development tree or from `options(sasonr.sashelp_dir)`; a member without its
  column attributes fails loud.

## Merge of the `dev` line (2026-11-13): two SAS releases

The two development lines are merged. Their gold standards come from two
maintenance releases of SAS 9.4, and where the releases disagree sasonr now
follows the one selected with `options(sasonr.sas_version = "TS1M7")`
(default) or `"TS1M2"` (see `?sasonr_sas_version`).

* **Per-release verification.** The feature registries (`functions.csv`,
  `formats.csv`, `proc_stats.csv`) have a `sas_version` column and one block
  of rows per release, each built only from that release's gold. Strict mode
  gates every function, format and statistic by its status for the selected
  release; nothing is carried over between releases. The conformance runner
  runs each case for the release in its `meta.yaml`.
* **Differences found between the releases** (each with gold on both sides):
  PROC MEANS moments use fused multiply-adds in TS1M7 and plain arithmetic in
  TS1M2 (CSS, USS ...; the TS1M2 kernel is verified without WEIGHT/FREQ and
  fails loud elsewhere); a subnormal numeric literal is an ERROR in TS1M7 and
  a value in TS1M2; %SYSFUNC with an invalid argument writes no WARNING in TS1M7.
  PROC FREQ RELRISK / RISKDIFF print other labels and exact limits in TS1M2:
  only the TS1M7 listing is implemented, TS1M2 fails loud.
* **MAXDEC=** uses the w.d display engine (as the TS1M2 probe showed): the
  former half-up rule on the exact binary value differed from the TS1M7 gold
  in 11 `rounding/proc` cases once the panel layout made them run.
* Holes closed while merging: CAT-family SQL columns of constant arguments
  take the value's length again; WEEKDAYdaysW intervals fail loud instead of
  being treated as invalid; BESTw. with an integer E mantissa fails loud;
  left-hand SUBSTR of an undeclared variable runs only for TS1M2; display
  text of the new FREQ and MEANS code goes through the one display engine;
  `test-proc-means.R` keeps strict mode on. `purrr` is no longer imported.

### From the `dev` line (2026-11-13): PROC FREQ statistics, MEANS moments, display rules

The dev branch merged the upstream `main` (round 11) and continues with the
clinical procedures and the value runtime.

* **PROC MEANS moments are bit-identical to SAS 9.4 TS1M2** (the TS1M7 kernel of round 12 is kept for TS1M7): CSS, VAR, STD, STDERR,
  CV and USS are computed with the shifted-sum closed form SAS uses
  (CSS = sum of squares of the deviations from the reported mean, finalised
  with the SAS correction term), and USS = CSS + SUM * MEAN.  SKEWNESS and
  KURTOSIS remain approximate.
* **The display rules are `[gold]`**: the MAXDEC= display of a printed table,
  PUT w.d and PROC PRINT w.d share one rounding engine; the default decimals
  of a printed table (FW=12, no MAXDEC=) follow the rule measured on 155
  exactly known doubles (E notation below 1E-5, 9 decimals below 1E-3,
  7 decimals otherwise, the 2-decimal / integer / E branches above 1000, and
  an E mantissa as wide as the field allows).  Values in the undetermined
  window of the SAS fuzz and subnormal values fail loud.  The N field of a
  printed table is never narrower than two columns (TS1M2 only).
* **PROC FREQ**: one-way tables ignore EXPECTED / DEVIATION / CELLCHI2 (the
  manual says they have no effect there); RISKDIFF and RELRISK print the risk
  estimates, the risk differences (Wald, Agresti-Caffo, Hauck-Anderson,
  Newcombe and the exact unconditional score limits), the odds ratio and
  relative risk table and the exact confidence limits; multiway tables print
  the Cochran-Mantel-Haenszel statistics (SCORES=TABLE / RANK), the common
  odds ratio and relative risks and the Breslow-Day test; the EXACT statement
  handles the 2x2 EXACT CHISQ; TESTP= / TESTF= print the specified
  proportions or frequencies; MISSPRINT lists the missing levels of a two-way
  table; BY groups print one table per group; fractional weights compute
  CHISQ but not Fisher; a single-row table prints no statistics block.  The
  exact RELRISK confidence limit that SAS computes with an internal nuisance
  search (documented algorithm gives 0.08997 where SAS prints 0.0901) stays
  fail loud.
* Holes found by an audit of the new PROC FREQ code are closed by guards:
  EXACT CHISQ with non-integer weights or without the TABLES CHISQ option,
  EXACT BINOMIAL with non-integer weights, SCORES=RANK combined with CHISQ /
  FISHER (the per-stratum Mantel-Haenszel rows silently used the table
  scores), the MISSING option with numeric table scores, MISSPRINT with LIST,
  statistics after a LIST table, MISSPRINT statistics on a table that is not
  2x2, RELRISK(CL=) together with RISKDIFF(CL=), EXACT RISKDIFF without the
  CL= table, RISKDIFF CL= with a zero cell, ALPHA= other than 0.05 outside
  BINOMIAL, non-integer weights for CMH / RELRISK / RISKDIFF, FISHER with
  non-integer weights outside 2x2, BY groups outside 2x2 and negative
  weights all fail loud instead of printing a half-computed result.
* The PROC SQL NOTE for an out-of-range function argument is reproduced
  (`NOTE: Invalid argument to function MDY. Missing values may be
  generated.`). The %SYSFUNC WARNING the dev line added is a TS1M2 behaviour
  without committed gold (TS1M7 writes no WARNING): TS1M7 keeps the NOTE
  only, TS1M2 fails loud.
* **PROC SQL computed character columns** get the lengths SAS gives them
  (CAT family of column arguments 200, constant arguments the length of the value), and the SQL
  context of the functions involved is now in the registry.
* **DATA step**: an assignment to `SUBSTR()` of an undeclared variable
  (TS1M2; TS1M7 fails loud) creates it as a character variable of length 8, silently truncating an
  over-long replacement; `EXIST()` reports WORK and SASHELP members (an
  explicit `TYPE='ANY'` returns 0, as SAS does, because ANY is not a member
  type); the sign of a zero follows the measured rules in SUM / MEAN / MIN /
  MAX and in PROC MEANS.
* **Conformance on the dev line before the merge (own corpus, 758 cases)**: `freq_stats` 56/57,
  `rounding/proc` 76/84, `functions` 367/370 (23 expected-error cases agree,
  3 blocked by the upstream `mismatch` status of MDY and CONSTANT),
  `semantics` 12/13, `fn_examples` 150/184 (the rest are the approximate
  mathematical functions), `parse_stress` 28/28.  The full test suite is
  1236 passing with one pre-existing locale failure.

## Round 13.1 (2026-11-12): review of round 13

An independent review of round 13 found one regression (PERCENT lost its
fail-loud band) and places where display text still bypassed the display
engine. All findings are fixed; each has a regression test.

* **PERCENTw.d** fails loud again when it would round down a value within
  6E-16 (relative) below a half point (SAS's PERCENT rounding there differs
  from `w.d` and has not been scanned). The unused `down_at` arguments are
  gone.
* **Exact integer digits only where verified**: without decimals below 1E18
  (gold: `32.` of 939586291501115008), with decimals below 1E15. From 1E18 on
  the E form is written again as before round 13; an integer of 16 or more
  digits with decimals fails loud.
* **One display engine.** The narrow BESTw. E form, HOURw.d, PROC FREQ / MEANS
  / PRINT numbers and the PVALUE threshold now take their text from the
  display engine instead of `sprintf()`/`formatC()`. The time formats' own
  rounding fails loud where it disagrees with the engine; macro values other
  than integers below 1E15 fail loud instead of using `%.15g`, and a number
  compared with text in %SYSEVALF uses its BEST16. text.
* **WHERE matrix evidence**: character functions are compared in WHERE through
  a probe that shows the length and trailing blanks of the result, nested
  calls of the function are evaluated by the WHERE clause, and `where` counts
  as verified only when the WHERE matrix has as many cases as the DATA step
  matrix (this found a case-id collision that had dropped one IFN case).
  Gold refreshed for 38 functions. The probe showed that a WHERE
  concatenation beyond 32767 characters differs from the DATA step; it now
  fails loud.
* **WHERE note**: a variable compared with anything but a variable is written
  first, with the operator mirrored (new real-SAS case of 18 shapes); a
  constant compared with an expression other than a call fails loud.
* The SAS-exception rule traverses SELECT groups; PROC SQL expressions restore
  the runtime state on error and fail loud on math-error NOTEs instead of
  dropping them (missing-value NOTEs: SAS writes none in PROC SQL, gold);
  `FIRSTOBS=MIN` is 1 (real-SAS case).

## Round 13 (2026-11-12): display rounding scans, WHERE matrix, CONSTANT and MDY

New ulp-by-ulp real-SAS scans of numeric display rounding (about 45,000
points) showed that the round-7 rule for `w.d` formats ("up to 2.25E-16 below
a half point rounds up") was wrong in both directions for values one to three
ulps below a half point, and that SAS shows at most 15 significant digits
where sasonr printed the exact binary digits. Both gave silent wrong text.

* **Display rounding rebuilt on the scan results.** SAS formats the display
  value `fl(|x| * (1 + 2^-52))`: BESTw. above 16 columns and `w.d` beyond 15
  significant digits show its first 15 digits truncated (this settles the
  former BEST32. carry window exactly); rounding to 0 decimals is half away
  from zero on it. With decimals, a value at or above the half point rounds
  up, the double nearest to the half point rounds up, and a value 3 ulps or
  more below it rounds down. In between SAS's choice depends on the binary
  exponent, not on the distance, and sasonr now **fails loud** instead of
  guessing; so do the 16th significant digit, values below 1 shown with 15 or
  more decimals and 14-digit E mantissas with a large rest. Integers are
  written with their exact digits.
* **WHERE context matrix.** Every DATA step case of the function matrix runs
  again inside a WHERE statement (`tests/conformance/fn_where/`, one program
  per function, the call's arguments as variables of a one-row input).
  Functions whose WHERE cases all agree gain `where` in `contexts_verified`.
  A WHERE expression now raises SAS's "Function F requires a numeric
  (character) expression as argument n" ERROR instead of converting, and the
  WHERE line of the NOTE writes a variable compared with a function call
  first, as SAS does.
* **CONSTANT('EXACTINT', 1E300)** reproduces SAS's exception (two
  "An exception has been encountered" blocks, the step stops with 0
  observations) in a step that runs once; elsewhere it fails loud.
* **MDY**: the %SYSFUNC NOTE is wrapped at LINESIZE; PROC SQL writes
  "Invalid argument to function MDY. Missing values may be generated." (other
  function data-error NOTEs in PROC SQL fail loud instead of being dropped).

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
