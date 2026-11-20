# Strict mode: approximate and unsupported features fail loud

Every function, format and informat that sasonr implements has a status
in the feature registry (`sas_feature_registry()`). The option
`sasonr.strict` decides what happens when a program uses a feature that
is not supported:

## Details

  - `TRUE` (the default): the program stops with a condition of class
    `sasonr_strict_error` (a subclass of `sasonr_unsupported`) that
    names the SAS feature, its status and the SAS line. This applies to
    `approximate` features (the last bits of numeric results may not be
    exact), `unsupported` ones, features missing from the registry, and
    functions called in a context (DATA step, WHERE, PROC SQL,
    `%SYSFUNC`) in which they are not supported.

  - `FALSE`: the program runs with the R implementation's values. In
    `sas_run()` the log gets one NOTE at the end of the submission that
    lists the approximate features used (event id `sasonr.approximate`
    in `$log_events`, also when the submission stops with an error), and
    each feature signals one warning of class `sasonr_strict_warning`
    per submission. A direct call of a function-layer function
    (`sas_f_*()`) warns with class `sasonr_strict_warning` once per
    function and R session.

`supported` features, `partial` ones and clock-dependent
`not_comparable` functions (`TODAY()`, `DATE()`, `TIME()`, `DATETIME()`)
run in the contexts in which they are supported. Arguments outside the
domain in which a function's results are exact (for example very large
arguments of some date functions) are gated in the same way. Features
that are not implemented at all fail loud with `sasonr_unsupported` in
both modes.

The same rules apply when a function of the function layer (`sas_f_*()`)
is called directly from R code: the function is checked as in a DATA
step, `sas_f_put()` and `sas_f_input()` check their format or informat
(given as text such as `"8.2"`), and arguments outside the exact input
domain go through the strict gate. Where the DATA step writes a
data-error NOTE or set `_ERROR_` (an invalid argument, invalid data), a
direct call returns the result a DATA step gives and signals a warning
of class `sasonr_data_error` that carries the NOTE text.

## Examples

``` r
# PROBT() is approximate: the last bits of some results may not be exact
try(sas_run("data a; y = probt(1.5, 10); run;"))
#> Error in strict_gate(what, line, feature = feature, fields = list(kind = kind,  : 
#>   function PROBT is approximate; set options(sasonr.strict = FALSE) to use it (SAS line 1)
res <- rlang::with_options(sasonr.strict = FALSE,
  suppressWarnings(sas_run("data a; y = probt(1.5, 10); run;")))
res$work$a$y
#> [1] 0.9177463
# the NOTE at the end of the log lists the approximate features
tail(res$log, 1)
#> [1] "NOTE: sasonr used approximate implementations: function PROBT."
```
