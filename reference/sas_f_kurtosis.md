# SAS `KURTOSIS(argument-1, ..., argument-n)`

Kurtosis. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Four or more numeric values. Missing values are ignored; with fewer
    than 4 nonmissing values or zero variance the result is missing.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments are ignored; the result is missing only when no
argument is non-missing. R summary functions return NA unless `na.rm =
TRUE`.

## See also

Other descriptive functions: `sas_f_cmiss()`, `sas_f_css()`,
`sas_f_cv()`, `sas_f_geomean()`, `sas_f_harmean()`, `sas_f_largest()`,
`sas_f_max()`, `sas_f_mean()`, `sas_f_median()`, `sas_f_min()`,
`sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# KURTOSIS: each call is explained above it and followed by its result

# KURTOSIS is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# KURTOSIS returns the sample excess kurtosis (about 0 for normal data); the
# outlier 10 makes it large.
sas_f_kurtosis(1, 2, 3, 4, 10)  # kurtosis(1, 2, 3, 4, 10) = 3.152
#> Warning: sasonr: function KURTOSIS is approximate
#> This warning is displayed once per session.
#> [1] 3.152

# The sample excess kurtosis of eight values.
sas_f_kurtosis(2, 4, 4, 4, 5, 5, 7, 9)  # kurtosis(2, 4, 4, 4, 5, 5, 7, 9) = 0.940625
#> [1] 0.940625

# Missing values are ignored.
sas_f_kurtosis(1, 2, NA_real_, 3, 4, 8)  # kurtosis(1, 2, ., 3, 4, 8) = 2.02101707637456
#> [1] 2.021017

# Values with zero variance have no defined kurtosis, so the result is missing.
sas_f_kurtosis(1, 1, 1, 1)  # kurtosis(1, 1, 1, 1) = .
#> Warning: NOTE: Invalid argument to function KURTOSIS(1,1,1,1).
#> [1] NA

options(old)
```
