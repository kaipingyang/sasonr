# SAS `GEOMEAN(argument-1 <, ..., argument-n>)`

Geometric mean. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    One or more numeric values (constants, variables or `OF` lists).
    Missing values, including special missing values, are ignored; if
    all are missing the result is missing.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments are ignored; the result is missing only when no
argument is non-missing. R summary functions return NA unless `na.rm =
TRUE`.

## See also

Other descriptive functions: `sas_f_cmiss()`, `sas_f_css()`,
`sas_f_cv()`, `sas_f_harmean()`, `sas_f_kurtosis()`, `sas_f_largest()`,
`sas_f_max()`, `sas_f_mean()`, `sas_f_median()`, `sas_f_min()`,
`sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# GEOMEAN: each call is explained above it and followed by its result

# GEOMEAN is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The geometric mean of 1, 2 and 4 is the cube root of 8, which is 2.
sas_f_geomean(1, 2, 4)  # geomean(1, 2, 4) = 2
#> Warning: sasonr: function GEOMEAN is approximate
#> This warning is displayed once per session.
#> [1] 2

# Missing values are ignored: the geometric mean of 1 and 9 is 3.
sas_f_geomean(1, NA_real_, 9)  # geomean(1, ., 9) = 3
#> [1] 3

# If any value is zero, the geometric mean is 0.
sas_f_geomean(0, 4)  # geomean(0, 4) = 0
#> [1] 0

# All arguments are missing, so the result is missing.
sas_f_geomean(NA_real_, NA_real_)  # geomean(., .) = .
#> [1] NA

options(old)
```
