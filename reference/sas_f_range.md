# SAS `RANGE(argument-1 <, ..., argument-n>)`

Range of the non-missing values. See the [SAS
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
`sas_f_cv()`, `sas_f_geomean()`, `sas_f_harmean()`, `sas_f_kurtosis()`,
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_median()`,
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`,
`sas_f_pctl()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# RANGE: each call is explained above it and followed by its result

# RANGE returns the maximum minus the minimum: 5 - 1 = 4.
sas_f_range(1, 5, 3)  # range(1, 5, 3) = 4
#> [1] 4

# Missing values are ignored; a single value has range 0.
sas_f_range(NA_real_, 2)  # range(., 2) = 0
#> [1] 0

# All arguments are missing, so the result is missing.
sas_f_range(NA_real_, NA_real_)  # range(., .) = .
#> [1] NA

# The range of -1 and 1 is 2.
sas_f_range(-1, 1)  # range(-1, 1) = 2
#> [1] 2
```
