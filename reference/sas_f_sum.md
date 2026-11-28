# SAS `SUM(argument-1 <, ..., argument-n>)`

Sum of the non-missing values. See the [SAS
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
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`,
`sas_f_std()`, `sas_f_stderr()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# SUM: each call is explained above it and followed by its result

# SUM adds its arguments: 6.
sas_f_sum(1, 2, 3)  # sum(1, 2, 3) = 6
#> [1] 6

# Missing values are ignored: 1 + 3 = 4.
sas_f_sum(1, NA_real_, 3)  # sum(1, ., 3) = 4
#> [1] 4

# All arguments are missing, so the result is missing.
sas_f_sum(NA_real_, NA_real_)  # sum(., .) = .
#> [1] NA

# The special missing value .A is ignored, so the sum is 1.
sas_f_sum(sas_missing(".A"), 1)  # sum(.A, 1) = 1
#> [1] 1
```
