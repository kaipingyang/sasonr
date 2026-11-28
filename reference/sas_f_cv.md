# SAS `CV(argument-1 <, ..., argument-n>)`

Coefficient of variation. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Two or more numeric values (constants, variables or `OF` lists).
    Missing values, including special missing values, are ignored; if
    too few nonmissing values remain the result is missing.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments are ignored; the result is missing only when no
argument is non-missing. R summary functions return NA unless `na.rm =
TRUE`.

## See also

Other descriptive functions: `sas_f_cmiss()`, `sas_f_css()`,
`sas_f_geomean()`, `sas_f_harmean()`, `sas_f_kurtosis()`,
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_median()`,
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`,
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`,
`sas_f_std()`, `sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`,
`sas_f_var()`

## Examples

``` r
# CV: each call is explained above it and followed by its result

# CV is the coefficient of variation: 100 times the standard deviation divided
# by the mean.
sas_f_cv(1, 2, 3, 4)  # cv(1, 2, 3, 4) = 51.6397779494322
#> [1] 51.63978

# Values without spread have a coefficient of variation of 0.
sas_f_cv(2, 2)  # cv(2, 2) = 0
#> [1] 0

# Missing values are ignored; the CV is computed from 1 and 3.
sas_f_cv(1, NA_real_, 3)  # cv(1, ., 3) = 70.7106781186548
#> [1] 70.71068
```
