# SAS `MAX(argument-1 <, ..., argument-n>)`

Largest non-missing value. See the [SAS
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
`sas_f_largest()`, `sas_f_mean()`, `sas_f_median()`, `sas_f_min()`,
`sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# MAX: each call is explained above it and followed by its result

# MAX returns the largest argument, 5.
sas_f_max(1, 5, 3)  # max(1, 5, 3) = 5
#> [1] 5

# Missing values are ignored, so the result is 2.
sas_f_max(NA_real_, 2)  # max(., 2) = 2
#> [1] 2

# All arguments are missing, so the result is missing.
sas_f_max(NA_real_, NA_real_)  # max(., .) = .
#> [1] NA

# Special missing values such as .A are ignored as well.
sas_f_max(sas_missing(".A"), 1)  # max(.A, 1) = 1
#> [1] 1

# The special missing value .B is ignored, so -1 is the largest value.
sas_f_max(-1, sas_missing(".B"))  # max(-1, .B) = -1
#> [1] -1
```
