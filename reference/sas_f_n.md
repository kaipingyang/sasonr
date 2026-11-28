# SAS `N(argument-1 <, ..., argument-n>)`

Number of non-missing values. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Numeric values. `.` and special missing values are not counted.

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
`sas_f_min()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# N: each call is explained above it and followed by its result

# N counts the nonmissing numeric arguments: 2.
sas_f_n(1, NA_real_, 3)  # n(1, ., 3) = 2
#> [1] 2

# All arguments are missing, so the count is 0.
sas_f_n(NA_real_, NA_real_)  # n(., .) = 0
#> [1] 0

# The special missing value .A is not counted.
sas_f_n(sas_missing(".A"), 1)  # n(.A, 1) = 1
#> [1] 1

# 0 is a nonmissing value, so it is counted.
sas_f_n(0)  # n(0) = 1
#> [1] 1
```
