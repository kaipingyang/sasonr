# SAS `NMISS(argument-1 <, ..., argument-n>)`

Number of missing numeric values. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Numeric values. `.` and special missing values such as `.A` are
    counted as missing.

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
`sas_f_min()`, `sas_f_n()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# NMISS: each call is explained above it and followed by its result

# NMISS counts the missing numeric arguments: 1.
sas_f_nmiss(1, NA_real_, 3)  # nmiss(1, ., 3) = 1
#> [1] 1

# The special missing values .A and ._ count as missing, 0 does not: 2.
sas_f_nmiss(sas_missing(".A"), sas_missing("._"), 0)  # nmiss(.A, ._, 0) = 2
#> [1] 2

# No argument is missing, so the count is 0.
sas_f_nmiss(1, 2)  # nmiss(1, 2) = 0
#> [1] 0
```
