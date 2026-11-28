# SAS `PCTL(percentage, value-1 <, ..., value-n>)`

Percentile (definition 5). See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - p:
    
    Percentage from 0 to 100 (percentile definition 5 of PROC
    UNIVARIATE). Outside that range or missing gives missing.

  - ...:
    
    Numeric values; missing values are ignored.

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
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# PCTL: each call is explained above it and followed by its result

# PCTL(50, ...) is the median: for 1, 2, 3, 4 the average of the middle two
# values, 2.5.
sas_f_pctl(50, 1, 2, 3, 4)  # pctl(50, 1, 2, 3, 4) = 2.5
#> [1] 2.5

# The 25th percentile of 1 to 4: n * p = 1 is a whole number, so the 1st and
# 2nd values are averaged, 1.5.
sas_f_pctl(25, 1, 2, 3, 4)  # pctl(25, 1, 2, 3, 4) = 1.5
#> [1] 1.5

# Missing values are ignored, and the 100th percentile is the maximum, 9.
sas_f_pctl(100, 5, NA_real_, 9)  # pctl(100, 5, ., 9) = 9
#> [1] 9

# Any percentile of a single value is that value.
sas_f_pctl(10, 7)  # pctl(10, 7) = 7
#> [1] 7
```
