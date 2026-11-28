# SAS `MEDIAN(value-1 <, ..., value-n>)`

Median of the non-missing values. See the [SAS
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
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_min()`,
`sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# MEDIAN: each call is explained above it and followed by its result

# With an even number of values the median is the mean of the middle two (2 and
# 3).
sas_f_median(1, 3, 2, 10)  # median(1, 3, 2, 10) = 2.5
#> [1] 2.5

# Missing values are ignored: the median of 5 and 1 is 3.
sas_f_median(5, NA_real_, 1)  # median(5, ., 1) = 3
#> [1] 3

# A single value is its own median.
sas_f_median(7)  # median(7) = 7
#> [1] 7

# All arguments are missing, so the result is missing.
sas_f_median(NA_real_, NA_real_)  # median(., .) = .
#> [1] NA
```
