# SAS `ORDINAL(k, argument-1 <, ..., argument-n>)`

k-th smallest value (missing values first). See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - k:
    
    Positive integer rank from 1 to the number of values; 1 returns the
    smallest.

  - ...:
    
    Numeric values; missing values are kept and sort lowest, so they can
    be returned.

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
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# ORDINAL: each call is explained above it and followed by its result

# ORDINAL(k, ...) returns the k-th smallest argument, counting missing values
# as smallest: the 2nd of 5, 1, 3 is 3.
sas_f_ordinal(2, 5, 1, 3)  # ordinal(2, 5, 1, 3) = 3
#> [1] 3

# Missing values sort lowest, so the smallest of ., 2 is the missing value.
sas_f_ordinal(1, NA_real_, 2)  # ordinal(1, ., 2) = .
#> [1] NA

# The 3rd smallest of 3, 1, 2 is 3.
sas_f_ordinal(3, 3, 1, 2)  # ordinal(3, 3, 1, 2) = 3
#> [1] 3
```
