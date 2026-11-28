# SAS `SMALLEST(k, value-1 <, ..., value-n>)`

k-th smallest non-missing value. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - k:
    
    Positive integer rank: 1 returns the smallest nonmissing value, 2
    the second smallest, and so on. If `k` exceeds the number of
    nonmissing values the result is missing.

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
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# SMALLEST: each call is explained above it and followed by its result

# SMALLEST(k, ...) returns the k-th smallest nonmissing value; with k = 1 the
# minimum of 3, 9, 5 is 3.
sas_f_smallest(1, 3, 9, 5)  # smallest(1, 3, 9, 5) = 3
#> [1] 3

# Missing values are ignored: with k = 2 the second smallest of 9 and 5 is 9.
sas_f_smallest(2, NA_real_, 9, 5)  # smallest(2, ., 9, 5) = 9
#> [1] 9

# k = 3 exceeds the number of values (2), so the result is missing.
sas_f_smallest(3, 1, 2)  # smallest(3, 1, 2) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA

# With k = 1 but no nonmissing value, the result is missing.
sas_f_smallest(1, NA_real_, NA_real_)  # smallest(1, ., .) = .
#> [1] NA
```
