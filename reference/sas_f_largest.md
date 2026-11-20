# SAS `LARGEST(k, value-1 <, ..., value-n>)`

k-th largest non-missing value. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - k:
    
    Positive integer rank: 1 returns the largest nonmissing value, 2 the
    second largest, and so on. If `k` exceeds the number of nonmissing
    values the result is missing.

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
`sas_f_max()`, `sas_f_mean()`, `sas_f_median()`, `sas_f_min()`,
`sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`, `sas_f_pctl()`,
`sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# LARGEST: each call is explained above it and followed by its result

# LARGEST(k, ...) returns the k-th largest nonmissing value; with k = 1 that is
# the maximum of 3, 9, 5: 9.
sas_f_largest(1, 3, 9, 5)  # largest(1, 3, 9, 5) = 9
#> [1] 9

# With k = 2 the second largest of 3, 9, 5 is 5.
sas_f_largest(2, 3, 9, 5)  # largest(2, 3, 9, 5) = 5
#> [1] 5

# Missing values are ignored: the largest of the remaining value 4 is 4.
sas_f_largest(1, NA_real_, 4, NA_real_)  # largest(1, ., 4, .) = 4
#> [1] 4

# k = 3 exceeds the number of nonmissing values (2), so the result is missing.
sas_f_largest(3, 3, NA_real_)  # largest(3, 3, .) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA
```
