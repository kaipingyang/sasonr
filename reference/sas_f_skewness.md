# SAS `SKEWNESS(argument-1, argument-2, argument-3 <, ..., argument-n>)`

Skewness. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Three or more numeric values. Missing values are ignored; with fewer
    than 3 nonmissing values or zero variance the result is missing.

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
`sas_f_pctl()`, `sas_f_range()`, `sas_f_smallest()`, `sas_f_std()`,
`sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# SKEWNESS: each call is explained above it and followed by its result

# The sample skewness; the large value 10 gives a positive skew.
sas_f_skewness(1, 2, 3, 10)  # skewness(1, 2, 3, 10) = 1.76363261480389
#> [1] 1.763633

# Missing values are ignored; 1, 2, 3 are symmetric, so the skewness is 0.
sas_f_skewness(1, 2, NA_real_, 3)  # skewness(1, 2, ., 3) = 0
#> [1] 0

# The sample skewness of eight values.
sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9)  # skewness(2, 4, 4, 4, 5, 5, 7, 9) = 0.8184875533568
#> [1] 0.8184876

# Values with zero variance have no defined skewness, so the result is missing.
sas_f_skewness(1, 1, 1)  # skewness(1, 1, 1) = .
#> Warning: NOTE: Invalid argument to function SKEWNESS(1,1,1).
#> [1] NA
```
