# SAS `VAR(argument-1, argument-2 <, ..., argument-n>)`

Variance. See the [SAS
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
`sas_f_cv()`, `sas_f_geomean()`, `sas_f_harmean()`, `sas_f_kurtosis()`,
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_median()`,
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`,
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`,
`sas_f_std()`, `sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`

## Examples

``` r
# VAR: each call is explained above it and followed by its result

# VAR returns the sample variance (divisor n - 1).
sas_f_var(1, 2, 3, 4)  # var(1, 2, 3, 4) = 1.66666666666667
#> [1] 1.666667

# Missing values are ignored: the variance of 5 and 7 is 2.
sas_f_var(5, NA_real_, 7)  # var(5, ., 7) = 2
#> [1] 2

# At least two nonmissing values are needed, so the result is missing.
sas_f_var(5, NA_real_)  # var(5, .) = .
#> [1] NA
```
