# SAS `STDERR(argument-1, argument-2 <, ..., argument-n>)`

Standard error of the mean. See the [SAS
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
`sas_f_std()`, `sas_f_sum()`, `sas_f_uss()`, `sas_f_var()`

## Examples

``` r
# STDERR: each call is explained above it and followed by its result

# STDERR returns the standard error of the mean, the standard deviation divided
# by sqrt(n).
sas_f_stderr(1, 2, 3, 4)  # stderr(1, 2, 3, 4) = 0.645497224367903
#> [1] 0.6454972

# The standard deviation of 2 and 4 is sqrt(2), divided by sqrt(2) gives 1.
sas_f_stderr(2, 4)  # stderr(2, 4) = 1
#> [1] 1

# At least two nonmissing values are needed, so the result is missing.
sas_f_stderr(5, NA_real_)  # stderr(5, .) = .
#> [1] NA
```
