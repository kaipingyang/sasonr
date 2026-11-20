# SAS `CSS(argument-1 <, ..., argument-n>)`

Corrected sum of squares. See the [SAS
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

Other descriptive functions: `sas_f_cmiss()`, `sas_f_cv()`,
`sas_f_geomean()`, `sas_f_harmean()`, `sas_f_kurtosis()`,
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_median()`,
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`,
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`,
`sas_f_std()`, `sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`,
`sas_f_var()`

## Examples

``` r
# CSS: each call is explained above it and followed by its result

# CSS is the corrected sum of squares about the mean (2.5): 2.25 + 0.25 + 0.25
# + 2.25 = 5.
sas_f_css(1, 2, 3, 4)  # css(1, 2, 3, 4) = 5
#> [1] 5

# Missing values are ignored; the mean of 1 and 3 is 2, so CSS = 1 + 1 = 2.
sas_f_css(1, NA_real_, 3)  # css(1, ., 3) = 2
#> [1] 2

# A single value equals its own mean, so CSS is 0.
sas_f_css(5)  # css(5) = 0
#> [1] 0

# All arguments are missing, so the result is missing.
sas_f_css(NA_real_, NA_real_)  # css(., .) = .
#> [1] NA
```
