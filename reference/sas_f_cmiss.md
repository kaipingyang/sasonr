# SAS `CMISS(argument-1 <, ..., argument-n>)`

Number of missing arguments (numeric or character). See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Numeric or character values. A numeric value counts as missing if it
    is `.` or a special missing value; a character value counts if it is
    blank.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments are ignored; the result is missing only when no
argument is non-missing. R summary functions return NA unless `na.rm =
TRUE`.

## See also

Other descriptive functions: `sas_f_css()`, `sas_f_cv()`,
`sas_f_geomean()`, `sas_f_harmean()`, `sas_f_kurtosis()`,
`sas_f_largest()`, `sas_f_max()`, `sas_f_mean()`, `sas_f_median()`,
`sas_f_min()`, `sas_f_n()`, `sas_f_nmiss()`, `sas_f_ordinal()`,
`sas_f_pctl()`, `sas_f_range()`, `sas_f_skewness()`, `sas_f_smallest()`,
`sas_f_std()`, `sas_f_stderr()`, `sas_f_sum()`, `sas_f_uss()`,
`sas_f_var()`

## Examples

``` r
# CMISS: each call is explained above it and followed by its result

# CMISS counts missing numeric or character arguments: . and the blank string
# '' are both missing, giving 2.
sas_f_cmiss(1, NA_real_, "a", " ")  # cmiss(1, ., 'a', '') = 2
#> [1] 2

# A special missing value such as .A counts as missing.
sas_f_cmiss(sas_missing(".A"))  # cmiss(.A) = 1
#> [1] 1

# Neither 'x' nor 2 is missing, so the count is 0.
sas_f_cmiss("x", 2)  # cmiss('x', 2) = 0
#> [1] 0
```
