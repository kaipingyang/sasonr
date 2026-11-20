# SAS `ABS(argument)`

Absolute value. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments (including special missing values) give an ordinary
missing value and are reported in the log; R propagates NA/NaN silently
and has no special missing values.

## See also

Other numeric functions: `sas_f_arcos()`, `sas_f_arsin()`,
`sas_f_atan()`, `sas_f_atan2()`, `sas_f_beta()`, `sas_f_ceil()`,
`sas_f_ceilz()`, `sas_f_choosen()`, `sas_f_coalesce()`, `sas_f_comb()`,
`sas_f_constant()`, `sas_f_cos()`, `sas_f_cosh()`, `sas_f_divide()`,
`sas_f_exp()`, `sas_f_fact()`, `sas_f_floor()`, `sas_f_floorz()`,
`sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# ABS: each call is explained above it and followed by its result

# The absolute value of a negative number drops the sign.
sas_f_abs(-3.5)  # abs(-3.5) = 3.5
#> [1] 3.5

# The absolute value of zero is zero.
sas_f_abs(0)  # abs(0) = 0
#> [1] 0

# A missing argument (.) gives a missing result.
sas_f_abs(NA_real_)  # abs(.) = .
#> [1] NA

# A special missing value such as .A also gives the ordinary missing value (.).
sas_f_abs(sas_missing(".A"))  # abs(.A) = .
#> [1] NA
```
