# SAS `DIVIDE(x, y)`

Division that returns special missing values for division by zero. See
the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    Numeric dividend (SAS x). Missing gives missing.

  - b:
    
    Numeric divisor (SAS y). Division by 0 gives the special missing
    value .I for a positive dividend, .M for a negative one, and . for
    0/0, without a log note.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments (including special missing values) give an ordinary
missing value and are reported in the log; R propagates NA/NaN silently
and has no special missing values.

## See also

Other numeric functions: `sas_f_abs()`, `sas_f_arcos()`,
`sas_f_arsin()`, `sas_f_atan()`, `sas_f_atan2()`, `sas_f_beta()`,
`sas_f_ceil()`, `sas_f_ceilz()`, `sas_f_choosen()`, `sas_f_coalesce()`,
`sas_f_comb()`, `sas_f_constant()`, `sas_f_cos()`, `sas_f_cosh()`,
`sas_f_exp()`, `sas_f_fact()`, `sas_f_floor()`, `sas_f_floorz()`,
`sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# DIVIDE: each call is explained above it and followed by its result

# DIVIDE returns x / y, here 0.5.
sas_f_divide(1, 2)  # divide(1, 2) = 0.5
#> [1] 0.5

# Dividing a positive number by 0 gives the special missing value .I (plus
# infinity) instead of an error.
sas_f_divide(1, 0)  # divide(1, 0) = .I
#> [1] NaN

# Dividing a negative number by 0 gives the special missing value .M (minus
# infinity).
sas_f_divide(-1, 0)  # divide(-1, 0) = .M
#> [1] NaN

# 0 / 0 is undefined, so the result is the ordinary missing value.
sas_f_divide(0, 0)  # divide(0, 0) = .
#> [1] NA

# A missing argument gives a missing result.
sas_f_divide(NA_real_, 2)  # divide(., 2) = .
#> [1] NA
```
