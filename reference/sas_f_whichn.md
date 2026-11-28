# SAS `WHICHN(argument, value-1 <, ..., value-n>)`

Index of the first matching numeric value. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value to look up. A missing value gives a missing result.

  - ...:
    
    Numeric values; the result is the index of the first one equal to
    `x`, or 0.

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
`sas_f_divide()`, `sas_f_exp()`, `sas_f_fact()`, `sas_f_floor()`,
`sas_f_floorz()`, `sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`,
`sas_f_int()`, `sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`,
`sas_f_log()`, `sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`,
`sas_f_mod()`, `sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`,
`sas_f_rounde()`, `sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`,
`sas_f_sinh()`, `sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`

## Examples

``` r
# WHICHN: each call is explained above it and followed by its result

# WHICHN returns the index of the first value equal to the first argument: 2 is
# the 2nd value.
sas_f_whichn(2, 1, 2, 3)  # whichn(2, 1, 2, 3) = 2
#> [1] 2

# No value matches, so the result is 0.
sas_f_whichn(9, 1, 2)  # whichn(9, 1, 2) = 0
#> [1] 0

# A missing first argument gives a missing result, even though . appears in the
# list.
sas_f_whichn(NA_real_, 1, NA_real_, 3)  # whichn(., 1, ., 3) = .
#> [1] NA
```
