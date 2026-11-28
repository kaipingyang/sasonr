# SAS `CHOOSEN(index, value-1 <, ..., value-n>)`

Numeric value selected by index. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - k:
    
    Numeric index of the value to return. A negative index counts from
    the end of the list (-1 is the last); 0, missing, or an index beyond
    the list gives a missing result with a note in the log.

  - ...:
    
    Numeric values to choose from.

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
`sas_f_ceil()`, `sas_f_ceilz()`, `sas_f_coalesce()`, `sas_f_comb()`,
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
# CHOOSEN: each call is explained above it and followed by its result

# Index 2 selects the second value, 20.
sas_f_choosen(2, 10, 20, 30)  # choosen(2, 10, 20, 30) = 20
#> [1] 20

# A negative index counts from the end of the list: -1 selects the last value,
# 30.
sas_f_choosen(-1, 10, 20, 30)  # choosen(-1, 10, 20, 30) = 30
#> [1] 30

# Index 5 is beyond the two values given, so the result is missing (with a note
# in the log).
sas_f_choosen(5, 1, 2)  # choosen(5, 1, 2) = .
#> Warning: NOTE: Invalid argument to function CHOOSEN(5,1,2).
#> [1] NA
```
