# SAS `LOG10(argument)`

Base-10 logarithm. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Positive numeric value. 0 or a negative value gives missing with a
    note in the log; missing gives missing.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

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
`sas_f_log()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# LOG10: each call is explained above it and followed by its result

# LOG10 is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The base-10 logarithm of 1000 is 3.
sas_f_log10(1000)  # log10(1000) = 3
#> Warning: sasonr: function LOG10 is approximate
#> This warning is displayed once per session.
#> [1] 3

# The base-10 logarithm of 2.
sas_f_log10(2)  # log10(2) = 0.301029995663981
#> [1] 0.30103

# The base-10 logarithm of 0.001 is -3.
sas_f_log10(0.001)  # log10(0.001) = -3
#> [1] -3

# The logarithm of 0 is undefined, so the result is missing (with a note in the
# log).
sas_f_log10(0)  # log10(0) = .
#> Warning: NOTE: Invalid argument to function LOG10(0).
#> [1] NA

options(old)
```
