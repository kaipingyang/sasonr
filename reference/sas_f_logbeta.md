# SAS `LOGBETA(a, b)`

Logarithm of the beta function. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    First shape parameter, a positive number. Missing gives missing.

  - b:
    
    Second shape parameter, a positive number. Missing gives missing.

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
`sas_f_log()`, `sas_f_log10()`, `sas_f_log2()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# LOGBETA: each call is explained above it and followed by its result

# LOGBETA is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# LOGBETA is the natural log of the beta function: log(B(2, 3)) = log(1/12).
sas_f_logbeta(2, 3)  # logbeta(2, 3) = -2.484906649788
#> Warning: sasonr: function LOGBETA is approximate
#> This warning is displayed once per session.
#> [1] -2.484907

# A missing argument gives a missing result.
sas_f_logbeta(NA_real_, 1)  # logbeta(., 1) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA

options(old)
```
