# SAS `EXP(argument)`

Exponential function. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result. Results that overflow give missing
    with a note in the log.

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
`sas_f_divide()`, `sas_f_fact()`, `sas_f_floor()`, `sas_f_floorz()`,
`sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# EXP: each call is explained above it and followed by its result

# EXP is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# exp(1) is the constant e.
sas_f_exp(1)  # exp(1) = 2.71828182845905
#> Warning: sasonr: function EXP is approximate
#> This warning is displayed once per session.
#> [1] 2.718282

# exp(0) is 1.
sas_f_exp(0)  # exp(0) = 1
#> [1] 1

# exp(-1) is 1/e.
sas_f_exp(-1)  # exp(-1) = 0.367879441171442
#> [1] 0.3678794

# A missing argument gives a missing result.
sas_f_exp(NA_real_)  # exp(.) = .
#> [1] NA

# exp(1000) overflows the largest double-precision number, so the result is
# missing (with a note in the log).
sas_f_exp(1000)  # exp(1000) = .
#> Warning: NOTE: Invalid argument to function EXP(1000).
#> [1] NA

options(old)
```
