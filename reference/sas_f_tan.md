# SAS `TAN(argument)`

Tangent. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value in radians. A missing value gives a missing result.

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
`sas_f_log()`, `sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`,
`sas_f_mod()`, `sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`,
`sas_f_rounde()`, `sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`,
`sas_f_sinh()`, `sas_f_sqrt()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# TAN: each call is explained above it and followed by its result

# The tangent of 0 is 0.
sas_f_tan(0)  # tan(0) = 0
#> [1] 0

# The tangent of 1 radian.
sas_f_tan(1)  # tan(1) = 1.5574077246549
#> [1] 1.557408

# The tangent is odd, so tan(-0.5) = -tan(0.5).
sas_f_tan(-0.5)  # tan(-0.5) = -0.54630248984379
#> [1] -0.5463025

# A missing argument gives a missing result.
sas_f_tan(NA_real_)  # tan(.) = .
#> [1] NA
```
