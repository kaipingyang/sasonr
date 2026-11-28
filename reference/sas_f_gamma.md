# SAS `GAMMA(argument)`

Gamma function. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value; must not be 0 or a negative integer (poles), which
    give missing with a note in the log. Missing gives missing.

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
`sas_f_floorz()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# GAMMA: each call is explained above it and followed by its result

# For a positive integer n, GAMMA(n) = (n-1)!, so GAMMA(5) = 24.
sas_f_gamma(5)  # gamma(5) = 24
#> [1] 24

# GAMMA(0.5) equals the square root of pi.
sas_f_gamma(0.5)  # gamma(0.5) = 1.77245385090552
#> [1] 1.772454

# The gamma function is also defined for negative non-integers.
sas_f_gamma(-1.5)  # gamma(-1.5) = 2.36327180120735
#> [1] 2.363272

# A missing argument gives a missing result.
sas_f_gamma(NA_real_)  # gamma(.) = .
#> [1] NA

# The gamma function has a pole at 0, so the result is missing (with a note in
# the log).
sas_f_gamma(0)  # gamma(0) = .
#> Warning: NOTE: Invalid argument to function GAMMA(0).
#> [1] NA
```
