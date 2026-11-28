# SAS `ATAN2(argument-1, argument-2)`

Arctangent of the quotient of two arguments. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - y:
    
    Numeric y coordinate (SAS argument-1). A missing value gives a
    missing result.

  - x:
    
    Numeric x coordinate (SAS argument-2); the signs of `y` and `x`
    select the quadrant of the angle in (-pi, pi\]. A missing value
    gives a missing result.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Missing arguments (including special missing values) give an ordinary
missing value and are reported in the log; R propagates NA/NaN silently
and has no special missing values.

## See also

Other numeric functions: `sas_f_abs()`, `sas_f_arcos()`,
`sas_f_arsin()`, `sas_f_atan()`, `sas_f_beta()`, `sas_f_ceil()`,
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
# ATAN2: each call is explained above it and followed by its result

# ATAN2(y, x) is the angle of the point (x, y); for (1, 1) it is pi/4 radians.
sas_f_atan2(1, 1)  # atan2(1, 1) = 0.785398163397448
#> [1] 0.7853982

# The point x = -1, y = 1 lies in the second quadrant, so the angle is 3*pi/4.
sas_f_atan2(1, -1)  # atan2(1, -1) = 2.35619449019234
#> [1] 2.356194

# With both coordinates negative the point lies in the third quadrant, so the
# angle is -3*pi/4.
sas_f_atan2(-1, -1)  # atan2(-1, -1) = -2.35619449019234
#> [1] -2.356194

# A missing argument gives a missing result.
sas_f_atan2(NA_real_, 1)  # atan2(., 1) = .
#> [1] NA
```
