# SAS `LGAMMA(argument)`

Natural logarithm of the gamma function. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Positive numeric value. Missing gives missing.

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
`sas_f_int()`, `sas_f_intz()`, `sas_f_lcm()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# LGAMMA: each call is explained above it and followed by its result

# LGAMMA is the natural log of the gamma function: for 10 it is log(9!).
sas_f_lgamma(10)  # lgamma(10) = 12.8018274800815
#> [1] 12.80183

# LGAMMA(0.5) is the log of the square root of pi.
sas_f_lgamma(0.5)  # lgamma(0.5) = 0.5723649429247
#> [1] 0.5723649

# LGAMMA is convenient for large arguments: log(99!) is about 359.13.
sas_f_lgamma(100)  # lgamma(100) = 359.134205369575
#> [1] 359.1342

# A missing argument gives a missing result.
sas_f_lgamma(NA_real_)  # lgamma(.) = .
#> [1] NA
```
