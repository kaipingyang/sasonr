# SAS `CEIL(argument)`

Smallest integer \>= the argument, with fuzz. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result. Fuzzed: a value within about 1E-12
    of an integer is treated as that integer.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

A value within 1.5e-12 above an integer counts as that integer; a zero
result is -0.

## See also

Other numeric functions: `sas_f_abs()`, `sas_f_arcos()`,
`sas_f_arsin()`, `sas_f_atan()`, `sas_f_atan2()`, `sas_f_beta()`,
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
# CEIL: each call is explained above it and followed by its result

# CEIL returns the smallest integer greater than or equal to the argument: 3.
sas_f_ceil(2.1)  # ceil(2.1) = 3
#> [1] 3

# For a negative number the ceiling moves towards zero: -2.
sas_f_ceil(-2.1)  # ceil(-2.1) = -2
#> [1] -2

# CEIL is fuzzed: a value within about 1E-12 of an integer is treated as that
# integer, so the result is 1.
sas_f_ceil(1 + 1e-13)  # ceil(1 + 1e-13) = 1
#> [1] 1

# A missing argument gives a missing result.
sas_f_ceil(NA_real_)  # ceil(.) = .
#> [1] NA
```
