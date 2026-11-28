# SAS `FLOOR(argument)`

Largest integer \<= the argument, with fuzz. See the [SAS
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

A value within 1.5e-12 below an integer counts as that integer.

## See also

Other numeric functions: `sas_f_abs()`, `sas_f_arcos()`,
`sas_f_arsin()`, `sas_f_atan()`, `sas_f_atan2()`, `sas_f_beta()`,
`sas_f_ceil()`, `sas_f_ceilz()`, `sas_f_choosen()`, `sas_f_coalesce()`,
`sas_f_comb()`, `sas_f_constant()`, `sas_f_cos()`, `sas_f_cosh()`,
`sas_f_divide()`, `sas_f_exp()`, `sas_f_fact()`, `sas_f_floorz()`,
`sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# FLOOR: each call is explained above it and followed by its result

# The largest integer less than or equal to 2.7 is 2.
sas_f_floor(2.7)  # floor(2.7) = 2
#> [1] 2

# For a negative number the floor moves away from zero: -3.
sas_f_floor(-2.1)  # floor(-2.1) = -3
#> [1] -3

# FLOOR is fuzzed: a value within about 1E-12 below an integer is treated as
# that integer, so the result is 3.
sas_f_floor(3 - 1e-13)  # floor(3 - 1e-13) = 3
#> [1] 3

# A missing argument gives a missing result.
sas_f_floor(NA_real_)  # floor(.) = .
#> [1] NA
```
