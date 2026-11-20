# SAS `PERM(n <, r>)`

Number of permutations. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - n:
    
    Total number of elements, a nonnegative integer. Missing gives
    missing.

  - r:
    
    Optional number of elements arranged, an integer from 0 to `n`; when
    omitted the result is n\!.

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
`sas_f_mod()`, `sas_f_modz()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# PERM: each call is explained above it and followed by its result

# PERM is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The number of ordered arrangements of 2 items out of 5 is 5 * 4 = 20.
sas_f_perm(5, 2)  # perm(5, 2) = 20
#> Warning: sasonr: function PERM is approximate
#> This warning is displayed once per session.
#> [1] 20

# With r omitted, PERM(n) is n! = 120.
sas_f_perm(5)  # perm(5) = 120
#> [1] 120

# There is exactly one way to arrange 0 items.
sas_f_perm(10, 0)  # perm(10, 0) = 1
#> [1] 1

# A missing argument gives a missing result.
sas_f_perm(NA_real_, 2)  # perm(., 2) = .
#> [1] NA

options(old)
```
