# SAS `ROUNDE(argument <, rounding-unit>)`

Rounds to a multiple of the unit, ties to even, with fuzz. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result.

  - unit:
    
    Optional positive rounding unit, default 1; the result is the
    multiple of the unit nearest to `x`. Exact ties are rounded to the
    even multiple.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Ties go to the even multiple as in `base::round()`, but with the same
fuzz as ROUND, and the result is the decimal multiple.

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
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# ROUNDE: each call is explained above it and followed by its result

# ROUNDE rounds a value exactly halfway between two integers to the even one:
# 2.5 gives 2.
sas_f_rounde(2.5)  # rounde(2.5) = 2
#> [1] 2

# 3.5 is halfway between 3 and 4, so the even integer 4 is chosen.
sas_f_rounde(3.5)  # rounde(3.5) = 4
#> [1] 4

# -2.5 is halfway between -3 and -2, so the even integer -2 is chosen.
sas_f_rounde(-2.5)  # rounde(-2.5) = -2
#> [1] -2

# The second argument is the rounding unit: 2.45 is halfway between 2.4 and
# 2.5, so the even multiple 2.4 is chosen.
sas_f_rounde(2.45, 0.1)  # rounde(2.45, 0.1) = 2.4
#> [1] 2.4

# A missing argument gives a missing result.
sas_f_rounde(NA_real_)  # rounde(.) = .
#> [1] NA
```
