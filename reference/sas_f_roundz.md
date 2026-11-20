# SAS `ROUNDZ(argument <, rounding-unit>)`

Rounds to a multiple of the unit, ties to even, without fuzz. See the
[SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result.

  - unit:
    
    Optional positive rounding unit, default 1; the result is the
    multiple of the unit nearest to `x`. No fuzzing is applied, and
    exact ties are rounded to the even multiple.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

No fuzz; ties go to the even multiple; the result is the decimal
multiple.

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
`sas_f_rounde()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# ROUNDZ: each call is explained above it and followed by its result

# ROUNDZ rounds an exact tie to the even integer: 2.5 gives 2.
sas_f_roundz(2.5)  # roundz(2.5) = 2
#> [1] 2

# The second argument is the rounding unit; 2.45 rounded to a multiple of 0.1
# gives 2.4.
sas_f_roundz(2.45, 0.1)  # roundz(2.45, 0.1) = 2.4
#> [1] 2.4

# ROUNDZ uses no fuzzing, and 1.005 is stored slightly below 1.005 in binary,
# so it rounds down to 1.
sas_f_roundz(1.005, 0.01)  # roundz(1.005, 0.01) = 1
#> [1] 1

# -3.5 is halfway between -4 and -3, so the even integer -4 is chosen.
sas_f_roundz(-3.5)  # roundz(-3.5) = -4
#> [1] -4

# A missing argument gives a missing result.
sas_f_roundz(NA_real_)  # roundz(.) = .
#> [1] NA
```
