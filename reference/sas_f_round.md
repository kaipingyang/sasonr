# SAS `ROUND(argument <, rounding-unit>)`

Rounds half away from zero to a multiple of the unit, with fuzz. See the
[SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value. A missing value (`.` or a special missing value such
    as `.A`) gives a missing result.

  - unit:
    
    Optional positive rounding unit, default 1; the result is the
    multiple of the unit nearest to `x`. Ties are rounded away from
    zero, with fuzzing to correct for binary representation.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Rounds half away from zero with a fuzz of about 1e-12 relative and
returns the double nearest to the decimal multiple; `base::round()`
rounds half to even on the binary value.

## See also

Other numeric functions: `sas_f_abs()`, `sas_f_arcos()`,
`sas_f_arsin()`, `sas_f_atan()`, `sas_f_atan2()`, `sas_f_beta()`,
`sas_f_ceil()`, `sas_f_ceilz()`, `sas_f_choosen()`, `sas_f_coalesce()`,
`sas_f_comb()`, `sas_f_constant()`, `sas_f_cos()`, `sas_f_cosh()`,
`sas_f_divide()`, `sas_f_exp()`, `sas_f_fact()`, `sas_f_floor()`,
`sas_f_floorz()`, `sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`,
`sas_f_int()`, `sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`,
`sas_f_log()`, `sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`,
`sas_f_mod()`, `sas_f_modz()`, `sas_f_perm()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# ROUND: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Round to the nearest integer: halves go away from zero.
sas_f_round(2.5)  # round(2.5) = 3
#> [1] 3

# Negative halves also go away from zero: -3.
sas_f_round(-2.5)  # round(-2.5) = -3
#> [1] -3

# Rounding unit 0.1: ROUND treats 2.45 as the exact decimal, so 2.5 (base R
# gives 2.4).
sas_f_round(2.45, 0.1)  # round(2.45,0.1) = 2.5
#> [1] 2.5

# Rounding unit 0.01: 1.005 gives 1.01.
sas_f_round(1.005, 0.01)  # round(1.005,0.01) = 1.01
#> [1] 1.01

# Rounding unit 100: the nearest multiple of 100.
sas_f_round(1234.5678, 100)  # round(1234.5678,100) = 1200
#> [1] 1200

# A rounding unit of 1/3: the nearest multiple of 1/3.
sas_f_round(2/3, 1/3)  # round(2/3,1/3) = 0.66666666666666
#> [1] 0.6666667
```
