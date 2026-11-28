# SAS `IFN(condition, true, false <, missing>)`

Numeric value chosen by a condition. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - cond:
    
    Numeric condition: nonzero and nonmissing is true, 0 is false,
    missing selects `m`.

  - a:
    
    Numeric value returned when the condition is true.

  - b:
    
    Numeric value returned when the condition is false.

  - m:
    
    Optional numeric value returned when the condition is missing; when
    omitted, `b` is returned.

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
`sas_f_floorz()`, `sas_f_gamma()`, `sas_f_gcd()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# IFN: each call is explained above it and followed by its result

# A true (nonzero, nonmissing) condition returns the second argument.
sas_f_ifn(1, 10, 20)  # ifn(1, 10, 20) = 10
#> [1] 10

# A condition of 0 is false, so the third argument is returned.
sas_f_ifn(0, 10, 20)  # ifn(0, 10, 20) = 20
#> [1] 20

# A missing condition returns the optional fourth argument.
sas_f_ifn(NA_real_, 10, 20, 99)  # ifn(., 10, 20, 99) = 99
#> [1] 99

# Any nonzero, nonmissing value, including a negative number, counts as true.
sas_f_ifn(-1, 10, 20)  # ifn(-1, 10, 20) = 10
#> [1] 10
```
