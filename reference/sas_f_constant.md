# SAS `CONSTANT(constant <, parameter>)`

Mathematical and machine constants. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - name:
    
    Character name of the constant, not case sensitive: 'PI', 'E',
    'EULER', 'BIG', 'LOGBIG', 'SQRTBIG', 'SMALL', 'LOGSMALL',
    'SQRTSMALL', 'MACEPS', 'LOGMACEPS', 'SQRTMACEPS' or 'EXACTINT'.

  - p:
    
    Optional numeric parameter, used only by 'EXACTINT': the number of
    bytes (integer from 3 to 8); without it 'EXACTINT' refers to 8
    bytes.

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
`sas_f_comb()`, `sas_f_cos()`, `sas_f_cosh()`, `sas_f_divide()`,
`sas_f_exp()`, `sas_f_fact()`, `sas_f_floor()`, `sas_f_floorz()`,
`sas_f_gamma()`, `sas_f_gcd()`, `sas_f_ifn()`, `sas_f_int()`,
`sas_f_intz()`, `sas_f_lcm()`, `sas_f_lgamma()`, `sas_f_log()`,
`sas_f_log10()`, `sas_f_log2()`, `sas_f_logbeta()`, `sas_f_mod()`,
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# CONSTANT: each call is explained above it and followed by its result

# CONSTANT('PI') returns the mathematical constant pi.
sas_f_constant("PI")  # constant('PI') = 3.14159265358979
#> [1] 3.141593

# 'E' returns the base of the natural logarithm.
sas_f_constant("E")  # constant('E') = 2.71828182845905
#> [1] 2.718282

# 'EULER' returns Euler's constant, about 0.5772.
sas_f_constant("EULER")  # constant('EULER') = 0.577215664901533
#> [1] 0.5772157

# 'MACEPS' returns the machine precision of double-precision numbers, 2 to the
# power -52.
sas_f_constant("MACEPS")  # constant('MACEPS') = 2.22044604925031e-16
#> [1] 2.220446e-16

# 'BIG' returns the largest double-precision number.
sas_f_constant("BIG")  # constant('BIG') = 1.79769313486232e+308
#> [1] 1.797693e+308
```
