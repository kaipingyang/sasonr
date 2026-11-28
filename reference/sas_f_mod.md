# SAS `MOD(dividend, divisor)`

Remainder, with fuzz; sign of the dividend. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    Numeric dividend; the remainder has its sign. Missing gives missing.

  - b:
    
    Numeric divisor. 0 gives missing with a note in the log. The
    computation is fuzzed to correct for binary rounding.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

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
`sas_f_modz()`, `sas_f_perm()`, `sas_f_round()`, `sas_f_rounde()`,
`sas_f_roundz()`, `sas_f_sign()`, `sas_f_sin()`, `sas_f_sinh()`,
`sas_f_sqrt()`, `sas_f_tan()`, `sas_f_tanh()`, `sas_f_whichn()`

## Examples

``` r
# MOD: each call is explained above it and followed by its result

# The remainder of 10 divided by 3 is 1.
sas_f_mod(10, 3)  # mod(10, 3) = 1
#> [1] 1

# The remainder has the sign of the dividend: -1.
sas_f_mod(-10, 3)  # mod(-10, 3) = -1
#> [1] -1

# With a negative divisor and a positive dividend the remainder is positive: 1.
sas_f_mod(10, -3)  # mod(10, -3) = 1
#> [1] 1

# MOD also works with non-integers: 5.5 - 2*2 = 1.5.
sas_f_mod(5.5, 2)  # mod(5.5, 2) = 1.5
#> [1] 1.5

# MOD is fuzzed: 0.3 / 0.1 is treated as exactly 3, so the remainder is 0.
sas_f_mod(0.3, 0.1)  # mod(0.3, 0.1) = 0
#> [1] 0

# Division by 0 gives a missing result (with a note in the log).
sas_f_mod(10, 0)  # mod(10, 0) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA
```
