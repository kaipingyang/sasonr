# SAS `COALESCE(argument-1 <, ..., argument-n>)`

First non-missing numeric argument. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Numeric values; the first one that is not `.` or a special missing
    value is returned. If all are missing the result is missing.

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
`sas_f_ceil()`, `sas_f_ceilz()`, `sas_f_choosen()`, `sas_f_comb()`,
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
# COALESCE: each call is explained above it and followed by its result

# COALESCE returns the first nonmissing argument, 2.
sas_f_coalesce(NA_real_, 2, 3)  # coalesce(., 2, 3) = 2
#> [1] 2

# All arguments are missing, so the result is missing.
sas_f_coalesce(NA_real_, NA_real_)  # coalesce(., .) = .
#> [1] NA

# Special missing values (.A, ._) are skipped like ., so the first nonmissing
# value is 5.
sas_f_coalesce(sas_missing(".A"), sas_missing("._"), 5)  # coalesce(.A, ._, 5) = 5
#> [1] 5
```
