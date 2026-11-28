# Runtime helper: PROC SQL IN list.

PROC SQL IN list.

## Arguments

  - x:
    
    Value tested by the IN operator; numeric or character.

  - ...:
    
    List values; the result is 1 if `x` equals any of them (a missing
    value equals another missing value), else 0.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Used by translated code; not a SAS function.

## See also

Other internal functions: `sas_f_cvt_c2n()`, `sas_f_cvt_n2c()`,
`sas_f_sqlcase()`, `sas_f_sqllike()`, `sas_f_sqlmonotonic()`,
`sas_f_sqlquant()`

## Examples

``` r
# SQLINLIST: each call is explained above it and followed by its result

# IN returns 1 if the value equals any item of the list.
sas_f_sqlinlist(2, 1, 2, 3)  # 2 in (1, 2, 3) = 1
#> [1] 1

# 'b' is not in the list, so the result is 0.
sas_f_sqlinlist("b", "a", "c")  # 'b' in ('a', 'c') = 0
#> [1] 0

# In PROC SQL a missing value equals another missing value, so . IN (., 1) is
# true.
sas_f_sqlinlist(NA, NA, 1)  # . in (., 1) = 1
#> [1] 1
```
