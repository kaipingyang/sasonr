# Runtime helper: PROC SQL CASE expression.

PROC SQL CASE expression.

## Arguments

  - ...:
    
    Alternating conditions and values of a PROC SQL CASE expression:
    when1, then1, when2, then2, ..., followed by the ELSE value, which
    is returned if no condition is true.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Used by translated code; not a SAS function.

## See also

Other internal functions: `sas_f_cvt_c2n()`, `sas_f_cvt_n2c()`,
`sas_f_sqlinlist()`, `sas_f_sqllike()`, `sas_f_sqlmonotonic()`,
`sas_f_sqlquant()`

## Examples

``` r
# SQLCASE: each call is explained above it and followed by its result

# A CASE expression returns the value of the first WHEN clause whose condition
# is true.
sas_f_sqlcase(2 > 1, "yes", "no")  # case when 2 > 1 then 'yes' else 'no' end = "yes"
#> [1] "yes"

# The first condition is false and the second is true, so the result is 2.
sas_f_sqlcase(1 > 2, 1, 2 > 1, 2, 3)  # case when 1 > 2 then 1 when 2 > 1 then 2 else 3 end = 2
#> [1] 2

# No WHEN condition is true, so the ELSE value 3 is returned.
sas_f_sqlcase(1 > 2, 1, 3)  # case when 1 > 2 then 1 else 3 end = 3
#> [1] 3
```
