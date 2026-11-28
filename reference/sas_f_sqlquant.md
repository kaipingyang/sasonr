# Runtime helper: PROC SQL quantified comparison.

PROC SQL quantified comparison.

## Arguments

  - x:
    
    Left-hand value of the comparison.

  - values:
    
    Values returned by the subquery.

  - op:
    
    Comparison operator as a string: '=', '^=', '\<', '\<=', '\>' or
    '\>='.

  - quant:
    
    Quantifier: 'any' is true if the comparison holds for at least one
    value; 'all' if it holds for every value.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Used by translated code; not a SAS function.

## See also

Other internal functions: `sas_f_cvt_c2n()`, `sas_f_cvt_n2c()`,
`sas_f_sqlcase()`, `sas_f_sqlinlist()`, `sas_f_sqllike()`,
`sas_f_sqlmonotonic()`

## Examples

``` r
# SQLQUANT: each call is explained above it and followed by its result

# ANY is true if the comparison holds for at least one value of the subquery: 1
# < 2 holds.
sas_f_sqlquant(1, c(2, 3), "<", "any")  # 1 < any (select v from work.t) = 1
#> [1] 1

# ALL requires the comparison to hold for every value; 3 > 3 is false, so the
# result is 0.
sas_f_sqlquant(3, c(2, 3), ">", "all")  # 3 > all (select v from work.t) = 0
#> [1] 0

# 3 >= 2 and 3 >= 3 both hold, so the result is 1.
sas_f_sqlquant(3, c(2, 3), ">=", "all")  # 3 >= all (select v from work.t) = 1
#> [1] 1
```
