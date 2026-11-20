# Runtime helper: Implicit character-to-numeric conversion of a function argument.

Implicit character-to-numeric conversion of a function argument.

## Arguments

  - x:
    
    Character value used where a number is expected. Leading and
    trailing blanks are ignored; a value that is not a valid number
    gives a missing result with a note in the log.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Used by translated code; not a SAS function.

## See also

Other internal functions: `sas_f_cvt_n2c()`, `sas_f_sqlcase()`,
`sas_f_sqlinlist()`, `sas_f_sqllike()`, `sas_f_sqlmonotonic()`,
`sas_f_sqlquant()`

## Examples

``` r
# CVT_C2N: each call is explained above it and followed by its result

# Using a character value in arithmetic converts it to a number; leading and
# trailing blanks are ignored.
sas_f_cvt_c2n(" 12 ")  # ' 12 ' + 0 = 12
#> [1] 12

# Scientific notation such as '1e3' is read as 1000.
sas_f_cvt_c2n("1e3")  # '1e3' + 0 = 1000
#> [1] 1000

# A leading minus sign and an omitted leading zero are accepted: '-.5' is -0.5.
sas_f_cvt_c2n("-.5")  # '-.5' + 0 = -0.5
#> [1] -0.5

# 'abc' is not a valid number, so the conversion gives a missing value (with a
# note in the log).
sas_f_cvt_c2n("abc")  # 'abc' + 0 = .
#> [1] NA
```
