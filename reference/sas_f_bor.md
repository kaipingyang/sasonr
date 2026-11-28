# SAS `BOR(argument-1, argument-2)`

Bitwise OR. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    Numeric value treated as an unsigned 32-bit integer (0 to
    4294967295); non-integers and values outside that range are invalid.
    Missing gives missing.

  - b:
    
    Numeric value treated as an unsigned 32-bit integer (0 to
    4294967295); non-integers and values outside that range are invalid.
    Missing gives missing.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_brshift()`, `sas_f_bxor()`, `sas_f_exist()`,
`sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# BOR: each call is explained above it and followed by its result

# Bitwise OR of 1100 (12) and 1010 (10) is 1110, which is 14.
sas_f_bor(12, 10)  # bor(12, 10) = 14
#> [1] 14

# The OR of 0 and 0 is 0.
sas_f_bor(0, 0)  # bor(0, 0) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_bor(NA_real_, 1)  # bor(., 1) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA
```
