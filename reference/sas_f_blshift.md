# SAS `BLSHIFT(argument-1, argument-2)`

Bitwise left shift. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    Numeric value treated as an unsigned 32-bit integer (0 to
    4294967295); non-integers and values outside that range are invalid.
    Missing gives missing.

  - n:
    
    Number of bit positions to shift left, an integer from 0 to 31; bits
    shifted beyond bit 32 are lost.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_bnot()`, `sas_f_bor()`,
`sas_f_brshift()`, `sas_f_bxor()`, `sas_f_exist()`, `sas_f_input()`,
`sas_f_inputc()`, `sas_f_inputn()`, `sas_f_missing()`, `sas_f_nvalid()`,
`sas_f_prxchange()`, `sas_f_prxmatch()`, `sas_f_prxparse()`,
`sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# BLSHIFT: each call is explained above it and followed by its result

# Shifting 1 left by 4 bits multiplies it by 2 to the power 4, giving 16.
sas_f_blshift(1, 4)  # blshift(1, 4) = 16
#> [1] 16

# Shifting 3 (binary 11) left by one bit gives 6 (binary 110).
sas_f_blshift(3, 1)  # blshift(3, 1) = 6
#> [1] 6

# A missing argument gives a missing result.
sas_f_blshift(NA_real_, 1)  # blshift(., 1) = .
#> Warning: NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.
#> [1] NA
```
