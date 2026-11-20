# SAS `BNOT(argument)`

Bitwise NOT. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - a:
    
    Numeric value treated as an unsigned 32-bit integer (0 to
    4294967295); non-integers and values outside that range are invalid.
    Missing gives missing. All 32 bits are inverted.

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
`sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`, `sas_f_exist()`,
`sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# BNOT: each call is explained above it and followed by its result

# BNOT flips all 32 bits, so the complement of 0 is 2 to the power 32 minus 1 =
# 4294967295.
sas_f_bnot(0)  # bnot(0) = 4294967295
#> [1] 4294967295

# The complement of a value with all 32 bits set is 0.
sas_f_bnot(4294967295)  # bnot(4294967295) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_bnot(NA_real_)  # bnot(.) = .
#> Warning: NOTE: Invalid argument to function BNOT(.).
#> [1] NA
```
