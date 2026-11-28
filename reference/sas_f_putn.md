# SAS `PUTN(source, format <, w <, d>>)`

Writes a numeric value with a format given at run time. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value to format.

  - fmt:
    
    Numeric format name given as a string, e.g. 'comma10.'.

  - w:
    
    Optional numeric width that overrides the format width.

  - d:
    
    Optional number of decimal places that overrides the format's.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

w.d rounds the exact binary value half away from zero with a 1-ulp fuzz,
writes -0.00 for negatives that round to zero and falls back to E
notation when the value does not fit the width; `sprintf()` rounds half
to even.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`,
`sas_f_missing()`, `sas_f_nvalid()`, `sas_f_prxchange()`,
`sas_f_prxmatch()`, `sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`

## Examples

``` r
# PUT: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Write 123 with the 8. format: right-aligned in 8 columns.
sas_f_putn(123, "8.")  # put(123,8.) = "     123"
#> [1] "     123"

# 8.2 rounds to two decimals.
sas_f_putn(12.345, "8.2")  # put(12.345,8.2) = "   12.35"
#> [1] "   12.35"

# Format 1. rounds 2.5 half away from zero: 3.
sas_f_putn(2.5, "1.")  # put(2.5,1.) = "3"
#> [1] "3"

# A value too wide for 8. is written in E notation.
sas_f_putn(1e10, "8.")  # put(1e10,8.) = "    1E10"
#> [1] "    1E10"

# DOLLAR10.2: dollar sign, thousands separator, minus sign.
sas_f_putn(-1234.5, "dollar10.2")  # put(-1234.5,dollar10.2) = "$-1,234.50"
#> [1] "$-1,234.50"

# PERCENT8.1 multiplies by 100 and adds a percent sign.
sas_f_putn(0.125, "percent8.1")  # put(0.125,percent8.1) = "  12.5%"
#> [1] "  12.5% "
```
