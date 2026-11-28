# SAS `INPUTN(source, informat <, w <, d>>)`

Reads a numeric value with an informat given at run time. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - s:
    
    Character source value to read.

  - fmt:
    
    Numeric informat name given as a string, e.g. 'comma8.'.

  - w:
    
    Optional numeric width that overrides the informat width.

  - d:
    
    Optional number of decimal places that overrides the informat's.

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
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputc()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# INPUT: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Read the text '123' with the 8. informat.
sas_f_inputn("123", "8.")  # input('123',8.) = 123
#> [1] 123

# Without a decimal point in the text, the informat 4.2 places two implied
# decimals: 12.34.
sas_f_inputn("1234", "4.2")  # input('1234',4.2) = 12.34
#> [1] 12.34

# A decimal point in the text overrides the informat's decimals.
sas_f_inputn("12.34", "5.1")  # input('12.34',5.1) = 12.34
#> [1] 12.34

# COMMA8. ignores the thousands separator.
sas_f_inputn("1,234", "comma8.")  # input('1,234',comma8.) = 1234
#> [1] 1234

# COMMA5. reads a value in parentheses as negative.
sas_f_inputn("(12)", "comma5.")  # input('(12)',comma5.) = -12
#> [1] -12
```
