# SAS `MISSING(numeric-expression | character-expression)`

1 if the value is missing (blank for character), else 0. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric or character value. A number is missing if it is `.` or a
    special missing value (`.A` to `.Z`, `._`); a character value is
    missing if it is blank.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Blank character values and special missing values (.A-.Z, .\_) are
missing; `is.na()` knows neither.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# MISSING: each call is explained above it and followed by its result

# MISSING returns 1 for the missing value (.).
sas_f_missing(NA_real_)  # missing(.) = 1
#> [1] 1

# Special missing values such as .A also count as missing.
sas_f_missing(sas_missing(".A"))  # missing(.A) = 1
#> [1] 1

# 0 is a valid number, so the result is 0.
sas_f_missing(0)  # missing(0) = 0
#> [1] 0

# For a character argument a blank string counts as missing.
sas_f_missing(" ")  # missing('') = 1
#> [1] 1

# A string with any nonblank character is not missing.
sas_f_missing(" a")  # missing(' a') = 0
#> [1] 0
```
