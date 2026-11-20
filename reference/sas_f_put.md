# SAS `PUT(source, format.)`

Writes a value with a format. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Value to format; numeric for numeric formats, character for
    character ($) formats.

  - fmt:
    
    Parsed format specification: a list with elements `name`, `w`, `d`,
    `char` and `text`, as produced by the translator from a format
    written as `name w.d` (e.g. `8.3`, `z5.`, `$5.`). In SAS code this
    is the format name itself, not a string.

  - align:
    
    Optional alignment written after the format: 'l' left-aligns (-L),
    'r' right-aligns (-R), 'c' centers (-C) the formatted value within
    the width. Default none.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`,
`sas_f_missing()`, `sas_f_nvalid()`, `sas_f_prxchange()`,
`sas_f_prxmatch()`, `sas_f_prxparse()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# PUT: each call is explained above it and followed by its result

# PUT formats 3.14159 with the w.d format 8.3: rounded to 3 decimals,
# right-aligned in 8 columns.
sas_f_put(3.14159, list(name = "", w = 8L, d = 3L, char = FALSE, text = "8.3"))  # put(3.14159, 8.3) = "   3.142"
#> [1] "   3.142"

# The Z5. format pads the number with leading zeros to width 5.
sas_f_put(42, list(name = "z", w = 5L, d = NA_integer_, char = FALSE, text = "z5."))  # put(42, z5.) = "00042"
#> [1] "00042"

# The COMMA10. format inserts thousands separators, right-aligned in 10
# columns.
sas_f_put(1234567, list(name = "comma", w = 10L, d = NA_integer_, char = FALSE, text = "comma10."))  # put(1234567, comma10.) = " 1,234,567"
#> [1] " 1,234,567"

# A missing value is written as a period; for a w.d format it sits where the
# decimal point would be.
sas_f_put(NA_real_, list(name = "", w = 8L, d = 2L, char = FALSE, text = "8.2"))  # put(., 8.2) = "     ."
#> [1] "     .  "

# The character format $5. writes 'ab' in a 5-character field, padded with
# trailing blanks.
sas_f_put("ab", list(name = "", w = 5L, d = NA_integer_, char = TRUE, text = "$5."))  # put('ab', $5.) = "ab"
#> [1] "ab   "
```
