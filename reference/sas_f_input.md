# SAS `INPUT(source, <? | ??> informat.)`

Reads a value with an informat. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - s:
    
    Source value to read; a numeric source is first converted with
    BEST12.

  - fmt:
    
    Parsed informat specification: a list with elements `name`, `w`,
    `d`, `char` and `text`, as produced by the translator from a
    informat written as `name w.d` (e.g. `8.3`, `z5.`, `$5.`). In SAS
    code this is the informat name itself, not a string.

  - qq:
    
    Error-control modifier written before the informat: ” (default)
    writes a note and sets the error flag for invalid data; '?'
    suppresses the note; '??' suppresses the note and the error flag.

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
`sas_f_exist()`, `sas_f_inputc()`, `sas_f_inputn()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# INPUT: each call is explained above it and followed by its result

# INPUT reads the character value '123' with the numeric informat 8. and
# returns the number 123.
sas_f_input("123", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."))  # input('123', 8.) = 123
#> [1] 123

# With informat 4.2 and no decimal point in the data, the last 2 digits are
# taken as decimals: 12.34.
sas_f_input("1234", list(name = "", w = 4L, d = 2L, char = FALSE, text = "4.2"))  # input('1234', 4.2) = 12.34
#> [1] 12.34

# The COMMA informat removes the commas before reading the number.
sas_f_input("1,234", list(name = "comma", w = 8L, d = NA_integer_, char = FALSE, text = "comma8."))  # input('1,234', comma8.) = 1234
#> [1] 1234

# The ?? modifier suppresses the invalid-data note and the error flag; the
# unreadable value gives missing.
sas_f_input("abc", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."), qq = "??")  # input('abc', ?? 8.) = .
#> [1] NA

# The character informat $3. reads only the first 3 characters.
sas_f_input("abcdef", list(name = "", w = 3L, d = NA_integer_, char = TRUE, text = "$3."))  # input('abcdef', $3.) = "abc"
#> [1] "abc"
```
