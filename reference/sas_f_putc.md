# SAS `PUTC(source, format <, w>)`

Writes a character value with a format given at run time. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character value to format.

  - fmt:
    
    Character format name given as a string, e.g. '$upcase3.'.

  - w:
    
    Optional numeric width that overrides the format width.

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
`sas_f_prxmatch()`, `sas_f_prxparse()`, `sas_f_put()`, `sas_f_putn()`

## Examples

``` r
# PUTC: each call is explained above it and followed by its result

# PUTC applies a character format given as a string; $5. pads 'abc' to width 5.
sas_f_putc("abc", "$5.")  # putc('abc', '$5.') = "abc"
#> [1] "abc  "

# The $UPCASE3. format writes the value in upper case.
sas_f_putc("abc", "$upcase3.")  # putc('abc', '$upcase3.') = "ABC"
#> [1] "ABC"

# $HEX6. writes 616263 (two hex digits per character); a new variable gets the
# length of the first argument (3), so SAS stores 616.
substr(sas_f_putc("abc", "$hex6."), 1, 3)  # putc('abc', '$hex6.') = "616"
#> [1] "616"

# The format $3. writes only the first 3 characters.
sas_f_putc("abcdef", "$3.")  # putc('abcdef', '$3.') = "abc"
#> [1] "abc"
```
