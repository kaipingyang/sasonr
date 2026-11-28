# SAS `INPUTC(source, informat <, w>)`

Reads a character value with an informat given at run time. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - s:
    
    Character source value to read.

  - fmt:
    
    Character informat name given as a string, e.g. '$upcase3.'.

  - w:
    
    Optional numeric width that overrides the informat width.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputn()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# INPUTC: each call is explained above it and followed by its result

# INPUTC reads a character value with a character informat that is given as a
# string.
sas_f_inputc("abc", "$3.")  # inputc('abc', '$3.') = "abc"
#> [1] "abc"

# The $UPCASE informat converts the value to upper case.
sas_f_inputc("abc", "$upcase3.")  # inputc('abc', '$upcase3.') = "ABC"
#> [1] "ABC"

# The width 3 in $3. reads only the first 3 characters.
sas_f_inputc("abcdef", "$3.")  # inputc('abcdef', '$3.') = "abc"
#> [1] "abc"

# The optional third argument overrides the informat width: $CHAR. with width 2
# reads 'ab'.
sas_f_inputc("abcdef", "$char.", 2)  # inputc('abcdef', '$char.', 2) = "ab"
#> [1] "ab"
```
