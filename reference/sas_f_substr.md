# SAS `SUBSTR(string, position <, length>)`

Extracts a substring. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string. A numeric value is first converted to character
    with the BEST12. format.

  - pos:
    
    Starting position, an integer from 1 to the length of `x`; a
    missing, smaller or larger position writes a note and sets the error
    flag. In a UTF-8 session positions count bytes: a character outside
    ASCII takes two to four positions.

  - len:
    
    Optional number of bytes; when omitted the rest of the string is
    returned. A length running past the end returns the rest of the
    string; a missing or nonpositive length is invalid (blank result). A
    start or end inside a multi-byte character is not supported (fails
    loud).

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Invalid positions or lengths set `_ERROR_` and write a NOTE; the result
length is that of the source. `SUBSTR()` counts bytes, `substr()` counts
characters (see `KSUBSTR()`). SUBSTR on the left of `=` with a value
that has bytes outside ASCII, and `%SYSFUNC(SUBSTR())` or PROC SQL calls
with such a value, are not supported.

## See also

Other character functions: `sas_f_anyalnum()`, `sas_f_anyalpha()`,
`sas_f_anydigit()`, `sas_f_anylower()`, `sas_f_anyprint()`,
`sas_f_anypunct()`, `sas_f_anyspace()`, `sas_f_anyupper()`,
`sas_f_byte()`, `sas_f_cat()`, `sas_f_cats()`, `sas_f_catt()`,
`sas_f_catx()`, `sas_f_char()`, `sas_f_choosec()`, `sas_f_coalescec()`,
`sas_f_collate()`, `sas_f_compare()`, `sas_f_compbl()`,
`sas_f_compress()`, `sas_f_count()`, `sas_f_countc()`, `sas_f_countw()`,
`sas_f_dequote()`, `sas_f_find()`, `sas_f_findc()`, `sas_f_findw()`,
`sas_f_first()`, `sas_f_ifc()`, `sas_f_index()`, `sas_f_indexc()`,
`sas_f_indexw()`, `sas_f_klength()`, `sas_f_ksubstr()`, `sas_f_left()`,
`sas_f_length()`, `sas_f_lengthc()`, `sas_f_lengthm()`,
`sas_f_lengthn()`, `sas_f_lowcase()`, `sas_f_notalnum()`,
`sas_f_notalpha()`, `sas_f_notdigit()`, `sas_f_notlower()`,
`sas_f_notprint()`, `sas_f_notpunct()`, `sas_f_notspace()`,
`sas_f_notupper()`, `sas_f_propcase()`, `sas_f_quote()`, `sas_f_rank()`,
`sas_f_repeat()`, `sas_f_reverse()`, `sas_f_right()`, `sas_f_scan()`,
`sas_f_strip()`, `sas_f_substrn()`, `sas_f_translate()`,
`sas_f_transtrn()`, `sas_f_tranwrd()`, `sas_f_trim()`, `sas_f_trimn()`,
`sas_f_upcase()`, `sas_f_verify()`, `sas_f_whichc()`

## Examples

``` r
# SUBSTR: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# From position 2 to the end.
sas_f_substr("abcdef", 2)  # substr('abcdef',2) = "bcdef"
#> [1] "bcdef"

# Three characters from position 2.
sas_f_substr("abcdef", 2, 3)  # substr('abcdef',2,3) = "bcd"
#> [1] "bcd"

# A position beyond the end is invalid: blank result, NOTE and _ERROR_ = 1 in a
# DATA step.
sas_f_substr("abcdef", 7)  # substr('abcdef',7) = ""
#> Warning: NOTE: Invalid second argument to function SUBSTR.
#> [1] ""

# A length beyond the end: the rest of the string (with a NOTE and _ERROR_ = 1
# in a DATA step).
sas_f_substr("abcdef", 2, 10)  # substr('abcdef',2,10) = "bcdef"
#> Warning: NOTE: Invalid third argument to function SUBSTR.
#> [1] "bcdef"

# In a UTF-8 session positions count bytes: the first three bytes of a value
# with two-byte characters can be two characters.
if (isTRUE(l10n_info()[["UTF-8"]])) {
  x <- "\u00e9t\u00e9"
  print(sas_f_substr(x, 1, 3))  # the first two characters of x
  print(sas_f_length(sas_f_substr(x, 1, 3)))  # 3
}
#> [1] "ét"
#> [1] 3
```
