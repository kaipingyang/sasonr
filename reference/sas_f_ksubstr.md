# SAS `KSUBSTR(string, position <, length>)`

Extracts a substring, counting positions and the length in characters.
See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string. A numeric value is first converted to character
    with the BEST12. format. A value with bytes outside ASCII is not
    supported (fails loud): the length of a variable that takes its
    length from `KSUBSTR()` is not implemented for such values.

  - pos:
    
    Starting character, an integer from 1 to the number of characters of
    `x`; a missing, smaller or larger position writes a note and sets
    the error flag.

  - len:
    
    Optional number of characters; when omitted the rest of the string
    is returned. A length running past the end returns the rest of the
    string; a missing or nonpositive length is invalid (blank result).

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

An invalid position or length gives a blank result, sets `_ERROR_` and
writes a NOTE (a nonpositive length also an ERROR); unlike `SUBSTR()`, a
missing or nonpositive length does not return the rest, and a length
past the end writes no NOTE. Arguments of magnitude between 32767 and
1E300, and lengths between 0 and 1, are not supported (fail loud). The
result length is that of the source. `SUBSTR()` counts bytes,
`KSUBSTR()` counts characters, as `substr()` does.

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
`sas_f_indexw()`, `sas_f_klength()`, `sas_f_left()`, `sas_f_length()`,
`sas_f_lengthc()`, `sas_f_lengthm()`, `sas_f_lengthn()`,
`sas_f_lowcase()`, `sas_f_notalnum()`, `sas_f_notalpha()`,
`sas_f_notdigit()`, `sas_f_notlower()`, `sas_f_notprint()`,
`sas_f_notpunct()`, `sas_f_notspace()`, `sas_f_notupper()`,
`sas_f_propcase()`, `sas_f_quote()`, `sas_f_rank()`, `sas_f_repeat()`,
`sas_f_reverse()`, `sas_f_right()`, `sas_f_scan()`, `sas_f_strip()`,
`sas_f_substr()`, `sas_f_substrn()`, `sas_f_translate()`,
`sas_f_transtrn()`, `sas_f_tranwrd()`, `sas_f_trim()`, `sas_f_trimn()`,
`sas_f_upcase()`, `sas_f_verify()`, `sas_f_whichc()`

## Examples

``` r
# KSUBSTR: each call is explained above it and followed by its result

# Three characters from position 2.
sas_f_ksubstr("abcdef", 2, 3)  # ksubstr('abcdef',2,3) = "bcd"
#> [1] "bcd"

# A position beyond the end is invalid
# (blank result, NOTE and _ERROR_ = 1 in a DATA step).
sas_f_ksubstr("abcdef", 7)  # ksubstr('abcdef',7) = ""
#> Warning: NOTE: Argument 2 to function KSUBSTR('abcdef',7) is invalid.
#> [1] ""
```
