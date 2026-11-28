# SAS `FIND(string, substring <, modifiers> <, start>)`

Position of a substring. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string. A numeric value is first converted to character
    with the BEST12. format. This is the string searched.

  - sub:
    
    Character substring to find. A zero-length substring gives 0.

  - a3:
    
    Either the modifiers (if character) or the start position (if
    numeric); the two may be given in either order. Modifiers: i ignores
    case; t trims trailing blanks from `string` and `substring`. Start:
    a positive value searches rightwards from that position, a negative
    value searches leftwards from position |start|, 0 gives 0; default
    1.

  - a4:
    
    Either the modifiers (if character) or the start position (if
    numeric); the two may be given in either order. Modifiers: i ignores
    case; t trims trailing blanks from `string` and `substring`. Start:
    a positive value searches rightwards from that position, a negative
    value searches leftwards from position |start|, 0 gives 0; default
    1.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS character values have a fixed length and are padded with blanks;
trailing blanks do not count in comparisons. R strings have no fixed
length.

## See also

Other character functions: `sas_f_anyalnum()`, `sas_f_anyalpha()`,
`sas_f_anydigit()`, `sas_f_anylower()`, `sas_f_anyprint()`,
`sas_f_anypunct()`, `sas_f_anyspace()`, `sas_f_anyupper()`,
`sas_f_byte()`, `sas_f_cat()`, `sas_f_cats()`, `sas_f_catt()`,
`sas_f_catx()`, `sas_f_char()`, `sas_f_choosec()`, `sas_f_coalescec()`,
`sas_f_collate()`, `sas_f_compare()`, `sas_f_compbl()`,
`sas_f_compress()`, `sas_f_count()`, `sas_f_countc()`, `sas_f_countw()`,
`sas_f_dequote()`, `sas_f_findc()`, `sas_f_findw()`, `sas_f_first()`,
`sas_f_ifc()`, `sas_f_index()`, `sas_f_indexc()`, `sas_f_indexw()`,
`sas_f_klength()`, `sas_f_ksubstr()`, `sas_f_left()`, `sas_f_length()`,
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
# FIND: each call is explained above it and followed by its result

# FIND returns the position of the first occurrence of 'bc', which is 2.
sas_f_find("abcabc", "bc")  # find('abcabc', 'bc') = 2
#> [1] 2

# A character third argument is a modifier: i ignores case, so 'BC' matches at
# position 2.
sas_f_find("abcabc", "BC", "i")  # find('abcabc', 'BC', 'i') = 2
#> [1] 2

# A numeric third argument is the start position: searching rightwards from
# position 3 finds 'bc' at 5.
sas_f_find("abcabc", "bc", 3)  # find('abcabc', 'bc', 3) = 5
#> [1] 5

# Negative start -4 searches leftwards from position 4 and finds the occurrence
# that begins at position 2.
sas_f_find("abcabc", "bc", -4)  # find('abcabc', 'bc', -4) = 2
#> [1] 2

# The substring does not occur, so the result is 0.
sas_f_find("abc", "x")  # find('abc', 'x') = 0
#> [1] 0
```
