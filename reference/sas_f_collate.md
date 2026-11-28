# SAS `COLLATE(start-position <, end-position> | <, , length>)`

String of characters in collating order. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - start:
    
    Numeric code of the first character (0 to 255) in the ASCII
    collating sequence. Missing gives a blank result.

  - end:
    
    Optional numeric code of the last character; when omitted the string
    runs to code 255 or for `len` characters.

  - len:
    
    Optional length of the result, used only when `end` is omitted
    (written in SAS as the third argument after an empty second one).
    Default 200 characters at most.

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
`sas_f_compare()`, `sas_f_compbl()`, `sas_f_compress()`,
`sas_f_count()`, `sas_f_countc()`, `sas_f_countw()`, `sas_f_dequote()`,
`sas_f_find()`, `sas_f_findc()`, `sas_f_findw()`, `sas_f_first()`,
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
# COLLATE: each call is explained above it and followed by its result

# COLLATE returns the characters with codes 65 through 70 of the collating
# sequence, 'ABCDEF'.
sas_f_collate(65, 70)  # collate(65, 70) = "ABCDEF"
#> [1] "ABCDEF"

# With the end position omitted, the third argument is the length: 10
# characters starting at code 48 ('0').
sas_f_collate(48, len = 10)  # collate(48, , 10) = "0123456789"
#> [1] "0123456789"

# Codes 97 through 99 are 'abc'.
sas_f_collate(97, 99)  # collate(97, 99) = "abc"
#> [1] "abc"
```
