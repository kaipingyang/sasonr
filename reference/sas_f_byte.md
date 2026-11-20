# SAS `BYTE(n)`

Character with the given byte code. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - n:
    
    Integer code from 0 to 255 in the ASCII (Latin-1) collating
    sequence. A missing value gives a blank result.

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
`sas_f_cat()`, `sas_f_cats()`, `sas_f_catt()`, `sas_f_catx()`,
`sas_f_char()`, `sas_f_choosec()`, `sas_f_coalescec()`,
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
`sas_f_strip()`, `sas_f_substr()`, `sas_f_substrn()`,
`sas_f_translate()`, `sas_f_transtrn()`, `sas_f_tranwrd()`,
`sas_f_trim()`, `sas_f_trimn()`, `sas_f_upcase()`, `sas_f_verify()`,
`sas_f_whichc()`

## Examples

``` r
# BYTE: each call is explained above it and followed by its result

# BYTE returns the character with code 65 in the ASCII collating sequence, 'A'.
sas_f_byte(65)  # byte(65) = "A"
#> [1] "A"

# Code 97 is the lowercase letter 'a'.
sas_f_byte(97)  # byte(97) = "a"
#> [1] "a"

# Code 48 is the digit character '0'.
sas_f_byte(48)  # byte(48) = "0"
#> [1] "0"

# A missing code gives a blank result.
sas_f_byte(NA_real_)  # byte(.) = ""
#> [1] ""
```
