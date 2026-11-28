# SAS `COMPRESS(source <, characters> <, modifiers>)`

Removes characters from a string. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string. A numeric value is first converted to character
    with the BEST12. format.

  - chars:
    
    Optional character list; each character is removed (or kept with
    modifier k). When both `chars` and `mods` are omitted, blanks are
    removed.

  - mods:
    
    Optional modifier string. k keeps the listed characters instead of
    removing them; i ignores case; t trims trailing blanks first; a adds
    letters, d digits, l lowercase letters, u uppercase letters, s
    whitespace, p punctuation, n letters, digits and underscore, f
    letters and underscore, x hexadecimal digits, c control characters,
    h horizontal tab to the list.

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
`sas_f_collate()`, `sas_f_compare()`, `sas_f_compbl()`, `sas_f_count()`,
`sas_f_countc()`, `sas_f_countw()`, `sas_f_dequote()`, `sas_f_find()`,
`sas_f_findc()`, `sas_f_findw()`, `sas_f_first()`, `sas_f_ifc()`,
`sas_f_index()`, `sas_f_indexc()`, `sas_f_indexw()`, `sas_f_klength()`,
`sas_f_ksubstr()`, `sas_f_left()`, `sas_f_length()`, `sas_f_lengthc()`,
`sas_f_lengthm()`, `sas_f_lengthn()`, `sas_f_lowcase()`,
`sas_f_notalnum()`, `sas_f_notalpha()`, `sas_f_notdigit()`,
`sas_f_notlower()`, `sas_f_notprint()`, `sas_f_notpunct()`,
`sas_f_notspace()`, `sas_f_notupper()`, `sas_f_propcase()`,
`sas_f_quote()`, `sas_f_rank()`, `sas_f_repeat()`, `sas_f_reverse()`,
`sas_f_right()`, `sas_f_scan()`, `sas_f_strip()`, `sas_f_substr()`,
`sas_f_substrn()`, `sas_f_translate()`, `sas_f_transtrn()`,
`sas_f_tranwrd()`, `sas_f_trim()`, `sas_f_trimn()`, `sas_f_upcase()`,
`sas_f_verify()`, `sas_f_whichc()`

## Examples

``` r
# COMPRESS: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# With one argument COMPRESS removes all blanks.
sas_f_compress("a b c")  # compress('a b c') = "abc"
#> [1] "abc"

# The second argument lists the characters to remove: here '-'.
sas_f_compress("a-b-c", "-")  # compress('a-b-c','-') = "abc"
#> [1] "abc"

# The list '' is one blank and modifier 'd' adds the digits: blanks and digits
# are removed.
sas_f_compress("a1 b2", "", "d")  # compress('a1 b2','','d') = "ab"
#> [1] "a b"

# Modifier 'k' keeps instead of removing: the listed blank and the digits ('d')
# are kept.
sas_f_compress("a1 b2", "", "kd")  # compress('a1 b2','','kd') = "1 2"
#> [1] "12"

# Modifier 'i' ignores case: removing 'a' also removes 'A'.
sas_f_compress("ABC abc", "a", "i")  # compress('ABC abc','a','i') = "BC bc"
#> [1] "BC bc"
```
