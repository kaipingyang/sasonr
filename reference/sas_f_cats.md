# SAS `CATS(item-1 <, ..., item-n>)`

Concatenates after removing leading and trailing blanks. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - ...:
    
    Character or numeric items to concatenate. Numeric items are
    converted with the BESTw. format and leading blanks are removed; a
    missing number becomes `.`. Leading and trailing blanks are removed
    from every item.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

  - .len:
    
    Internal: length of the result buffer (CATX).

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
`sas_f_byte()`, `sas_f_cat()`, `sas_f_catt()`, `sas_f_catx()`,
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
# CATS: each call is explained above it and followed by its result

# CATS removes leading and trailing blanks from every argument before
# concatenating.
sas_f_cats(" a ", "b ", " c")  # cats(' a ', 'b ', ' c') = "abc"
#> [1] "abc"

# The number 12.5 is converted to character without blanks, giving 'x12.5'.
sas_f_cats("x", 12.5)  # cats('x', 12.5) = "x12.5"
#> [1] "x12.5"

# Blank strings contribute nothing, so the result is blank.
sas_f_cats(" ", " ")  # cats('', '') = ""
#> [1] ""

# A missing numeric argument is converted to '.', giving 'a.b'.
sas_f_cats("a", NA_real_, "b")  # cats('a', ., 'b') = "a.b"
#> [1] "a.b"
```
