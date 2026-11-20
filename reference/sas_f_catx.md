# SAS `CATX(delimiter, item-1 <, ..., item-n>)`

Strips items and joins the non-blank ones with a delimiter. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - sep:
    
    Delimiter string inserted between consecutive nonblank items, used
    as given (its blanks are kept).

  - ...:
    
    Character or numeric items to concatenate. Numeric items are
    converted with the BESTw. format and leading blanks are removed; a
    missing number becomes `.`. Leading and trailing blanks are removed
    from every item, and blank or missing items are skipped together
    with their delimiter.

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
`sas_f_byte()`, `sas_f_cat()`, `sas_f_cats()`, `sas_f_catt()`,
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
# CATX: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Join the items with the delimiter '-'.
sas_f_catx("-", "a", "b", "c")  # catx('-','a','b','c') = "a-b-c"
#> [1] "a-b-c"

# Leading and trailing blanks of every item are removed before joining.
sas_f_catx("-", "  a ", "b  ", " c")  # catx('-','  a ','b  ',' c') = "a-b-c"
#> [1] "a-b-c"

# A blank item is skipped (no doubled delimiter).
sas_f_catx("-", "a", " ", "c")  # catx('-','a',' ','c') = "a-c"
#> [1] "a-c"

# A numeric missing item is not skipped: it is written as '.'.
sas_f_catx("-", "a", NA, "c")  # catx('-','a',.,'c') = "a-.-c"
#> [1] "a-NA-c"
```
