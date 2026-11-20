# SAS `FINDW(string, word <, delimiters> <, modifiers> <, start>)`

Position (or number) of a word. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string. A numeric value is first converted to character
    with the BEST12. format. This is the string searched.

  - word:
    
    Character word to find; leading and trailing blanks are ignored. It
    must be delimited on both sides by delimiters or the string ends.

  - dlm:
    
    Optional delimiter characters. Default: blank \! $ percent-sign & (
    ) \* + , - . / ; \< ^ |.

  - a4:
    
    Either the modifiers (if character) or the start position (if
    numeric). Modifiers: i ignores case; e returns the word number
    instead of the character position; t trims trailing blanks; a, d, l,
    u, s, p, n, f, x, c, h add the matching character classes to the
    delimiters; k makes the delimiters the characters NOT listed.
    Default blank (no modifiers). If numeric, it is the start position
    (see `a5`).

  - a5:
    
    Start position (numeric), used when `a4` holds modifiers. A positive
    value searches rightwards from that position; a negative value
    searches leftwards from position |start|. Default 1.

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
`sas_f_dequote()`, `sas_f_find()`, `sas_f_findc()`, `sas_f_first()`,
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
# FINDW: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# FINDW returns the position of the word 'cat', delimited by blanks: 5.
sas_f_findw("the cat sat", "cat")  # findw('the cat sat', 'cat') = 5
#> [1] 5

# 'at' occurs only inside other words, never as a whole word, so the result is
# 0.
sas_f_findw("the cat sat", "at")  # findw('the cat sat', 'at') = 0
#> [1] 0

# With ',' as the delimiter, the word 'b' starts at position 3.
sas_f_findw("a,b,c", "b", ",")  # findw('a,b,c', 'b', ',') = 3
#> [1] 3

# Modifier i ignores case, so 'cat' matches 'Cat' at position 5.
sas_f_findw("The Cat", "cat", " ", "i")  # findw('The Cat', 'cat', ' ', 'i') = 5
#> [1] 5

# Modifier e returns the word number instead of the character position: 'sat'
# is the 3rd word.
sas_f_findw("the cat sat", "sat", " ", "e")  # findw('the cat sat', 'sat', ' ', 'e') = 3
#> [1] 3
```
