# Runtime helper: PROC SQL LIKE operator.

PROC SQL LIKE operator.

## Arguments

  - x:
    
    Character value tested by LIKE; trailing blanks are ignored.

  - pattern:
    
    LIKE pattern: percent sign matches any sequence of characters,
    underscore matches exactly one character, other characters match
    themselves.

  - escape:
    
    Optional single escape character (ESCAPE clause); the character
    after it in `pattern` is matched literally.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Used by translated code; not a SAS function.

## See also

Other internal functions: `sas_f_cvt_c2n()`, `sas_f_cvt_n2c()`,
`sas_f_sqlcase()`, `sas_f_sqlinlist()`, `sas_f_sqlmonotonic()`,
`sas_f_sqlquant()`

## Examples

``` r
# SQLLIKE: each call is explained above it and followed by its result

# In a LIKE pattern percent matches any sequence of characters, so 'a%' matches
# strings that start with 'a'.
as.numeric(sas_f_sqllike("abc", "a%"))  # 'abc' like 'a%' = 1
#> [1] 1

# Underscore matches exactly one character, so '_b_' matches any 3-character
# string with 'b' in the middle.
as.numeric(sas_f_sqllike("abc", "_b_"))  # 'abc' like '_b_' = 1
#> [1] 1

# 'abc' does not start with 'b', so the result is 0.
as.numeric(sas_f_sqllike("abc", "b%"))  # 'abc' like 'b%' = 0
#> [1] 0

# ESCAPE '\' makes the following percent sign literal, so 'a\%c' matches
# exactly 'a%c'.
as.numeric(sas_f_sqllike("a%c", "a\\%c", "\\"))  # 'a%c' like 'a\%c' escape '\' = 1
#> [1] 1
```
