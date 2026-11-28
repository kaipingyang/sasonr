# SAS `NVALID(string <, validvarname>)`

1 if the string is a valid SAS variable name. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Character string to check as a SAS variable name; trailing blanks
    are ignored.

  - opt:
    
    Optional naming rules: 'V7' (default) requires a letter or
    underscore first, then letters, digits or underscores, up to 32
    characters; 'ANY' accepts any 1 to 32 characters; 'UPCASE' is V7
    with uppercase letters only; 'NLITERAL' accepts V7 names and name
    literals such as 'my var'n.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_exist()`, `sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`,
`sas_f_missing()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# NVALID: each call is explained above it and followed by its result

# NVALID returns 1 if the string is a valid SAS variable name; under the
# default V7 rules 'abc' is valid.
sas_f_nvalid("abc")  # nvalid('abc') = 1
#> [1] 1

# A V7 name cannot start with a digit, so the result is 0.
sas_f_nvalid("1abc")  # nvalid('1abc') = 0
#> [1] 0

# V7 names cannot contain blanks, so 'my var' is not valid.
sas_f_nvalid("my var", "v7")  # nvalid('my var', 'v7') = 0
#> [1] 0

# Option 'any' accepts any characters (1 to 32 bytes), including blanks.
sas_f_nvalid("my var", "any")  # nvalid('my var', 'any') = 1
#> [1] 1

# Option 'nliteral' accepts V7 names as well as name literals such as 'my
# var'n; '_x1' is a V7 name.
sas_f_nvalid("_x1", "nliteral")  # nvalid('_x1', 'nliteral') = 1
#> [1] 1
```
