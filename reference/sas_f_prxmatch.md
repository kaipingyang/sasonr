# SAS `PRXMATCH(regular-expression-id | perl-regular-expression, source)`

Position of the first match of a Perl regular expression. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - re:
    
    Pattern identifier returned by PRXPARSE, or a Perl regular
    expression string in delimiters (e.g. '/b+/').

  - x:
    
    Character source string to search; the result is the position of the
    first match, or 0.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

  - session:
    
    A session from `sas_session()` that holds the PRXPARSE ids, for
    calls outside `sas_run()` (inside a run, the running session is
    used).

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
`sas_f_missing()`, `sas_f_nvalid()`, `sas_f_prxchange()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# PRXMATCH: each call is explained above it and followed by its result

# PRXMATCH returns the position of the first match of the Perl regular
# expression /b+/: 2.
sas_f_prxmatch("/b+/", "abbc")  # prxmatch('/b+/', 'abbc') = 2
#> [1] 2

# There is no match, so the result is 0.
sas_f_prxmatch("/z/", "abc")  # prxmatch('/z/', 'abc') = 0
#> [1] 0

# The i option makes matching case-insensitive, so /B/i matches 'b' at position
# 2.
sas_f_prxmatch("/B/i", "abc")  # prxmatch('/B/i', 'abc') = 2
#> [1] 2

# ^ anchors the match at the start, and the string starts with a blank, so
# there is no match.
sas_f_prxmatch("/^a/", " abc")  # prxmatch('/^a/', ' abc') = 0
#> [1] 0
```
