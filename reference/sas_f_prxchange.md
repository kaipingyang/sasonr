# SAS `PRXCHANGE(perl-regular-expression, times, source)`

Substitutes with a Perl regular expression. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - re:
    
    Pattern identifier from PRXPARSE or a Perl substitution string
    's/pattern/replacement/' (capture groups referenced as $1, $2).

  - times:
    
    Number of matches to replace; -1 replaces all matches.

  - x:
    
    Character source string.

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
`sas_f_missing()`, `sas_f_nvalid()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# PRXCHANGE: each call is explained above it and followed by its result

# Times -1 applies the Perl substitution s/a/X/ to every match.
sas_f_prxchange("s/a/X/", -1, "banana")  # prxchange('s/a/X/', -1, 'banana') = "bXnXnX"
#> [1] "bXnXnX"

# Times 1 replaces only the first match.
sas_f_prxchange("s/a/X/", 1, "banana")  # prxchange('s/a/X/', 1, 'banana') = "bXnana"
#> [1] "bXnana"

# The capture groups $1 and $2 swap the two words.
sas_f_prxchange("s/(\\w+) (\\w+)/$2 $1/", -1, "hello world")  # prxchange('s/(\w+) (\w+)/$2 $1/', -1, 'hello world') = "world hello"
#> [1] "world hello"

# The i option after the pattern makes matching case-insensitive.
sas_f_prxchange("s/x/y/i", -1, "XxX")  # prxchange('s/x/y/i', -1, 'XxX') = "yyy"
#> [1] "yyy"
```
