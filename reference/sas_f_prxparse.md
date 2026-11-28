# SAS `PRXPARSE(perl-regular-expression)`

Compiles a Perl regular expression. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - re:
    
    Perl regular expression in delimiters, e.g. '/a+/i' or a
    substitution 's/a/b/'; options such as i follow the closing
    delimiter.

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
`sas_f_prxmatch()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`

## Examples

``` r
# PRXPARSE: each call is explained above it and followed by its result

# Outside sas_run() the PRXPARSE ids are kept in an explicit session.
s <- sas_session()

# PRXPARSE compiles a Perl regular expression and returns a numeric pattern
# identifier; the first one is 1.
sas_f_prxparse("/a+/", session = s)  # prxparse('/a+/') = 1
#> [1] 1

# Each further PRXPARSE call gets the next identifier, 2.
sas_f_prxparse("/b/i", session = s)  # prxparse('/b/i') = 2
#> [1] 2
```
