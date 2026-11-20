# SAS `SYMGET(argument)`

Value of a macro variable at run time. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - symtab:
    
    Macro symbol table to look the variable up in (an environment or
    macro session supplied by the generated code; not part of the SAS
    syntax).

  - name:
    
    Character name of the macro variable, not case sensitive; trailing
    blanks are ignored. A blank or invalid name gives a blank result
    with a note in the log.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Reads the macro symbol table of the running SAS program.

## Examples

``` r
# SYMGET: each call is explained above it and followed by its result

# SYMGET returns the value of the macro variable mv, which was set with a LET
# statement.
sas_f_symget(list2env(list(mv = "hello")), "mv")  # symget('mv') = "hello"
#> [1] "hello"

# Macro variable names are not case sensitive.
sas_f_symget(list2env(list(mv = "hello")), "MV")  # symget('MV') = "hello"
#> [1] "hello"

# A name with a leading blank is invalid, so the result is blank (with a note
# in the log).
sas_f_symget(list2env(list(mv = "hello")), " mv ")  # symget(' mv ') = ""
#> Warning: NOTE: Invalid argument to function SYMGET(' mv ').
#> [1] ""
```
