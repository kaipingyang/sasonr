# Parse a SAS program

Parse a SAS program

## Usage

``` r
sas_parse(code, resolved = FALSE, sql_open = FALSE)
```

## Arguments

  - code:
    
    SAS source code, or a token tibble from `sas_lex()`.

  - resolved:
    
    `TRUE` when the text has already been through the macro processor:
    `&`/`%` left inside double-quoted strings are then plain text.

  - sql\_open:
    
    Internal: `TRUE` when a PROC SQL step is already open (statements
    before QUIT belong to it).

## Value

A `sasonr_program`: a list with element `steps`.

## Examples

``` r
p <- sas_parse("data b; set a; if x > 1; run;")
str(p$steps[[1]]$stmts, max.level = 1)
#> List of 2
#>  $ :List of 5
#>  $ :List of 3
```
