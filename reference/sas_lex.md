# Tokenize SAS source code

Tokenize SAS source code

## Usage

``` r
sas_lex(text, lossless = FALSE)
```

## Arguments

  - text:
    
    SAS source (character; multiple elements are joined by newlines).

  - lossless:
    
    If `TRUE`, return the lossless token table instead: every character
    of the source belongs to one row, comments, whitespace and line
    feeds included, so `paste0(tok$text, collapse = "")` gives the
    source back. `sas_lex(text, lossless = TRUE)` returns this token
    table directly; it is the public way to get it.

## Value

A tibble with columns `type`, `value`, `line`, `col`; with `lossless =
TRUE`, a tibble with columns `id`, `kind`, `text`, `value`, `line`,
`col`, `end_line`, `end_col`, where `kind` is a token type or one of
`ws`, `newline`, `comment` (and `error` for characters that do not form
a token).

## Examples

``` r
sas_lex("x = substr(name, 1, 3); /* comment */")
#> # A tibble: 11 × 4
#>    type   value   line   col
#>    <chr>  <chr>  <int> <int>
#>  1 name   x          1     1
#>  2 op     =          1     3
#>  3 name   substr     1     5
#>  4 lparen (          1    11
#>  5 name   name       1    12
#>  6 comma  ,          1    16
#>  7 num    1          1    18
#>  8 comma  ,          1    19
#>  9 num    3          1    21
#> 10 rparen )          1    22
#> 11 semi   ;          1    23
sas_lex("x = 1; /* comment */", lossless = TRUE)
#> # A tibble: 8 × 8
#>      id kind    text            value            line   col end_line end_col
#>   <int> <chr>   <chr>           <chr>           <int> <int>    <int>   <int>
#> 1     1 name    "x"             "x"                 1     1        1       1
#> 2     2 ws      " "             " "                 1     2        1       2
#> 3     3 op      "="             "="                 1     3        1       3
#> 4     4 ws      " "             " "                 1     4        1       4
#> 5     5 num     "1"             "1"                 1     5        1       5
#> 6     6 semi    ";"             ";"                 1     6        1       6
#> 7     7 ws      " "             " "                 1     7        1       7
#> 8     8 comment "/* comment */" "/* comment */"     1     8        1      20
```
