# Show tidylog messages for an expression

Runs `expr` with `options(tidylog.display = list(message))`, so that the
dplyr verbs of a script from `sas_script(tidy = TRUE)` report what they
did as messages, and restores the option afterwards.

## Usage

``` r
sasonr_tidy_log(expr)
```

## Arguments

  - expr:
    
    Code to run.

## Value

The value of `expr`.

## Examples

``` r
sasonr_tidy_log(1 + 1)
#> [1] 2
```
