# Analyse a parsed DATA step

Analyse a parsed DATA step

## Usage

``` r
sas_analyze(step, meta = list(), level = "auto")
```

## Arguments

  - step:
    
    One element of `sas_parse(code)$steps`.

  - meta:
    
    Named list of input data set metadata (see `sas_translate()`).

  - level:
    
    `"auto"` or a forced level (`"L3"`).

## Value

A list describing the PDV, annotated statements, and the level.

## Examples

``` r
p <- sas_parse("data b; do i = 1 to 3; s + i; output; end; run;")
a <- sas_analyze(p$steps[[1]])
a$level
#> [1] "L3"
```
