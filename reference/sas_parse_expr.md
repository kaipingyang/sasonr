# Parse a single SAS expression

Parse a single SAS expression

## Usage

``` r
sas_parse_expr(text, where = FALSE)
```

## Arguments

  - text:
    
    SAS expression source.

  - where:
    
    Parse in WHERE context (enables BETWEEN, CONTAINS, IS MISSING).

## Value

Expression AST (nested lists with a `node` field).

## Examples

``` r
e <- sas_parse_expr("round(x * 2, 0.1) > 3")
e$node
#> [1] "compare"
sas_parse_expr("age between 18 and 65", where = TRUE)$node
#> [1] "between"
```
