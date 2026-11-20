# Count "missing values were generated" for one operation site

Count "missing values were generated" for one operation site

## Usage

``` r
sas_mv_count(.at, ..., .exact = TRUE)
```

## Arguments

  - .at:
    
    `c(line, column)` of the operator or function name.

  - ...:
    
    The operands; rows where any operand is missing are counted.

  - .exact:
    
    `FALSE` when vectorised code may over-count (the step is re-run row
    by row).
