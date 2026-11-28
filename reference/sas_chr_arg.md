# Check a character argument of a SAS function for bytes outside ASCII

Used by translated code: a function other than the case and length
functions with an argument that has a byte outside ASCII goes through
the strict gate (?sasonr\_strict). Other arguments are returned
unchanged.

## Usage

``` r
sas_chr_arg(x, .name, .line = NULL, .exact = TRUE)
```

## Arguments

  - x:
    
    Argument value.

  - .name:
    
    Function name.

  - .line:
    
    Line of the call.

  - .exact:
    
    `FALSE` in vectorised code (the step is re-run row by row).
