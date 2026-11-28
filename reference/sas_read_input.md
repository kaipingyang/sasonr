# Read all observations of a step whose only reader is one INPUT statement

Used by vectorised (L1/L2) code: each iteration executes the INPUT
statement once; the variables it reads start missing in every iteration.

## Usage

``` r
sas_read_input(inf, spec, end_var = NULL)
```
