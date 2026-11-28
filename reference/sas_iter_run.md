# Run the statements of an iteration

`expr` is evaluated in the caller's environment (where the PDV is). The
result is how the iteration ended: "ok" (also after RETURN), "delete" or
"stop".

## Usage

``` r
sas_iter_run(expr)
```
