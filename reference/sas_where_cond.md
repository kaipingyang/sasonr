# Value of a WHERE condition

sasonr evaluates a WHERE condition without data-error NOTEs (missing
values generated, invalid arguments of functions and of `**`, invalid
numeric data, mathematical operations that could not be performed) and
without setting `_ERROR_`: those records of the evaluation are dropped.
An ERROR message of a function and a division by zero in the condition
are not supported.

## Usage

``` r
sas_where_cond(x)
```
