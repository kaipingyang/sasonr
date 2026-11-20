# Forward fill for a retained variable (vectorised carry-forward)

Row r takes the value of the last assignment k whose flag `sets[[k]]` is
true on that row, otherwise the result of row r - 1 (or `init`).

## Usage

``` r
sas_carry(values, sets, init)
```
