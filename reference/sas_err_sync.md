# Copy a pending data error into `_ERROR_` (and its NOTEs into the log)

Called from generated L3 code after statements that can raise data
errors.

## Usage

``` r
sas_err_sync(pdv)
```
