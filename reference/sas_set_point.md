# Execute a SET statement with POINT=: observation `k` into the PDV

An invalid observation number (missing, not an integer, out of range)
sets `_ERROR_` and leaves the PDV (measured). A POINT= reader writes no
NOTE about the observations it read.

## Usage

``` r
sas_set_point(r, pdv, k)
```
