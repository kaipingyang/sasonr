# End-of-iteration PDV dump when `_ERROR_` is set

Returns the log lines: the input buffer (with the RULE line the first
time in a step) when an INPUT record is current, then `var=value ...
_ERROR_=1 _N_=n`. At most 20 dumps per step (ERRORS=20).

## Usage

``` r
sas_iter_dump(pdv, spec, inf = NULL)
```
