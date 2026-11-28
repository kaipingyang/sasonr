# An UPDATE statement's reader: the observations of the step

Per BY group, the first master observation with the transaction
observations of the group applied in order, then the group's other
master observations unchanged; a BY group only in the transaction data
set starts from missing values (measured). A missing value in a
transaction does not replace a value (`missing = TRUE`:
UPDATEMODE=NOMISSINGCHECK, it does); the special missing values .A-.Z
do; `._` is not supported.

## Usage

``` r
sas_update_reader(dfs, by, descending, missing = FALSE, line = NULL)
```
