# Build the observation plan of a match-merge (MERGE + BY)

A BY group in which more than one data set has several rows queues the
log NOTE about repeats of BY values. Repeats in several data sets that
never share a BY group write no NOTE.

## Usage

``` r
sas_merge_plan(dfs, by, descending = rep(FALSE, length(by)), in_vars = NULL)
```
