# The input table of a match-merge (MERGE + BY): one row per row of the plan

One row per row of `sas_merge_plan()`, with the values the program data
vector holds after the MERGE read of that row: at the first row of a BY
group every variable starts missing; each data set that has a row writes
its variables in MERGE order (a later data set wins); a data set that
has run out of rows in the group writes nothing, so its variables keep
their value from the previous row of the group. IN= variables and
FIRST./LAST. flags come from the plan. Only valid when the step does not
change a variable read by MERGE (the row-wise path carries the changed
value).

## Usage

``` r
sas_merge_input(dfs, by, descending = rep(FALSE, length(by)), in_vars = NULL)
```
