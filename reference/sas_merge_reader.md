# A MERGE statement's reader: the match-merge plan of the data sets `dfs`

`reset`: the values its variables take at the start of a BY group;
`check`: the line of a MERGE that may stop before its last observation
(its read counts are known only when it read all of them).

## Usage

``` r
sas_merge_reader(
  dfs,
  by,
  descending,
  reset,
  in_vars = rep(NA_character_, length(dfs)),
  check = NULL,
  notes = NULL
)
```
