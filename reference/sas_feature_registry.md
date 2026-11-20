# Feature registry (functions, formats and informats) with the status of each feature

Feature registry (functions, formats and informats) with the status of
each feature

## Usage

``` r
sas_feature_registry(kind = c("functions", "formats"))
```

## Arguments

  - kind:
    
    `"functions"` or `"formats"`.

## Value

A data frame, one row per function or per format/informat: the name, the
kind, the status (`supported`, `partial`, `approximate`, `unsupported`,
`not_comparable`; see
[sasonr\_strict](https://kaipingyang.github.io/sasonr/reference/sasonr_strict.md)),
for functions the contexts in which they are supported, and the argument
counts, argument types and default widths of the SAS 9.4 inventory.
