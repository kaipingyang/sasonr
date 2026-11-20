# Write WORK data sets as CSV files with an attribute table

Writes `<MEMNAME>.csv` for each data set of a `sas_run()` result (column
names as stored, formatted values, blank missing values) and
`__attr.csv` with columns memname, varnum, name, type, length, format,
informat, label.

## Usage

``` r
sas_export_work(res, dir, values = TRUE)
```

## Arguments

  - res:
    
    A `sasonr_result`.

  - dir:
    
    Output directory (created if needed).

  - values:
    
    Write the `<MEMNAME>.csv` value files (`FALSE`: only the attribute
    table).

## Examples

``` r
res <- sas_run("data a; x = 1.5; y = 'b'; run;")
out <- file.path(tempdir(), "work_csv")
sas_export_work(res, out)
readLines(file.path(out, "A.csv"))
#> [1] "x,y"   "1.5,b"
```
