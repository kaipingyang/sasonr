# Data-level PROC MEANS, PROC TABULATE, PROC FREQ and PROC TRANSPOSE

Run one procedure on a data frame without writing a SAS program.
`sas_means_df()`, `sas_tabulate_df()` and `sas_freq_df()` return the ODS
table of the step (a `sasonr_ods_table`, a tibble with the SAS table
attributes), or a list of them when there are several (one per BY group
or table request); `sas_freq_df()` supports one-way (OneWayFreqs) and
two-way (CrossTabFreqs) tables without statistics options so far;
`sas_transpose_df()` returns the output data set. The arguments are read
by the same parser and specification builder as the PROC step of a
program, and the computation is the one `sas_run()` calls for that step,
so the values and the column attributes are the same. `sas_log()` gives
the WARNINGs and NOTEs of the computation and `sas_html()` the HTML
document of the tables (both are S3 generics with methods for these
results).

## Usage

``` r
sas_means_df(
  data,
  var = NULL,
  class = NULL,
  by = NULL,
  stats = NULL,
  options = ""
)

sas_tabulate_df(data, table, class = NULL, var = NULL, by = NULL, options = "")

sas_freq_df(data, tables, by = NULL, weight = NULL, options = "")

sas_transpose_df(data, var = NULL, by = NULL, id = NULL, options = "")
```

## Arguments

  - data:
    
    A data frame.

  - var, class, by, id:
    
    Variable names (character vectors).

  - stats:
    
    Statistic keywords of the PROC MEANS statement (`"n"`, `"mean"`,
    `"std"`, ...); `NULL` gives the default statistics.

  - options:
    
    Further options of the PROC statement, as SAS text.

  - table:
    
    The TABLE statement of PROC TABULATE, as SAS text (`"grp all, x*(n
    mean)"`).

  - tables:
    
    The TABLES statement of PROC FREQ, as SAS text (`"grp sex"`, `"grp /
    nocum"`).

  - weight:
    
    The WEIGHT variable (one name).

## Value

See Description.

## Examples

``` r
d <- data.frame(grp = c("a", "a", "b"), x = c(1, 2, 4))
sas_means_df(d, var = "x", class = "grp", stats = c("n", "mean"))
#> <sasonr_ods_table> Summary (Summary statistics)
#>   path:     Means.Summary
#>   proc:     Means, template base.summary
#>   columns:
#>     grp [class]
#>     NObs [stat] "N Obs"
#>     x_N [stat] "N"
#>     x_Mean [stat] "Mean"
#>   header 1: Analysis Variable : x 
#>   header 2: grp | N Obs | N | Mean
#>   rows:     2
as.data.frame(sas_tabulate_df(d, "grp all, x*(n sum)", class = "grp", var = "x"))
#>    grp _C1_ _C2_
#> 1    a    2    3
#> 2    b    1    4
#> 3 <NA>    3    7
as.data.frame(sas_freq_df(d, "grp"))
#>   grp Frequency  Percent CumFrequency CumPercent
#> 1   a         2 66.66667            2   66.66667
#> 2   b         1 33.33333            3  100.00000
sas_transpose_df(d, var = "x")
#> # A tibble: 1 × 4
#>   `_name_`  col1  col2  col3
#>   <chr>    <dbl> <dbl> <dbl>
#> 1 x            1     2     4
```
