# PROC TABULATE

``` r
library(sasonr)
```

PROC TABULATE builds a table from CLASS and VAR variables and a TABLE
statement: the page, row and column dimensions, crossed with `*`,
concatenated with blanks, and `ALL` for totals. sasonr writes the table
in the listing with its box lines, as an ODS table, and in the HTML or
RTF document.

``` r
prog <- "
options nocenter;
data trial;
  input arm $ sex $ age;
  datalines;
A F 34
A M 41
A F 29
B M 52
B F 47
B M 38
;
run;
"
```

## Counts by two classes

``` r
res <- sas_run(paste(prog, "
proc tabulate data=trial;
  class arm sex;
  table sex all, arm*n;
run;"))
cat(res$listing, sep = "\n")
#> -----------------------------------------------------------
#> |                               |           arm           |
#> |                               |-------------------------|
#> |                               |     A      |     B      |
#> |                               |------------+------------|
#> |                               |     N      |     N      |
#> |-------------------------------+------------+------------|
#> |sex                            |            |            |
#> |-------------------------------|            |            |
#> |F                              |        2.00|        1.00|
#> |-------------------------------+------------+------------|
#> |M                              |        1.00|        2.00|
#> |-------------------------------+------------+------------|
#> |All                            |        3.00|        3.00|
#> -----------------------------------------------------------
```

## Statistics of an analysis variable

``` r
res <- sas_run(paste(prog, "
proc tabulate data=trial;
  class arm;
  var age;
  table arm all, age*(n mean min max);
run;"))
cat(res$listing, sep = "\n")
#> -------------------------------------------------------------------------------------
#> |                               |                        age                        |
#> |                               |---------------------------------------------------|
#> |                               |     N      |    Mean    |    Min     |    Max     |
#> |-------------------------------+------------+------------+------------+------------|
#> |arm                            |            |            |            |            |
#> |-------------------------------|            |            |            |            |
#> |A                              |        3.00|       34.67|       29.00|       41.00|
#> |-------------------------------+------------+------------+------------+------------|
#> |B                              |        3.00|       45.67|       38.00|       52.00|
#> |-------------------------------+------------+------------+------------+------------|
#> |All                            |        6.00|       40.17|       29.00|       52.00|
#> -------------------------------------------------------------------------------------
```

## On a data frame

`sas_tabulate_df()` takes the TABLE statement as text.

``` r
df <- data.frame(arm = c("A", "A", "B"), age = c(34, 41, 52))
as.data.frame(sas_tabulate_df(df, "arm all, age*(n mean)", class = "arm", var = "age"))
#>    arm _C1_     _C2_
#> 1    A    2 37.50000
#> 2    B    1 52.00000
#> 3 <NA>    3 42.33333
```
