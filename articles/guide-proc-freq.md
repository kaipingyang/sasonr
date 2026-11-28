# PROC FREQ

``` r
library(sasonr)
```

PROC FREQ counts the values of variables: one-way tables with
percentages and cumulative columns, two-way tables with row and column
percentages, and the chi-square tests of a two-way table.

``` r
prog <- "
options nocenter;
data ae;
  input subj $ arm $ sev $;
  datalines;
S01 A MILD
S02 A MILD
S03 A SEVERE
S04 B MILD
S05 B MODERATE
S06 B SEVERE
S07 B SEVERE
;
run;
"
```

## One-way tables

``` r
res <- sas_run(paste(prog, "proc freq data=ae; tables sev; run;"))
cat(res$listing, sep = "\n")
#> The FREQ Procedure
#> 
#>                                      Cumulative    Cumulative
#> sev         Frequency     Percent     Frequency      Percent
#> -------------------------------------------------------------
#> MILD               3       42.86             3        42.86
#> MODERATE           1       14.29             4        57.14
#> SEVERE             3       42.86             7       100.00
```

## Two-way tables

``` r
res <- sas_run(paste(prog, "proc freq data=ae; tables arm*sev; run;"))
cat(res$listing, sep = "\n")
#> The FREQ Procedure
#> 
#> Table of arm by sev
#> 
#> arm       sev
#> Frequency|
#> Percent  |
#> Row Pct  |
#> Col Pct  |MILD    |MODERATE|SEVERE  |  Total
#> ---------+--------+--------+--------+
#> A        |      2 |      0 |      1 |      3
#>          |  28.57 |   0.00 |  14.29 |  42.86
#>          |  66.67 |   0.00 |  33.33 |
#>          |  66.67 |   0.00 |  33.33 |
#> ---------+--------+--------+--------+
#> B        |      1 |      1 |      2 |      4
#>          |  14.29 |  14.29 |  28.57 |  57.14
#>          |  25.00 |  25.00 |  50.00 |
#>          |  33.33 | 100.00 |  66.67 |
#> ---------+--------+--------+--------+
#> Total           3        1        3        7
#>             42.86    14.29    42.86   100.00
```

## OUT= data sets

`OUT=` on the TABLES statement writes the counts and percentages as a
data set.

``` r
res <- sas_run(paste(prog, "proc freq data=ae noprint; tables arm*sev / out=counts; run;"))
res$work$counts
#> # A tibble: 5 × 4
#>   arm   sev      count percent
#>   <chr> <chr>    <dbl>   <dbl>
#> 1 A     MILD         2    28.6
#> 2 A     SEVERE       1    14.3
#> 3 B     MILD         1    14.3
#> 4 B     MODERATE     1    14.3
#> 5 B     SEVERE       2    28.6
```

## On a data frame

`sas_freq_df()` runs a one-way table on a data frame.

``` r
df <- data.frame(sev = c("MILD", "MILD", "SEVERE"))
as.data.frame(sas_freq_df(df, "sev"))
#>      sev Frequency  Percent CumFrequency CumPercent
#> 1 MILD           2 66.66667            2   66.66667
#> 2 SEVERE         1 33.33333            3  100.00000
```
