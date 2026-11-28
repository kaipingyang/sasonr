# PROC SORT and PROC TRANSPOSE

``` r
library(sasonr)
```

PROC SORT and PROC TRANSPOSE reshape data sets; they write no listing,
only the data set and the NOTEs of the log.

``` r
prog <- "
data labs;
  input subj $ visit param $ aval;
  datalines;
S02 1 ALT 31
S01 2 ALT 28
S01 1 ALT 25
S02 2 ALT .
S01 1 AST 22
S02 1 AST 30
;
run;
"
```

## PROC SORT

`BY` sorts in ascending order; `DESCENDING` reverses the variable after
it. Missing values sort before every number. `OUT=` writes a new data
set and leaves the input as it is.

``` r
res <- sas_run(paste(prog, "
proc sort data=labs out=bysubj;
  by subj descending visit;
run;"))
res$work$bysubj
#> # A tibble: 6 × 4
#>   subj  visit param  aval
#>   <chr> <dbl> <chr> <dbl>
#> 1 S01       2 ALT      28
#> 2 S01       1 ALT      25
#> 3 S01       1 AST      22
#> 4 S02       2 ALT      NA
#> 5 S02       1 ALT      31
#> 6 S02       1 AST      30
cat(sas_log(res), sep = "\n")
#> 1          
#> 2          data labs;
#> 3            input subj $ visit param $ aval;
#> 4            datalines;
#> 
#> NOTE: The data set WORK.LABS has 6 observations and 4 variables.
#> 11         ;
#> 12         run;
#> 13         
#> 14         proc sort data=labs out=bysubj;
#> 15           by subj descending visit;
#> 16         run;
#> 
#> NOTE: There were 6 observations read from the data set WORK.LABS.
#> NOTE: The data set WORK.BYSUBJ has 6 observations and 4 variables.
```

`NODUPKEY` keeps the first observation of each BY group and writes how
many it deleted.

``` r
res <- sas_run(paste(prog, "
proc sort data=labs out=first nodupkey;
  by subj;
run;"))
res$work$first
#> # A tibble: 2 × 4
#>   subj  visit param  aval
#>   <chr> <dbl> <chr> <dbl>
#> 1 S01       2 ALT      28
#> 2 S02       1 ALT      31
```

`sas_sort_df()` sorts a data frame the same way without a program.

``` r
df <- data.frame(subj = c("S02", "S01", "S01"), aval = c(31, NA, 25))
sas_sort_df(df, by = c("subj", "aval"))
#> # A tibble: 3 × 2
#>   subj   aval
#>   <chr> <dbl>
#> 1 S01      NA
#> 2 S01      25
#> 3 S02      31
```

## PROC TRANSPOSE

`ID` names the new columns, `BY` keeps one row per group, `VAR` chooses
the variables to transpose; `_NAME_` holds the name of the transposed
variable.

``` r
res <- sas_run(paste(prog, "
proc sort data=labs; by subj visit; run;
proc transpose data=labs out=wide;
  by subj visit;
  id param;
  var aval;
run;"))
res$work$wide
#> # A tibble: 4 × 5
#>   subj  visit `_name_`   alt   ast
#>   <chr> <dbl> <chr>    <dbl> <dbl>
#> 1 S01       1 aval        25    22
#> 2 S01       2 aval        28    NA
#> 3 S02       1 aval        31    30
#> 4 S02       2 aval        NA    NA
```

`sas_transpose_df()` runs the same transposition on a data frame.

``` r
df <- data.frame(subj = c("S01", "S01", "S02"), param = c("ALT", "AST", "ALT"), aval = c(25, 22, 31))
sas_transpose_df(df, var = "aval", by = "subj", id = "param")
#> # A tibble: 2 × 4
#>   subj  `_name_`   alt   ast
#>   <chr> <chr>    <dbl> <dbl>
#> 1 S01   aval        25    22
#> 2 S02   aval        31    NA
```
