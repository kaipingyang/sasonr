# PROC CONTENTS, PROC DATASETS, PROC APPEND and PROC DELETE

``` r
library(sasonr)
```

These procedures describe and manage the members of a library (WORK
here).

``` r
prog <- "
options nocenter;
data adsl;
  length subj $ 6;
  input subj $ age;
  label age = 'Age (years)';
  format age 3.;
  datalines;
S01 34
S02 41
;
run;
data extra;
  length subj $ 6;
  input subj $ age;
  datalines;
S03 29
;
run;
"
```

## PROC CONTENTS

The variables of a data set with their type, length, format and label.

``` r
res <- sas_run(paste(prog, "proc contents data=adsl; run;"))
cat(res$listing, sep = "\n")
#> The CONTENTS Procedure
#> 
#> Data Set Name        WORK.ADSL                                                Observations          2
#> Member Type          DATA                                                     Variables             2
#> Engine               V9                                                       Indexes               0
#> Created              11/28/2026 10:13:25                                      Observation Length    16
#> Last Modified        11/28/2026 10:13:25                                      Deleted Observations  0
#> Protection                                                                    Compressed            NO
#> Data Set Type                                                                 Sorted                NO
#> Label                                                                                               
#> Data Representation  SOLARIS_X86_64, LINUX_X86_64, ALPHA_TRU64, LINUX_IA64
#> Encoding             utf-8  Unicode (UTF-8)
#> 
#>      Alphabetic List of Variables and Attributes
#> 
#> #    Variable    Type    Len    Format    Label
#> 2    age         Num       8    3.        Age (years)
#> 1    subj        Char      6
```

## PROC APPEND

`BASE=` gets the observations of `DATA=`.

``` r
res <- sas_run(paste(prog, "proc append base=adsl data=extra; run;"))
res$work$adsl
#> # A tibble: 3 × 2
#>   subj    age
#>   <chr> <dbl>
#> 1 S01      34
#> 2 S02      41
#> 3 S03      29
```

## PROC DATASETS and PROC DELETE

`DELETE` removes members; the log lists what was deleted.

``` r
res <- sas_run(paste(prog, "proc datasets library=work nolist; delete extra; run; quit;"))
names(res$work)
#> [1] "adsl"
res <- sas_run(paste(prog, "proc delete data=extra; run;"))
names(res$work)
#> [1] "adsl"
```
