# PROC UNIVARIATE

``` r
library(sasonr)
```

PROC UNIVARIATE describes the distribution of numeric variables:
moments, basic measures of location and variability, tests for location,
quantiles and extreme observations.

``` r
res <- sas_run("
options nocenter;
data vitals;
  input sbp @@;
  datalines;
128 124 135 131 118 121 140 126 133 119
;
run;
proc univariate data=vitals;
  var sbp;
run;
")
cat(res$listing, sep = "\n")
#> The UNIVARIATE Procedure
#> Variable:  sbp
#> 
#>                             Moments
#> 
#> N                          10    Sum Weights                 10
#> Mean                    127.5    Sum Observations          1275
#> Std Deviation      7.26100697    Variance            52.7222222
#> Skewness           0.28734399    Kurtosis            -0.8884838
#> Uncorrected SS         163037    Corrected SS             474.5
#> Coeff Variation    5.69490743    Std Error Mean      2.29613201
#> 
#> 
#>               Basic Statistical Measures
#> 
#>     Location                    Variability
#> 
#> Mean     127.5000     Std Deviation            7.26101
#> Median   127.0000     Variance                52.72222
#> Mode        .         Range                   22.00000
#>                       Interquartile Range     12.00000
#> 
#> 
#>            Tests for Location: Mu0=0
#> 
#> Test           -Statistic-    -----p Value------
#> 
#> Student's t    t  55.52817    Pr > |t|    <.0001
#> Sign           M         5    Pr >= |M|   0.0020
#> Signed Rank    S      27.5    Pr >= |S|   0.0020
#> 
#> 
#> Quantiles (Definition 5)
#> 
#> Level         Quantile
#> 
#> 100% Max         140.0
#> 99%              140.0
#> 95%              140.0
#> 90%              137.5
#> 75% Q3           133.0
#> 50% Median       127.0
#> 25% Q1           121.0
#> 10%              118.5
#> 5%               118.0
#> 1%               118.0
#> 0% Min           118.0
#> 
#> 
#>         Extreme Observations
#> 
#> ----Lowest----        ----Highest---
#> 
#> Value      Obs        Value      Obs
#> 
#>   118        5          128        1
#>   119       10          131        4
#>   121        6          133        9
#>   124        2          135        3
#>   126        8          140        7
```
