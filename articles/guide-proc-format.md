# PROC FORMAT

``` r
library(sasonr)
```

PROC FORMAT defines user formats: `VALUE` maps values or ranges to
labels, `PICTURE` writes numbers through a template of digit selectors.
The formats go into the session’s format catalogue; PUT, FORMAT
statements and the procedures use them like the built-in formats.

## VALUE formats

``` r
res <- sas_run("
options nocenter;
proc format;
  value agegrp low -< 18 = 'Child'
               18 -< 65  = 'Adult'
               65 - high = 'Senior';
  value $sexf 'F' = 'Female' 'M' = 'Male';
run;
data pts;
  input id sex $ age;
  grp = put(age, agegrp.);
  format sex $sexf.;
  datalines;
1 F 12
2 M 40
3 F 71
;
run;
proc print data=pts noobs; run;
")
cat(res$listing, sep = "\n")
#> 
#> id     sex      age     grp
#> 
#>  1    Female     12    Child
#>  2    Male       40    Adult
#>  3    Female     71    Senior
res$work$pts
#> # A tibble: 3 × 4
#>      id sex     age grp   
#>   <dbl> <chr> <dbl> <chr> 
#> 1     1 F        12 Child 
#> 2     2 M        40 Adult 
#> 3     3 F        71 Senior
```

## PICTURE formats

Digit selectors `9` (a digit, zeros kept) and `0` (a digit, leading
zeros dropped) fill the template from the right; other characters are
written as they are. `PREFIX=` and `MULT=` are options of the picture.

``` r
res <- sas_run("
proc format;
  picture pct low - high = '009.9%';
  picture usd low - high = '00,000,009' (prefix = '$');
run;
data _null_;
  x = 12.34; put x pct.;
  y = 1234567; put y usd.;
run;
")
cat(grep("^[ 0-9$]", sas_log(res, echo = FALSE), value = TRUE), sep = "\n")
#>  12.3%
#> $1,234,567
```

## What fails loud

Picture options sasonr does not implement yet (`FILL=`, `ROUND`, the
date/time directives) stop with a `sasonr_unsupported` condition.
