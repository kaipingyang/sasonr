# DATA step

``` r
library(sasonr)
```

The DATA step is compiled into R code. Simple steps become vectorised
`dplyr` pipelines; steps with cross-row state use grouped vectorised
code when it is provably equivalent, and an explicit row loop that
follows the SAS program data vector (PDV) otherwise.
`sas_script(sas_translate(code), tidy = TRUE)` writes the translation as
an R script; `sas_translate()` shows the code of each step for
debugging.

This chapter is organised by topic and grows with the implementation.

## Conditional logic

``` r
sas_run("
data _null_;
  do x = 1 to 3;
    if x = 1 then do; y = 'one'; z = 1; end;
    else if x = 2 then do; y = 'two'; z = 2; end;
    else do; y = 'oth'; z = 3; end;
    put x= y= z=;
  end;
run;
")$log
#>  [1] "1          "                                                 
#>  [2] "2          data _null_;"                                     
#>  [3] "3            do x = 1 to 3;"                                 
#>  [4] "4              if x = 1 then do; y = 'one'; z = 1; end;"     
#>  [5] "5              else if x = 2 then do; y = 'two'; z = 2; end;"
#>  [6] "6              else do; y = 'oth'; z = 3; end;"              
#>  [7] "7              put x= y= z=;"                                
#>  [8] "8            end;"                                           
#>  [9] "9          run;"                                             
#> [10] ""                                                            
#> [11] "x=1 y=one z=1"                                               
#> [12] "x=2 y=two z=2"                                               
#> [13] "x=3 y=oth z=3"
```

In a `SELECT` group the first true `WHEN` runs, and a dangling `ELSE`
belongs to the nearest `IF`:

``` r
sas_run("
data _null_;
  x = 5;
  select;
    when (x < 3)  g = 'L';
    when (x < 10) g = 'M';
    otherwise     g = 'H';
  end;
  if x > 1 then if x > 10 then h = 1; else h = 2;
  put g= h=;
run;
")$log
#>  [1] "1          "                                                 
#>  [2] "2          data _null_;"                                     
#>  [3] "3            x = 5;"                                         
#>  [4] "4            select;"                                        
#>  [5] "5              when (x < 3)  g = 'L';"                       
#>  [6] "6              when (x < 10) g = 'M';"                       
#>  [7] "7              otherwise     g = 'H';"                       
#>  [8] "8            end;"                                           
#>  [9] "9            if x > 1 then if x > 10 then h = 1; else h = 2;"
#> [10] "10           put g= h=;"                                     
#> [11] "11         run;"                                             
#> [12] ""                                                            
#> [13] "g=M h=2"
```

## Missing values

A missing numeric value is smaller than any number; arithmetic
propagates missing values, while SAS functions such as `SUM` skip them:

``` r
sas_run("
data _null_;
  x = .;
  a = (x < 0); b = (x = .); c = (.A < .); d = (._ < .);
  e = min(x, 3); f = x + 1; g = sum(x, 1);
  put a= b= c= d= e= f= g=;
run;
")$log
#>  [1] "1          "                                                                                  
#>  [2] "2          data _null_;"                                                                      
#>  [3] "3            x = .;"                                                                          
#>  [4] "4            a = (x < 0); b = (x = .); c = (.A < .); d = (._ < .);"                           
#>  [5] "5            e = min(x, 3); f = x + 1; g = sum(x, 1);"                                        
#>  [6] "6            put a= b= c= d= e= f= g=;"                                                       
#>  [7] "7          run;"                                                                              
#>  [8] ""                                                                                             
#>  [9] "a=1 b=1 c=0 d=1 e=3 f=. g=1"                                                                  
#> [10] "NOTE: Missing values were generated as a result of performing an operation on missing values."
#> [11] "      Each place is given by: (Number of times) at (Line):(Column)."                          
#> [12] "      1 at 5:24  "
```

## BY groups, RETAIN and FIRST./LAST.

``` r
code <- "
data lab;
  input usubjid $ visit aval;
  datalines;
S01 1 10
S01 2 12
S02 1 20
S02 2 18
;
run;
data chg;
  set lab;
  by usubjid;
  retain base;
  if first.usubjid then base = aval;
  chg = aval - base;
run;
"
sas_run(code)$work$chg
#> # A tibble: 4 × 5
#>   usubjid visit  aval  base   chg
#>   <chr>   <dbl> <dbl> <dbl> <dbl>
#> 1 S01         1    10    10     0
#> 2 S01         2    12    10     2
#> 3 S02         1    20    20     0
#> 4 S02         2    18    20    -2
```

The second step is vectorised (level L2). As a tidy R script:

``` r
sas_script(sas_translate(code), tidy = TRUE)
#> # Translation of the SAS program, written by sasonr.
#> # Tidy form: steps that need a sasonr session run in a session block.
#> library(sasonr)
#> library(dplyr)
#> library(tidylog)
#> 
#> # data lab;  (SAS line 2)
#> lab <- sas_read_input(
#>   sas_infile_open(
#>     c("S01 1 10", "S01 2 12", "S02 1 20", "S02 2 18"),
#>     c(5L, 6L, 7L, 8L),
#>     opts = list(lines = 1L, end_text = ";")
#>   ),
#>   list(
#>     items = list(
#>       list(
#>         k = "var",
#>         name = "usubjid",
#>         disp = "usubjid",
#>         char = TRUE,
#>         len = 8L,
#>         mode = "list"
#>       ),
#>       list(
#>         k = "var",
#>         name = "visit",
#>         disp = "visit",
#>         char = FALSE,
#>         len = NA_integer_,
#>         mode = "list"
#>       ),
#>       list(
#>         k = "var",
#>         name = "aval",
#>         disp = "aval",
#>         char = FALSE,
#>         len = NA_integer_,
#>         mode = "list"
#>       )
#>     ),
#>     hold = ""
#>   )
#> ) |>
#>   select(usubjid, visit, aval)
#> 
#> # data chg;  (SAS line 11)
#> chg <- lab |>
#>   sas_check_sorted("usubjid", FALSE) |>
#>   mutate(first.usubjid = sas_first(usubjid)) |>
#>   group_by(.grp = cumsum(first.usubjid)) |>
#>   mutate(base = as.numeric(dplyr::first(aval))) |>
#>   ungroup() |>
#>   mutate(chg = sas_arith(aval, base, "-")) |>
#>   select(usubjid, visit, aval, base, chg)
```

## LAG inside a condition

`LAG` only updates its queue when it is executed, so a conditional `LAG`
differs from a shifted column. sasonr runs such steps row by row:

``` r
sas_run("
data a; do i = 1 to 5; output; end; run;
data _null_;
  set a;
  if i in (1, 3, 5) then l = lag(i);
  put i= l=;
run;
")$log
#>  [1] "1          "                                                   
#>  [2] "2          data a; do i = 1 to 5; output; end; run;"           
#>  [3] ""                                                              
#>  [4] "NOTE: The data set WORK.A has 5 observations and 1 variables." 
#>  [5] "3          data _null_;"                                       
#>  [6] "4            set a;"                                           
#>  [7] "5            if i in (1, 3, 5) then l = lag(i);"               
#>  [8] "6            put i= l=;"                                       
#>  [9] "7          run;"                                               
#> [10] ""                                                              
#> [11] "i=1 l=."                                                       
#> [12] "i=2 l=."                                                       
#> [13] "i=3 l=1"                                                       
#> [14] "i=4 l=."                                                       
#> [15] "i=5 l=3"                                                       
#> [16] "NOTE: There were 5 observations read from the data set WORK.A."
```

## Arrays and DO loops

``` r
sas_run("
data _null_;
  array a{*} x1-x3 (1 2 3);
  array t{3} _temporary_ (10 20 30);
  tot = 0;
  do i = 1 to dim(a); tot + a{i} * t{i}; end;
  s = sum(of x1-x3);
  put tot= s=;
run;
")$log
#>  [1] "1          "                                             
#>  [2] "2          data _null_;"                                 
#>  [3] "3            array a{*} x1-x3 (1 2 3);"                  
#>  [4] "4            array t{3} _temporary_ (10 20 30);"         
#>  [5] "5            tot = 0;"                                   
#>  [6] "6            do i = 1 to dim(a); tot + a{i} * t{i}; end;"
#>  [7] "7            s = sum(of x1-x3);"                         
#>  [8] "8            put tot= s=;"                               
#>  [9] "9          run;"                                         
#> [10] ""                                                        
#> [11] "tot=140 s=6"
```

## Character length

The length of a character variable is set the first time the compiler
sees it, so later assignments are truncated:

``` r
sas_run("
data _null_;
  x = 'ab'; x = 'abcd';
  if 0 then y = 'long text';
  y = 'hi';
  put x= y=;
run;
")$log
#> [1] "1          "                            
#> [2] "2          data _null_;"                
#> [3] "3            x = 'ab'; x = 'abcd';"     
#> [4] "4            if 0 then y = 'long text';"
#> [5] "5            y = 'hi';"                 
#> [6] "6            put x= y=;"                
#> [7] "7          run;"                        
#> [8] ""                                       
#> [9] "x=ab y=hi"
```

## Not supported yet

Features that sasonr does not support yet stop the program instead of
running with a guess:

``` r
sas_run("data a; x = 1; link sub; return; sub: x = 2; return; run;")
#> Error in `sasonr_abort()`:
#> ! Unsupported: DATA step statement LINK (SAS line 1)
```
