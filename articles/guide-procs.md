# PROC MEANS: log, listing and HTML output

``` r
library(sasonr)
```

A procedure step has three outputs in sasonr while the HTML destination
is open:

  - the **log**: the program lines with the NOTEs of each step;
  - the **listing**: the procedure output as text, laid out at
    `LINESIZE`;
  - the **HTML** document: the same tables with the blue-and-grey style
    of the HTML destination.

PROC MEANS builds its output once, as ODS tables (`sas_ods_table`); the
listing and the HTML document are both rendered from these tables, so
they always show the same numbers. This article uses one small invented
data set throughout.

## The program

``` r
res <- sas_run("
data trial;
  input arm $ visit week aval;
  datalines;
A 1 0 10.2
A 1 4 11.8
A 2 0 9.6
A 2 4 12.1
B 1 0 10.9
B 1 4 10.4
B 2 0 11.3
B 2 4 9.8
;
run;

title 'Change from baseline';
proc means data=trial n mean std min max maxdec=2;
  class arm week;
  var aval;
run;
")
```

## The log

`sas_log()` gives the program lines with their numbers and the NOTEs of
each step:

``` r
sas_log(res)
#> 1          
#> 2          data trial;
#> 3            input arm $ visit week aval;
#> 4            datalines;
#> 
#> NOTE: The data set WORK.TRIAL has 8 observations and 4 variables.
#> 13         ;
#> 14         run;
#> 15         
#> 16         title 'Change from baseline';
#> 17         proc means data=trial n mean std min max maxdec=2;
#> 18           class arm week;
#> 19           var aval;
#> 20         run;
#> 
#> NOTE: There were 8 observations read from the data set WORK.TRIAL.
```

`sas_log(res, echo = FALSE)` keeps only the messages; in a terminal with
colours, `print(res)` shows NOTEs in blue, WARNINGs in green and ERRORs
in red. The same log as HTML:

## The listing

`$listing` holds the procedure output as text, with the title, the
procedure title, the CLASS columns and the statistic columns laid out at
`LINESIZE` (MAXDEC=2 here):

``` r
cat(res$listing, sep = "\n")
#> Change from baseline
#> 
#> The MEANS Procedure
#> 
#> Analysis Variable : aval 
#> 
#>                               N
#> arm                 week    Obs    N            Mean         Std Dev         Minimum         Maximum
#> ----------------------------------------------------------------------------------------------------
#> A                      0      2    2            9.90            0.42            9.60           10.20
#>                        4      2    2           11.95            0.21           11.80           12.10
#> B                      0      2    2           11.10            0.28           10.90           11.30
#>                        4      2    2           10.10            0.42            9.80           10.40
#> ----------------------------------------------------------------------------------------------------
```

## The HTML output

`sas_html()` writes the procedure output of a result as an HTML
document: one table per ODS table, under the titles. The document embeds
its style sheet (`system.file("css", "sasonr.css", package =
"sasonr")`):

``` r
sas_html(res, file = "means.html")
```

`sas_html(res, log = TRUE)` adds the log after the tables. The HTML
destination is open when a program starts; `ODS HTML CLOSE;` closes it
(the tables of the following steps are left out of the document) and
`ODS HTML;` opens it again.

## BY groups

With a BY statement each BY group gets its own table under a BY line, in
the listing and in the HTML document:

``` r
res_by <- sas_run("
data trial;
  input arm $ visit week aval;
  datalines;
A 1 0 10.2
A 1 4 11.8
A 2 0 9.6
A 2 4 12.1
B 1 0 10.9
B 1 4 10.4
B 2 0 11.3
B 2 4 9.8
;
run;
proc sort data=trial; by arm; run;
title 'Analysis value by arm';
proc means data=trial n mean maxdec=1;
  by arm;
  var aval;
run;
")
cat(res_by$listing, sep = "\n")
#> Analysis value by arm
#> 
#> 
#> arm=A
#> 
#> The MEANS Procedure
#> 
#> Analysis Variable : aval 
#> 
#> N            Mean
#> -----------------
#> 4            10.9
#> -----------------
#> 
#> 
#> arm=B
#> 
#> Analysis Variable : aval 
#> 
#> N            Mean
#> -----------------
#> 4            10.6
#> -----------------
```

## The ODS tables

`$ods` holds the ODS tables of a result in output order. Each is a
tibble of the raw values (numbers unformatted) with the table’s name,
label, path, BY values and column headers as attributes:

``` r
tab <- res$ods[[1]]
attr(tab, "name")
#> [1] "Summary"
attr(tab, "path")
#> [1] "Means.Summary"
tab
#> <sas_ods_table> Summary (Summary statistics)
#>   path:     Means.Summary
#>   proc:     Means, template base.summary
#>   columns:
#>     arm [class]
#>     week [class]
#>     NObs [stat] "N Obs"
#>     aval_N [stat] "N"
#>     aval_Mean [stat] "Mean"
#>     aval_StdDev [stat] "Std Dev"
#>     aval_Min [stat] "Minimum"
#>     aval_Max [stat] "Maximum"
#>   header 1: Analysis Variable : aval 
#>   header 2: arm | week | N Obs | N | Mean | Std Dev | Minimum | Maximum
#>   rows:     4
```

## ODS OUTPUT: the table as a data set

`ODS OUTPUT Summary=<data set>;` before the PROC MEANS step writes the
Summary table to WORK, with the usual data set NOTE in the log:

``` r
res_out <- sas_run("
data trial;
  input arm $ aval;
  datalines;
A 10.2
A 11.8
B 10.9
B 10.4
;
run;
ods output Summary=stats;
proc means data=trial n mean;
  class arm;
  var aval;
run;
")
sas_log(res_out, echo = FALSE)
#> NOTE: The data set WORK.TRIAL has 4 observations and 2 variables.
#> NOTE: The data set WORK.STATS has 2 observations and 4 variables.
#> NOTE: There were 4 observations read from the data set WORK.TRIAL.
res_out$work$stats
#> # A tibble: 2 × 4
#>   arm    nobs aval_n aval_mean
#>   <chr> <dbl>  <dbl>     <dbl>
#> 1 A         2      2      11  
#> 2 B         2      2      10.6
```

## What fails loud

sasonr stops with a `sasonr_unsupported` condition, naming the SAS line,
for output requests it does not implement: `ODS HTML FILE=` and the
other ODS HTML options (write the document with `sas_html(file = )`
instead), `ODS OUTPUT` with MATCH\_ALL, PERSIST= or BY groups, the ODS
actions (CLOSE, SELECT, EXCLUDE, …), and FOOTNOTE statements in the HTML
document.
