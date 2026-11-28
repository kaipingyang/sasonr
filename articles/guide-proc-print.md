# PROC PRINT

``` r
library(sasonr)
```

PROC PRINT lists the observations of a data set. sasonr writes the
listing as text laid out at `LINESIZE`, and the table in the HTML or RTF
document when one of those destinations is open. The examples use one
small invented data set.

``` r
prog <- "
options nocenter;
data vitals;
  input subj $ arm $ visit sbp dbp;
  label sbp = 'Systolic BP' dbp = 'Diastolic BP';
  datalines;
S01 A 1 128 82
S01 A 2 124 80
S02 A 1 135 88
S02 A 2 131 85
S03 B 1 118 76
S03 B 2 121 79
;
run;
"
```

## A plain listing

The `Obs` column numbers the observations; numbers without a format are
right-aligned and the column headings are centred over their columns.

``` r
res <- sas_run(paste(prog, "proc print data=vitals; run;"))
cat(res$listing, sep = "\n")
#> 
#> Obs    subj    arm    visit    sbp    dbp
#> 
#>  1     S01      A       1      128     82
#>  2     S01      A       2      124     80
#>  3     S02      A       1      135     88
#>  4     S02      A       2      131     85
#>  5     S03      B       1      118     76
#>  6     S03      B       2      121     79
```

## VAR, ID and NOOBS

`VAR` chooses and orders the columns; `ID` replaces the `Obs` column by
a variable; `NOOBS` drops it.

``` r
res <- sas_run(paste(prog, "proc print data=vitals; id subj; var visit sbp dbp; run;"))
cat(res$listing, sep = "\n")
#> 
#> subj    visit    sbp    dbp
#> 
#> S01       1      128     82
#> S01       2      124     80
#> S02       1      135     88
#> S02       2      131     85
#> S03       1      118     76
#> S03       2      121     79
```

## Labels and SPLIT=

`LABEL` uses the variable labels as headings. A label wider than its
column is split at blanks; `SPLIT=` names the character where the
heading breaks.

``` r
res <- sas_run(paste(prog, "proc print data=vitals noobs label; var subj sbp dbp; run;"))
cat(res$listing, sep = "\n")
#> 
#>         Systolic    Diastolic
#> subj       BP           BP
#> 
#> S01        128          82
#> S01        124          80
#> S02        135          88
#> S02        131          85
#> S03        118          76
#> S03        121          79
```

## BY groups, SUM and N

With `BY` the data must be sorted by the BY variables; each group gets
its BY line. `SUM` adds the group totals and the grand total.

``` r
res <- sas_run(paste(prog, "
proc sort data=vitals; by arm; run;
proc print data=vitals noobs;
  by arm;
  var subj visit sbp;
  sum sbp;
run;"))
cat(res$listing, sep = "\n")
#> 
#> arm=A
#> 
#> subj    visit    sbp
#> 
#> S01       1      128
#> S01       2      124
#> S02       1      135
#> S02       2      131
#> ----             ---
#> arm              518
#> 
#> 
#> 
#> arm=B
#> 
#> subj    visit    sbp
#> 
#> S03       1      118
#> S03       2      121
#> ----             ---
#> arm              239
#>                  ===
#>                  757
```

`N` writes the number of observations of each group.

``` r
res <- sas_run(paste(prog, "
proc sort data=vitals; by arm; run;
proc print data=vitals noobs n;
  by arm;
  var subj sbp;
run;"))
cat(res$listing, sep = "\n")
#> 
#> arm=A
#> 
#> subj    sbp
#> 
#> S01     128
#> S01     124
#> S02     135
#> S02     131
#> 
#> N = 4
#> 
#> 
#> 
#> arm=B
#> 
#> subj    sbp
#> 
#> S03     118
#> S03     121
#> 
#> N = 2
```

## Tables wider than the line

The gap between columns is 4 blanks, or 3, 2, 1 when that makes the
table fit on the line. When it does not fit with a gap of 1, variable
names wider than one-position values are written down the columns;
otherwise the columns are split into panels, each panel repeating the
`ID` column.

``` r
wide <- paste0("options nocenter ls=64; data w; length ",
               paste(sprintf("v%02d", 1:12), collapse = " "), " $8; ",
               paste0(sprintf("v%02d", 1:12), "='xxxxxx';", collapse = " "),
               " run; proc print data=w noobs; run;")
cat(sas_run(wide)$listing, sep = "\n")
#> 
#>  v01       v02       v03       v04       v05       v06
#> 
#> xxxxxx    xxxxxx    xxxxxx    xxxxxx    xxxxxx    xxxxxx
#> 
#>  v07       v08       v09       v10       v11       v12
#> 
#> xxxxxx    xxxxxx    xxxxxx    xxxxxx    xxxxxx    xxxxxx
```

## What fails loud

sasonr stops with a `sasonr_unsupported` condition, naming the SAS line,
for the layouts it does not implement yet, for example column panels
with more than one observation, BY groups or `SUM`, and the page headers
of a listing longer than one page under a small `PAGESIZE=`.

``` r
sas_run(paste0("options nocenter ls=64; data w; length ",
               paste(sprintf("v%02d", 1:12), collapse = " "), " $8; ",
               paste0(sprintf("v%02d", 1:12), "='xxxxxx';", collapse = " "),
               " output; output; run; proc print data=w noobs; run;"))
#> Error in `sasonr_abort()`:
#> ! Unsupported: a PROC PRINT table wider than LINESIZE with more than one observation, BY groups, SUM or N (column panels measured on one observation)
```
