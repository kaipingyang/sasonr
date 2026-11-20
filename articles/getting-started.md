# Getting started

``` r
library(sasonr)
```

## Run a SAS program

`sas_run()` takes SAS source code, runs it on the R engine and returns
the log, the listing and the WORK data sets.

``` r
code <- "
data dm;
  input usubjid $ age sex $;
  datalines;
S01 45 M
S02 70 F
S03 .  F
;
run;

data adsl;
  set dm;
  length agegr1 $5;
  if missing(age) then agegr1 = 'Miss';
  else if age < 65 then agegr1 = '<65';
  else agegr1 = '>=65';
  put usubjid= age= agegr1=;
run;
"
res <- sas_run(code)
sas_log(res)
#> 1          
#> 2          data dm;
#> 3            input usubjid $ age sex $;
#> 4            datalines;
#> 
#> NOTE: The data set WORK.DM has 3 observations and 3 variables.
#> 8          ;
#> 9          run;
#> 10         
#> 11         data adsl;
#> 12           set dm;
#> 13           length agegr1 $5;
#> 14           if missing(age) then agegr1 = 'Miss';
#> 15           else if age < 65 then agegr1 = '<65';
#> 16           else agegr1 = '>=65';
#> 17           put usubjid= age= agegr1=;
#> 18         run;
#> 
#> usubjid=S01 age=45 agegr1=<65
#> usubjid=S02 age=70 agegr1=>=65
#> usubjid=S03 age=. agegr1=Miss
#> NOTE: There were 3 observations read from the data set WORK.DM.
#> NOTE: The data set WORK.ADSL has 3 observations and 4 variables.
res$work$adsl
#> # A tibble: 3 × 4
#>   usubjid   age sex   agegr1
#>   <chr>   <dbl> <chr> <chr> 
#> 1 S01        45 M     <65   
#> 2 S02        70 F     >=65  
#> 3 S03        NA F     Miss
```

Missing values have SAS semantics (a missing numeric value is smaller
than any number), and character variables have a fixed length, here set
by the `LENGTH` statement.

## Translate a program into an R script

`sas_script()` writes the translation of a program as an R script that
runs after `library(sasonr)`. With `tidy = TRUE`, each vectorised DATA
step becomes a dplyr pipe on R data frames (the script loads dplyr and
tidylog, so every step reports what it did); steps that need sasonr’s
own runtime run in a short session block.

``` r
code2 <- "
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
sas_script(sas_translate(code2), tidy = TRUE)
#> # Translation of the SAS program, written by sasonr.
#> # Tidy form: steps that need a sasonr session run in a session block.
#> library(sasonr)
#> library(dplyr)
#> library(tidylog)
#> 
#> # data lab;  (SAS line 2)
#> # ---- DATA LAB (SAS line 2) -- level L1
#> # L1: no cross-row state
#> lab <- sas_read_input(sas_infile_open(c("S01 1 10", "S01 2 12", "S02 1 20", "S02 2 18"), c(5L, 6L, 7L, 8L), opts = list(lines = 1L, end_text = ";")), list(items = list(
#>     list(k = "var", name = "usubjid", disp = "usubjid", char = TRUE, len = 8L, mode = "list"),
#>     list(k = "var", name = "visit", disp = "visit", char = FALSE, len = NA_integer_, mode = "list"),
#>     list(k = "var", name = "aval", disp = "aval", char = FALSE, len = NA_integer_, mode = "list")
#>   ), hold = "")) |>
#>   select(usubjid, visit, aval)
#> 
#> # data chg;  (SAS line 12)
#> # ---- DATA CHG (SAS line 12) -- level L2
#> # L2: BY-group processing (FIRST./LAST.); retained value carried forward (baseline / LOCF)
#> chg <- lab |>
#>   sas_check_sorted("usubjid", FALSE) |>
#>   mutate(
#>     first.usubjid = sas_first(usubjid)
#>   ) |>
#>   group_by(.grp = cumsum(first.usubjid)) |>
#>   mutate(
#>     base = as.numeric(dplyr::first(aval))
#>   ) |>
#>   ungroup() |>
#>   mutate(
#>     chg = sas_arith(aval, base, "-")
#>   ) |>
#>   select(usubjid, visit, aval, base, chg)
```

Without `tidy`, every SAS step becomes one call of a step verb on a
session: `sas_data_step()` holds the generated code of a DATA step and
the SAS text it came from, `sas_proc_sort()` and `sas_proc_means()` show
the options the program gave, and other steps keep their SAS text. This
script gives the same WORK data sets and the same log as `sas_run()`.

``` r
code3 <- "
data adsl;
  input usubjid $ age;
  datalines;
S01 70
S02 41
;
run;
proc sort data=adsl out=bysubj; by descending age; run;
"
sas_script(sas_translate(code3))
#> # Translation of the SAS program, written by sasonr.
#> library(sasonr)
#> 
#> s <- sas_session()
#> 
#> s |> sas_data_step("adsl", \(work) {
#>   # ---- DATA ADSL (SAS line 2) -- level L1
#>   # L1: no cross-row state
#>   .d <- sas_read_input(sas_infile_open(c("S01 70", "S02 41"), c(5L, 6L), opts = list(lines = 1L, end_text = ";")), list(items = list(
#>       list(k = "var", name = "usubjid", disp = "usubjid", char = TRUE, len = 8L, mode = "list"),
#>       list(k = "var", name = "age", disp = "age", char = FALSE, len = NA_integer_, mode = "list")
#>     ), hold = ""))
#>   work$adsl <- .d |>
#>     dplyr::select(usubjid, age)
#> }, sas = r"(data adsl;
#>   input usubjid $ age;
#>   datalines;
#> S01 70
#> S02 41
#> ;)", line = 2L)
#> 
#> s |> sas_proc_sort("data=adsl out=bysubj", by = "descending age", line = 9L)
```

`sas_script()` also takes the result of `sas_run()`: the script is then
the trace of that run, including the steps that macro code generated.

## Look at the code of one step

`sas_translate()` returns the generated R code step by step, for
debugging. Simple steps become vectorised `dplyr` pipelines; steps with
cross-row state such as `RETAIN`, `LAG` or `FIRST.`/`LAST.` use grouped
vectorised code when the result can be proven identical, and an explicit
row loop otherwise. The header of each step names its level and the
reason.

``` r
tr <- sas_translate(code2)
cat(tr$steps[[2]]$code)
#> # ---- DATA CHG (SAS line 12) -- level L2
#> # L2: BY-group processing (FIRST./LAST.); retained value carried forward (baseline / LOCF)
#> .d <- sas_ds(work, "lab") |> sas_note_read("lab") |>
#>   sas_check_sorted("usubjid", FALSE) |>
#>   dplyr::mutate(
#>     first.usubjid = sas_first(usubjid)
#>   ) |>
#>   dplyr::group_by(.grp = cumsum(first.usubjid)) |>
#>   dplyr::mutate(
#>     base = as.numeric(dplyr::first(aval))
#>   ) |>
#>   dplyr::ungroup() |>
#>   dplyr::mutate(
#>     chg = sas_arith(aval, base, "-", .at = c(17L, 14L))
#>   )
#> work$chg <- .d |>
#>   dplyr::select(usubjid, visit, aval, base, chg)
sas_run(code2)$work$chg
#> # A tibble: 4 × 5
#>   usubjid visit  aval  base   chg
#>   <chr>   <dbl> <dbl> <dbl> <dbl>
#> 1 S01         1    10    10     0
#> 2 S01         2    12    10     2
#> 3 S02         1    20    20     0
#> 4 S02         2    18    20    -2
```

## Macro language

Macro processing and step execution interleave: a step is run as soon as
it is complete, so values created with `CALL SYMPUTX` are available to
the macro code that follows.

``` r
code3 <- "
%macro check(ds);
  data _null_;
    call symputx('n', 3);
  run;
  %if &n > 2 %then %put NOTE: &ds is large;
  %else %put NOTE: &ds is small;
%mend check;
%check(adsl)
"
sas_log(sas_run(code3), echo = FALSE)
#> NOTE: adsl is large
```

`echo = FALSE` leaves the program lines out: the log shows only the
messages and the PUT output.

## Unsupported features fail loudly

sasonr never guesses. A feature it does not support stops the program
with a classed condition that carries the SAS line number.

``` r
sas_run("data a; x = 1; run; proc report data=a; run;")
#> Error in `sasonr_abort()`:
#> ! Unsupported: PROC REPORT (SAS line 1)
```

The same applies by default (`sasonr.strict = TRUE`) to functions,
formats and informats whose status in the feature registry is
`approximate` (the last bits of numeric results may not be exact, for
example in the probability functions) or `unsupported`: they stop with a
`sasonr_strict_error` condition (see `?sasonr_strict`).

``` r
sas_run("data a; p = probnorm(1.96); run;")
#> Error in `strict_gate()`:
#> ! function PROBNORM is approximate; set options(sasonr.strict = FALSE) to use it (SAS line 1)
```

To run them anyway, with a warning of class `sasonr_strict_warning`,
turn strict mode off:

``` r
old <- options(sasonr.strict = FALSE)
res <- suppressWarnings(sas_run("data a; p = probnorm(1.96); run;"))
res$work$a$p
#> [1] 0.9750021
options(old)
```
