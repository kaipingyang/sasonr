# Create a persistent session

A session holds the state kept between submissions: the WORK library and
its metadata, macro variables and macro definitions, user formats (PROC
FORMAT), system options set with `OPTIONS`, titles, filerefs and
`_LAST_`. Pass it to `sas_run()` several times to run a program in
several submissions. `sas_run()` without `session` runs in a fresh
session that is discarded afterwards.

## Usage

``` r
sas_session(work = list())
```

## Arguments

  - work:
    
    Named list of data frames to load into `WORK`.

## Value

A `SasSession` object (R6).

## Details

Each submission starts outside syntax-check mode and with its own
`$status`, `$errors` and `$log` in the returned result;
`SYSERR`/`SYSCC`, like every other macro variable, carry over. The PROC
SQL connection is opened on first use and closed by
`sas_session_close()` (or when the session is garbage collected).

## See also

`sas_work()`, `sas_macro_vars()`, `sas_log()`

## Examples

``` r
s <- sas_session()
sas_run("data a; x = 1; run; %let n = 3;", session = s)
#> 
#> ── Log ──
#> 
#> No ERRORs or WARNINGs
#> 1          data a; x = 1; run; %let n = 3;
#> 
#> NOTE: The data set WORK.A has 1 observations and 1 variables.
#> ── WORK data sets ──
#> 
#> WORK.A: 1 obs, 1 vars
#> Levels: L1
res <- sas_run("data b; set a; y = x * &n; run;", session = s)
res$work$b
#> # A tibble: 1 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     3
sas_work(s)$a
#> # A tibble: 1 × 1
#>       x
#>   <dbl>
#> 1     1
sas_macro_vars(s)$n
#> [1] "3"
sas_session_close(s)
```
