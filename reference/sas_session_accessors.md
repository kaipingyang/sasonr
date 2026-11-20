# Inspect a session

Functional accessors of a `sas_session()`: the WORK data sets (trailing
blanks of character values removed, as in `sas_run()` results), their
metadata, the macro variables and the cumulative listing of all
submissions (`sas_log()` gives the cumulative log).
`sas_session_close()` closes the PROC SQL connection.

## Usage

``` r
sas_work(session, trim = TRUE)

sas_session_add(session, work)

sas_meta(session)

sas_macro_vars(session, scope = c("global", "automatic"))

sas_listing(session)

sas_session_close(session)
```

## Arguments

  - session:
    
    A session from `sas_session()`.

  - trim:
    
    `TRUE` (trailing blanks of character values removed) or `FALSE` (the
    values as stored, padded to the variable length).

  - work:
    
    Named list of data frames to add to (or replace in) `WORK`.

  - scope:
    
    `"global"` (user-defined global macro variables) or `"automatic"`
    (automatic macro variables such as `SYSCC`).

## Value

`sas_work()`: named list of tibbles. `sas_meta()`: named list of
variable metadata. `sas_macro_vars()`: named list of character values.
`sas_listing()`: character vector. `sas_session_add()`,
`sas_session_close()`: the session, invisibly.

## Examples

``` r
s <- sas_session()
sas_run("%let study = X01; data a; x = 1; put x=; run;", session = s)
#> 
#> ── Log ──
#> 
#> No ERRORs or WARNINGs
#> 1          %let study = X01; data a; x = 1; put x=; run;
#> 
#> x=1
#> NOTE: The data set WORK.A has 1 observations and 1 variables.
#> ── WORK data sets ──
#> 
#> WORK.A: 1 obs, 1 vars
#> Levels: L1
names(sas_work(s))
#> [1] "a"
sas_macro_vars(s)
#> $study
#> [1] "X01"
#> 
sas_log(s)
#> 1          %let study = X01; data a; x = 1; put x=; run;
#> 
#> x=1
#> NOTE: The data set WORK.A has 1 observations and 1 variables.
sas_session_close(s)
```
