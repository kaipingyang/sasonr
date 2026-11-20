# Run one SAS step in a session

The verbs of a script written by `sas_script()`. Each takes a session
from `sas_session()`, runs one step in it as `sas_run()` does (the log,
the listing, WORK and the macro variables of the session change), and
returns the session invisibly, so the calls can be chained with `|>`.

## Usage

``` r
sas_data_step(session, data, fn, sas, line = 1L, level = NULL)

sas_proc_sort(session, options = "", ..., line = 1L)

sas_proc_means(session, options = "", ..., line = 1L)

sas_proc_sql(session, sql, options = "", line = 1L)

sas_proc(session, sas, line = 1L)

sas_options(session, options, line = 1L)

sas_title(session, title = "", n = 1L, line = 1L)

sas_global(session, sas, line = 1L)

sas_deferred(session, sas, line = 1L)
```

## Arguments

  - session:
    
    A session from `sas_session()`.

  - data:
    
    The output data sets of the DATA step (for the reader; the SAS text
    names them).

  - fn:
    
    A function of `work` whose body is the code of the step.

  - sas:
    
    SAS text of the step.

  - line:
    
    The SAS line the step starts on (line numbers in the log).

  - level:
    
    `NULL` (the level of the translation) or `"L3"` (the step ran row by
    row).

  - options:
    
    Options of the PROC (or OPTIONS) statement as SAS text.

  - ...:
    
    Statements of the PROC step: `name = "text"`.

  - sql:
    
    SQL statements, each ending with a semicolon.

  - title:
    
    The text of a TITLE statement (quoted SAS strings).

  - n:
    
    The title line (`TITLEn`).

## Value

The session, invisibly.

## Details

  - `sas_data_step()` runs the R code of `fn` (the code sasonr generated
    for the DATA step `sas`) in place of the translation of `sas`; the
    SAS text gives the variable attributes of the output data sets and
    the log. When vectorised code meets a data error the step is run
    again row by row from its SAS text, as in `sas_run()`.

  - `sas_proc_sort()`, `sas_proc_means()`: the options of the PROC
    statement and one argument per statement (`by = "usubjid"`, `output
    = "out=m mean="`), as the program wrote them.

  - `sas_proc_sql()`: the SQL statements of a PROC SQL step.

  - `sas_proc()`: any PROC step as SAS text.

  - `sas_options()`, `sas_title()`, `sas_global()`: global statements.

  - `sas_deferred()`: SAS text with macro code, run through the macro
    processor (code that depends on values known only at run time).

## Examples

``` r
s <- sas_session(work = list(a = data.frame(x = c(2, 1, 3))))
s |> sas_proc_sort("data=a out=b", by = "descending x")
s |> sas_proc_means("data=b noprint", var = "x", output = "out=m mean=xm")
sas_work(s)$m
#> # A tibble: 1 × 3
#>   `_type_` `_freq_`    xm
#>      <dbl>    <dbl> <dbl>
#> 1        0        3     2
```
