# Run a SAS program on the R engine

The macro processor hands complete steps to the executor one at a time
(at RUN; / QUIT; / step boundaries), so CALL SYMPUT(X) results are
visible to the macro code that follows the step, including inside macro
bodies.

## Usage

``` r
sas_run(
  code,
  work = list(),
  level = c("auto", "L3"),
  on_error = getOption("sasonr.on_error", c("sas", "abort")),
  session = NULL
)
```

## Arguments

  - code:
    
    SAS source code.

  - work:
    
    Named list of input data frames available as `WORK` data sets.

  - level:
    
    `"auto"` (choose L1/L2/L3 per step) or `"L3"` (force row loops).

  - on\_error:
    
    What an ERROR of the program does (a syntax error, an unknown
    function, a missing input data set, an ERROR in PROC SQL ...).
    `"sas"` (the default; option `sasonr.on_error`) uses the batch-mode
    error handling: the failing step is not run, syntax-check mode
    (OBS=0) applies to the rest of the program (later steps are compiled
    but not run; global statements and the macro language still are),
    `SYSERR`/`SYSCC` are set and the result has `status = "error"`, the
    ERROR lines in `$log` and WORK as the failing step leaves it.
    `"abort"` stops with the classed R condition instead. Features
    sasonr does not support (`sasonr_unsupported`) always stop the run:
    they are not errors of the program.

  - session:
    
    A session from `sas_session()`: the program runs as one more
    submission to it (WORK, macro variables, user formats, options and
    titles of earlier submissions are visible; `work` data sets are
    added to its WORK). `NULL` (the default) runs in a fresh session
    that is discarded afterwards.

## Value

A `sasonr_result`: `$log` (the log: the program lines with the NOTEs,
WARNINGs, ERRORs and PUT output; see `sas_log()`), `$log_events` (a
tibble of the log events), `$listing` (procedure output), `$ods` (the
ODS tables of the procedure output in output order, each a
`sas_ods_table`; PROC MEANS and SUMMARY), `$work` (output data sets with
trailing blanks removed), `$translation`, `$macro_vars` (global macro
variables), `$status` (`"ok"` or `"error"`), `$errors` (the ERROR
messages) and `$error_line` (SAS line of the first ERROR). With a
`session`, `$log`, `$log_events`, `$listing`, `$translation`, `$status`
and `$errors` describe this submission, `$work` and `$macro_vars` the
whole session.

## Examples

``` r
res <- sas_run("
data class;
  input name $ age;
  length group $ 5;
  if age >= 13 then group = 'teen'; else group = 'child';
  datalines;
Alice 12
Bob 14
;
run;
proc print data=class; run;
")
res$work$class
#> # A tibble: 2 × 3
#>   name    age group
#>   <chr> <dbl> <chr>
#> 1 Alice    12 child
#> 2 Bob      14 teen 
cat(res$listing, sep = "\n")
#> 
#> Obs    name     age    group
#> 
#>   1    Alice     12    child
#>   2    Bob       14    teen
```
