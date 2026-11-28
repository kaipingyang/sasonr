# List the unsupported features of a SAS program

`sas_check()` answers one question: which features does this program use
that sasonr does not implement? It walks the program as
`sas_translate()` does (the macro processor runs, each step is parsed,
analysed and translated, and the variables a step creates are passed on
to the steps that follow) without running any step. A step that uses a
feature sasonr does not support is reported and the walk goes on with
the next step, so one call lists all of them, where `sas_run()` stops at
the first one.

## Usage

``` r
sas_check(code, work = list())
```

## Arguments

  - code:
    
    SAS source code.

  - work:
    
    Named list of input data frames available as `WORK` data sets.

## Value

A tibble of class `sasonr_check`, one row per finding: `step` (the index
of the step in the program), `line` and `col` (its place in the program,
`NA` when unknown), `severity` (`"error"`: an unsupported feature;
`"info"`: checked when the program runs), `feature` (what is not
supported) and `message` (the full condition message).

## Details

A step whose text depends on a macro variable that only a run sets (CALL
SYMPUT, CALL SYMPUTX or `INTO :` in an earlier step) is not guessed: it
is reported with severity `"info"` and checked when the program runs.

What `sas_check()` does not tell you:

  - Syntax errors are not reported. They are part of the program, and
    `sas_run()` writes them to the log when the program runs.

  - Some refusals depend on the data and appear only when the program
    runs: a value outside the supported domain of a function, a summary
    over rows whose order is not known, a date difference that spans
    years in a basis that is not implemented, for example. `sas_run()`
    still stops at those.

Passing `sas_check()` therefore does not mean that the program will run.

## Examples

``` r
chk <- sas_check("
data a; x = 1; run;
proc rank data=a out=b; var x; run;
")
chk
#> This program uses 1 unsupported feature:
#> - PROC RANK (line 3)
chk$feature
#> [1] "PROC RANK"
```
