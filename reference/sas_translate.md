# Translate a SAS program into R (tidyverse) code

The macro processor runs first; steps are translated in order. Nothing
is executed, so macro variables created by CALL SYMPUT are not available
to later macro code (use `sas_run()` for that).

## Usage

``` r
sas_translate(code, work = list(), level = c("auto", "L3"))
```

## Arguments

  - code:
    
    SAS source code.

  - work:
    
    Named list of input data frames available as `WORK` data sets.

  - level:
    
    `"auto"` (choose L1/L2/L3 per step) or `"L3"` (force row loops).

## Value

A `sasonr_translation` object; `$steps[[i]]$code` holds the R code.

The code of a step calls base R, `dplyr::`/`tibble::` functions and the
run-time helpers that sasonr exports (documented as internal). It is
evaluated by `sas_run()` in an environment that holds `work` (an
environment with the WORK data sets as data frames), `.log` and `.lst`
(character vectors the step appends its log and listing lines to),
`.meta` (the variable attributes the step records), `.symtab` (the macro
symbol table) and `.titles` (the current TITLE lines), with the session
of the run bound for the log NOTEs and `_ERROR_`. Steps that sasonr runs
itself (PROC SQL, PROC DATASETS) have no R translation: their code is a
call of `sas_untranslated()`, which stops with a condition of class
`sasonr_unsupported`.

Option `sasonr.passes` selects the readability passes applied to the
code of vectorised DATA steps: `"by_first_value"` (a retained numeric
variable set only at `FIRST.` of a BY group from a column becomes one
grouped value per BY group) and `"unused_first_last"` (`FIRST.`/`LAST.`
columns that no later expression reads are not created). All are on by
default; `options(sasonr.passes = character())` turns them off. A pass
rewrites code only where the result is the same: the same WORK data sets
and the same log. `sas_run()` runs the code with the same passes.

## Examples

``` r
tr <- sas_translate("data b; set a; y = round(x, 0.1); run;",
                    work = list(a = data.frame(x = c(1.25, 2.35))))
tr
#> # ---- DATA B (SAS line 1) -- level L1
#> # L1: no cross-row state
#> .d <- sas_ds(work, "a") |>
#>   sas_note_read("a") |>
#>   dplyr::mutate(y = do.call(sas_f_round, c(sas_mv_args(c(1L, 20L), x, 0.1))))
#> work$b <- .d |> dplyr::select(x, y)
#> 
```
