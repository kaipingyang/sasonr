# The translation of a SAS program as an R script

`sas_script()` turns the result of `sas_translate()` (the translation of
the program, before it runs) or of `sas_run()` (the trace of that run:
the code that ran) into a script: one call of a step verb per SAS step
(`sas_data_step()`, `sas_proc_sort()`, `sas_proc_means()`,
`sas_proc_sql()`, `sas_proc()`, `sas_options()`, `sas_title()`,
`sas_global()`, `sas_deferred()`), each running its step in a session
created by `sas_session()`. Printing the object writes the script;
`as.character()` gives its lines.

## Usage

``` r
sas_script(x, tidy = FALSE)
```

## Arguments

  - x:
    
    A `sasonr_translation` (`sas_translate()`) or a `sasonr_result`
    (`sas_run()`).

  - tidy:
    
    `FALSE` (one session for the whole script) or `TRUE` (see Details).

## Value

A `sasonr_script`: a list of step nodes, each with `verb`, `lang` (the
call that runs the step), `text` (its R text), `sas` (the SAS text),
`sas_lines` (its SAS lines), `macro_stack` (empty list), `include`
(NULL), `kind` (`"static"`, `"deferred"` or `"traced"`) and `comments`
(the SAS comments of the step: `lines` (the R comment lines), `line`,
`position` (`"leading"` or `"trailing"`) and `place` (`"step"`: above
the step verb, `"body"`: above the line of the generated code of the
statement, `"after"`: below the last step)).

## Details

Each DATA step is written as the R code sasonr generated for it,
unchanged, inside `sas_data_step()`. A PROC SORT or PROC MEANS step
shows only the options and statements the program gave; other PROC steps
and PROC SQL keep their SAS text. In a translation, macro code whose
result depends on values known only at run time (CALL SYMPUT, PROC SQL
INTO:, `%SYSFUNC`, automatic macro variables) is kept as SAS text in
`sas_deferred()`, which runs it through the macro processor; a trace has
no such steps, but it is valid for the data of that run only.

SAS comments (`/* ... */` and `* ... ;`) are kept as R comment lines: a
comment on the line where a statement ends belongs to that statement,
any other comment to the next statement. Comments of the first statement
of a step go above the step verb; comments of a later statement of a
DATA step go above the line of the generated code for that statement, or
above the step when there is no such line. Macro comments (`%* ... ;`)
and comments in macro definitions or in code a macro generates are not
copied.

With `tidy = TRUE` the script has no session where it does not need one:
DATA steps that run vectorised (levels L1 and L2) read and write R
variables (`adsl <- dm |> mutate(...)`), and the script starts with
`library(dplyr)` and `library(tidylog)` (tidylog then reports what each
verb did; see `sasonr_tidy_log()`). The other steps (row loops, PUT or
INFILE, macro variables, user formats, PROC steps, PROC SQL, global
statements) run in a session block with a comment that says why; the
data sets go into the session with `sas_session_add()` and come back
with `sas_work(s, trim = FALSE)`. The values are the same, but data sets
made by the tidy steps carry no labels or formats into later session
steps.

## Examples

``` r
tr <- sas_translate("
data b;
  set a;
  y = x * 2;
run;
proc sort data=b; by descending y; run;
", work = list(a = data.frame(x = c(1, 3, 2))))
sas_script(tr)
#> # Translation of the SAS program, written by sasonr.
#> library(sasonr)
#> 
#> s <- sas_session(work = list(a = a))
#> 
#> s |> sas_data_step("b", \(work) {
#>   # ---- DATA B (SAS line 2) -- level L1
#>   # L1: no cross-row state
#>   .d <- sas_ds(work, "a") |> sas_note_read("a") |>
#>     dplyr::mutate(
#>       y = sas_arith(x, 2, "*", .at = c(4L, 9L))
#>     )
#>   work$b <- .d |>
#>     dplyr::select(x, y)
#> }, sas = r"(data b;
#>   set a;
#>   y = x * 2;
#> run;)", line = 2L)
#> 
#> s |> sas_proc_sort("data=b", by = "descending y", line = 6L)
sas_script(tr, tidy = TRUE)
#> # Translation of the SAS program, written by sasonr.
#> # Tidy form: steps that need a sasonr session run in a session block.
#> library(sasonr)
#> library(dplyr)
#> library(tidylog)
#> 
#> a <- sas_work(sas_session(work = list(a = a)), trim = FALSE)$a  # SAS character values
#> # data b;  (SAS line 2)
#> # ---- DATA B (SAS line 2) -- level L1
#> # L1: no cross-row state
#> b <- a |>
#>   mutate(
#>     y = sas_arith(x, 2, "*")
#>   ) |>
#>   select(x, y)
#> 
#> # In a sasonr session: PROC SORT runs in the session
#> .s <- sas_session(work = list(a = a, b = b))
#> .s |> sas_proc_sort("data=b", by = "descending y", line = 6L)
#> list2env(sas_work(.s, trim = FALSE), environment())
```
