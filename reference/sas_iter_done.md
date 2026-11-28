# Does the step end after this iteration?

After a STOP (or the end of a reader's input), after the one iteration
of a step without readers, and after an iteration that ran no reader
(the looping NOTE; `why`: the reason this case is not supported, if it
is not).

## Usage

``` r
sas_iter_done(st, status, why = NULL)
```
