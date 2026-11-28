# CALL EXECUTE

Runs the macro code of its argument now (`%LET`, `%PUT`, macro calls)
and queues the text that results; the engine runs the queue after the
DATA step ends (engine\_call\_execute\_drain()). Returns the `%PUT`
lines, which go to the step's log buffer between its PUT lines.

## Usage

``` r
sas_call_execute(symtab, value, .line = NULL)
```
