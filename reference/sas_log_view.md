# The log view of an object (the S3 generic behind sas\_log())

Methods: a result of `sas_run()` and a session (the log events of the
submissions), an ODS table or list of tables of a data-level PROC
function (`sas_means_df()`: the WARNINGs and NOTEs of the computation).

## Usage

``` r
sas_log_view(x, format, echo, compact)
```

## Arguments

  - x:
    
    The object.

  - format, echo, compact:
    
    As in `sas_log()`.

## Value

As `sas_log()`.
