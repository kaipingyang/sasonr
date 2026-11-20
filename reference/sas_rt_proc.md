# Run a procedure of the PROC registry

Computes the tables and output data sets of one PROC step, then routes
them: the data sets go to WORK (through `sas_proc_store()` in the
translated code), the tables are rendered to the listing.

## Usage

``` r
sas_rt_proc(name, d, spec)
```

## Arguments

  - name:
    
    The procedure name as written (`"means"`, `"summary"`).

  - d:
    
    Input data (after WHERE).

  - spec:
    
    Specification built by the translator.

## Value

list(listing, out, tables).
