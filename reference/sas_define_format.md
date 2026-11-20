# Register a user-defined format or informat in the run-time catalogue

Register a user-defined format or informat in the run-time catalogue

## Usage

``` r
sas_define_format(def)
```

## Arguments

  - def:
    
    list(kind = "value"/"invalue", name, char, entries, default\_w,
    upcase, just); each entry: list(other, lo, hi, lo\_excl, hi\_excl,
    label, nested (format spec or NULL)).
