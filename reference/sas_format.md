# Apply a SAS format to values

Numeric formats return right-justified fields of width `w`; character
formats return left-justified fields (no width: the value unchanged).

## Usage

``` r
sas_format(x, fmt = NULL)
```

## Arguments

  - fmt:
    
    Format spec (see `parse_format()`); `NULL` means BEST12. / `$`.
