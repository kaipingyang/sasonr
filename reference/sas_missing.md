# SAS missing value

`.` is `NA_real_`; special missing values `.A`-`.Z` and `._` are encoded
as NaN payloads so that they keep the SAS sort order `._ < . < .A < ...
< .Z`.

## Usage

``` r
sas_missing(code = ".")
```

## Arguments

  - code:
    
    `"."`, `"._"`, or `".A"`-`".Z"` (the leading dot is optional).

## Examples

``` r
sas_missing()      # .  (NA_real_)
#> [1] NA
a <- sas_missing(".A")
is.na(a)
#> [1] TRUE
sas_f_missing(a)   # SAS MISSING(.A) is 1
#> [1] 1
sas_f_max(a, 1)    # missing values are ignored by MAX
#> [1] 1
```
