# SAS `YEAR(date)`

Year. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric SAS date value (days since 1 January 1960; negative values
    are dates before 1960). A missing value gives a missing result.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datejul()`, `sas_f_datepart()`, `sas_f_datetime()`,
`sas_f_day()`, `sas_f_dhms()`, `sas_f_hms()`, `sas_f_hour()`,
`sas_f_intck()`, `sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`,
`sas_f_mdy()`, `sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`,
`sas_f_second()`, `sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`,
`sas_f_week()`, `sas_f_weekday()`, `sas_f_yrdif()`

## Examples

``` r
# YEAR: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# YEAR returns the year of a SAS date value.
sas_f_year(23450)  # year('15MAR2024'd) = 2024
#> [1] 2024

# Date 0 is 1 January 1960.
sas_f_year(0)  # year(0) = 1960
#> [1] 1960

# Date -1 is 31 December 1959.
sas_f_year(-1)  # year(-1) = 1959
#> [1] 1959

# A missing argument gives a missing result.
sas_f_year(NA_real_)  # year(.) = .
#> [1] NA
```
