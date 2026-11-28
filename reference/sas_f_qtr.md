# SAS `QTR(date)`

Quarter of the year. See the [SAS
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
`sas_f_mdy()`, `sas_f_minute()`, `sas_f_month()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# QTR: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# March is in quarter 1.
sas_f_qtr(23450)  # qtr('15MAR2024'd) = 1
#> [1] 1

# November is in quarter 4.
sas_f_qtr(23695)  # qtr('15NOV2024'd) = 4
#> [1] 4

# Date 0 (1 January 1960) is in quarter 1.
sas_f_qtr(0)  # qtr(0) = 1
#> [1] 1

# A missing argument gives a missing result.
sas_f_qtr(NA_real_)  # qtr(.) = .
#> [1] NA
```
