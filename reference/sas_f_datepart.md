# SAS `DATEPART(datetime)`

Date part of a datetime value. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric SAS datetime value (seconds since midnight, 1 January 1960).
    A missing value gives a missing result.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datejul()`, `sas_f_datetime()`, `sas_f_day()`, `sas_f_dhms()`,
`sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`, `sas_f_intnx()`,
`sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`, `sas_f_minute()`,
`sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`, `sas_f_time()`,
`sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`, `sas_f_weekday()`,
`sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DATEPART: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DATEPART extracts the date (days since 1960) from a datetime value (seconds
# since 1960).
sas_f_datepart(2026117800)  # datepart('15MAR2024:10:30:00'dt) = 23450
#> [1] 23450

# Datetime 0 is midnight on 1 January 1960, which is date 0.
sas_f_datepart(0)  # datepart(0) = 0
#> [1] 0

# One second before 1960 falls on 31 December 1959, date -1.
sas_f_datepart(-1)  # datepart(-1) = -1
#> [1] -1

# A missing argument gives a missing result.
sas_f_datepart(NA_real_)  # datepart(.) = .
#> [1] NA
```
