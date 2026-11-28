# SAS `TIMEPART(datetime)`

Time part of a datetime value. See the [SAS
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
`sas_f_datejul()`, `sas_f_datepart()`, `sas_f_datetime()`,
`sas_f_day()`, `sas_f_dhms()`, `sas_f_hms()`, `sas_f_hour()`,
`sas_f_intck()`, `sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`,
`sas_f_mdy()`, `sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`,
`sas_f_second()`, `sas_f_time()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# TIMEPART: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# TIMEPART extracts the time of day in seconds from a datetime value: 10:30:15
# is 37815.
sas_f_timepart(2026117815)  # timepart('15MAR2024:10:30:15'dt) = 37815
#> [1] 37815

# One second before 1960 is 23:59:59 on the previous day, 86399 seconds.
sas_f_timepart(-1)  # timepart(-1) = 86399
#> [1] 86399

# Datetime 0 is midnight, time 0.
sas_f_timepart(0)  # timepart(0) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_timepart(NA_real_)  # timepart(.) = .
#> [1] NA
```
