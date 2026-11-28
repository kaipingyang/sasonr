# SAS `HOUR(time | datetime)`

Hour. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric SAS time value (seconds since midnight) or datetime value
    (seconds since 1 January 1960); only the time of day is used. A
    missing value gives a missing result.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datejul()`, `sas_f_datepart()`, `sas_f_datetime()`,
`sas_f_day()`, `sas_f_dhms()`, `sas_f_hms()`, `sas_f_intck()`,
`sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# HOUR: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# HOUR extracts the hour from a SAS time value.
sas_f_hour(37815)  # hour('10:30:15't) = 10
#> [1] 10

# HOUR also accepts a datetime value and returns the hour of the day.
sas_f_hour(2026166399)  # hour('15MAR2024:23:59:59'dt) = 23
#> [1] 23

# Time 0 is midnight, hour 0.
sas_f_hour(0)  # hour(0) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_hour(NA_real_)  # hour(.) = .
#> [1] NA
```
