# SAS `DHMS(date, hour, minute, second)`

Datetime from date, hour, minute and second. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - d:
    
    Numeric SAS date value (days since 1 January 1960; negative values
    are dates before 1960). A missing value gives a missing result.

  - h:
    
    Numeric hours; values above 23 carry over into later days. Missing
    gives missing.

  - m:
    
    Numeric minutes; values above 59 carry over. Missing gives missing.

  - s:
    
    Numeric seconds, fractions allowed. Missing gives missing.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datejul()`, `sas_f_datepart()`, `sas_f_datetime()`,
`sas_f_day()`, `sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`,
`sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DHMS: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DHMS builds a datetime value (seconds since 1960) from a date, hour, minute
# and second.
sas_f_dhms(23450, 10, 30, 15)  # dhms('15MAR2024'd, 10, 30, 15) = 2026117815
#> [1] 2026117815

# Hours beyond 23 carry over to the next day: 25 hours from date 0 is 90000
# seconds.
sas_f_dhms(0, 25, 0, 0)  # dhms(0, 25, 0, 0) = 90000
#> [1] 90000

# Fractional seconds are kept.
sas_f_dhms(0, 0, 0, 0.5)  # dhms(0, 0, 0, 0.5) = 0.5
#> [1] 0.5

# A missing argument gives a missing result.
sas_f_dhms(NA_real_, 1, 1, 1)  # dhms(., 1, 1, 1) = .
#> [1] NA
```
