# SAS `HMS(hour, minute, second)`

Time value from hour, minute and second. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - h:
    
    Numeric hours. Missing gives missing.

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
`sas_f_day()`, `sas_f_dhms()`, `sas_f_hour()`, `sas_f_intck()`,
`sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# HMS: each call is explained above it and followed by its result

# HMS converts hours, minutes and seconds to a SAS time value in seconds:
# 10*3600 + 30*60 + 15.
sas_f_hms(10, 30, 15)  # hms(10, 30, 15) = 37815
#> [1] 37815

# Minutes may exceed 59: 90 minutes are 5400 seconds.
sas_f_hms(0, 90, 0)  # hms(0, 90, 0) = 5400
#> [1] 5400

# Fractional seconds are kept.
sas_f_hms(1, 0, 0.25)  # hms(1, 0, 0.25) = 3600.25
#> [1] 3600.25

# A missing argument gives a missing result.
sas_f_hms(NA_real_, 1, 1)  # hms(., 1, 1) = .
#> [1] NA
```
