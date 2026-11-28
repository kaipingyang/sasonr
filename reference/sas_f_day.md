# SAS `DAY(date)`

Day of the month. See the [SAS
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
`sas_f_dhms()`, `sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`,
`sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DAY: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DAY returns the day of the month of a SAS date value.
sas_f_day(23450)  # day('15MAR2024'd) = 15
#> [1] 15

# Date 0 is 1 January 1960, so the day is 1.
sas_f_day(0)  # day(0) = 1
#> [1] 1

# 29 February exists in the leap year 2024.
sas_f_day(23435)  # day('29FEB2024'd) = 29
#> [1] 29

# A missing argument gives a missing result.
sas_f_day(NA_real_)  # day(.) = .
#> [1] NA
```
