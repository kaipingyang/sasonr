# SAS `YRDIF(sdate, edate <, basis>)`

Years between two dates under a day-count basis. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - start:
    
    Starting SAS date value (SAS sdate). Missing gives missing.

  - end:
    
    Ending SAS date value (SAS edate). Missing gives missing.

  - basis:
    
    Optional basis: 'AGE' (default) gives the age in completed years
    plus a fraction; 'ACT/ACT' divides actual days in each calendar year
    by 365 or 366; '30/360' uses 30-day months and 360-day years;
    'ACT/360' and 'ACT/365' divide actual days by 360 or 365.

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
`sas_f_week()`, `sas_f_weekday()`, `sas_f_year()`

## Examples

``` r
# YRDIF: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# '30/360' counts 30-day months and 360-day years: 54 months are 4.5 years.
sas_f_yrdif(21915, 23558, "30/360")  # yrdif('01JAN2020'd, '01JUL2024'd, '30/360') = 4.5
#> [1] 4.5

# 'ACT/365' divides the actual number of days by 365.
sas_f_yrdif(21915, 23558, "ACT/365")  # yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/365') = 4.5013698630137
#> [1] 4.50137
```
