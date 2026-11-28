# SAS `DATEJUL(julian-date)`

SAS date from a Julian date (yyddd or yyyyddd). See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - j:
    
    Julian date as a number yyddd or yyyyddd (year followed by the
    3-digit day of the year); a 2-digit year is resolved with the
    YEARCUTOFF option. Missing gives missing.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datepart()`, `sas_f_datetime()`, `sas_f_day()`, `sas_f_dhms()`,
`sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`, `sas_f_intnx()`,
`sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`, `sas_f_minute()`,
`sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`, `sas_f_time()`,
`sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`, `sas_f_weekday()`,
`sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DATEJUL: each call is explained above it and followed by its result

# DATEJUL converts a Julian date yyyyddd (day 75 of 2024) to a SAS date value:
# 15 March 2024 is 23450.
sas_f_datejul(2024075)  # datejul(2024075) = 23450
#> [1] 23450

# Day 1 of 2024 is 1 January 2024, SAS date 23376.
sas_f_datejul(2024001)  # datejul(2024001) = 23376
#> [1] 23376

# 1 January 1960 is SAS date 0, the origin of SAS dates.
sas_f_datejul(1960001)  # datejul(1960001) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_datejul(NA_real_)  # datejul(.) = .
#> [1] NA
```
