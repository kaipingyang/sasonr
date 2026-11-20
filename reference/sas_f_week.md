# SAS `WEEK(<date <, descriptor>>)`

Week number of the year. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Optional SAS date value; when omitted, today's date is used. Missing
    gives missing.

  - desc:
    
    Optional descriptor (first letter used): 'U' (default) weeks start
    on Sunday, days before the first Sunday are week 0; 'V' ISO 8601
    weeks starting on Monday, week 1 contains 4 January; 'W' weeks start
    on Monday, days before the first Monday are week 0.

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
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# WEEK: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# With the default descriptor 'u' weeks start on Sunday and days before the
# first Sunday are week 0: week 10.
sas_f_week(23450)  # week('15MAR2024'd) = 10
#> [1] 10

# Descriptor 'v' gives the ISO 8601 week (weeks start on Monday; week 1
# contains 4 January): week 11.
sas_f_week(23450, "v")  # week('15MAR2024'd, 'v') = 11
#> [1] 11

# Descriptor 'w' counts weeks starting on Monday, with days before the first
# Monday in week 0: week 11.
sas_f_week(23450, "w")  # week('15MAR2024'd, 'w') = 11
#> [1] 11

# Under ISO 8601 rules Sunday 1 January 2023 belongs to week 52 of the previous
# year.
sas_f_week(23011, "v")  # week('01JAN2023'd, 'v') = 52
#> [1] 52
```
