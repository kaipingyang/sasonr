# SAS `DATDIF(sdate, edate, basis)`

Days between two dates under a day-count basis. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - start:
    
    Starting SAS date value (SAS sdate). Missing gives missing.

  - end:
    
    Ending SAS date value (SAS edate); before `start` gives a negative
    result. Missing gives missing.

  - basis:
    
    Day-count basis: 'ACT/ACT' (or 'ACTUAL') counts actual days;
    '30/360' (or '360') counts 30-day months, treating the 31st as the
    30th. Default 'ACT/ACT'.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_date()`, `sas_f_datejul()`,
`sas_f_datepart()`, `sas_f_datetime()`, `sas_f_day()`, `sas_f_dhms()`,
`sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`, `sas_f_intnx()`,
`sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`, `sas_f_minute()`,
`sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`, `sas_f_time()`,
`sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`, `sas_f_weekday()`,
`sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DATDIF: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# With basis 'ACT/ACT' DATDIF counts actual days: 31 in January plus 29 in
# February 2024 = 60.
sas_f_datdif(23376, 23436, "ACT/ACT")  # datdif('01JAN2024'd, '01MAR2024'd, 'ACT/ACT') = 60
#> [1] 60

# Basis '30/360' treats every month as 30 days, so two months are 60 days.
sas_f_datdif(23376, 23436, "30/360")  # datdif('01JAN2024'd, '01MAR2024'd, '30/360') = 60
#> [1] 60

# Under '30/360' the 31st counts as the 30th, so 31 January to 1 March is 31
# days.
sas_f_datdif(23406, 23436, "30/360")  # datdif('31JAN2024'd, '01MAR2024'd, '30/360') = 31
#> [1] 31

# If the end date is before the start date, the result is negative.
sas_f_datdif(23436, 23376, "ACT/ACT")  # datdif('01MAR2024'd, '01JAN2024'd, 'ACT/ACT') = -60
#> [1] -60
```
