# SAS `DATETIME()`

Current datetime. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_date()`,
`sas_f_datejul()`, `sas_f_datepart()`, `sas_f_day()`, `sas_f_dhms()`,
`sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`, `sas_f_intnx()`,
`sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`, `sas_f_minute()`,
`sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`, `sas_f_time()`,
`sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`, `sas_f_weekday()`,
`sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DATETIME: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATETIME() returns the current date and time (the clock of the session), so
# its value changes over time; the examples check its type and range only.

# DATETIME() returns the current date and time in seconds since midnight, 1
# January 1960.
as.numeric(sas_f_datetime() > 2019686400)  # datetime() > '01JAN2024:00:00:00'dt = 1
#> [1] 1

# The date part of the current datetime equals today's date.
as.numeric(sas_f_datepart(sas_f_datetime()) == sas_f_today())  # datepart(datetime()) = today() = 1
#> [1] 1
```
