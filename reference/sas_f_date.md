# SAS `DATE()`

Current date. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

SAS dates count days and SAS datetimes count seconds from 1960-01-01;
R's Date and POSIXct start at 1970-01-01.

## See also

Other datetime functions: `sas_f_datdif()`, `sas_f_datejul()`,
`sas_f_datepart()`, `sas_f_datetime()`, `sas_f_day()`, `sas_f_dhms()`,
`sas_f_hms()`, `sas_f_hour()`, `sas_f_intck()`, `sas_f_intnx()`,
`sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`, `sas_f_minute()`,
`sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`, `sas_f_time()`,
`sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`, `sas_f_weekday()`,
`sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# DATE: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATE() returns the current date (the clock of the session), so its value
# changes over time; the examples check its type and range only.

# DATE() is an alias of TODAY(): both return the current date as a SAS date
# value.
as.numeric(sas_f_date() == sas_f_today())  # date() = today() = 1
#> [1] 1

# The current date is later than 1 January 2024.
as.numeric(sas_f_date() > 23376)  # date() > '01JAN2024'd = 1
#> [1] 1

# A SAS date value is a whole number of days since 1 January 1960.
as.numeric(sas_f_date() == sas_f_int(sas_f_date()))  # date() = int(date()) = 1
#> [1] 1
```
