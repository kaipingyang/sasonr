# SAS `INTCK(interval, start, end <, method>)`

Number of interval boundaries between two dates. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - interval:
    
    Character interval name such as 'DAY', 'WEEK', 'MONTH', 'QTR',
    'YEAR', 'DTDAY', 'HOUR'; may include a multiplier and shift index
    (e.g. 'MONTH2.2').

  - from:
    
    Starting date, time or datetime value (must match the interval
    type).

  - to:
    
    Ending date, time or datetime value.

  - method:
    
    Optional counting method: 'DISCRETE' (default, 'D') counts interval
    boundaries crossed; 'CONTINUOUS' ('C') counts complete intervals
    measured from `from`.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

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
`sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# INTCK: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Discrete method (default): one month boundary between 31 Jan and 1 Feb 2020.
sas_f_intck("month", 21945, 21946)  # intck('month','31JAN2020'd,'01FEB2020'd) = 1
#> [1] 1

# Continuous method 'c': less than a full month between 31 Jan and 1 Feb 2020,
# so 0.
sas_f_intck("month", 21945, 21946, "c")  # intck('month','31JAN2020'd,'01FEB2020'd,'c') = 0
#> [1] 0

# One year boundary between 31 Dec 2019 and 1 Jan 2020.
sas_f_intck("year", 21914, 21915)  # intck('year','31DEC2019'd,'01JAN2020'd) = 1
#> [1] 1

# A start after the end counts boundaries backwards: -2 months.
sas_f_intck("month", 21975, 21915)  # intck('month','01MAR2020'd,'01JAN2020'd) = -2
#> [1] -2
```
