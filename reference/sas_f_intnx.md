# SAS `INTNX(interval, start, increment <, alignment>)`

Date advanced by a number of intervals. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - interval:
    
    Character interval name such as 'DAY', 'WEEK', 'MONTH', 'QTR',
    'YEAR', 'DTDAY', 'HOUR'; may include a multiplier and shift index
    (e.g. 'MONTH2.2').

  - start:
    
    Starting date, time or datetime value (must match the interval
    type).

  - n:
    
    Integer number of intervals to move; negative moves backwards, 0
    stays in the interval of `start`. Non-integers are truncated.

  - align:
    
    Optional alignment of the result within the interval: 'BEGINNING'
    ('B', default), 'MIDDLE' ('M'), 'END' ('E') or 'SAME' ('S', same
    position as `start`).

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
`sas_f_intck()`, `sas_f_juldate()`, `sas_f_juldate7()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# INTNX: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Advance 1 month: the beginning of the next month (alignment 'b' is the
# default).
sas_f_intnx("month", 21929, 1)  # intnx('month','15JAN2020'd,1) = 21946
#> [1] 21946

# Alignment 'e': the end of the next month.
sas_f_intnx("month", 21929, 1, "e")  # intnx('month','15JAN2020'd,1,'e') = 21974
#> [1] 21974

# Alignment 's': the same day in the next month.
sas_f_intnx("month", 21929, 1, "s")  # intnx('month','15JAN2020'd,1,'s') = 21960
#> [1] 21960

# Alignment 's' from 31 Jan: the same day does not exist in February, so the
# last day (29 Feb 2020).
sas_f_intnx("month", 21945, 1, "s")  # intnx('month','31JAN2020'd,1,'s') = 21974
#> [1] 21974

# A negative increment goes back: the beginning of the previous month.
sas_f_intnx("month", 21929, -1)  # intnx('month','15JAN2020'd,-1) = 21884
#> [1] 21884
```
