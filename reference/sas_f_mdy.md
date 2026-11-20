# SAS `MDY(month, day, year)`

SAS date from month, day and year. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - m:
    
    Month, an integer from 1 to 12.

  - d:
    
    Day of the month, an integer valid for the month and year.

  - y:
    
    Year with 2 or 4 digits; a 2-digit year is resolved with the
    YEARCUTOFF option. An invalid combination or a missing argument
    gives missing (with a note for invalid dates).

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
`sas_f_intck()`, `sas_f_intnx()`, `sas_f_juldate()`, `sas_f_juldate7()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# MDY: each call is explained above it and followed by its result

# MDY builds a SAS date from month, day and year: 15 March 2024 is 23450.
sas_f_mdy(3, 15, 2024)  # mdy(3, 15, 2024) = 23450
#> [1] 23450

# 30 February is not a valid date, so the result is missing (with a note in the
# log).
sas_f_mdy(2, 30, 2024)  # mdy(2, 30, 2024) = .
#> Warning: NOTE: Invalid argument to function MDY(2,30,2024).
#> [1] NA

# 1 January 1960 is SAS date 0.
sas_f_mdy(1, 1, 1960)  # mdy(1, 1, 1960) = 0
#> [1] 0

# A missing argument gives a missing result.
sas_f_mdy(NA_real_, 1, 2024)  # mdy(., 1, 2024) = .
#> [1] NA
```
