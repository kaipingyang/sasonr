# SAS `JULDATE7(date)`

Seven-digit Julian date (yyyyddd). See the [SAS
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
`sas_f_day()`, `sas_f_dhms()`, `sas_f_hms()`, `sas_f_hour()`,
`sas_f_intck()`, `sas_f_intnx()`, `sas_f_juldate()`, `sas_f_mdy()`,
`sas_f_minute()`, `sas_f_month()`, `sas_f_qtr()`, `sas_f_second()`,
`sas_f_time()`, `sas_f_timepart()`, `sas_f_today()`, `sas_f_week()`,
`sas_f_weekday()`, `sas_f_year()`, `sas_f_yrdif()`

## Examples

``` r
# JULDATE7: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# JULDATE7 always returns a 7-digit Julian date yyyyddd: day 75 of 2024 is
# 2024075.
sas_f_juldate7(23450)  # juldate7('15MAR2024'd) = 2024075
#> [1] 2024075

# SAS date 0 (1 January 1960) is 1960001.
sas_f_juldate7(0)  # juldate7(0) = 1960001
#> [1] 1960001

# 31 December 1899 is day 365 of 1899.
sas_f_juldate7(-21915)  # juldate7('31DEC1899'd) = 1899365
#> [1] 1899365

# A missing argument gives a missing result.
sas_f_juldate7(NA_real_)  # juldate7(.) = .
#> [1] NA
```
