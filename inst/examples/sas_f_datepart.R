# DATEPART: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DATEPART extracts the date (days since 1960) from a datetime value (seconds
# since 1960).
sas_f_datepart(2026117800)  # SAS 9.4: datepart('15MAR2024:10:30:00'dt) = 23450

# Datetime 0 is midnight on 1 January 1960, which is date 0.
sas_f_datepart(0)  # SAS 9.4: datepart(0) = 0

# One second before 1960 falls on 31 December 1959, date -1.
sas_f_datepart(-1)  # SAS 9.4: datepart(-1) = -1

# A missing argument gives a missing result.
sas_f_datepart(NA_real_)  # SAS 9.4: datepart(.) = .
