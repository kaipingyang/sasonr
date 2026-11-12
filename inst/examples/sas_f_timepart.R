# TIMEPART: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# TIMEPART extracts the time of day in seconds from a datetime value: 10:30:15
# is 37815.
sas_f_timepart(2026117815)  # SAS 9.4: timepart('15MAR2024:10:30:15'dt) = 37815

# One second before 1960 is 23:59:59 on the previous day, 86399 seconds.
sas_f_timepart(-1)  # SAS 9.4: timepart(-1) = 86399

# Datetime 0 is midnight, time 0.
sas_f_timepart(0)  # SAS 9.4: timepart(0) = 0

# A missing argument gives a missing result.
sas_f_timepart(NA_real_)  # SAS 9.4: timepart(.) = .
