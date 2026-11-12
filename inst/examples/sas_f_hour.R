# HOUR: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# HOUR extracts the hour from a SAS time value.
sas_f_hour(37815)  # SAS 9.4: hour('10:30:15't) = 10

# HOUR also accepts a datetime value and returns the hour of the day.
sas_f_hour(2026166399)  # SAS 9.4: hour('15MAR2024:23:59:59'dt) = 23

# Time 0 is midnight, hour 0.
sas_f_hour(0)  # SAS 9.4: hour(0) = 0

# A missing argument gives a missing result.
sas_f_hour(NA_real_)  # SAS 9.4: hour(.) = .
