# MINUTE: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# MINUTE extracts the minute from a SAS time value.
sas_f_minute(37815)  # SAS 9.4: minute('10:30:15't) = 30

# MINUTE also accepts a datetime value.
sas_f_minute(2026166399)  # SAS 9.4: minute('15MAR2024:23:59:59'dt) = 59

# Time 0 is midnight, minute 0.
sas_f_minute(0)  # SAS 9.4: minute(0) = 0

# A missing argument gives a missing result.
sas_f_minute(NA_real_)  # SAS 9.4: minute(.) = .
