# SECOND: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# SECOND extracts the seconds from a SAS time value.
sas_f_second(37815)  # SAS 9.4: second('10:30:15't) = 15

# Fractional seconds of a datetime value are kept: 15.5.
sas_f_second(2026117815.5)  # SAS 9.4: second('15MAR2024:10:30:15.5'dt) = 15.5

# Time 0 is midnight, second 0.
sas_f_second(0)  # SAS 9.4: second(0) = 0

# A missing argument gives a missing result.
sas_f_second(NA_real_)  # SAS 9.4: second(.) = .
