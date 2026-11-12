# DHMS: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DHMS builds a datetime value (seconds since 1960) from a date, hour, minute
# and second.
sas_f_dhms(23450, 10, 30, 15)  # SAS 9.4: dhms('15MAR2024'd, 10, 30, 15) = 2026117815

# Hours beyond 23 carry over to the next day: 25 hours from date 0 is 90000
# seconds.
sas_f_dhms(0, 25, 0, 0)  # SAS 9.4: dhms(0, 25, 0, 0) = 90000

# Fractional seconds are kept.
sas_f_dhms(0, 0, 0, 0.5)  # SAS 9.4: dhms(0, 0, 0, 0.5) = 0.5

# A missing argument gives a missing result.
sas_f_dhms(NA_real_, 1, 1, 1)  # SAS 9.4: dhms(., 1, 1, 1) = .
