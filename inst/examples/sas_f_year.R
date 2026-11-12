# YEAR: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# YEAR returns the year of a SAS date value.
sas_f_year(23450)  # SAS 9.4: year('15MAR2024'd) = 2024

# Date 0 is 1 January 1960.
sas_f_year(0)  # SAS 9.4: year(0) = 1960

# Date -1 is 31 December 1959.
sas_f_year(-1)  # SAS 9.4: year(-1) = 1959

# A missing argument gives a missing result.
sas_f_year(NA_real_)  # SAS 9.4: year(.) = .
