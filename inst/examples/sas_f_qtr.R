# QTR: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# March is in quarter 1.
sas_f_qtr(23450)  # SAS 9.4: qtr('15MAR2024'd) = 1

# November is in quarter 4.
sas_f_qtr(23695)  # SAS 9.4: qtr('15NOV2024'd) = 4

# Date 0 (1 January 1960) is in quarter 1.
sas_f_qtr(0)  # SAS 9.4: qtr(0) = 1

# A missing argument gives a missing result.
sas_f_qtr(NA_real_)  # SAS 9.4: qtr(.) = .
