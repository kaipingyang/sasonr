# MONTH: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# MONTH returns the month (1 to 12) of a SAS date value: March is 3.
sas_f_month(23450)  # SAS 9.4: month('15MAR2024'd) = 3

# Date 0 is 1 January 1960, month 1.
sas_f_month(0)  # SAS 9.4: month(0) = 1

# 31 December is in month 12.
sas_f_month(23741)  # SAS 9.4: month('31DEC2024'd) = 12

# A missing argument gives a missing result.
sas_f_month(NA_real_)  # SAS 9.4: month(.) = .
