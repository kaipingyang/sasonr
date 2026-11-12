# DAY: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# DAY returns the day of the month of a SAS date value.
sas_f_day(23450)  # SAS 9.4: day('15MAR2024'd) = 15

# Date 0 is 1 January 1960, so the day is 1.
sas_f_day(0)  # SAS 9.4: day(0) = 1

# 29 February exists in the leap year 2024.
sas_f_day(23435)  # SAS 9.4: day('29FEB2024'd) = 29

# A missing argument gives a missing result.
sas_f_day(NA_real_)  # SAS 9.4: day(.) = .
