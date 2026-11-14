# WEEKDAY: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# WEEKDAY returns 1 for Sunday through 7 for Saturday; 15 March 2024 was a
# Friday, 6.
sas_f_weekday(23450)  # weekday('15MAR2024'd) = 6

# 1 January 1960 (date 0) was a Friday.
sas_f_weekday(0)  # weekday(0) = 6

# 17 March 2024 was a Sunday.
sas_f_weekday(23452)  # weekday('17MAR2024'd) = 1

# A missing argument gives a missing result.
sas_f_weekday(NA_real_)  # weekday(.) = .
