# DATEJUL: each call is explained above it and followed by the result of the same call in SAS 9.4

# DATEJUL converts a Julian date yyyyddd (day 75 of 2024) to a SAS date value:
# 15 March 2024 is 23450.
sas_f_datejul(2024075)  # SAS 9.4: datejul(2024075) = 23450

# Day 1 of 2024 is 1 January 2024, SAS date 23376.
sas_f_datejul(2024001)  # SAS 9.4: datejul(2024001) = 23376

# 1 January 1960 is SAS date 0, the origin of SAS dates.
sas_f_datejul(1960001)  # SAS 9.4: datejul(1960001) = 0

# A missing argument gives a missing result.
sas_f_datejul(NA_real_)  # SAS 9.4: datejul(.) = .
