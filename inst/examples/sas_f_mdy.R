# MDY: each call is explained above it and followed by the result of the same call in SAS 9.4

# MDY builds a SAS date from month, day and year: 15 March 2024 is 23450.
sas_f_mdy(3, 15, 2024)  # SAS 9.4: mdy(3, 15, 2024) = 23450

# 30 February is not a valid date, so the result is missing (with a note in the
# log).
sas_f_mdy(2, 30, 2024)  # SAS 9.4: mdy(2, 30, 2024) = .

# 1 January 1960 is SAS date 0.
sas_f_mdy(1, 1, 1960)  # SAS 9.4: mdy(1, 1, 1960) = 0

# A missing argument gives a missing result.
sas_f_mdy(NA_real_, 1, 2024)  # SAS 9.4: mdy(., 1, 2024) = .
