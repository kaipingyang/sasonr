# SUM: each call is explained above it and followed by the result of the same call in SAS 9.4

# SUM adds its arguments: 6.
sas_f_sum(1, 2, 3)  # SAS 9.4: sum(1, 2, 3) = 6

# Missing values are ignored: 1 + 3 = 4.
sas_f_sum(1, NA_real_, 3)  # SAS 9.4: sum(1, ., 3) = 4

# All arguments are missing, so the result is missing.
sas_f_sum(NA_real_, NA_real_)  # SAS 9.4: sum(., .) = .

# The special missing value .A is ignored, so the sum is 1.
sas_f_sum(sas_missing(".A"), 1)  # SAS 9.4: sum(.A, 1) = 1
