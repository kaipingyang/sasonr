# USS: each call is explained above it and followed by the result of the same call in SAS 9.4

# USS is the uncorrected sum of squares: 1 + 4 + 9 = 14.
sas_f_uss(1, 2, 3)  # SAS 9.4: uss(1, 2, 3) = 14

# Missing values are ignored: 1 + 4 = 5.
sas_f_uss(1, NA_real_, 2)  # SAS 9.4: uss(1, ., 2) = 5

# All arguments are missing, so the result is missing.
sas_f_uss(NA_real_, NA_real_)  # SAS 9.4: uss(., .) = .
