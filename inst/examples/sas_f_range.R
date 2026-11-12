# RANGE: each call is explained above it and followed by the result of the same call in SAS 9.4

# RANGE returns the maximum minus the minimum: 5 - 1 = 4.
sas_f_range(1, 5, 3)  # SAS 9.4: range(1, 5, 3) = 4

# Missing values are ignored; a single value has range 0.
sas_f_range(NA_real_, 2)  # SAS 9.4: range(., 2) = 0

# All arguments are missing, so the result is missing.
sas_f_range(NA_real_, NA_real_)  # SAS 9.4: range(., .) = .

# The range of -1 and 1 is 2.
sas_f_range(-1, 1)  # SAS 9.4: range(-1, 1) = 2
