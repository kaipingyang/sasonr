# MEAN: each call is explained above it and followed by the result of the same call in SAS 9.4

# MEAN returns the arithmetic mean, 2.5.
sas_f_mean(1, 2, 3, 4)  # SAS 9.4: mean(1, 2, 3, 4) = 2.5

# Missing values are ignored: (1 + 3) / 2 = 2.
sas_f_mean(1, NA_real_, 3)  # SAS 9.4: mean(1, ., 3) = 2

# All arguments are missing, so the result is missing.
sas_f_mean(NA_real_, NA_real_)  # SAS 9.4: mean(., .) = .

# The special missing value .A is ignored, so the mean is 4.
sas_f_mean(sas_missing(".A"), 4)  # SAS 9.4: mean(.A, 4) = 4
