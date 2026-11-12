# MAX: each call is explained above it and followed by the result of the same call in SAS 9.4

# MAX returns the largest argument, 5.
sas_f_max(1, 5, 3)  # SAS 9.4: max(1, 5, 3) = 5

# Missing values are ignored, so the result is 2.
sas_f_max(NA_real_, 2)  # SAS 9.4: max(., 2) = 2

# All arguments are missing, so the result is missing.
sas_f_max(NA_real_, NA_real_)  # SAS 9.4: max(., .) = .

# Special missing values such as .A are ignored as well.
sas_f_max(sas_missing(".A"), 1)  # SAS 9.4: max(.A, 1) = 1

# The special missing value .B is ignored, so -1 is the largest value.
sas_f_max(-1, sas_missing(".B"))  # SAS 9.4: max(-1, .B) = -1
