# MIN: each call is explained above it and followed by its result

# MIN returns the smallest argument, 1.
sas_f_min(1, 5, 3)  # min(1, 5, 3) = 1

# Missing values are ignored, so the result is 2.
sas_f_min(NA_real_, 2)  # min(., 2) = 2

# All arguments are missing, so the result is missing.
sas_f_min(NA_real_, NA_real_)  # min(., .) = .

# Special missing values such as .A are ignored as well.
sas_f_min(sas_missing(".A"), 1)  # min(.A, 1) = 1
