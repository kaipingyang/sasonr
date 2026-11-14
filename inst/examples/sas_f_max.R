# MAX: each call is explained above it and followed by its result

# MAX returns the largest argument, 5.
sas_f_max(1, 5, 3)  # max(1, 5, 3) = 5

# Missing values are ignored, so the result is 2.
sas_f_max(NA_real_, 2)  # max(., 2) = 2

# All arguments are missing, so the result is missing.
sas_f_max(NA_real_, NA_real_)  # max(., .) = .

# Special missing values such as .A are ignored as well.
sas_f_max(sas_missing(".A"), 1)  # max(.A, 1) = 1

# The special missing value .B is ignored, so -1 is the largest value.
sas_f_max(-1, sas_missing(".B"))  # max(-1, .B) = -1
