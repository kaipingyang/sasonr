# N: each call is explained above it and followed by the result of the same call in SAS 9.4

# N counts the nonmissing numeric arguments: 2.
sas_f_n(1, NA_real_, 3)  # SAS 9.4: n(1, ., 3) = 2

# All arguments are missing, so the count is 0.
sas_f_n(NA_real_, NA_real_)  # SAS 9.4: n(., .) = 0

# The special missing value .A is not counted.
sas_f_n(sas_missing(".A"), 1)  # SAS 9.4: n(.A, 1) = 1

# 0 is a nonmissing value, so it is counted.
sas_f_n(0)  # SAS 9.4: n(0) = 1
