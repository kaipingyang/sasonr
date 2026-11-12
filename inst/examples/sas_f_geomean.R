# GEOMEAN: each call is explained above it and followed by the result of the same call in SAS 9.4

# The geometric mean of 1, 2 and 4 is the cube root of 8, which is 2.
sas_f_geomean(1, 2, 4)  # SAS 9.4: geomean(1, 2, 4) = 2

# Missing values are ignored: the geometric mean of 1 and 9 is 3.
sas_f_geomean(1, NA_real_, 9)  # SAS 9.4: geomean(1, ., 9) = 3

# If any value is zero, the geometric mean is 0.
sas_f_geomean(0, 4)  # SAS 9.4: geomean(0, 4) = 0

# All arguments are missing, so the result is missing.
sas_f_geomean(NA_real_, NA_real_)  # SAS 9.4: geomean(., .) = .
