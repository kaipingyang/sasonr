# PCTL: each call is explained above it and followed by the result of the same call in SAS 9.4

# PCTL(50, ...) is the median: for 1, 2, 3, 4 the average of the middle two
# values, 2.5.
sas_f_pctl(50, 1, 2, 3, 4)  # SAS 9.4: pctl(50, 1, 2, 3, 4) = 2.5

# The 25th percentile of 1 to 4: n * p = 1 is a whole number, so the 1st and
# 2nd values are averaged, 1.5.
sas_f_pctl(25, 1, 2, 3, 4)  # SAS 9.4: pctl(25, 1, 2, 3, 4) = 1.5

# Missing values are ignored, and the 100th percentile is the maximum, 9.
sas_f_pctl(100, 5, NA_real_, 9)  # SAS 9.4: pctl(100, 5, ., 9) = 9

# Any percentile of a single value is that value.
sas_f_pctl(10, 7)  # SAS 9.4: pctl(10, 7) = 7
