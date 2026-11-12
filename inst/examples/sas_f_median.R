# MEDIAN: each call is explained above it and followed by the result of the same call in SAS 9.4

# With an even number of values the median is the mean of the middle two (2 and
# 3).
sas_f_median(1, 3, 2, 10)  # SAS 9.4: median(1, 3, 2, 10) = 2.5

# Missing values are ignored: the median of 5 and 1 is 3.
sas_f_median(5, NA_real_, 1)  # SAS 9.4: median(5, ., 1) = 3

# A single value is its own median.
sas_f_median(7)  # SAS 9.4: median(7) = 7

# All arguments are missing, so the result is missing.
sas_f_median(NA_real_, NA_real_)  # SAS 9.4: median(., .) = .
