# SMALLEST: each call is explained above it and followed by the result of the same call in SAS 9.4

# SMALLEST(k, ...) returns the k-th smallest nonmissing value; with k = 1 the
# minimum of 3, 9, 5 is 3.
sas_f_smallest(1, 3, 9, 5)  # SAS 9.4: smallest(1, 3, 9, 5) = 3

# Missing values are ignored: with k = 2 the second smallest of 9 and 5 is 9.
sas_f_smallest(2, NA_real_, 9, 5)  # SAS 9.4: smallest(2, ., 9, 5) = 9

# k = 3 exceeds the number of values (2), so the result is missing.
sas_f_smallest(3, 1, 2)  # SAS 9.4: smallest(3, 1, 2) = .

# With k = 1 but no nonmissing value, the result is missing.
sas_f_smallest(1, NA_real_, NA_real_)  # SAS 9.4: smallest(1, ., .) = .
