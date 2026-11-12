# LARGEST: each call is explained above it and followed by the result of the same call in SAS 9.4

# LARGEST(k, ...) returns the k-th largest nonmissing value; with k = 1 that is
# the maximum of 3, 9, 5: 9.
sas_f_largest(1, 3, 9, 5)  # SAS 9.4: largest(1, 3, 9, 5) = 9

# With k = 2 the second largest of 3, 9, 5 is 5.
sas_f_largest(2, 3, 9, 5)  # SAS 9.4: largest(2, 3, 9, 5) = 5

# Missing values are ignored: the largest of the remaining value 4 is 4.
sas_f_largest(1, NA_real_, 4, NA_real_)  # SAS 9.4: largest(1, ., 4, .) = 4

# k = 3 exceeds the number of nonmissing values (2), so the result is missing.
sas_f_largest(3, 3, NA_real_)  # SAS 9.4: largest(3, 3, .) = .
