# ORDINAL: each call is explained above it and followed by the result of the same call in SAS 9.4

# ORDINAL(k, ...) returns the k-th smallest argument, counting missing values
# as smallest: the 2nd of 5, 1, 3 is 3.
sas_f_ordinal(2, 5, 1, 3)  # SAS 9.4: ordinal(2, 5, 1, 3) = 3

# Missing values sort lowest, so the smallest of ., 2 is the missing value.
sas_f_ordinal(1, NA_real_, 2)  # SAS 9.4: ordinal(1, ., 2) = .

# The 3rd smallest of 3, 1, 2 is 3.
sas_f_ordinal(3, 3, 1, 2)  # SAS 9.4: ordinal(3, 3, 1, 2) = 3
