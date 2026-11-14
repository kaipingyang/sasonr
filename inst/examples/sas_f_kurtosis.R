# KURTOSIS: each call is explained above it and followed by its result

# KURTOSIS returns the sample excess kurtosis (about 0 for normal data); the
# outlier 10 makes it large.
sas_f_kurtosis(1, 2, 3, 4, 10)  # kurtosis(1, 2, 3, 4, 10) = 3.152

# The sample excess kurtosis of eight values.
sas_f_kurtosis(2, 4, 4, 4, 5, 5, 7, 9)  # kurtosis(2, 4, 4, 4, 5, 5, 7, 9) = 0.940625

# Missing values are ignored.
sas_f_kurtosis(1, 2, NA_real_, 3, 4, 8)  # kurtosis(1, 2, ., 3, 4, 8) = 2.02101707637456

# Values with zero variance have no defined kurtosis, so the result is missing.
sas_f_kurtosis(1, 1, 1, 1)  # kurtosis(1, 1, 1, 1) = .
