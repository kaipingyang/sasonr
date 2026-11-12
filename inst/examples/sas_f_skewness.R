# SKEWNESS: each call is explained above it and followed by the result of the same call in SAS 9.4

# Missing values are ignored; 1, 2, 3 are symmetric, so the skewness is 0.
sas_f_skewness(1, 2, NA_real_, 3)  # SAS 9.4: skewness(1, 2, ., 3) = 0

# The sample skewness of eight values.
sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9)  # SAS 9.4: skewness(2, 4, 4, 4, 5, 5, 7, 9) = 0.8184875533568

# Values with zero variance have no defined skewness, so the result is missing.
sas_f_skewness(1, 1, 1)  # SAS 9.4: skewness(1, 1, 1) = .
