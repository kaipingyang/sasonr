# SKEWNESS: each call is explained above it and followed by its result

# The sample skewness; the large value 10 gives a positive skew.
sas_f_skewness(1, 2, 3, 10)  # skewness(1, 2, 3, 10) = 1.76363261480389

# Missing values are ignored; 1, 2, 3 are symmetric, so the skewness is 0.
sas_f_skewness(1, 2, NA_real_, 3)  # skewness(1, 2, ., 3) = 0

# The sample skewness of eight values.
sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9)  # skewness(2, 4, 4, 4, 5, 5, 7, 9) = 0.8184875533568

# Values with zero variance have no defined skewness, so the result is missing.
sas_f_skewness(1, 1, 1)  # skewness(1, 1, 1) = .
