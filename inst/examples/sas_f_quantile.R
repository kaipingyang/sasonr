# QUANTILE: each call is explained above it and followed by its result

# For a discrete distribution the quantile is the smallest x with CDF >= p: the
# binomial median is 5.
sas_f_quantile("BINOMIAL", 0.5, 0.5, 10)  # quantile('BINOMIAL', 0.5, 0.5, 10) = 5

# A missing probability gives a missing result.
sas_f_quantile("NORMAL", NA_real_)  # quantile('NORMAL', .) = .
