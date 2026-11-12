# QUANTILE: each call is explained above it and followed by the result of the same call in SAS 9.4

# For a discrete distribution the quantile is the smallest x with CDF >= p: the
# binomial median is 5.
sas_f_quantile("BINOMIAL", 0.5, 0.5, 10)  # SAS 9.4: quantile('BINOMIAL', 0.5, 0.5, 10) = 5

# A missing probability gives a missing result.
sas_f_quantile("NORMAL", NA_real_)  # SAS 9.4: quantile('NORMAL', .) = .
