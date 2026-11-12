# CDF: each call is explained above it and followed by the result of the same call in SAS 9.4

# P(X <= 2) for a Poisson variable with mean 1.5.
sas_f_cdf("POISSON", 2, 1.5)  # SAS 9.4: cdf('POISSON', 2, 1.5) = 0.808846830538058

# A missing quantile gives a missing result.
sas_f_cdf("NORMAL", NA_real_)  # SAS 9.4: cdf('NORMAL', .) = .
