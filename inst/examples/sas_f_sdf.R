# SDF: each call is explained above it and followed by the result of the same call in SAS 9.4

# Exponential distribution with the default scale 1: P(X > 1) = exp(-1).
sas_f_sdf("EXPONENTIAL", 1)  # SAS 9.4: sdf('EXPONENTIAL', 1) = 0.367879441171442

# A missing quantile gives a missing result.
sas_f_sdf("NORMAL", NA_real_)  # SAS 9.4: sdf('NORMAL', .) = .
