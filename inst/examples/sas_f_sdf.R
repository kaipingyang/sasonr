# SDF: each call is explained above it and followed by its result

# Exponential distribution with the default scale 1: P(X > 1) = exp(-1).
sas_f_sdf("EXPONENTIAL", 1)  # sdf('EXPONENTIAL', 1) = 0.367879441171442

# A missing quantile gives a missing result.
sas_f_sdf("NORMAL", NA_real_)  # sdf('NORMAL', .) = .
