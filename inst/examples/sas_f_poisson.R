# POISSON: each call is explained above it and followed by its result

# P(X <= 0) with mean 0.5 is exp(-0.5).
sas_f_poisson(0.5, 0)  # poisson(0.5, 0) = 0.606530659712633

# A missing mean gives a missing result.
sas_f_poisson(NA_real_, 1)  # poisson(., 1) = .
