# TAN: each call is explained above it and followed by the result of the same call in SAS 9.4

# The tangent of 0 is 0.
sas_f_tan(0)  # SAS 9.4: tan(0) = 0

# The tangent of 1 radian.
sas_f_tan(1)  # SAS 9.4: tan(1) = 1.5574077246549

# The tangent is odd, so tan(-0.5) = -tan(0.5).
sas_f_tan(-0.5)  # SAS 9.4: tan(-0.5) = -0.54630248984379

# A missing argument gives a missing result.
sas_f_tan(NA_real_)  # SAS 9.4: tan(.) = .
