# SINH: each call is explained above it and followed by the result of the same call in SAS 9.4

# The hyperbolic sine of 0 is 0.
sas_f_sinh(0)  # SAS 9.4: sinh(0) = 0

# The hyperbolic sine of 1 is (e - 1/e) / 2.
sas_f_sinh(1)  # SAS 9.4: sinh(1) = 1.1752011936438

# A missing argument gives a missing result.
sas_f_sinh(NA_real_)  # SAS 9.4: sinh(.) = .
