# SIN: each call is explained above it and followed by the result of the same call in SAS 9.4

# The sine of 0 is 0.
sas_f_sin(0)  # SAS 9.4: sin(0) = 0

# The sine of 1 radian.
sas_f_sin(1)  # SAS 9.4: sin(1) = 0.841470984807897

# -3.5 radians lies in the second quadrant, so the sine is positive.
sas_f_sin(-3.5)  # SAS 9.4: sin(-3.5) = 0.35078322768962

# A missing argument gives a missing result.
sas_f_sin(NA_real_)  # SAS 9.4: sin(.) = .
