# COS: each call is explained above it and followed by the result of the same call in SAS 9.4

# The cosine of 0 is 1.
sas_f_cos(0)  # SAS 9.4: cos(0) = 1

# The cosine of 1 radian.
sas_f_cos(1)  # SAS 9.4: cos(1) = 0.54030230586814

# Cosine is an even function, so cos(-3.5) equals cos(3.5).
sas_f_cos(-3.5)  # SAS 9.4: cos(-3.5) = -0.936456687290796

# A missing argument gives a missing result.
sas_f_cos(NA_real_)  # SAS 9.4: cos(.) = .
