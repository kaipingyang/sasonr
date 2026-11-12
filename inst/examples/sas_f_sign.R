# SIGN: each call is explained above it and followed by the result of the same call in SAS 9.4

# A negative number has sign -1.
sas_f_sign(-3)  # SAS 9.4: sign(-3) = -1

# Zero has sign 0.
sas_f_sign(0)  # SAS 9.4: sign(0) = 0

# A positive number has sign 1.
sas_f_sign(2.5)  # SAS 9.4: sign(2.5) = 1

# A missing argument gives a missing result.
sas_f_sign(NA_real_)  # SAS 9.4: sign(.) = .
