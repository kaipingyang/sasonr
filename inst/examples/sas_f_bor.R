# BOR: each call is explained above it and followed by the result of the same call in SAS 9.4

# Bitwise OR of 1100 (12) and 1010 (10) is 1110, which is 14.
sas_f_bor(12, 10)  # SAS 9.4: bor(12, 10) = 14

# The OR of 0 and 0 is 0.
sas_f_bor(0, 0)  # SAS 9.4: bor(0, 0) = 0

# A missing argument gives a missing result.
sas_f_bor(NA_real_, 1)  # SAS 9.4: bor(., 1) = .
