# BXOR: each call is explained above it and followed by the result of the same call in SAS 9.4

# Bitwise exclusive OR of 1100 (12) and 1010 (10) is 0110, which is 6.
sas_f_bxor(12, 10)  # SAS 9.4: bxor(12, 10) = 6

# The exclusive OR of a value with itself is always 0.
sas_f_bxor(5, 5)  # SAS 9.4: bxor(5, 5) = 0

# A missing argument gives a missing result.
sas_f_bxor(NA_real_, 1)  # SAS 9.4: bxor(., 1) = .
