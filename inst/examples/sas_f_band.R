# BAND: each call is explained above it and followed by its result

# Bitwise AND of 1100 (12) and 1010 (10) keeps only the common bit 1000, which
# is 8.
sas_f_band(12, 10)  # band(12, 10) = 8

# ANDing with 15 (binary 1111) keeps the low four bits of 255, giving 15.
sas_f_band(255, 15)  # band(255, 15) = 15

# A missing argument gives a missing result.
sas_f_band(NA_real_, 1)  # band(., 1) = .
