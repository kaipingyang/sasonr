# BAND: each call is explained above it and followed by the result of the same call in SAS 9.4

# Bitwise AND of 1100 (12) and 1010 (10) keeps only the common bit 1000, which
# is 8.
sas_f_band(12, 10)  # SAS 9.4: band(12, 10) = 8

# ANDing with 15 (binary 1111) keeps the low four bits of 255, giving 15.
sas_f_band(255, 15)  # SAS 9.4: band(255, 15) = 15

# A missing argument gives a missing result.
sas_f_band(NA_real_, 1)  # SAS 9.4: band(., 1) = .
