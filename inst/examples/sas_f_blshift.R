# BLSHIFT: each call is explained above it and followed by the result of the same call in SAS 9.4

# Shifting 1 left by 4 bits multiplies it by 2 to the power 4, giving 16.
sas_f_blshift(1, 4)  # SAS 9.4: blshift(1, 4) = 16

# Shifting 3 (binary 11) left by one bit gives 6 (binary 110).
sas_f_blshift(3, 1)  # SAS 9.4: blshift(3, 1) = 6

# A missing argument gives a missing result.
sas_f_blshift(NA_real_, 1)  # SAS 9.4: blshift(., 1) = .
