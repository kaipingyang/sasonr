# BRSHIFT: each call is explained above it and followed by its result

# Shifting 16 right by 2 bits divides it by 4, giving 4.
sas_f_brshift(16, 2)  # brshift(16, 2) = 4

# Shifting 255 right by 4 bits drops the low four bits, giving 15.
sas_f_brshift(255, 4)  # brshift(255, 4) = 15

# A missing argument gives a missing result.
sas_f_brshift(NA_real_, 1)  # brshift(., 1) = .
