# TINV: each call is explained above it and followed by its result

# The t distribution is symmetric about 0, so its median is 0.
sas_f_tinv(0.5, 5)  # tinv(0.5, 5) = 0

# A missing probability gives a missing result.
sas_f_tinv(NA_real_, 5)  # tinv(., 5) = .
