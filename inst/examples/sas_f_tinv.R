# TINV: each call is explained above it and followed by the result of the same call in SAS 9.4

# The t distribution is symmetric about 0, so its median is 0.
sas_f_tinv(0.5, 5)  # SAS 9.4: tinv(0.5, 5) = 0

# A missing probability gives a missing result.
sas_f_tinv(NA_real_, 5)  # SAS 9.4: tinv(., 5) = .
