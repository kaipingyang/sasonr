# FINV: each call is explained above it and followed by the result of the same call in SAS 9.4

# A missing probability gives a missing result.
sas_f_finv(NA_real_, 1, 1)  # SAS 9.4: finv(., 1, 1) = .
