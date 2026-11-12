# CINV: each call is explained above it and followed by the result of the same call in SAS 9.4

# A missing probability gives a missing result.
sas_f_cinv(NA_real_, 2)  # SAS 9.4: cinv(., 2) = .
