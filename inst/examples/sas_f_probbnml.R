# PROBBNML: each call is explained above it and followed by the result of the same call in SAS 9.4

# P(X <= n) is always 1.
sas_f_probbnml(0.2, 5, 5)  # SAS 9.4: probbnml(0.2, 5, 5) = 1

# A missing probability gives a missing result.
sas_f_probbnml(NA_real_, 5, 1)  # SAS 9.4: probbnml(., 5, 1) = .
