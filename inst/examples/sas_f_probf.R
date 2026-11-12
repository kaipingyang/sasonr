# PROBF: each call is explained above it and followed by the result of the same call in SAS 9.4

# P(F <= 4) for an F distribution with 2 and 10 degrees of freedom.
sas_f_probf(4, 2, 10)  # SAS 9.4: probf(4, 2, 10) = 0.947077850598655

# A missing argument gives a missing result.
sas_f_probf(NA_real_, 1, 1)  # SAS 9.4: probf(., 1, 1) = .
