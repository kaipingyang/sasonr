# PROBT: each call is explained above it and followed by the result of the same call in SAS 9.4

# The t distribution is symmetric about 0, so P(T <= 0) is 0.5.
sas_f_probt(0, 5)  # SAS 9.4: probt(0, 5) = 0.5

# A missing argument gives a missing result.
sas_f_probt(NA_real_, 5)  # SAS 9.4: probt(., 5) = .
