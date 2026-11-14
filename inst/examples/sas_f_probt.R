# PROBT: each call is explained above it and followed by its result

# P(T <= 2) for a t distribution with 10 degrees of freedom.
sas_f_probt(2, 10)  # probt(2, 10) = 0.96330598261463

# The t distribution is symmetric about 0, so P(T <= 0) is 0.5.
sas_f_probt(0, 5)  # probt(0, 5) = 0.5

# A missing argument gives a missing result.
sas_f_probt(NA_real_, 5)  # probt(., 5) = .
