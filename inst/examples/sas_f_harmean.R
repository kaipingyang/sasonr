# HARMEAN: each call is explained above it and followed by its result

# The harmonic mean is n divided by the sum of reciprocals: 3 / (1 + 0.5 +
# 0.25).
sas_f_harmean(1, 2, 4)  # harmean(1, 2, 4) = 1.71428571428571

# Missing values are ignored: the harmonic mean of 2 and 2 is 2.
sas_f_harmean(2, NA_real_, 2)  # harmean(2, ., 2) = 2

# The harmonic mean of equal values is that value.
sas_f_harmean(1, 1)  # harmean(1, 1) = 1

# All arguments are missing, so the result is missing.
sas_f_harmean(NA_real_, NA_real_)  # harmean(., .) = .
