# SQRT: each call is explained above it and followed by its result

# The square root of 16 is 4.
sas_f_sqrt(16)  # sqrt(16) = 4

# The square root of 2.
sas_f_sqrt(2)  # sqrt(2) = 1.4142135623731

# The square root of a negative number is undefined, so the result is missing
# (with a note in the log).
sas_f_sqrt(-1)  # sqrt(-1) = .

# A missing argument gives a missing result.
sas_f_sqrt(NA_real_)  # sqrt(.) = .
