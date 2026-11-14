# GAMMA: each call is explained above it and followed by its result

# For a positive integer n, GAMMA(n) = (n-1)!, so GAMMA(5) = 24.
sas_f_gamma(5)  # gamma(5) = 24

# GAMMA(0.5) equals the square root of pi.
sas_f_gamma(0.5)  # gamma(0.5) = 1.77245385090552

# The gamma function is also defined for negative non-integers.
sas_f_gamma(-1.5)  # gamma(-1.5) = 2.36327180120735

# A missing argument gives a missing result.
sas_f_gamma(NA_real_)  # gamma(.) = .

# The gamma function has a pole at 0, so the result is missing (with a note in
# the log).
sas_f_gamma(0)  # gamma(0) = .
