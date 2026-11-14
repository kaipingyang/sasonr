# CEIL: each call is explained above it and followed by its result

# CEIL returns the smallest integer greater than or equal to the argument: 3.
sas_f_ceil(2.1)  # ceil(2.1) = 3

# For a negative number the ceiling moves towards zero: -2.
sas_f_ceil(-2.1)  # ceil(-2.1) = -2

# CEIL is fuzzed: a value within about 1E-12 of an integer is treated as that
# integer, so the result is 1.
sas_f_ceil(1 + 1e-13)  # ceil(1 + 1e-13) = 1

# A missing argument gives a missing result.
sas_f_ceil(NA_real_)  # ceil(.) = .
