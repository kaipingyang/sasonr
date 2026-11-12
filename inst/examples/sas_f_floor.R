# FLOOR: each call is explained above it and followed by the result of the same call in SAS 9.4

# The largest integer less than or equal to 2.7 is 2.
sas_f_floor(2.7)  # SAS 9.4: floor(2.7) = 2

# For a negative number the floor moves away from zero: -3.
sas_f_floor(-2.1)  # SAS 9.4: floor(-2.1) = -3

# FLOOR is fuzzed: a value within about 1E-12 below an integer is treated as
# that integer, so the result is 3.
sas_f_floor(3 - 1e-13)  # SAS 9.4: floor(3 - 1e-13) = 3

# A missing argument gives a missing result.
sas_f_floor(NA_real_)  # SAS 9.4: floor(.) = .
