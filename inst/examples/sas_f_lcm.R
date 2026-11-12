# LCM: each call is explained above it and followed by the result of the same call in SAS 9.4

# The least common multiple of 4 and 6 is 12.
sas_f_lcm(4, 6)  # SAS 9.4: lcm(4, 6) = 12

# The least common multiple of 3, 5 and 7 is 105.
sas_f_lcm(3, 5, 7)  # SAS 9.4: lcm(3, 5, 7) = 105

# A missing argument gives a missing result.
sas_f_lcm(4, NA_real_)  # SAS 9.4: lcm(4, .) = .
