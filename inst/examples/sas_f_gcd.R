# GCD: each call is explained above it and followed by the result of the same call in SAS 9.4

# The greatest common divisor of 12 and 18 is 6.
sas_f_gcd(12, 18)  # SAS 9.4: gcd(12, 18) = 6

# 7 and 13 have no common divisor other than 1.
sas_f_gcd(7, 13)  # SAS 9.4: gcd(7, 13) = 1

# GCD accepts any number of arguments: the greatest common divisor of 12, 18
# and 8 is 2.
sas_f_gcd(12, 18, 8)  # SAS 9.4: gcd(12, 18, 8) = 2

# A missing argument gives a missing result.
sas_f_gcd(12, NA_real_)  # SAS 9.4: gcd(12, .) = .
