# PERM: each call is explained above it and followed by its result

# The number of ordered arrangements of 2 items out of 5 is 5 * 4 = 20.
sas_f_perm(5, 2)  # perm(5, 2) = 20

# With r omitted, PERM(n) is n! = 120.
sas_f_perm(5)  # perm(5) = 120

# There is exactly one way to arrange 0 items.
sas_f_perm(10, 0)  # perm(10, 0) = 1

# A missing argument gives a missing result.
sas_f_perm(NA_real_, 2)  # perm(., 2) = .
