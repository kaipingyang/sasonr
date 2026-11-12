# BNOT: each call is explained above it and followed by the result of the same call in SAS 9.4

# BNOT flips all 32 bits, so the complement of 0 is 2 to the power 32 minus 1 =
# 4294967295.
sas_f_bnot(0)  # SAS 9.4: bnot(0) = 4294967295

# The complement of a value with all 32 bits set is 0.
sas_f_bnot(4294967295)  # SAS 9.4: bnot(4294967295) = 0

# A missing argument gives a missing result.
sas_f_bnot(NA_real_)  # SAS 9.4: bnot(.) = .
