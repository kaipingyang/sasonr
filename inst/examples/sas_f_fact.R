# FACT: each call is explained above it and followed by its result

# 5! = 5 * 4 * 3 * 2 * 1 = 120.
sas_f_fact(5)  # fact(5) = 120

# 0! is 1 by definition.
sas_f_fact(0)  # fact(0) = 1

# 20! is about 2.43E18.
sas_f_fact(20)  # fact(20) = 2.43290200817664e+18

# A missing argument gives a missing result.
sas_f_fact(NA_real_)  # fact(.) = .
