# FACT: each call is explained above it and followed by the result of the same call in SAS 9.4

# 5! = 5 * 4 * 3 * 2 * 1 = 120.
sas_f_fact(5)  # SAS 9.4: fact(5) = 120

# 0! is 1 by definition.
sas_f_fact(0)  # SAS 9.4: fact(0) = 1

# 20! is about 2.43E18.
sas_f_fact(20)  # SAS 9.4: fact(20) = 2.43290200817664e+18

# A missing argument gives a missing result.
sas_f_fact(NA_real_)  # SAS 9.4: fact(.) = .
