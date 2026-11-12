# LOG10: each call is explained above it and followed by the result of the same call in SAS 9.4

# The base-10 logarithm of 1000 is 3.
sas_f_log10(1000)  # SAS 9.4: log10(1000) = 3

# The base-10 logarithm of 2.
sas_f_log10(2)  # SAS 9.4: log10(2) = 0.301029995663981

# The base-10 logarithm of 0.001 is -3.
sas_f_log10(0.001)  # SAS 9.4: log10(0.001) = -3

# The logarithm of 0 is undefined, so the result is missing (with a note in the
# log).
sas_f_log10(0)  # SAS 9.4: log10(0) = .
