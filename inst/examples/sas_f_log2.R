# LOG2: each call is explained above it and followed by the result of the same call in SAS 9.4

# The base-2 logarithm of 8 is 3.
sas_f_log2(8)  # SAS 9.4: log2(8) = 3

# The base-2 logarithm of 10.
sas_f_log2(10)  # SAS 9.4: log2(10) = 3.32192809488736

# The base-2 logarithm of 0.5 is -1.
sas_f_log2(0.5)  # SAS 9.4: log2(0.5) = -1

# The logarithm of a negative number is undefined, so the result is missing
# (with a note in the log).
sas_f_log2(-1)  # SAS 9.4: log2(-1) = .
