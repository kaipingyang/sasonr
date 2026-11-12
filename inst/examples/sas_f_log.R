# LOG: each call is explained above it and followed by the result of the same call in SAS 9.4

# The natural logarithm of 10.
sas_f_log(10)  # SAS 9.4: log(10) = 2.30258509299405

# The logarithm of 1 is 0.
sas_f_log(1)  # SAS 9.4: log(1) = 0

# The logarithm of 0 is undefined, so the result is missing (with a note in the
# log).
sas_f_log(0)  # SAS 9.4: log(0) = .

# The logarithm of a negative number is undefined, so the result is missing
# (with a note in the log).
sas_f_log(-1)  # SAS 9.4: log(-1) = .

# A missing argument gives a missing result.
sas_f_log(NA_real_)  # SAS 9.4: log(.) = .
