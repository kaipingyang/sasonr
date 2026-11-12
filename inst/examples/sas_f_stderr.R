# STDERR: each call is explained above it and followed by the result of the same call in SAS 9.4

# STDERR returns the standard error of the mean, the standard deviation divided
# by sqrt(n).
sas_f_stderr(1, 2, 3, 4)  # SAS 9.4: stderr(1, 2, 3, 4) = 0.645497224367903

# The standard deviation of 2 and 4 is sqrt(2), divided by sqrt(2) gives 1.
sas_f_stderr(2, 4)  # SAS 9.4: stderr(2, 4) = 1

# At least two nonmissing values are needed, so the result is missing.
sas_f_stderr(5, NA_real_)  # SAS 9.4: stderr(5, .) = .
