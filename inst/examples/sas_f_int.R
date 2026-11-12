# INT: each call is explained above it and followed by the result of the same call in SAS 9.4

# INT returns the integer part, dropping the fraction: 2.
sas_f_int(2.7)  # SAS 9.4: int(2.7) = 2

# INT truncates towards zero, so -2.7 gives -2.
sas_f_int(-2.7)  # SAS 9.4: int(-2.7) = -2

# INT is fuzzed: a value within about 1E-12 of an integer gives that integer,
# 3.
sas_f_int(3 - 1e-13)  # SAS 9.4: int(3 - 1e-13) = 3

# A missing argument gives a missing result.
sas_f_int(NA_real_)  # SAS 9.4: int(.) = .
