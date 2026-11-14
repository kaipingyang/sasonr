# EXP: each call is explained above it and followed by its result

# exp(1) is the constant e.
sas_f_exp(1)  # exp(1) = 2.71828182845905

# exp(0) is 1.
sas_f_exp(0)  # exp(0) = 1

# exp(-1) is 1/e.
sas_f_exp(-1)  # exp(-1) = 0.367879441171442

# A missing argument gives a missing result.
sas_f_exp(NA_real_)  # exp(.) = .

# exp(1000) overflows the largest double-precision number, so the result is
# missing (with a note in the log).
sas_f_exp(1000)  # exp(1000) = .
