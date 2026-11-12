# VAR: each call is explained above it and followed by the result of the same call in SAS 9.4

# VAR returns the sample variance (divisor n - 1).
sas_f_var(1, 2, 3, 4)  # SAS 9.4: var(1, 2, 3, 4) = 1.66666666666667

# Missing values are ignored: the variance of 5 and 7 is 2.
sas_f_var(5, NA_real_, 7)  # SAS 9.4: var(5, ., 7) = 2

# At least two nonmissing values are needed, so the result is missing.
sas_f_var(5, NA_real_)  # SAS 9.4: var(5, .) = .
