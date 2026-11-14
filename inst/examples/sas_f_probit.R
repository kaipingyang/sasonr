# PROBIT: each call is explained above it and followed by its result

# The probability 0.5 maps to 0.
sas_f_probit(0.5)  # probit(0.5) = 0

# A missing argument gives a missing result.
sas_f_probit(NA_real_)  # probit(.) = .
