# TANH: each call is explained above it and followed by the result of the same call in SAS 9.4

# The hyperbolic tangent of 0 is 0.
sas_f_tanh(0)  # SAS 9.4: tanh(0) = 0

# The hyperbolic tangent of 1.
sas_f_tanh(1)  # SAS 9.4: tanh(1) = 0.761594155955765

# The hyperbolic tangent approaches 1 for large arguments; at 20 it equals 1 in
# double precision.
sas_f_tanh(20)  # SAS 9.4: tanh(20) = 1

# A missing argument gives a missing result.
sas_f_tanh(NA_real_)  # SAS 9.4: tanh(.) = .
