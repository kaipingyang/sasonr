# PROBNORM: each call is explained above it and followed by its result

# The standard normal CDF at 1.96 is about 0.975.
sas_f_probnorm(1.96)  # probnorm(1.96) = 0.97500210485178

# The standard normal CDF at 0 is 0.5.
sas_f_probnorm(0)  # probnorm(0) = 0.5

# A missing argument gives a missing result.
sas_f_probnorm(NA_real_)  # probnorm(.) = .
