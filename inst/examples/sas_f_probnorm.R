# PROBNORM: each call is explained above it and followed by the result of the same call in SAS 9.4

# The standard normal CDF at 1.96 is about 0.975.
sas_f_probnorm(1.96)  # SAS 9.4: probnorm(1.96) = 0.97500210485178

# The standard normal CDF at 0 is 0.5.
sas_f_probnorm(0)  # SAS 9.4: probnorm(0) = 0.5

# A missing argument gives a missing result.
sas_f_probnorm(NA_real_)  # SAS 9.4: probnorm(.) = .
