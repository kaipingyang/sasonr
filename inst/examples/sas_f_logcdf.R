# LOGCDF: each call is explained above it and followed by the result of the same call in SAS 9.4

# The log of the standard normal CDF at 0 is log(0.5).
sas_f_logcdf("NORMAL", 0)  # SAS 9.4: logcdf('NORMAL', 0) = -0.693147180559945

# Far in the lower tail LOGCDF stays accurate although the CDF itself is only
# about 7.6E-24.
sas_f_logcdf("NORMAL", -10)  # SAS 9.4: logcdf('NORMAL', -10) = -53.2312851505125

# A missing quantile gives a missing result.
sas_f_logcdf("NORMAL", NA_real_)  # SAS 9.4: logcdf('NORMAL', .) = .
