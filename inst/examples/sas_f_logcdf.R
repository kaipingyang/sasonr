# LOGCDF: each call is explained above it and followed by its result

# LOGCDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The log of the standard normal CDF at 0 is log(0.5).
sas_f_logcdf("NORMAL", 0)  # logcdf('NORMAL', 0) = -0.693147180559945

# Far in the lower tail LOGCDF stays accurate although the CDF itself is only
# about 7.6E-24.
sas_f_logcdf("NORMAL", -10)  # logcdf('NORMAL', -10) = -53.2312851505125

# A missing quantile gives a missing result.
sas_f_logcdf("NORMAL", NA_real_)  # logcdf('NORMAL', .) = .

options(old)
