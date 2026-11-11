# LOGCDF: each call is followed by the result of the same call in SAS 9.4
sas_f_logcdf("NORMAL", 0)  # SAS: logcdf('NORMAL', 0) -> -0.693147180559945
sas_f_logcdf("NORMAL", -10)  # SAS: logcdf('NORMAL', -10) -> -53.2312851505125
sas_f_logcdf("POISSON", 2, 1)  # SAS: logcdf('POISSON', 2, 1) -> -0.083709268125845
sas_f_logcdf("NORMAL", NA_real_)  # SAS: logcdf('NORMAL', .) -> .
