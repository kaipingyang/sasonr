# PROBNORM: each call is followed by the result of the same call in SAS 9.4
sas_f_probnorm(1.96)  # SAS: probnorm(1.96) -> 0.97500210485178
sas_f_probnorm(0)  # SAS: probnorm(0) -> 0.5
sas_f_probnorm(-10)  # SAS: probnorm(-10) -> 7.619853024161e-24
sas_f_probnorm(NA_real_)  # SAS: probnorm(.) -> .
