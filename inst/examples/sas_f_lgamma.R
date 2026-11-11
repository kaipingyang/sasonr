# LGAMMA: each call is followed by the result of the same call in SAS 9.4
sas_f_lgamma(10)  # SAS: lgamma(10) -> 12.8018274800815
sas_f_lgamma(0.5)  # SAS: lgamma(0.5) -> 0.5723649429247
sas_f_lgamma(100)  # SAS: lgamma(100) -> 359.134205369575
sas_f_lgamma(NA_real_)  # SAS: lgamma(.) -> .
