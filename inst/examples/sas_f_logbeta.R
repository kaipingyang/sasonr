# LOGBETA: each call is followed by the result of the same call in SAS 9.4
sas_f_logbeta(2, 3)  # SAS: logbeta(2, 3) -> -2.484906649788
sas_f_logbeta(0.5, 0.5)  # SAS: logbeta(0.5, 0.5) -> 1.1447298858494
sas_f_logbeta(100, 100)  # SAS: logbeta(100, 100) -> -139.66525908671
sas_f_logbeta(NA_real_, 1)  # SAS: logbeta(., 1) -> .
