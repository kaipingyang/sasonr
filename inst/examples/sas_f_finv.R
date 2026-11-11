# FINV: each call is followed by the result of the same call in SAS 9.4
sas_f_finv(0.95, 2, 10)  # SAS: finv(0.95, 2, 10) -> 4.1028210151304
sas_f_finv(0.5, 1, 1)  # SAS: finv(0.5, 1, 1) -> 1
sas_f_finv(NA_real_, 1, 1)  # SAS: finv(., 1, 1) -> .
