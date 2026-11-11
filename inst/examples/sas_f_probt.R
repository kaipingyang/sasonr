# PROBT: each call is followed by the result of the same call in SAS 9.4
sas_f_probt(2, 10)  # SAS: probt(2, 10) -> 0.96330598261463
sas_f_probt(0, 5)  # SAS: probt(0, 5) -> 0.5
sas_f_probt(2, 10, 1)  # SAS: probt(2, 10, 1) -> 0.80761156253
sas_f_probt(NA_real_, 5)  # SAS: probt(., 5) -> .
