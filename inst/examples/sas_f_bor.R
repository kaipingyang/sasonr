# BOR: each call is followed by the result of the same call in SAS 9.4
sas_f_bor(12, 10)  # SAS: bor(12, 10) -> 14
sas_f_bor(0, 0)  # SAS: bor(0, 0) -> 0
sas_f_bor(NA_real_, 1)  # SAS: bor(., 1) -> .
