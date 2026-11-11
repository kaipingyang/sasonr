# SUM: each call is followed by the result of the same call in SAS 9.4
sas_f_sum(1, 2, 3)  # SAS: sum(1, 2, 3) -> 6
sas_f_sum(1, NA_real_, 3)  # SAS: sum(1, ., 3) -> 4
sas_f_sum(NA_real_, NA_real_)  # SAS: sum(., .) -> .
sas_f_sum(sas_missing(".A"), 1)  # SAS: sum(.A, 1) -> 1
