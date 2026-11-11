# MAX: each call is followed by the result of the same call in SAS 9.4
sas_f_max(1, 5, 3)  # SAS: max(1, 5, 3) -> 5
sas_f_max(NA_real_, 2)  # SAS: max(., 2) -> 2
sas_f_max(NA_real_, NA_real_)  # SAS: max(., .) -> .
sas_f_max(sas_missing(".A"), 1)  # SAS: max(.A, 1) -> 1
sas_f_max(-1, sas_missing(".B"))  # SAS: max(-1, .B) -> -1
