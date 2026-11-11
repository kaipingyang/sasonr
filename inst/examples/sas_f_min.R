# MIN: each call is followed by the result of the same call in SAS 9.4
sas_f_min(1, 5, 3)  # SAS: min(1, 5, 3) -> 1
sas_f_min(NA_real_, 2)  # SAS: min(., 2) -> 2
sas_f_min(NA_real_, NA_real_)  # SAS: min(., .) -> .
sas_f_min(sas_missing(".A"), 1)  # SAS: min(.A, 1) -> 1
