# N: each call is followed by the result of the same call in SAS 9.4
sas_f_n(1, NA_real_, 3)  # SAS: n(1, ., 3) -> 2
sas_f_n(NA_real_, NA_real_)  # SAS: n(., .) -> 0
sas_f_n(sas_missing(".A"), 1)  # SAS: n(.A, 1) -> 1
sas_f_n(0)  # SAS: n(0) -> 1
