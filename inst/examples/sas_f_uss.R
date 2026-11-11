# USS: each call is followed by the result of the same call in SAS 9.4
sas_f_uss(1, 2, 3)  # SAS: uss(1, 2, 3) -> 14
sas_f_uss(1, NA_real_, 2)  # SAS: uss(1, ., 2) -> 5
sas_f_uss(NA_real_, NA_real_)  # SAS: uss(., .) -> .
