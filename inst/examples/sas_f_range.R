# RANGE: each call is followed by the result of the same call in SAS 9.4
sas_f_range(1, 5, 3)  # SAS: range(1, 5, 3) -> 4
sas_f_range(NA_real_, 2)  # SAS: range(., 2) -> 0
sas_f_range(NA_real_, NA_real_)  # SAS: range(., .) -> .
sas_f_range(-1, 1)  # SAS: range(-1, 1) -> 2
