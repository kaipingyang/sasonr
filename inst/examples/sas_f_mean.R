# MEAN: each call is followed by the result of the same call in SAS 9.4
sas_f_mean(1, 2, 3, 4)  # SAS: mean(1, 2, 3, 4) -> 2.5
sas_f_mean(1, NA_real_, 3)  # SAS: mean(1, ., 3) -> 2
sas_f_mean(NA_real_, NA_real_)  # SAS: mean(., .) -> .
sas_f_mean(sas_missing(".A"), 4)  # SAS: mean(.A, 4) -> 4
