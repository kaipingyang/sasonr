# HARMEAN: each call is followed by the result of the same call in SAS 9.4
sas_f_harmean(1, 2, 4)  # SAS: harmean(1, 2, 4) -> 1.71428571428571
sas_f_harmean(2, NA_real_, 2)  # SAS: harmean(2, ., 2) -> 2
sas_f_harmean(1, 1)  # SAS: harmean(1, 1) -> 1
sas_f_harmean(NA_real_, NA_real_)  # SAS: harmean(., .) -> .
