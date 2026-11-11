# PCTL: each call is followed by the result of the same call in SAS 9.4
sas_f_pctl(50, 1, 2, 3, 4)  # SAS: pctl(50, 1, 2, 3, 4) -> 2.5
sas_f_pctl(25, 1, 2, 3, 4)  # SAS: pctl(25, 1, 2, 3, 4) -> 1.5
sas_f_pctl(100, 5, NA_real_, 9)  # SAS: pctl(100, 5, ., 9) -> 9
sas_f_pctl(10, 7)  # SAS: pctl(10, 7) -> 7
