# ABS: each call is followed by the result of the same call in SAS 9.4
sas_f_abs(-3.5)  # SAS: abs(-3.5) -> 3.5
sas_f_abs(0)  # SAS: abs(0) -> 0
sas_f_abs(NA_real_)  # SAS: abs(.) -> .
sas_f_abs(sas_missing(".A"))  # SAS: abs(.A) -> .
