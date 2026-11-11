# EXP: each call is followed by the result of the same call in SAS 9.4
sas_f_exp(1)  # SAS: exp(1) -> 2.71828182845905
sas_f_exp(0)  # SAS: exp(0) -> 1
sas_f_exp(-1)  # SAS: exp(-1) -> 0.367879441171442
sas_f_exp(NA_real_)  # SAS: exp(.) -> .
sas_f_exp(1000)  # SAS: exp(1000) -> .
