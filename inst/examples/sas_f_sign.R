# SIGN: each call is followed by the result of the same call in SAS 9.4
sas_f_sign(-3)  # SAS: sign(-3) -> -1
sas_f_sign(0)  # SAS: sign(0) -> 0
sas_f_sign(2.5)  # SAS: sign(2.5) -> 1
sas_f_sign(NA_real_)  # SAS: sign(.) -> .
