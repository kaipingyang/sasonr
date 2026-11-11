# INTZ: each call is followed by the result of the same call in SAS 9.4
sas_f_intz(2.7)  # SAS: intz(2.7) -> 2
sas_f_intz(-2.7)  # SAS: intz(-2.7) -> -2
sas_f_intz(3 - 1e-13)  # SAS: intz(3 - 1e-13) -> 2
sas_f_intz(NA_real_)  # SAS: intz(.) -> .
