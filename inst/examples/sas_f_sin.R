# SIN: each call is followed by the result of the same call in SAS 9.4
sas_f_sin(0)  # SAS: sin(0) -> 0
sas_f_sin(1)  # SAS: sin(1) -> 0.841470984807897
sas_f_sin(-3.5)  # SAS: sin(-3.5) -> 0.35078322768962
sas_f_sin(NA_real_)  # SAS: sin(.) -> .
