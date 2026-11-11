# SQRT: each call is followed by the result of the same call in SAS 9.4
sas_f_sqrt(16)  # SAS: sqrt(16) -> 4
sas_f_sqrt(2)  # SAS: sqrt(2) -> 1.4142135623731
sas_f_sqrt(-1)  # SAS: sqrt(-1) -> .
sas_f_sqrt(NA_real_)  # SAS: sqrt(.) -> .
