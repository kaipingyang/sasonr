# DIVIDE: each call is followed by the result of the same call in SAS 9.4
sas_f_divide(1, 2)  # SAS: divide(1, 2) -> 0.5
sas_f_divide(1, 0)  # SAS: divide(1, 0) -> .I
sas_f_divide(-1, 0)  # SAS: divide(-1, 0) -> .M
sas_f_divide(0, 0)  # SAS: divide(0, 0) -> .
sas_f_divide(NA_real_, 2)  # SAS: divide(., 2) -> .
