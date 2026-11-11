# BETA: each call is followed by the result of the same call in SAS 9.4
sas_f_beta(2, 3)  # SAS: beta(2, 3) -> 0.0833333333333333
sas_f_beta(0.5, 0.5)  # SAS: beta(0.5, 0.5) -> 3.1415926535898
sas_f_beta(1, 1)  # SAS: beta(1, 1) -> 1
sas_f_beta(NA_real_, 1)  # SAS: beta(., 1) -> .
