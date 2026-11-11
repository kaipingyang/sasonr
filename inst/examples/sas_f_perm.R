# PERM: each call is followed by the result of the same call in SAS 9.4
sas_f_perm(5, 2)  # SAS: perm(5, 2) -> 20
sas_f_perm(5)  # SAS: perm(5) -> 120
sas_f_perm(10, 0)  # SAS: perm(10, 0) -> 1
sas_f_perm(NA_real_, 2)  # SAS: perm(., 2) -> .
