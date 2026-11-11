# BRSHIFT: each call is followed by the result of the same call in SAS 9.4
sas_f_brshift(16, 2)  # SAS: brshift(16, 2) -> 4
sas_f_brshift(255, 4)  # SAS: brshift(255, 4) -> 15
sas_f_brshift(NA_real_, 1)  # SAS: brshift(., 1) -> .
