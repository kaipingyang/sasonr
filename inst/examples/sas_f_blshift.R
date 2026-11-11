# BLSHIFT: each call is followed by the result of the same call in SAS 9.4
sas_f_blshift(1, 4)  # SAS: blshift(1, 4) -> 16
sas_f_blshift(3, 1)  # SAS: blshift(3, 1) -> 6
sas_f_blshift(NA_real_, 1)  # SAS: blshift(., 1) -> .
