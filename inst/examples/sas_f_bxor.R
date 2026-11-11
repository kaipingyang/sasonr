# BXOR: each call is followed by the result of the same call in SAS 9.4
sas_f_bxor(12, 10)  # SAS: bxor(12, 10) -> 6
sas_f_bxor(5, 5)  # SAS: bxor(5, 5) -> 0
sas_f_bxor(NA_real_, 1)  # SAS: bxor(., 1) -> .
