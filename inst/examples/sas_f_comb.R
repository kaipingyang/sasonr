# COMB: each call is followed by the result of the same call in SAS 9.4
sas_f_comb(5, 2)  # SAS: comb(5, 2) -> 10
sas_f_comb(10, 0)  # SAS: comb(10, 0) -> 1
sas_f_comb(52, 5)  # SAS: comb(52, 5) -> 2598960
sas_f_comb(NA_real_, 2)  # SAS: comb(., 2) -> .
