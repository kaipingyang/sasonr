# CINV: each call is followed by the result of the same call in SAS 9.4
sas_f_cinv(0.95, 3)  # SAS: cinv(0.95, 3) -> 7.81472790325
sas_f_cinv(0.5, 1)  # SAS: cinv(0.5, 1) -> 0.45493642312
sas_f_cinv(0.95, 3, 2)  # SAS: cinv(0.95, 3, 2) -> 12.28375372313
sas_f_cinv(NA_real_, 2)  # SAS: cinv(., 2) -> .
