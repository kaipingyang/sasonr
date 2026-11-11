# FLOOR: each call is followed by the result of the same call in SAS 9.4
sas_f_floor(2.7)  # SAS: floor(2.7) -> 2
sas_f_floor(-2.1)  # SAS: floor(-2.1) -> -3
sas_f_floor(3 - 1e-13)  # SAS: floor(3 - 1e-13) -> 3
sas_f_floor(NA_real_)  # SAS: floor(.) -> .
