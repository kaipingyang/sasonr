# FLOORZ: each call is followed by the result of the same call in SAS 9.4
sas_f_floorz(2.7)  # SAS: floorz(2.7) -> 2
sas_f_floorz(3 - 1e-13)  # SAS: floorz(3 - 1e-13) -> 2
sas_f_floorz(-2.5)  # SAS: floorz(-2.5) -> -3
sas_f_floorz(NA_real_)  # SAS: floorz(.) -> .
