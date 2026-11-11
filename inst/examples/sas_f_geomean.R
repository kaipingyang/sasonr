# GEOMEAN: each call is followed by the result of the same call in SAS 9.4
sas_f_geomean(1, 2, 4)  # SAS: geomean(1, 2, 4) -> 2
sas_f_geomean(1, NA_real_, 9)  # SAS: geomean(1, ., 9) -> 3
sas_f_geomean(0, 4)  # SAS: geomean(0, 4) -> 0
sas_f_geomean(NA_real_, NA_real_)  # SAS: geomean(., .) -> .
