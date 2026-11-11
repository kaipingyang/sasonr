# MISSING: each call is followed by the result of the same call in SAS 9.4
sas_f_missing(NA_real_)  # SAS: missing(.) -> 1
sas_f_missing(sas_missing(".A"))  # SAS: missing(.A) -> 1
sas_f_missing(0)  # SAS: missing(0) -> 0
sas_f_missing("")  # SAS: missing('') -> 1
sas_f_missing(" a")  # SAS: missing(' a') -> 0
