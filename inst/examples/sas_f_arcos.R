# ARCOS: each call is followed by the result of the same call in SAS 9.4
sas_f_arcos(1)  # SAS: arcos(1) -> 0
sas_f_arcos(0)  # SAS: arcos(0) -> 1.5707963267949
sas_f_arcos(0.5)  # SAS: arcos(0.5) -> 1.0471975511966
sas_f_arcos(NA_real_)  # SAS: arcos(.) -> .
sas_f_arcos(2)  # SAS: arcos(2) -> .
