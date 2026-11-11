# ARSIN: each call is followed by the result of the same call in SAS 9.4
sas_f_arsin(0.5)  # SAS: arsin(0.5) -> 0.523598775598299
sas_f_arsin(-1)  # SAS: arsin(-1) -> -1.5707963267949
sas_f_arsin(NA_real_)  # SAS: arsin(.) -> .
sas_f_arsin(1.5)  # SAS: arsin(1.5) -> .
