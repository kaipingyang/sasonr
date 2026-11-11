# ATAN: each call is followed by the result of the same call in SAS 9.4
sas_f_atan(1)  # SAS: atan(1) -> 0.785398163397448
sas_f_atan(-1e10)  # SAS: atan(-1e10) -> -1.5707963266949
sas_f_atan(0)  # SAS: atan(0) -> 0
sas_f_atan(NA_real_)  # SAS: atan(.) -> .
