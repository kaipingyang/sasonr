# ROUNDZ: each call is followed by the result of the same call in SAS 9.4
sas_f_roundz(2.5)  # SAS: roundz(2.5) -> 2
sas_f_roundz(2.45, 0.1)  # SAS: roundz(2.45, 0.1) -> 2.4
sas_f_roundz(1.005, 0.01)  # SAS: roundz(1.005, 0.01) -> 1
sas_f_roundz(-3.5)  # SAS: roundz(-3.5) -> -4
sas_f_roundz(NA_real_)  # SAS: roundz(.) -> .
