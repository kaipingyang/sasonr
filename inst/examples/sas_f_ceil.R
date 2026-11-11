# CEIL: each call is followed by the result of the same call in SAS 9.4
sas_f_ceil(2.1)  # SAS: ceil(2.1) -> 3
sas_f_ceil(-2.1)  # SAS: ceil(-2.1) -> -2
sas_f_ceil(1 + 1e-13)  # SAS: ceil(1 + 1e-13) -> 1
sas_f_ceil(NA_real_)  # SAS: ceil(.) -> .
