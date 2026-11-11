# CEILZ: each call is followed by the result of the same call in SAS 9.4
sas_f_ceilz(2.1)  # SAS: ceilz(2.1) -> 3
sas_f_ceilz(1 + 1e-13)  # SAS: ceilz(1 + 1e-13) -> 2
sas_f_ceilz(-2.5)  # SAS: ceilz(-2.5) -> -2
sas_f_ceilz(NA_real_)  # SAS: ceilz(.) -> .
