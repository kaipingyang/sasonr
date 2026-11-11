# INT: each call is followed by the result of the same call in SAS 9.4
sas_f_int(2.7)  # SAS: int(2.7) -> 2
sas_f_int(-2.7)  # SAS: int(-2.7) -> -2
sas_f_int(3 - 1e-13)  # SAS: int(3 - 1e-13) -> 3
sas_f_int(NA_real_)  # SAS: int(.) -> .
