# BNOT: each call is followed by the result of the same call in SAS 9.4
sas_f_bnot(0)  # SAS: bnot(0) -> 4294967295
sas_f_bnot(4294967295)  # SAS: bnot(4294967295) -> 0
sas_f_bnot(NA_real_)  # SAS: bnot(.) -> .
