# COS: each call is followed by the result of the same call in SAS 9.4
sas_f_cos(0)  # SAS: cos(0) -> 1
sas_f_cos(1)  # SAS: cos(1) -> 0.54030230586814
sas_f_cos(-3.5)  # SAS: cos(-3.5) -> -0.936456687290796
sas_f_cos(NA_real_)  # SAS: cos(.) -> .
