# TAN: each call is followed by the result of the same call in SAS 9.4
sas_f_tan(0)  # SAS: tan(0) -> 0
sas_f_tan(1)  # SAS: tan(1) -> 1.5574077246549
sas_f_tan(-0.5)  # SAS: tan(-0.5) -> -0.54630248984379
sas_f_tan(NA_real_)  # SAS: tan(.) -> .
