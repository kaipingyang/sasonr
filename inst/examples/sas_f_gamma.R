# GAMMA: each call is followed by the result of the same call in SAS 9.4
sas_f_gamma(5)  # SAS: gamma(5) -> 24
sas_f_gamma(0.5)  # SAS: gamma(0.5) -> 1.77245385090552
sas_f_gamma(-1.5)  # SAS: gamma(-1.5) -> 2.36327180120735
sas_f_gamma(NA_real_)  # SAS: gamma(.) -> .
sas_f_gamma(0)  # SAS: gamma(0) -> .
