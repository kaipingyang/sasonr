# PROBIT: each call is followed by the result of the same call in SAS 9.4
sas_f_probit(0.975)  # SAS: probit(0.975) -> 1.9599639845401
sas_f_probit(0.5)  # SAS: probit(0.5) -> 0
sas_f_probit(1e-10)  # SAS: probit(1e-10) -> -6.3613409024041
sas_f_probit(NA_real_)  # SAS: probit(.) -> .
