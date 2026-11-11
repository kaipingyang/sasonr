# ROUNDE: each call is followed by the result of the same call in SAS 9.4
sas_f_rounde(2.5)  # SAS: rounde(2.5) -> 2
sas_f_rounde(3.5)  # SAS: rounde(3.5) -> 4
sas_f_rounde(-2.5)  # SAS: rounde(-2.5) -> -2
sas_f_rounde(2.45, 0.1)  # SAS: rounde(2.45, 0.1) -> 2.4
sas_f_rounde(NA_real_)  # SAS: rounde(.) -> .
