# MOD: each call is followed by the result of the same call in SAS 9.4
sas_f_mod(10, 3)  # SAS: mod(10, 3) -> 1
sas_f_mod(-10, 3)  # SAS: mod(-10, 3) -> -1
sas_f_mod(10, -3)  # SAS: mod(10, -3) -> 1
sas_f_mod(5.5, 2)  # SAS: mod(5.5, 2) -> 1.5
sas_f_mod(0.3, 0.1)  # SAS: mod(0.3, 0.1) -> 0
sas_f_mod(10, 0)  # SAS: mod(10, 0) -> .
