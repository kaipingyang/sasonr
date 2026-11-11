# TINV: each call is followed by the result of the same call in SAS 9.4
sas_f_tinv(0.975, 10)  # SAS: tinv(0.975, 10) -> 2.2281388519863
sas_f_tinv(0.5, 5)  # SAS: tinv(0.5, 5) -> 0
sas_f_tinv(0.975, 10, 1)  # SAS: tinv(0.975, 10, 1) -> 3.540505535
sas_f_tinv(NA_real_, 5)  # SAS: tinv(., 5) -> .
