# LCM: each call is followed by the result of the same call in SAS 9.4
sas_f_lcm(4, 6)  # SAS: lcm(4, 6) -> 12
sas_f_lcm(3, 5, 7)  # SAS: lcm(3, 5, 7) -> 105
sas_f_lcm(4, NA_real_)  # SAS: lcm(4, .) -> .
