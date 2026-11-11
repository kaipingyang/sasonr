# GCD: each call is followed by the result of the same call in SAS 9.4
sas_f_gcd(12, 18)  # SAS: gcd(12, 18) -> 6
sas_f_gcd(7, 13)  # SAS: gcd(7, 13) -> 1
sas_f_gcd(12, 18, 8)  # SAS: gcd(12, 18, 8) -> 2
sas_f_gcd(12, NA_real_)  # SAS: gcd(12, .) -> .
