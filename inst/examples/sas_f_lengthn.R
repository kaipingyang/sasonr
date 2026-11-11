# LENGTHN: each call is followed by the result of the same call in SAS 9.4
sas_f_lengthn("abc  ")  # SAS: lengthn('abc  ') -> 3
sas_f_lengthn("")  # SAS: lengthn('') -> 0
sas_f_lengthn("  a")  # SAS: lengthn('  a') -> 3
