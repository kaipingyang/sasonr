# NOTUPPER: each call is followed by the result of the same call in SAS 9.4
sas_f_notupper("ABc")  # SAS: notupper('ABc') -> 3
sas_f_notupper("ABC")  # SAS: notupper('ABC') -> 0
sas_f_notupper("aBC", -3)  # SAS: notupper('aBC', -3) -> 1
