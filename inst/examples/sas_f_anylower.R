# ANYLOWER: each call is followed by the result of the same call in SAS 9.4
sas_f_anylower("ABc")  # SAS: anylower('ABc') -> 3
sas_f_anylower("ABC")  # SAS: anylower('ABC') -> 0
sas_f_anylower("aBc", -2)  # SAS: anylower('aBc', -2) -> 1
