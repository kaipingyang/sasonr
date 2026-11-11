# NOTALPHA: each call is followed by the result of the same call in SAS 9.4
sas_f_notalpha("ab1")  # SAS: notalpha('ab1') -> 3
sas_f_notalpha("abc")  # SAS: notalpha('abc') -> 0
sas_f_notalpha("a1b", 3)  # SAS: notalpha('a1b', 3) -> 0
