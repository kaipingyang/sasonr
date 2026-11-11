# ANYALPHA: each call is followed by the result of the same call in SAS 9.4
sas_f_anyalpha("12ab")  # SAS: anyalpha('12ab') -> 3
sas_f_anyalpha("123")  # SAS: anyalpha('123') -> 0
sas_f_anyalpha("ab12", 3)  # SAS: anyalpha('ab12', 3) -> 0
sas_f_anyalpha("ab12", -4)  # SAS: anyalpha('ab12', -4) -> 2
