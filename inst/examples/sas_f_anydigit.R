# ANYDIGIT: each call is followed by the result of the same call in SAS 9.4
sas_f_anydigit("ab12")  # SAS: anydigit('ab12') -> 3
sas_f_anydigit("abc")  # SAS: anydigit('abc') -> 0
sas_f_anydigit("1a2", 2)  # SAS: anydigit('1a2', 2) -> 3
sas_f_anydigit("1a2", -2)  # SAS: anydigit('1a2', -2) -> 1
