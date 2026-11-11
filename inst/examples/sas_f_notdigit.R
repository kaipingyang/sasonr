# NOTDIGIT: each call is followed by the result of the same call in SAS 9.4
sas_f_notdigit("12a")  # SAS: notdigit('12a') -> 3
sas_f_notdigit("123")  # SAS: notdigit('123') -> 0
sas_f_notdigit("1a2b", -3)  # SAS: notdigit('1a2b', -3) -> 2
