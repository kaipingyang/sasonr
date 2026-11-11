# NOTPUNCT: each call is followed by the result of the same call in SAS 9.4
sas_f_notpunct("!!a")  # SAS: notpunct('!!a') -> 3
sas_f_notpunct("!?")  # SAS: notpunct('!?') -> 0
sas_f_notpunct("a!b", 2)  # SAS: notpunct('a!b', 2) -> 3
