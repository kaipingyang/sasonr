# COUNTW: each call is followed by the result of the same call in SAS 9.4
sas_f_countw("a b  c")  # SAS: countw('a b  c') -> 3
sas_f_countw("a,b,,c", ",")  # SAS: countw('a,b,,c', ',') -> 3
sas_f_countw("a,b,,c", ",", "m")  # SAS: countw('a,b,,c', ',', 'm') -> 4
sas_f_countw("")  # SAS: countw('') -> 0
