# WHICHC: each call is followed by the result of the same call in SAS 9.4
sas_f_whichc("b", "a", "b", "c")  # SAS: whichc('b', 'a', 'b', 'c') -> 2
sas_f_whichc("x", "a", "b")  # SAS: whichc('x', 'a', 'b') -> 0
sas_f_whichc("a ", "a")  # SAS: whichc('a ', 'a') -> 1
