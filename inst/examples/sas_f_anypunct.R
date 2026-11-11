# ANYPUNCT: each call is followed by the result of the same call in SAS 9.4
sas_f_anypunct("ab,c")  # SAS: anypunct('ab,c') -> 3
sas_f_anypunct("abc")  # SAS: anypunct('abc') -> 0
sas_f_anypunct("a!b?", 3)  # SAS: anypunct('a!b?', 3) -> 4
