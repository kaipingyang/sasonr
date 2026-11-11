# NOTALNUM: each call is followed by the result of the same call in SAS 9.4
sas_f_notalnum("ab1!")  # SAS: notalnum('ab1!') -> 4
sas_f_notalnum("abc")  # SAS: notalnum('abc') -> 0
sas_f_notalnum("a!b", -3)  # SAS: notalnum('a!b', -3) -> 2
