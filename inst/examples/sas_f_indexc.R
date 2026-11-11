# INDEXC: each call is followed by the result of the same call in SAS 9.4
sas_f_indexc("abcdef", "fd")  # SAS: indexc('abcdef', 'fd') -> 4
sas_f_indexc("ABC", "b")  # SAS: indexc('ABC', 'b') -> 0
