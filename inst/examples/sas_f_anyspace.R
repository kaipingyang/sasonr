# ANYSPACE: each call is followed by the result of the same call in SAS 9.4
sas_f_anyspace("ab c")  # SAS: anyspace('ab c') -> 3
sas_f_anyspace("abc")  # SAS: anyspace('abc') -> 0
sas_f_anyspace("a b c", 3)  # SAS: anyspace('a b c', 3) -> 4
