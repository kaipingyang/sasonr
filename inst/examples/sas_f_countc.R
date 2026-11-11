# COUNTC: each call is followed by the result of the same call in SAS 9.4
sas_f_countc("banana", "an")  # SAS: countc('banana', 'an') -> 5
sas_f_countc("abc", "x")  # SAS: countc('abc', 'x') -> 0
sas_f_countc("aBc", "b", "i")  # SAS: countc('aBc', 'b', 'i') -> 1
