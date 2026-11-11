# FIND: each call is followed by the result of the same call in SAS 9.4
sas_f_find("abcabc", "bc")  # SAS: find('abcabc', 'bc') -> 2
sas_f_find("abcabc", "BC", "i")  # SAS: find('abcabc', 'BC', 'i') -> 2
sas_f_find("abcabc", "bc", 3)  # SAS: find('abcabc', 'bc', 3) -> 5
sas_f_find("abcabc", "bc", -4)  # SAS: find('abcabc', 'bc', -4) -> 2
sas_f_find("abc", "x")  # SAS: find('abc', 'x') -> 0
