# FINDC: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_findc("abc", "cb")  # SAS: findc('abc', 'cb') -> 2
sas_f_findc("abc", "x")  # SAS: findc('abc', 'x') -> 0
sas_f_findc("abc", "B", "i")  # SAS: findc('abc', 'B', 'i') -> 2
sas_f_findc("abc", "a", "k")  # SAS: findc('abc', 'a', 'k') -> 2
sas_f_findc("ab1", "", "d")  # SAS: findc('ab1', '', 'd') -> 3
