# LENGTHC: each call is followed by the result of the same call in SAS 9.4
sas_f_lengthc("abc  ")  # SAS: lengthc('abc  ') -> 5
sas_f_lengthc("")  # SAS: lengthc('') -> 1
sas_f_lengthc("a")  # SAS: lengthc('a') -> 1
