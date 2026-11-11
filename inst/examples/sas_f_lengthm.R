# LENGTHM: each call is followed by the result of the same call in SAS 9.4
sas_f_lengthm("abc  ")  # SAS: lengthm('abc  ') -> 5
sas_f_lengthm("")  # SAS: lengthm('') -> 1
sas_f_lengthm("abcdef")  # SAS: lengthm('abcdef') -> 6
