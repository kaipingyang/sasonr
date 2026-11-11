# LENGTH: each call is followed by the result of the same call in SAS 9.4
sas_f_length("abc  ")  # SAS: length('abc  ') -> 3
sas_f_length("")  # SAS: length('') -> 1
sas_f_length("  a")  # SAS: length('  a') -> 3
sas_f_length(123)  # SAS: length(123) -> 12
