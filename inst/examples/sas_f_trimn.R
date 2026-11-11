# TRIMN: each call is followed by the result of the same call in SAS 9.4
sas_f_trimn("abc  ")  # SAS: trimn('abc  ') -> "abc"
sas_f_trimn("  a")  # SAS: trimn('  a') -> "  a"
